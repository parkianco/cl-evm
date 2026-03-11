;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; gas.lisp - Gas Metering and Cost Calculations
;;;;
;;;; Implements EVM gas metering per Ethereum Yellow Paper and
;;;; EIP-2929 (Berlin), EIP-3529 (London), EIP-1559 (London).

(in-package #:cl-evm)

;;; ==========================================================================
;;; Gas Constants
;;; ==========================================================================

;; Base costs
(defconstant +gas-zero+ 0)
(defconstant +gas-base+ 2)
(defconstant +gas-very-low+ 3)
(defconstant +gas-low+ 5)
(defconstant +gas-mid+ 8)
(defconstant +gas-high+ 10)

;; Memory operations
(defconstant +gas-memory+ 3)
(defconstant +gas-copy+ 3)

;; Storage operations (EIP-2929 Berlin)
(defconstant +gas-cold-sload+ 2100)
(defconstant +gas-cold-account-access+ 2600)
(defconstant +gas-warm-access+ 100)
(defconstant +gas-sstore-set+ 20000)
(defconstant +gas-sstore-reset+ 2900)
(defconstant +gas-sstore-clear-refund+ 4800)  ; EIP-3529

;; Call operations
(defconstant +gas-call-stipend+ 2300)
(defconstant +gas-call-value+ 9000)
(defconstant +gas-new-account+ 25000)

;; SHA3
(defconstant +gas-sha3+ 30)
(defconstant +gas-sha3-word+ 6)

;; Create
(defconstant +gas-create+ 32000)
(defconstant +gas-code-deposit+ 200)

;; Transaction
(defconstant +gas-tx-base+ 21000)
(defconstant +gas-tx-create+ 32000)
(defconstant +gas-tx-data-zero+ 4)
(defconstant +gas-tx-data-nonzero+ 16)  ; EIP-2028

;; Log
(defconstant +gas-log+ 375)
(defconstant +gas-log-topic+ 375)
(defconstant +gas-log-data+ 8)

;; EXP
(defconstant +gas-exp+ 10)
(defconstant +gas-exp-byte+ 50)  ; Per byte of exponent

;; SELFDESTRUCT
(defconstant +gas-selfdestruct+ 5000)

;;; ==========================================================================
;;; Access State (EIP-2929)
;;; ==========================================================================

(defstruct access-state
  "Tracks warm/cold access for addresses and storage slots per EIP-2929."
  (warm-addresses (make-hash-table :test 'equal) :type hash-table)
  (warm-slots (make-hash-table :test 'equal) :type hash-table))

(defun access-state-address-warm-p (state address)
  "Check if an address has been accessed (is warm)."
  (gethash address (access-state-warm-addresses state)))

(defun access-state-mark-address-warm (state address)
  "Mark an address as warm (accessed)."
  (setf (gethash address (access-state-warm-addresses state)) t))

(defun access-state-slot-warm-p (state address slot)
  "Check if a storage slot has been accessed (is warm)."
  (let ((key (cons address slot)))
    (gethash key (access-state-warm-slots state))))

(defun access-state-mark-slot-warm (state address slot)
  "Mark a storage slot as warm (accessed)."
  (let ((key (cons address slot)))
    (setf (gethash key (access-state-warm-slots state)) t)))

;;; ==========================================================================
;;; Gas Meter Structure
;;; ==========================================================================

(defstruct gas-meter
  "Tracks gas usage during EVM execution."
  (limit 0 :type (unsigned-byte 64))
  (used 0 :type (unsigned-byte 64))
  (refund 0 :type (integer 0))
  (access-state (make-access-state) :type access-state))

(defun gas-meter-remaining (meter)
  "Get remaining gas."
  (- (gas-meter-limit meter) (gas-meter-used meter)))

;;; ==========================================================================
;;; Gas Charging
;;; ==========================================================================

(define-condition out-of-gas-error (error)
  ((required :initarg :required :reader out-of-gas-required)
   (available :initarg :available :reader out-of-gas-available))
  (:report (lambda (c s)
             (format s "Out of gas: required ~D, available ~D"
                     (out-of-gas-required c)
                     (out-of-gas-available c)))))

(defun charge-gas (meter amount)
  "Charge gas from the meter. Signals OUT-OF-GAS-ERROR if insufficient."
  (let ((remaining (gas-meter-remaining meter)))
    (when (> amount remaining)
      (error 'out-of-gas-error :required amount :available remaining))
    (incf (gas-meter-used meter) amount)
    t))

(defun check-gas (meter amount)
  "Check if there is enough gas without charging."
  (<= amount (gas-meter-remaining meter)))

(defun refund-gas (meter amount)
  "Add to the refund counter."
  (incf (gas-meter-refund meter) amount))

(defun finalize-gas-refund (meter)
  "Apply refunds at end of transaction. Capped at used/5 per EIP-3529."
  (let* ((max-refund (floor (gas-meter-used meter) 5))
         (actual-refund (min (gas-meter-refund meter) max-refund)))
    actual-refund))

;;; ==========================================================================
;;; Gas Cost Calculations
;;; ==========================================================================

(defun calculate-memory-expansion-cost (current-size new-size)
  "Calculate gas cost for memory expansion."
  (if (<= new-size current-size)
      0
      (let* ((current-words (ceiling current-size 32))
             (new-words (ceiling new-size 32))
             (current-cost (+ (* +gas-memory+ current-words)
                             (floor (* current-words current-words) 512)))
             (new-cost (+ (* +gas-memory+ new-words)
                         (floor (* new-words new-words) 512))))
        (- new-cost current-cost))))

(defun calculate-sha3-cost (length)
  "Calculate gas cost for SHA3 (Keccak256) operation."
  (+ +gas-sha3+
     (* +gas-sha3-word+ (ceiling length 32))))

(defun calculate-exp-cost (exponent)
  "Calculate gas cost for EXP operation."
  (if (zerop exponent)
      +gas-exp+
      (+ +gas-exp+
         (* +gas-exp-byte+ (ceiling (integer-length exponent) 8)))))

(defun calculate-log-cost (data-size num-topics)
  "Calculate gas cost for LOG operation."
  (+ +gas-log+
     (* +gas-log-topic+ num-topics)
     (* +gas-log-data+ data-size)))

(defun calculate-copy-cost (size)
  "Calculate gas cost for copy operations (CALLDATACOPY, CODECOPY, etc.)."
  (* +gas-copy+ (ceiling size 32)))

(defun calculate-sload-cost (meter address slot)
  "Calculate SLOAD cost with EIP-2929 warm/cold accounting."
  (let ((access-state (gas-meter-access-state meter)))
    (if (access-state-slot-warm-p access-state address slot)
        +gas-warm-access+
        (progn
          (access-state-mark-slot-warm access-state address slot)
          +gas-cold-sload+))))

(defun calculate-sstore-cost (meter address slot current-value new-value original-value)
  "Calculate SSTORE cost with EIP-2929/2200/3529 accounting.
   Returns (values gas-cost refund)."
  (let* ((access-state (gas-meter-access-state meter))
         (is-cold (not (access-state-slot-warm-p access-state address slot)))
         (cold-cost (if is-cold +gas-cold-sload+ 0)))

    ;; Mark slot as warm
    (when is-cold
      (access-state-mark-slot-warm access-state address slot))

    (cond
      ;; Same value - no-op
      ((= current-value new-value)
       (values (+ cold-cost +gas-warm-access+) 0))

      ;; Current equals original (first change)
      ((= current-value original-value)
       (cond
         ;; Setting to zero from non-zero
         ((and (/= original-value 0) (= new-value 0))
          (values (+ cold-cost +gas-sstore-reset+) +gas-sstore-clear-refund+))
         ;; Setting non-zero from zero
         ((= original-value 0)
          (values (+ cold-cost +gas-sstore-set+) 0))
         ;; Changing non-zero to different non-zero
         (t
          (values (+ cold-cost +gas-sstore-reset+) 0))))

      ;; Current differs from original (subsequent change)
      (t
       (let ((refund 0))
         ;; Potential refund adjustments for EIP-2200 edge cases
         (when (and (/= original-value 0) (= current-value 0) (/= new-value 0))
           ;; Restoring from zero, remove prior refund
           (setf refund (- +gas-sstore-clear-refund+)))
         (when (and (/= original-value 0) (/= current-value 0) (= new-value 0))
           ;; Now clearing, add refund
           (incf refund +gas-sstore-clear-refund+))
         (when (= original-value new-value)
           ;; Restoring to original
           (if (= original-value 0)
               (incf refund (- +gas-sstore-set+ +gas-warm-access+))
               (incf refund (- +gas-sstore-reset+ +gas-warm-access+))))
         (values (+ cold-cost +gas-warm-access+) refund))))))

(defun calculate-call-cost (meter target-address value has-value-transfer is-new-account)
  "Calculate CALL cost with EIP-2929 warm/cold accounting."
  (let* ((access-state (gas-meter-access-state meter))
         (is-cold (not (access-state-address-warm-p access-state target-address)))
         (base-cost (if is-cold +gas-cold-account-access+ +gas-warm-access+)))

    (when is-cold
      (access-state-mark-address-warm access-state target-address))

    (+ base-cost
       (if (and has-value-transfer (plusp value)) +gas-call-value+ 0)
       (if is-new-account +gas-new-account+ 0))))

(defun calculate-create-cost (init-code-size)
  "Calculate CREATE/CREATE2 cost."
  (+ +gas-create+
     (* +gas-sha3-word+ (ceiling init-code-size 32))))  ; init code hashing
