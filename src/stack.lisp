;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; stack.lisp - EVM Stack Operations
;;;;
;;;; Implements the 1024-depth EVM stack with 256-bit words.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Error Conditions
;;; ==========================================================================

(define-condition stack-underflow-error (error)
  ((required :initarg :required :reader stack-underflow-required)
   (available :initarg :available :reader stack-underflow-available))
  (:report (lambda (c s)
             (format s "Stack underflow: required ~D items, have ~D"
                     (stack-underflow-required c)
                     (stack-underflow-available c)))))

(define-condition stack-overflow-error (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Stack overflow: maximum depth ~D exceeded"
                     +max-stack-depth+))))

;;; ==========================================================================
;;; Stack Structure
;;; ==========================================================================

(defstruct (evm-stack (:constructor %make-evm-stack))
  "EVM stack with 1024-depth limit and 256-bit words."
  (data (make-array +max-stack-depth+ :initial-element 0) :type simple-vector)
  (depth 0 :type (integer 0 #.+max-stack-depth+)))

(defun make-evm-stack ()
  "Create a new empty EVM stack."
  (%make-evm-stack))

(defun stack-depth (stack)
  "Get current stack depth."
  (evm-stack-depth stack))

(defun stack-clear (stack)
  "Clear the stack."
  (setf (evm-stack-depth stack) 0))

;;; ==========================================================================
;;; Stack Operations
;;; ==========================================================================

(defun stack-push (stack value)
  "Push a value onto the stack. Signals STACK-OVERFLOW-ERROR if full."
  (let ((depth (evm-stack-depth stack)))
    (when (>= depth +max-stack-depth+)
      (error 'stack-overflow-error))
    (setf (aref (evm-stack-data stack) depth) (u256-mask value))
    (incf (evm-stack-depth stack))
    value))

(defun stack-pop (stack)
  "Pop a value from the stack. Signals STACK-UNDERFLOW-ERROR if empty."
  (let ((depth (evm-stack-depth stack)))
    (when (zerop depth)
      (error 'stack-underflow-error :required 1 :available 0))
    (decf (evm-stack-depth stack))
    (aref (evm-stack-data stack) (evm-stack-depth stack))))

(defun stack-peek (stack &optional (n 0))
  "Peek at the Nth item from top of stack (0 = top).
   Signals STACK-UNDERFLOW-ERROR if not enough items."
  (let ((depth (evm-stack-depth stack)))
    (when (> (1+ n) depth)
      (error 'stack-underflow-error :required (1+ n) :available depth))
    (aref (evm-stack-data stack) (- depth 1 n))))

(defun stack-set (stack n value)
  "Set the Nth item from top of stack."
  (let ((depth (evm-stack-depth stack)))
    (when (> (1+ n) depth)
      (error 'stack-underflow-error :required (1+ n) :available depth))
    (setf (aref (evm-stack-data stack) (- depth 1 n))
          (u256-mask value))))

(defun stack-dup (stack n)
  "Duplicate the Nth item from top (1 = DUP1 = duplicate top).
   Pushes a copy of the Nth item onto the top."
  (let ((depth (evm-stack-depth stack)))
    (when (> n depth)
      (error 'stack-underflow-error :required n :available depth))
    (when (>= depth +max-stack-depth+)
      (error 'stack-overflow-error))
    (let ((value (aref (evm-stack-data stack) (- depth n))))
      (setf (aref (evm-stack-data stack) depth) value)
      (incf (evm-stack-depth stack))
      value)))

(defun stack-swap (stack n)
  "Swap top with Nth item from top (1 = SWAP1 = swap with second).
   Exchanges stack[0] with stack[n]."
  (let ((depth (evm-stack-depth stack)))
    (when (> (1+ n) depth)
      (error 'stack-underflow-error :required (1+ n) :available depth))
    (let* ((top-idx (1- depth))
           (nth-idx (- depth 1 n))
           (top-val (aref (evm-stack-data stack) top-idx))
           (nth-val (aref (evm-stack-data stack) nth-idx)))
      (setf (aref (evm-stack-data stack) top-idx) nth-val)
      (setf (aref (evm-stack-data stack) nth-idx) top-val))
    t))

(defun stack-pop-n (stack n)
  "Pop N values from stack, returning them as a list (first = was on top)."
  (let ((depth (evm-stack-depth stack)))
    (when (> n depth)
      (error 'stack-underflow-error :required n :available depth))
    (loop repeat n collect (stack-pop stack))))
