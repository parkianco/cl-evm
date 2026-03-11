;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; util.lisp - Utilities and Keccak256 implementation
;;;;
;;;; Provides helper functions and an inlined Keccak256 implementation
;;;; for the EVM's SHA3 opcode.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Constants
;;; ==========================================================================

(defconstant +word-size+ 32
  "Size of an EVM word in bytes (256 bits).")

(defconstant +max-stack-depth+ 1024
  "Maximum EVM stack depth.")

(defconstant +max-call-depth+ 1024
  "Maximum call depth for CALL/CREATE operations.")

(defconstant +max-code-size+ 24576
  "Maximum contract code size (24KB, per EIP-170).")

(defconstant +u256-max+
  (1- (ash 1 256))
  "Maximum value for a 256-bit unsigned integer.")

(defconstant +u256-sign-bit+
  (ash 1 255)
  "Sign bit position for signed 256-bit integers.")

;;; ==========================================================================
;;; U256 Arithmetic
;;; ==========================================================================

(declaim (inline u256-mask))
(defun u256-mask (value)
  "Mask a value to 256 bits."
  (logand value +u256-max+))

(defun u256-add (a b)
  "Add two U256 values with overflow wrapping."
  (u256-mask (+ a b)))

(defun u256-sub (a b)
  "Subtract two U256 values with underflow wrapping."
  (u256-mask (- a b)))

(defun u256-mul (a b)
  "Multiply two U256 values with overflow wrapping."
  (u256-mask (* a b)))

(defun u256-div (a b)
  "Divide two U256 values. Returns 0 if divisor is 0."
  (if (zerop b) 0 (floor a b)))

(defun u256-mod (a b)
  "Modulo of two U256 values. Returns 0 if divisor is 0."
  (if (zerop b) 0 (mod a b)))

(defun to-signed (value)
  "Convert unsigned 256-bit to signed."
  (if (>= value +u256-sign-bit+)
      (- value (ash 1 256))
      value))

(defun to-unsigned (value)
  "Convert signed to unsigned 256-bit."
  (u256-mask value))

(defun u256-sdiv (a b)
  "Signed division of two U256 values."
  (if (zerop b)
      0
      (let ((sa (to-signed a))
            (sb (to-signed b)))
        (to-unsigned (truncate sa sb)))))

(defun u256-smod (a b)
  "Signed modulo of two U256 values."
  (if (zerop b)
      0
      (let ((sa (to-signed a))
            (sb (to-signed b)))
        (to-unsigned (rem sa sb)))))

(defun u256-addmod (a b n)
  "Add a and b modulo n. Returns 0 if n is 0."
  (if (zerop n) 0 (mod (+ a b) n)))

(defun u256-mulmod (a b n)
  "Multiply a and b modulo n. Returns 0 if n is 0."
  (if (zerop n) 0 (mod (* a b) n)))

(defun u256-exp (base exponent)
  "Compute base^exponent mod 2^256."
  (if (zerop exponent)
      1
      (loop with result = 1
            with b = base
            with e = exponent
            while (plusp e)
            do (when (oddp e)
                 (setf result (u256-mask (* result b))))
               (setf b (u256-mask (* b b)))
               (setf e (ash e -1))
            finally (return result))))

(defun u256-signextend (b x)
  "Sign extend x from (b+1) bytes."
  (if (< b 31)
      (let* ((bit-position (* 8 (1+ b)))
             (sign-bit (ash 1 (1- bit-position)))
             (mask (1- (ash 1 bit-position))))
        (if (zerop (logand x sign-bit))
            (logand x mask)
            (logior x (logxor +u256-max+ mask))))
      x))

;;; ==========================================================================
;;; Byte Conversion Utilities
;;; ==========================================================================

(defun bytes-to-integer (bytes &key (start 0) (end (length bytes)))
  "Convert a byte sequence to an integer (big-endian)."
  (loop with result = 0
        for i from start below end
        do (setf result (logior (ash result 8) (aref bytes i)))
        finally (return result)))

(defun integer-to-bytes (integer &optional (size +word-size+))
  "Convert an integer to a byte vector (big-endian)."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (loop for i from (1- size) downto 0
          for byte-pos from 0
          while (or (plusp integer) (= byte-pos 0))
          do (setf (aref bytes i) (logand integer #xFF))
             (setf integer (ash integer -8)))
    bytes))

(defun hex-to-bytes (hex-string)
  "Convert a hex string to a byte vector."
  (let* ((clean (if (and (>= (length hex-string) 2)
                         (string= (subseq hex-string 0 2) "0x"))
                    (subseq hex-string 2)
                    hex-string))
         (len (length clean))
         (padded (if (oddp len) (concatenate 'string "0" clean) clean))
         (byte-len (/ (length padded) 2)))
    (let ((bytes (make-array byte-len :element-type '(unsigned-byte 8))))
      (loop for i from 0 below byte-len
            do (setf (aref bytes i)
                     (parse-integer padded :start (* i 2) :end (* (1+ i) 2)
                                          :radix 16)))
      bytes)))

(defun bytes-to-hex (bytes &key (prefix t))
  "Convert a byte vector to a hex string."
  (let ((hex (with-output-to-string (s)
               (loop for byte across bytes
                     do (format s "~2,'0x" byte)))))
    (if prefix
        (concatenate 'string "0x" hex)
        hex)))

;;; ==========================================================================
;;; Keccak-256 Implementation
;;; ==========================================================================

;;; Keccak-f[1600] permutation for SHA-3/Keccak-256
;;; Based on the NIST FIPS 202 specification.

(defconstant +keccak-rounds+ 24
  "Number of rounds in Keccak-f[1600].")

(defconstant +keccak-rate+ 136
  "Rate in bytes for Keccak-256 (1088 bits = 136 bytes).")

(defconstant +keccak-capacity+ 64
  "Capacity in bytes for Keccak-256 (512 bits = 64 bytes).")

;; Round constants for iota step
(defparameter *keccak-rc*
  (make-array 24 :element-type '(unsigned-byte 64)
              :initial-contents
              '(#x0000000000000001 #x0000000000008082
                #x800000000000808a #x8000000080008000
                #x000000000000808b #x0000000080000001
                #x8000000080008081 #x8000000000008009
                #x000000000000008a #x0000000000000088
                #x0000000080008009 #x000000008000000a
                #x000000008000808b #x800000000000008b
                #x8000000000008089 #x8000000000008003
                #x8000000000008002 #x8000000000000080
                #x000000000000800a #x800000008000000a
                #x8000000080008081 #x8000000000008080
                #x0000000080000001 #x8000000080008008)))

;; Rotation offsets for rho step
(defparameter *keccak-rotations*
  (make-array '(5 5) :element-type '(unsigned-byte 8)
              :initial-contents
              '((0 36 3 41 18)
                (1 44 10 45 2)
                (62 6 43 15 61)
                (28 55 25 21 56)
                (27 20 39 8 14))))

(declaim (inline keccak-rotate64))
(defun keccak-rotate64 (x n)
  "Rotate 64-bit value x left by n bits."
  (declare (type (unsigned-byte 64) x)
           (type (integer 0 63) n))
  (logior (ldb (byte 64 0) (ash x n))
          (ldb (byte 64 0) (ash x (- n 64)))))

(defun keccak-f1600 (state)
  "Apply the Keccak-f[1600] permutation to the 5x5 state array."
  (declare (type (simple-array (unsigned-byte 64) (5 5)) state))
  (let ((c (make-array 5 :element-type '(unsigned-byte 64)))
        (d (make-array 5 :element-type '(unsigned-byte 64)))
        (b (make-array '(5 5) :element-type '(unsigned-byte 64))))
    (dotimes (round +keccak-rounds+)
      ;; Theta step
      (dotimes (x 5)
        (setf (aref c x)
              (logxor (aref state x 0)
                      (aref state x 1)
                      (aref state x 2)
                      (aref state x 3)
                      (aref state x 4))))
      (dotimes (x 5)
        (setf (aref d x)
              (logxor (aref c (mod (+ x 4) 5))
                      (keccak-rotate64 (aref c (mod (+ x 1) 5)) 1))))
      (dotimes (x 5)
        (dotimes (y 5)
          (setf (aref state x y)
                (logxor (aref state x y) (aref d x)))))

      ;; Rho and Pi steps
      (dotimes (x 5)
        (dotimes (y 5)
          (setf (aref b y (mod (+ (* 2 x) (* 3 y)) 5))
                (keccak-rotate64 (aref state x y)
                                (aref *keccak-rotations* x y)))))

      ;; Chi step
      (dotimes (x 5)
        (dotimes (y 5)
          (setf (aref state x y)
                (logxor (aref b x y)
                        (logand (lognot (aref b (mod (+ x 1) 5) y))
                                (aref b (mod (+ x 2) 5) y))))))

      ;; Iota step
      (setf (aref state 0 0)
            (logxor (aref state 0 0) (aref *keccak-rc* round)))))
  state)

(defun keccak256 (data)
  "Compute the Keccak-256 hash of DATA (byte vector).
   Returns a 32-byte hash vector.

   NOTE: This is Keccak-256 as used by Ethereum (pre-FIPS),
   which uses 0x01 as the domain separator, not 0x06 as in SHA3-256."
  (let* ((input (if (typep data '(simple-array (unsigned-byte 8) (*)))
                    data
                    (coerce data '(simple-array (unsigned-byte 8) (*)))))
         (state (make-array '(5 5) :element-type '(unsigned-byte 64)
                                   :initial-element 0))
         (rate +keccak-rate+)
         (len (length input))
         ;; Padding: 10*1 pattern with Keccak domain separator 0x01
         (padded-len (* rate (ceiling (+ len 1) rate)))
         (padded (make-array (max padded-len rate)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))

    ;; Copy input
    (replace padded input)

    ;; Keccak padding: append 0x01, then zeros, then 0x80
    (setf (aref padded len) #x01)
    (let ((pad-end (1- (max padded-len rate))))
      (setf (aref padded pad-end)
            (logior (aref padded pad-end) #x80)))

    ;; Absorb phase
    (loop for block-start from 0 below (length padded) by rate
          do (loop for i from 0 below rate by 8
                   for x = (mod (floor i 8) 5)
                   for y = (floor (floor i 8) 5)
                   when (< y 5)
                   do (setf (aref state x y)
                           (logxor (aref state x y)
                                   (loop for j from 0 below 8
                                         sum (ash (aref padded (+ block-start i j))
                                                 (* j 8))))))
          do (keccak-f1600 state))

    ;; Squeeze phase (only need 32 bytes for Keccak-256)
    (let ((output (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 32
            for x = (mod (floor i 8) 5)
            for y = (floor (floor i 8) 5)
            for byte-pos = (mod i 8)
            do (setf (aref output i)
                     (ldb (byte 8 (* byte-pos 8)) (aref state x y))))
      output)))
