;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; storage.lisp - EVM Storage Operations
;;;;
;;;; Implements persistent key-value storage with EIP-2929 warm/cold tracking
;;;; and original value caching for SSTORE gas calculations.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Storage Structure
;;; ==========================================================================

(defstruct (evm-storage (:constructor %make-evm-storage))
  "EVM persistent storage - key/value mapping with 256-bit keys and values.
   Tracks original values for gas calculations and warm/cold access state."
  ;; Current values
  (slots (make-hash-table :test 'eql) :type hash-table)
  ;; Original values at start of transaction (for SSTORE gas calc)
  (original (make-hash-table :test 'eql) :type hash-table)
  ;; Warm slots (accessed this transaction)
  (warm (make-hash-table :test 'eql) :type hash-table))

(defun make-evm-storage (&optional initial-values)
  "Create a new EVM storage, optionally populated with initial values.
   INITIAL-VALUES is an alist of (key . value) pairs."
  (let ((storage (%make-evm-storage)))
    (when initial-values
      (loop for (key . value) in initial-values
            do (setf (gethash key (evm-storage-slots storage)) value)
               (setf (gethash key (evm-storage-original storage)) value)))
    storage))

;;; ==========================================================================
;;; Storage Access
;;; ==========================================================================

(defun storage-load (storage slot)
  "Load a value from storage. Returns 0 if slot is empty."
  (gethash slot (evm-storage-slots storage) 0))

(defun storage-store (storage slot value)
  "Store a value to storage."
  ;; Record original value if not already recorded
  (multiple-value-bind (orig exists-p)
      (gethash slot (evm-storage-original storage))
    (declare (ignore orig))
    (unless exists-p
      (setf (gethash slot (evm-storage-original storage))
            (gethash slot (evm-storage-slots storage) 0))))
  ;; Store the new value
  (if (zerop value)
      (remhash slot (evm-storage-slots storage))
      (setf (gethash slot (evm-storage-slots storage)) value)))

(defun storage-get-original (storage slot)
  "Get the original value of a slot (at start of transaction).
   Returns 0 if slot was originally empty."
  (gethash slot (evm-storage-original storage)
           (gethash slot (evm-storage-slots storage) 0)))

(defun storage-is-warm-p (storage slot)
  "Check if a slot has been accessed (is warm)."
  (gethash slot (evm-storage-warm storage)))

(defun storage-mark-warm (storage slot)
  "Mark a slot as warm (accessed)."
  (setf (gethash slot (evm-storage-warm storage)) t))

(defun storage-clear (storage)
  "Clear all storage (for testing)."
  (clrhash (evm-storage-slots storage))
  (clrhash (evm-storage-original storage))
  (clrhash (evm-storage-warm storage)))

(defun storage-reset-access (storage)
  "Reset warm access tracking (at start of new transaction)."
  (clrhash (evm-storage-warm storage))
  (clrhash (evm-storage-original storage)))

(defun storage-commit (storage)
  "Commit storage changes - update originals to current values.
   Called at end of successful transaction."
  (clrhash (evm-storage-original storage))
  (maphash (lambda (k v)
             (setf (gethash k (evm-storage-original storage)) v))
           (evm-storage-slots storage)))

(defun storage-revert (storage)
  "Revert storage to original values.
   Called on transaction revert."
  (clrhash (evm-storage-slots storage))
  (maphash (lambda (k v)
             (unless (zerop v)
               (setf (gethash k (evm-storage-slots storage)) v)))
           (evm-storage-original storage)))

;;; ==========================================================================
;;; Transient Storage (EIP-1153)
;;; ==========================================================================

(defstruct (transient-storage (:constructor make-transient-storage))
  "Transient storage - cleared at end of transaction (EIP-1153)."
  (slots (make-hash-table :test 'eql) :type hash-table))

(defun transient-load (storage slot)
  "Load from transient storage."
  (gethash slot (transient-storage-slots storage) 0))

(defun transient-store (storage slot value)
  "Store to transient storage."
  (if (zerop value)
      (remhash slot (transient-storage-slots storage))
      (setf (gethash slot (transient-storage-slots storage)) value)))

(defun transient-clear (storage)
  "Clear transient storage (at end of transaction)."
  (clrhash (transient-storage-slots storage)))
