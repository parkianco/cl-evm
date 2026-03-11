;;;; memory.lisp - EVM Memory Operations
;;;;
;;;; Implements byte-addressable EVM memory with word-aligned access
;;;; and quadratic expansion cost.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Memory Structure
;;; ==========================================================================

(defstruct (evm-memory (:constructor %make-evm-memory))
  "EVM memory - byte-addressable, grows in 32-byte words."
  (data (make-array 0 :element-type '(unsigned-byte 8)
                      :adjustable t
                      :fill-pointer 0)
   :type (vector (unsigned-byte 8)))
  (high-water 0 :type (unsigned-byte 64)))

(defun make-evm-memory ()
  "Create a new empty EVM memory."
  (%make-evm-memory))

(defun memory-size (memory)
  "Get current memory size in bytes (always multiple of 32)."
  (* 32 (ceiling (evm-memory-high-water memory) 32)))

;;; ==========================================================================
;;; Memory Expansion
;;; ==========================================================================

(defun memory-expand (memory offset size)
  "Expand memory if needed to accommodate offset+size bytes.
   Returns the number of new words added (for gas calculation)."
  (when (zerop size)
    (return-from memory-expand 0))

  (let* ((required (+ offset size))
         (current-words (ceiling (evm-memory-high-water memory) 32))
         (required-words (ceiling required 32))
         (new-words (- required-words current-words)))

    (when (> required (evm-memory-high-water memory))
      (setf (evm-memory-high-water memory) required))

    (when (> required-words current-words)
      (let* ((new-size (* required-words 32))
             (old-data (evm-memory-data memory))
             (new-data (make-array new-size
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t
                                   :fill-pointer new-size
                                   :initial-element 0)))
        (replace new-data old-data)
        (setf (evm-memory-data memory) new-data)))

    new-words))

;;; ==========================================================================
;;; Memory Access
;;; ==========================================================================

(defun memory-load-byte (memory offset)
  "Load a single byte from memory."
  (if (>= offset (length (evm-memory-data memory)))
      0
      (aref (evm-memory-data memory) offset)))

(defun memory-store-byte (memory offset value)
  "Store a single byte to memory."
  (memory-expand memory offset 1)
  (setf (aref (evm-memory-data memory) offset)
        (logand value #xFF)))

(defun memory-load (memory offset)
  "Load a 32-byte word from memory as a U256."
  (memory-expand memory offset 32)
  (let ((data (evm-memory-data memory)))
    (loop with result = 0
          for i from offset below (+ offset 32)
          do (setf result (logior (ash result 8)
                                  (if (< i (length data))
                                      (aref data i)
                                      0)))
          finally (return result))))

(defun memory-store (memory offset value)
  "Store a 32-byte word (U256) to memory."
  (memory-expand memory offset 32)
  (let ((data (evm-memory-data memory)))
    (loop for i from 31 downto 0
          for byte-idx from (+ offset 31) downto offset
          do (setf (aref data byte-idx) (logand value #xFF))
             (setf value (ash value -8)))))

(defun memory-load-range (memory offset size)
  "Load SIZE bytes from memory starting at OFFSET.
   Returns a byte vector."
  (when (zerop size)
    (return-from memory-load-range
      (make-array 0 :element-type '(unsigned-byte 8))))

  (memory-expand memory offset size)
  (let ((result (make-array size :element-type '(unsigned-byte 8)
                                 :initial-element 0))
        (data (evm-memory-data memory)))
    (loop for i from 0 below size
          for src-idx = (+ offset i)
          when (< src-idx (length data))
          do (setf (aref result i) (aref data src-idx)))
    result))

(defun memory-store-range (memory offset data)
  "Store DATA (byte vector) to memory starting at OFFSET."
  (let ((size (length data)))
    (when (zerop size)
      (return-from memory-store-range))
    (memory-expand memory offset size)
    (let ((mem-data (evm-memory-data memory)))
      (loop for i from 0 below size
            do (setf (aref mem-data (+ offset i))
                     (aref data i))))))

(defun memory-copy (memory dest-offset src-offset size)
  "Copy SIZE bytes within memory from src to dest (MCOPY, EIP-5656).
   Handles overlapping regions correctly."
  (when (zerop size)
    (return-from memory-copy))

  ;; Expand memory for both src and dest
  (memory-expand memory src-offset size)
  (memory-expand memory dest-offset size)

  (let ((data (evm-memory-data memory)))
    ;; Use temporary buffer for overlapping regions
    (let ((temp (make-array size :element-type '(unsigned-byte 8))))
      ;; Copy to temp
      (loop for i from 0 below size
            do (setf (aref temp i)
                     (aref data (+ src-offset i))))
      ;; Copy from temp to dest
      (loop for i from 0 below size
            do (setf (aref data (+ dest-offset i))
                     (aref temp i))))))
