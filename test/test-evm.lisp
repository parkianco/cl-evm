;;;; test-evm.lisp - Tests for cl-evm
;;;;
;;;; Basic test suite for the EVM implementation.

(defpackage #:cl-evm/test
  (:use #:cl #:cl-evm))

(in-package #:cl-evm/test)

;;; ==========================================================================
;;; Test Helpers
;;; ==========================================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  "Define a test case."
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "~&PASS: ~A~%" ',name)
                t)
       (error (e)
         (incf *fail-count*)
         (format t "~&FAIL: ~A~%  Error: ~A~%" ',name e)
         nil))))

(defun assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  (unless (equal expected actual)
    (error "Assertion failed~@[: ~A~]~%  Expected: ~S~%  Actual: ~S"
           message expected actual)))

(defun run-all-tests ()
  "Run all defined tests."
  (setf *test-count* 0 *pass-count* 0 *fail-count* 0)
  (format t "~&Running cl-evm tests...~%~%")

  ;; Run individual tests
  (test-u256-arithmetic)
  (test-stack-operations)
  (test-memory-operations)
  (test-storage-operations)
  (test-keccak256)
  (test-simple-bytecode)
  (test-arithmetic-opcodes)
  (test-comparison-opcodes)
  (test-push-operations)
  (test-dup-swap)
  (test-memory-opcodes)
  (test-jump-operations)
  (test-gas-metering)

  (format t "~%========================================~%")
  (format t "Results: ~D/~D passed, ~D failed~%"
          *pass-count* *test-count* *fail-count*)
  (zerop *fail-count*))

;;; ==========================================================================
;;; U256 Arithmetic Tests
;;; ==========================================================================

(deftest test-u256-arithmetic
  (assert-equal 5 (u256-add 2 3) "2+3=5")
  (assert-equal 0 (u256-sub 5 5) "5-5=0")
  (assert-equal 6 (u256-mul 2 3) "2*3=6")
  (assert-equal 3 (u256-div 10 3) "10/3=3")
  (assert-equal 1 (u256-mod 10 3) "10%3=1")
  (assert-equal 8 (u256-exp 2 3) "2^3=8")
  (assert-equal 0 (u256-div 5 0) "5/0=0")
  (assert-equal 0 (u256-mod 5 0) "5%0=0")
  ;; Overflow wrapping
  (let ((max (1- (ash 1 256))))
    (assert-equal 0 (u256-add max 1) "max+1=0")
    (assert-equal max (u256-sub 0 1) "0-1=max")))

;;; ==========================================================================
;;; Stack Tests
;;; ==========================================================================

(deftest test-stack-operations
  (let ((stack (make-evm-stack)))
    (assert-equal 0 (stack-depth stack) "Empty stack")
    (stack-push stack 42)
    (assert-equal 1 (stack-depth stack) "After push")
    (assert-equal 42 (stack-peek stack) "Peek")
    (assert-equal 42 (stack-pop stack) "Pop")
    (assert-equal 0 (stack-depth stack) "After pop")

    ;; Dup and swap
    (stack-push stack 1)
    (stack-push stack 2)
    (stack-push stack 3)
    (stack-dup stack 2)  ; Duplicate 2nd item
    (assert-equal 2 (stack-peek stack) "After DUP2")
    (stack-pop stack)
    (stack-swap stack 1)  ; Swap top two
    (assert-equal 2 (stack-peek stack) "After SWAP1")))

;;; ==========================================================================
;;; Memory Tests
;;; ==========================================================================

(deftest test-memory-operations
  (let ((memory (make-evm-memory)))
    (assert-equal 0 (memory-size memory) "Empty memory")
    (memory-store memory 0 #x1234)
    (assert-equal 32 (memory-size memory) "After store")
    (assert-equal #x1234 (memory-load memory 0) "Load after store")

    ;; Byte operations
    (memory-store-byte memory 100 #xAB)
    (assert-equal #xAB (memory-load-byte memory 100) "Byte load")

    ;; Range operations
    (memory-store-range memory 200 #(1 2 3 4))
    (let ((range (memory-load-range memory 200 4)))
      (assert-equal 1 (aref range 0))
      (assert-equal 4 (aref range 3)))))

;;; ==========================================================================
;;; Storage Tests
;;; ==========================================================================

(deftest test-storage-operations
  (let ((storage (make-evm-storage)))
    (assert-equal 0 (storage-load storage 1) "Empty slot")
    (storage-store storage 1 42)
    (assert-equal 42 (storage-load storage 1) "After store")
    (assert-equal 42 (storage-get-original storage 1) "Original value")

    ;; Warm/cold tracking
    (assert-equal nil (storage-is-warm-p storage 2) "Cold slot")
    (storage-mark-warm storage 2)
    (assert-equal t (storage-is-warm-p storage 2) "Warm slot")))

;;; ==========================================================================
;;; Keccak256 Tests
;;; ==========================================================================

(deftest test-keccak256
  ;; Empty input
  (let ((hash (keccak256 #())))
    (assert-equal 32 (length hash) "Hash length"))

  ;; Known test vector: keccak256("") =
  ;; c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
  (let* ((hash (keccak256 #()))
         (hex (bytes-to-hex hash :prefix nil)))
    (assert-equal "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                  (string-downcase hex)
                  "Empty hash"))

  ;; keccak256("abc") =
  ;; 4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45
  (let* ((hash (keccak256 #(97 98 99)))  ; "abc"
         (hex (bytes-to-hex hash :prefix nil)))
    (assert-equal "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
                  (string-downcase hex)
                  "abc hash")))

;;; ==========================================================================
;;; Simple Bytecode Execution Tests
;;; ==========================================================================

(deftest test-simple-bytecode
  ;; PUSH1 0x05 PUSH1 0x03 ADD STOP
  ;; Should leave 8 on stack
  (let* ((code #(#x60 #x05 #x60 #x03 #x01 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal t (execution-context-halted-p ctx) "Halted")
    (assert-equal 8 (stack-peek (execution-context-stack ctx)) "5+3=8")))

(deftest test-arithmetic-opcodes
  ;; Test SUB: PUSH1 10, PUSH1 3, SUB -> 7
  (let* ((code #(#x60 #x0A #x60 #x03 #x03 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 7 (stack-peek (execution-context-stack ctx)) "10-3=7"))

  ;; Test MUL: PUSH1 4, PUSH1 5, MUL -> 20
  (let* ((code #(#x60 #x04 #x60 #x05 #x02 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 20 (stack-peek (execution-context-stack ctx)) "4*5=20"))

  ;; Test DIV: PUSH1 20, PUSH1 4, DIV -> 5
  (let* ((code #(#x60 #x14 #x60 #x04 #x04 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 5 (stack-peek (execution-context-stack ctx)) "20/4=5")))

(deftest test-comparison-opcodes
  ;; LT: PUSH1 3, PUSH1 5, LT -> 1 (5 < 3 is false... wait, stack order)
  ;; LT pops a, then b, returns (b < a)
  ;; So PUSH 3, PUSH 5 gives stack [3, 5], LT pops 5 then 3, returns 3<5 = 1
  (let* ((code #(#x60 #x03 #x60 #x05 #x10 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 1 (stack-peek (execution-context-stack ctx)) "5<3? no, 3<5? yes"))

  ;; ISZERO: PUSH1 0, ISZERO -> 1
  (let* ((code #(#x60 #x00 #x15 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 1 (stack-peek (execution-context-stack ctx)) "ISZERO(0)=1"))

  ;; EQ: PUSH1 5, PUSH1 5, EQ -> 1
  (let* ((code #(#x60 #x05 #x60 #x05 #x14 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 1 (stack-peek (execution-context-stack ctx)) "5==5")))

(deftest test-push-operations
  ;; PUSH0
  (let* ((code #(#x5f #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 0 (stack-peek (execution-context-stack ctx)) "PUSH0"))

  ;; PUSH2 0x0102
  (let* ((code #(#x61 #x01 #x02 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal #x0102 (stack-peek (execution-context-stack ctx)) "PUSH2"))

  ;; PUSH4 0x01020304
  (let* ((code #(#x63 #x01 #x02 #x03 #x04 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal #x01020304 (stack-peek (execution-context-stack ctx)) "PUSH4")))

(deftest test-dup-swap
  ;; DUP1: PUSH1 5, DUP1 -> [5, 5]
  (let* ((code #(#x60 #x05 #x80 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (let ((stack (execution-context-stack ctx)))
      (assert-equal 5 (stack-peek stack 0) "DUP1 top")
      (assert-equal 5 (stack-peek stack 1) "DUP1 second")))

  ;; SWAP1: PUSH1 1, PUSH1 2, SWAP1 -> [1, 2]
  (let* ((code #(#x60 #x01 #x60 #x02 #x90 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (let ((stack (execution-context-stack ctx)))
      (assert-equal 1 (stack-peek stack 0) "SWAP1 top")
      (assert-equal 2 (stack-peek stack 1) "SWAP1 second"))))

(deftest test-memory-opcodes
  ;; MSTORE then MLOAD
  ;; PUSH1 42, PUSH1 0, MSTORE, PUSH1 0, MLOAD
  (let* ((code #(#x60 #x2A #x60 #x00 #x52 #x60 #x00 #x51 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 42 (stack-peek (execution-context-stack ctx)) "MSTORE/MLOAD")))

(deftest test-jump-operations
  ;; PUSH1 5, JUMP, INVALID, JUMPDEST, PUSH1 42, STOP
  ;; Jump to offset 5 (JUMPDEST), push 42, stop
  (let* ((code #(#x60 #x04 #x56 #xfe #x5b #x60 #x2A #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 42 (stack-peek (execution-context-stack ctx)) "JUMP"))

  ;; JUMPI - conditional
  ;; PUSH1 1, PUSH1 6, JUMPI, PUSH1 1, STOP, JUMPDEST, PUSH1 2, STOP
  (let* ((code #(#x60 #x01 #x60 #x07 #x57 #x60 #x01 #x00 #x5b #x60 #x02 #x00))
         (ctx (create-execution-context :code code)))
    (execute-bytecode ctx)
    (assert-equal 2 (stack-peek (execution-context-stack ctx)) "JUMPI taken")))

;;; ==========================================================================
;;; Gas Metering Tests
;;; ==========================================================================

(deftest test-gas-metering
  ;; Simple program, check gas consumption
  (let* ((code #(#x60 #x05 #x60 #x03 #x01 #x00))  ; PUSH1 PUSH1 ADD STOP
         (ctx (create-execution-context :code code :gas-limit 100)))
    (execute-bytecode ctx)
    (let ((used (gas-meter-used (execution-context-gas-meter ctx))))
      ;; PUSH1 = 3, PUSH1 = 3, ADD = 3, STOP = 0
      (assert-equal 9 used "Gas used for PUSH1 PUSH1 ADD STOP")))

  ;; Out of gas
  (let* ((code #(#x60 #x05 #x60 #x03 #x01 #x00))
         (ctx (create-execution-context :code code :gas-limit 5)))
    (handler-case
        (progn
          (execute-bytecode ctx)
          (error "Should have run out of gas"))
      (out-of-gas-error ()
        t))))  ; Expected
