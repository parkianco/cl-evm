;;;; interpreter.lisp - EVM Bytecode Interpreter
;;;;
;;;; Main execution loop and opcode dispatch.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Error Conditions
;;; ==========================================================================

(define-condition evm-error (error)
  ((message :initarg :message :reader evm-error-message))
  (:report (lambda (c s)
             (format s "EVM Error: ~A" (evm-error-message c)))))

(define-condition invalid-opcode-error (evm-error)
  ((opcode :initarg :opcode :reader invalid-opcode-value))
  (:report (lambda (c s)
             (format s "Invalid opcode: 0x~2,'0X" (invalid-opcode-value c)))))

(define-condition invalid-jump-error (evm-error)
  ((destination :initarg :destination :reader invalid-jump-destination))
  (:report (lambda (c s)
             (format s "Invalid jump destination: ~D" (invalid-jump-destination c)))))

(define-condition write-protection-error (evm-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Write operation in static context"))))

(define-condition call-depth-exceeded-error (evm-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Maximum call depth (~D) exceeded" +max-call-depth+))))

;;; ==========================================================================
;;; Block Info Structure
;;; ==========================================================================

(defstruct block-info
  "Information about the current block."
  (number 0 :type (unsigned-byte 64))
  (timestamp 0 :type (unsigned-byte 64))
  (coinbase 0 :type integer)  ; 160-bit address
  (difficulty 0 :type integer)  ; prevrandao post-merge
  (gas-limit 0 :type (unsigned-byte 64))
  (base-fee 0 :type integer)
  (chain-id 1 :type (unsigned-byte 64))
  (blockhash-fn nil :type (or null function)))

;;; ==========================================================================
;;; Log Entry Structure
;;; ==========================================================================

(defstruct log-entry
  "A log entry (event) emitted during execution."
  (address 0 :type integer)
  (topics nil :type list)
  (data #() :type (vector (unsigned-byte 8))))

;;; ==========================================================================
;;; Execution Context
;;; ==========================================================================

(defstruct (execution-context (:constructor %make-execution-context))
  "Complete EVM execution context."
  ;; Program state
  (pc 0 :type (unsigned-byte 64))
  (code #() :type (vector (unsigned-byte 8)))
  (code-size 0 :type (unsigned-byte 64))
  (jump-dests nil :type (or null hash-table))

  ;; Runtime state
  (stack nil :type (or null evm-stack))
  (memory nil :type (or null evm-memory))
  (storage nil :type (or null evm-storage))
  (transient-storage nil :type (or null transient-storage))
  (gas-meter nil :type (or null gas-meter))

  ;; Call context
  (address 0 :type integer)
  (caller 0 :type integer)
  (origin 0 :type integer)
  (call-value 0 :type integer)
  (call-data #() :type (vector (unsigned-byte 8)))
  (gas-price 0 :type integer)
  (call-depth 0 :type (unsigned-byte 16))

  ;; Environment
  (block-info nil :type (or null block-info))

  ;; Execution result
  (halted-p nil :type boolean)
  (reverted-p nil :type boolean)
  (return-value (make-array 0 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (return-data (make-array 0 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (logs nil :type list)

  ;; Flags
  (static-p nil :type boolean))

(defun create-execution-context (&key code
                                      (gas-limit 1000000)
                                      (address 0)
                                      (caller 0)
                                      (origin nil)
                                      (call-value 0)
                                      (call-data #())
                                      (gas-price 0)
                                      (storage nil)
                                      (block-info nil)
                                      (static-p nil))
  "Create a new execution context for running EVM bytecode."
  (let* ((code-vec (if (typep code '(vector (unsigned-byte 8)))
                       code
                       (coerce code '(vector (unsigned-byte 8)))))
         (ctx (%make-execution-context
               :pc 0
               :code code-vec
               :code-size (length code-vec)
               :jump-dests (analyze-jump-dests code-vec)
               :stack (make-evm-stack)
               :memory (make-evm-memory)
               :storage (or storage (make-evm-storage))
               :transient-storage (make-transient-storage)
               :gas-meter (make-gas-meter :limit gas-limit)
               :address address
               :caller caller
               :origin (or origin caller)
               :call-value call-value
               :call-data (if (typep call-data '(vector (unsigned-byte 8)))
                              call-data
                              (coerce call-data '(vector (unsigned-byte 8))))
               :gas-price gas-price
               :call-depth 0
               :block-info (or block-info (make-block-info))
               :static-p static-p)))
    ctx))

;;; ==========================================================================
;;; Jump Destination Analysis
;;; ==========================================================================

(defun analyze-jump-dests (code)
  "Analyze bytecode to find all valid JUMPDEST locations."
  (let ((dests (make-hash-table :test 'eql))
        (len (length code))
        (pc 0))
    (loop while (< pc len)
          for opcode = (aref code pc)
          do (cond
               ;; JUMPDEST
               ((= opcode #x5b)
                (setf (gethash pc dests) t)
                (incf pc))
               ;; PUSH1-PUSH32
               ((and (>= opcode #x60) (<= opcode #x7f))
                (incf pc (+ 1 (- opcode #x5f))))
               ;; Other opcodes
               (t (incf pc))))
    dests))

(defun valid-jump-dest-p (ctx dest)
  "Check if DEST is a valid JUMPDEST location."
  (and (< dest (execution-context-code-size ctx))
       (gethash dest (execution-context-jump-dests ctx))))

;;; ==========================================================================
;;; Bytecode Reading
;;; ==========================================================================

(defun read-code-byte (ctx offset)
  "Read a byte from code at PC+offset, returns 0 if out of bounds."
  (let ((pos (+ (execution-context-pc ctx) offset)))
    (if (< pos (execution-context-code-size ctx))
        (aref (execution-context-code ctx) pos)
        0)))

(defun read-immediate (ctx size)
  "Read SIZE bytes from code after current opcode as big-endian integer."
  (loop with result = 0
        for i from 1 to size
        do (setf result (logior (ash result 8) (read-code-byte ctx i)))
        finally (return result)))

;;; ==========================================================================
;;; Main Execution Loop
;;; ==========================================================================

(defun execute-bytecode (ctx)
  "Execute bytecode in the given context until halt."
  (loop until (execution-context-halted-p ctx)
        do (step-instruction ctx))
  ctx)

(defun step-instruction (ctx)
  "Execute a single instruction."
  (when (execution-context-halted-p ctx)
    (return-from step-instruction ctx))

  (when (>= (execution-context-pc ctx) (execution-context-code-size ctx))
    (setf (execution-context-halted-p ctx) t)
    (return-from step-instruction ctx))

  (let* ((pc (execution-context-pc ctx))
         (opcode (aref (execution-context-code ctx) pc))
         (info (get-opcode-info opcode)))

    ;; Check for invalid opcode
    (unless info
      (error 'invalid-opcode-error :opcode opcode))

    ;; Charge base gas
    (charge-gas (execution-context-gas-meter ctx)
                (opcode-info-gas-cost info))

    ;; Check stack requirements
    (let ((stack (execution-context-stack ctx))
          (stack-in (opcode-info-stack-in info)))
      (when (> stack-in (stack-depth stack))
        (error 'stack-underflow-error :required stack-in
                                      :available (stack-depth stack))))

    ;; Check write protection
    (when (and (execution-context-static-p ctx)
               (or (opcode-info-writes-storage info)
                   (opcode-info-writes-memory info)
                   (member opcode '(#xa0 #xa1 #xa2 #xa3 #xa4  ; LOG0-4
                                    #xf0 #xf5 #xff))))        ; CREATE, CREATE2, SELFDESTRUCT
      (error 'write-protection-error))

    ;; Dispatch
    (execute-opcode ctx opcode)

    ;; Advance PC (unless jump already did)
    (when (and (= pc (execution-context-pc ctx))
               (not (execution-context-halted-p ctx)))
      (let ((imm-size (opcode-info-immediate-size info)))
        (setf (execution-context-pc ctx)
              (+ pc 1 imm-size)))))

  ctx)

;;; ==========================================================================
;;; Opcode Dispatch
;;; ==========================================================================

(defun execute-opcode (ctx opcode)
  "Dispatch execution of a single opcode."
  (cond
    ;; STOP
    ((= opcode #x00) (execute-stop ctx))

    ;; Arithmetic (0x01-0x0b)
    ((<= #x01 opcode #x0b) (execute-arithmetic ctx opcode))

    ;; Comparison and Bitwise (0x10-0x1d)
    ((<= #x10 opcode #x1d) (execute-comparison-bitwise ctx opcode))

    ;; SHA3
    ((= opcode #x20) (execute-sha3 ctx))

    ;; Environment (0x30-0x3f)
    ((<= #x30 opcode #x3f) (execute-environment ctx opcode))

    ;; Block info (0x40-0x48)
    ((<= #x40 opcode #x48) (execute-block-info ctx opcode))

    ;; Stack/Memory/Storage/Flow (0x50-0x5e)
    ((<= #x50 opcode #x5e) (execute-stack-memory-storage ctx opcode))

    ;; PUSH0 and PUSH1-PUSH32 (0x5f-0x7f)
    ((<= #x5f opcode #x7f) (execute-push ctx opcode))

    ;; DUP1-DUP16 (0x80-0x8f)
    ((<= #x80 opcode #x8f) (execute-dup ctx opcode))

    ;; SWAP1-SWAP16 (0x90-0x9f)
    ((<= #x90 opcode #x9f) (execute-swap ctx opcode))

    ;; LOG0-LOG4 (0xa0-0xa4)
    ((<= #xa0 opcode #xa4) (execute-log ctx opcode))

    ;; System operations (0xf0-0xff)
    ((<= #xf0 opcode #xff) (execute-system ctx opcode))

    (t (error 'invalid-opcode-error :opcode opcode))))

;;; ==========================================================================
;;; Arithmetic Operations
;;; ==========================================================================

(defun execute-stop (ctx)
  (setf (execution-context-halted-p ctx) t))

(defun execute-arithmetic (ctx opcode)
  (let ((stack (execution-context-stack ctx)))
    (case opcode
      (#x01  ; ADD
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-add a b))))

      (#x02  ; MUL
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-mul a b))))

      (#x03  ; SUB
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-sub a b))))

      (#x04  ; DIV
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-div a b))))

      (#x05  ; SDIV
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-sdiv a b))))

      (#x06  ; MOD
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-mod a b))))

      (#x07  ; SMOD
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (u256-smod a b))))

      (#x08  ; ADDMOD
       (let ((a (stack-pop stack))
             (b (stack-pop stack))
             (n (stack-pop stack)))
         (stack-push stack (u256-addmod a b n))))

      (#x09  ; MULMOD
       (let ((a (stack-pop stack))
             (b (stack-pop stack))
             (n (stack-pop stack)))
         (stack-push stack (u256-mulmod a b n))))

      (#x0a  ; EXP
       (let* ((base (stack-pop stack))
              (exponent (stack-pop stack))
              (exp-cost (calculate-exp-cost exponent)))
         ;; Charge dynamic gas
         (charge-gas (execution-context-gas-meter ctx) (- exp-cost +gas-exp+))
         (stack-push stack (u256-exp base exponent))))

      (#x0b  ; SIGNEXTEND
       (let ((b (stack-pop stack))
             (x (stack-pop stack)))
         (stack-push stack (u256-signextend b x)))))))

;;; ==========================================================================
;;; Comparison and Bitwise Operations
;;; ==========================================================================

(defun execute-comparison-bitwise (ctx opcode)
  (let ((stack (execution-context-stack ctx)))
    (case opcode
      (#x10  ; LT
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (if (< a b) 1 0))))

      (#x11  ; GT
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (if (> a b) 1 0))))

      (#x12  ; SLT
       (let ((a (to-signed (stack-pop stack)))
             (b (to-signed (stack-pop stack))))
         (stack-push stack (if (< a b) 1 0))))

      (#x13  ; SGT
       (let ((a (to-signed (stack-pop stack)))
             (b (to-signed (stack-pop stack))))
         (stack-push stack (if (> a b) 1 0))))

      (#x14  ; EQ
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (if (= a b) 1 0))))

      (#x15  ; ISZERO
       (let ((a (stack-pop stack)))
         (stack-push stack (if (zerop a) 1 0))))

      (#x16  ; AND
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (logand a b))))

      (#x17  ; OR
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (logior a b))))

      (#x18  ; XOR
       (let ((a (stack-pop stack))
             (b (stack-pop stack)))
         (stack-push stack (logxor a b))))

      (#x19  ; NOT
       (let ((a (stack-pop stack)))
         (stack-push stack (logxor a +u256-max+))))

      (#x1a  ; BYTE
       (let ((i (stack-pop stack))
             (x (stack-pop stack)))
         (if (>= i 32)
             (stack-push stack 0)
             (stack-push stack (ldb (byte 8 (* 8 (- 31 i))) x)))))

      (#x1b  ; SHL
       (let ((shift (stack-pop stack))
             (value (stack-pop stack)))
         (if (>= shift 256)
             (stack-push stack 0)
             (stack-push stack (u256-mask (ash value shift))))))

      (#x1c  ; SHR
       (let ((shift (stack-pop stack))
             (value (stack-pop stack)))
         (if (>= shift 256)
             (stack-push stack 0)
             (stack-push stack (ash value (- shift))))))

      (#x1d  ; SAR
       (let ((shift (stack-pop stack))
             (value (stack-pop stack)))
         (let ((signed (to-signed value)))
           (if (>= shift 256)
               (stack-push stack (if (minusp signed) +u256-max+ 0))
               (stack-push stack (to-unsigned (ash signed (- shift)))))))))))

;;; ==========================================================================
;;; SHA3 Operation
;;; ==========================================================================

(defun execute-sha3 (ctx)
  (let* ((stack (execution-context-stack ctx))
         (memory (execution-context-memory ctx))
         (offset (stack-pop stack))
         (size (stack-pop stack)))

    ;; Charge dynamic gas
    (let ((sha3-cost (calculate-sha3-cost size))
          (mem-cost (calculate-memory-expansion-cost
                     (memory-size memory) (+ offset size))))
      (charge-gas (execution-context-gas-meter ctx)
                  (+ (- sha3-cost +gas-sha3+) mem-cost)))

    ;; Compute hash
    (let* ((data (memory-load-range memory offset size))
           (hash (keccak256 data)))
      (stack-push stack (bytes-to-integer hash)))))

;;; ==========================================================================
;;; Environment Operations
;;; ==========================================================================

(defun execute-environment (ctx opcode)
  (let ((stack (execution-context-stack ctx)))
    (case opcode
      (#x30  ; ADDRESS
       (stack-push stack (execution-context-address ctx)))

      (#x31  ; BALANCE - simplified, returns 0
       (stack-pop stack)  ; address
       (stack-push stack 0))

      (#x32  ; ORIGIN
       (stack-push stack (execution-context-origin ctx)))

      (#x33  ; CALLER
       (stack-push stack (execution-context-caller ctx)))

      (#x34  ; CALLVALUE
       (stack-push stack (execution-context-call-value ctx)))

      (#x35  ; CALLDATALOAD
       (let* ((offset (stack-pop stack))
              (data (execution-context-call-data ctx))
              (len (length data)))
         (stack-push stack
                     (loop with result = 0
                           for i from 0 below 32
                           for pos = (+ offset i)
                           do (setf result (logior (ash result 8)
                                                   (if (< pos len)
                                                       (aref data pos)
                                                       0)))
                           finally (return result)))))

      (#x36  ; CALLDATASIZE
       (stack-push stack (length (execution-context-call-data ctx))))

      (#x37  ; CALLDATACOPY
       (let* ((memory (execution-context-memory ctx))
              (dest-offset (stack-pop stack))
              (data-offset (stack-pop stack))
              (size (stack-pop stack))
              (data (execution-context-call-data ctx)))
         ;; Gas for memory expansion and copy
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ dest-offset size)))
               (copy-cost (calculate-copy-cost size)))
           (charge-gas (execution-context-gas-meter ctx) (+ mem-cost copy-cost)))
         ;; Copy data
         (loop for i from 0 below size
               for src = (+ data-offset i)
               for dst = (+ dest-offset i)
               do (memory-store-byte memory dst
                                    (if (< src (length data))
                                        (aref data src)
                                        0)))))

      (#x38  ; CODESIZE
       (stack-push stack (execution-context-code-size ctx)))

      (#x39  ; CODECOPY
       (let* ((memory (execution-context-memory ctx))
              (dest-offset (stack-pop stack))
              (code-offset (stack-pop stack))
              (size (stack-pop stack))
              (code (execution-context-code ctx)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ dest-offset size)))
               (copy-cost (calculate-copy-cost size)))
           (charge-gas (execution-context-gas-meter ctx) (+ mem-cost copy-cost)))
         (loop for i from 0 below size
               for src = (+ code-offset i)
               for dst = (+ dest-offset i)
               do (memory-store-byte memory dst
                                    (if (< src (length code))
                                        (aref code src)
                                        0)))))

      (#x3a  ; GASPRICE
       (stack-push stack (execution-context-gas-price ctx)))

      (#x3b  ; EXTCODESIZE - simplified
       (stack-pop stack)
       (stack-push stack 0))

      (#x3c  ; EXTCODECOPY - simplified
       (stack-pop stack)  ; address
       (stack-pop stack)  ; dest
       (stack-pop stack)  ; offset
       (stack-pop stack)) ; size

      (#x3d  ; RETURNDATASIZE
       (stack-push stack (length (execution-context-return-data ctx))))

      (#x3e  ; RETURNDATACOPY
       (let* ((memory (execution-context-memory ctx))
              (dest-offset (stack-pop stack))
              (data-offset (stack-pop stack))
              (size (stack-pop stack))
              (data (execution-context-return-data ctx)))
         (when (> (+ data-offset size) (length data))
           (error 'evm-error :message "Return data out of bounds"))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ dest-offset size)))
               (copy-cost (calculate-copy-cost size)))
           (charge-gas (execution-context-gas-meter ctx) (+ mem-cost copy-cost)))
         (loop for i from 0 below size
               do (memory-store-byte memory (+ dest-offset i)
                                    (aref data (+ data-offset i))))))

      (#x3f  ; EXTCODEHASH - simplified
       (stack-pop stack)
       (stack-push stack 0)))))

;;; ==========================================================================
;;; Block Info Operations
;;; ==========================================================================

(defun execute-block-info (ctx opcode)
  (let ((stack (execution-context-stack ctx))
        (block (execution-context-block-info ctx)))
    (case opcode
      (#x40  ; BLOCKHASH
       (let ((number (stack-pop stack)))
         (if (and (block-info-blockhash-fn block))
             (stack-push stack (funcall (block-info-blockhash-fn block) number))
             (stack-push stack 0))))

      (#x41  ; COINBASE
       (stack-push stack (block-info-coinbase block)))

      (#x42  ; TIMESTAMP
       (stack-push stack (block-info-timestamp block)))

      (#x43  ; NUMBER
       (stack-push stack (block-info-number block)))

      (#x44  ; PREVRANDAO (was DIFFICULTY)
       (stack-push stack (block-info-difficulty block)))

      (#x45  ; GASLIMIT
       (stack-push stack (block-info-gas-limit block)))

      (#x46  ; CHAINID
       (stack-push stack (block-info-chain-id block)))

      (#x47  ; SELFBALANCE - simplified
       (stack-push stack 0))

      (#x48  ; BASEFEE
       (stack-push stack (block-info-base-fee block))))))

;;; ==========================================================================
;;; Stack/Memory/Storage/Flow Operations
;;; ==========================================================================

(defun execute-stack-memory-storage (ctx opcode)
  (let ((stack (execution-context-stack ctx))
        (memory (execution-context-memory ctx))
        (storage (execution-context-storage ctx)))
    (case opcode
      (#x50  ; POP
       (stack-pop stack))

      (#x51  ; MLOAD
       (let ((offset (stack-pop stack)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ offset 32))))
           (charge-gas (execution-context-gas-meter ctx) mem-cost))
         (stack-push stack (memory-load memory offset))))

      (#x52  ; MSTORE
       (let ((offset (stack-pop stack))
             (value (stack-pop stack)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ offset 32))))
           (charge-gas (execution-context-gas-meter ctx) mem-cost))
         (memory-store memory offset value)))

      (#x53  ; MSTORE8
       (let ((offset (stack-pop stack))
             (value (stack-pop stack)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ offset 1))))
           (charge-gas (execution-context-gas-meter ctx) mem-cost))
         (memory-store-byte memory offset (logand value #xFF))))

      (#x54  ; SLOAD
       (let* ((slot (stack-pop stack))
              (gas-cost (calculate-sload-cost
                         (execution-context-gas-meter ctx)
                         (execution-context-address ctx)
                         slot)))
         (charge-gas (execution-context-gas-meter ctx) (- gas-cost +gas-warm-access+))
         (storage-mark-warm storage slot)
         (stack-push stack (storage-load storage slot))))

      (#x55  ; SSTORE
       (let* ((slot (stack-pop stack))
              (new-value (stack-pop stack))
              (current-value (storage-load storage slot))
              (original-value (storage-get-original storage slot)))
         (multiple-value-bind (gas-cost refund)
             (calculate-sstore-cost (execution-context-gas-meter ctx)
                                   (execution-context-address ctx)
                                   slot
                                   current-value
                                   new-value
                                   original-value)
           (charge-gas (execution-context-gas-meter ctx) (- gas-cost +gas-warm-access+))
           (when (plusp refund)
             (refund-gas (execution-context-gas-meter ctx) refund)))
         (storage-store storage slot new-value)))

      (#x56  ; JUMP
       (let ((dest (stack-pop stack)))
         (unless (valid-jump-dest-p ctx dest)
           (error 'invalid-jump-error :destination dest))
         (setf (execution-context-pc ctx) dest)))

      (#x57  ; JUMPI
       (let ((dest (stack-pop stack))
             (cond-val (stack-pop stack)))
         (unless (zerop cond-val)
           (unless (valid-jump-dest-p ctx dest)
             (error 'invalid-jump-error :destination dest))
           (setf (execution-context-pc ctx) dest))))

      (#x58  ; PC
       (stack-push stack (execution-context-pc ctx)))

      (#x59  ; MSIZE
       (stack-push stack (memory-size memory)))

      (#x5a  ; GAS
       (stack-push stack (gas-meter-remaining (execution-context-gas-meter ctx))))

      (#x5b  ; JUMPDEST
       ;; No operation, just a valid jump target
       )

      (#x5c  ; TLOAD (EIP-1153)
       (let* ((slot (stack-pop stack))
              (tstorage (execution-context-transient-storage ctx)))
         (stack-push stack (transient-load tstorage slot))))

      (#x5d  ; TSTORE (EIP-1153)
       (let ((slot (stack-pop stack))
             (value (stack-pop stack))
             (tstorage (execution-context-transient-storage ctx)))
         (transient-store tstorage slot value)))

      (#x5e  ; MCOPY (EIP-5656)
       (let ((dest (stack-pop stack))
             (src (stack-pop stack))
             (size (stack-pop stack)))
         (let ((mem-cost (max (calculate-memory-expansion-cost
                               (memory-size memory) (+ dest size))
                              (calculate-memory-expansion-cost
                               (memory-size memory) (+ src size))))
               (copy-cost (calculate-copy-cost size)))
           (charge-gas (execution-context-gas-meter ctx) (+ mem-cost copy-cost)))
         (memory-copy memory dest src size))))))

;;; ==========================================================================
;;; Push Operations
;;; ==========================================================================

(defun execute-push (ctx opcode)
  (let ((stack (execution-context-stack ctx)))
    (if (= opcode #x5f)
        ;; PUSH0
        (stack-push stack 0)
        ;; PUSH1-PUSH32
        (let* ((size (- opcode #x5f))
               (value (read-immediate ctx size)))
          (stack-push stack value)))))

;;; ==========================================================================
;;; Dup Operations
;;; ==========================================================================

(defun execute-dup (ctx opcode)
  (let ((stack (execution-context-stack ctx))
        (n (- opcode #x7f)))  ; DUP1 = 0x80, n=1
    (stack-dup stack n)))

;;; ==========================================================================
;;; Swap Operations
;;; ==========================================================================

(defun execute-swap (ctx opcode)
  (let ((stack (execution-context-stack ctx))
        (n (- opcode #x8f)))  ; SWAP1 = 0x90, n=1
    (stack-swap stack n)))

;;; ==========================================================================
;;; Log Operations
;;; ==========================================================================

(defun execute-log (ctx opcode)
  (let* ((stack (execution-context-stack ctx))
         (memory (execution-context-memory ctx))
         (num-topics (- opcode #xa0))
         (offset (stack-pop stack))
         (size (stack-pop stack))
         (topics (loop repeat num-topics collect (stack-pop stack))))

    ;; Gas for log operation
    (let ((log-cost (calculate-log-cost size num-topics))
          (mem-cost (calculate-memory-expansion-cost
                     (memory-size memory) (+ offset size))))
      (charge-gas (execution-context-gas-meter ctx)
                  (+ (- log-cost +gas-log+) mem-cost)))

    ;; Create log entry
    (let ((data (memory-load-range memory offset size)))
      (push (make-log-entry :address (execution-context-address ctx)
                           :topics topics
                           :data data)
            (execution-context-logs ctx)))))

;;; ==========================================================================
;;; System Operations
;;; ==========================================================================

(defun execute-system (ctx opcode)
  (let ((stack (execution-context-stack ctx))
        (memory (execution-context-memory ctx)))
    (case opcode
      (#xf0  ; CREATE - simplified stub
       (stack-pop stack)  ; value
       (stack-pop stack)  ; offset
       (stack-pop stack)  ; size
       (stack-push stack 0))  ; Return 0 (failure)

      (#xf1  ; CALL - simplified stub
       (stack-pop stack)  ; gas
       (stack-pop stack)  ; address
       (stack-pop stack)  ; value
       (stack-pop stack)  ; args offset
       (stack-pop stack)  ; args size
       (stack-pop stack)  ; ret offset
       (stack-pop stack)  ; ret size
       (stack-push stack 0))  ; Return 0 (failure)

      (#xf2  ; CALLCODE - simplified stub
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-push stack 0))

      (#xf3  ; RETURN
       (let ((offset (stack-pop stack))
             (size (stack-pop stack)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ offset size))))
           (charge-gas (execution-context-gas-meter ctx) mem-cost))
         (setf (execution-context-return-value ctx)
               (memory-load-range memory offset size))
         (setf (execution-context-halted-p ctx) t)))

      (#xf4  ; DELEGATECALL - simplified stub
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-push stack 0))

      (#xf5  ; CREATE2 - simplified stub
       (stack-pop stack)  ; value
       (stack-pop stack)  ; offset
       (stack-pop stack)  ; size
       (stack-pop stack)  ; salt
       (stack-push stack 0))

      (#xfa  ; STATICCALL - simplified stub
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-pop stack)
       (stack-push stack 0))

      (#xfd  ; REVERT
       (let ((offset (stack-pop stack))
             (size (stack-pop stack)))
         (let ((mem-cost (calculate-memory-expansion-cost
                          (memory-size memory) (+ offset size))))
           (charge-gas (execution-context-gas-meter ctx) mem-cost))
         (setf (execution-context-return-value ctx)
               (memory-load-range memory offset size))
         (setf (execution-context-reverted-p ctx) t)
         (setf (execution-context-halted-p ctx) t)))

      (#xfe  ; INVALID
       (error 'invalid-opcode-error :opcode #xfe))

      (#xff  ; SELFDESTRUCT - simplified stub
       (stack-pop stack)
       (setf (execution-context-halted-p ctx) t)))))

;;; ==========================================================================
;;; Disassembler
;;; ==========================================================================

(defun disassemble-bytecode (code &key (start 0) (end nil))
  "Disassemble bytecode to a list of (pc opcode-name [operand])."
  (let ((result nil)
        (len (or end (length code)))
        (pc start))
    (loop while (< pc len)
          for opcode = (aref code pc)
          for info = (get-opcode-info opcode)
          for name = (if info (opcode-info-name info) :invalid)
          for imm-size = (if info (opcode-info-immediate-size info) 0)
          do (if (plusp imm-size)
                 (let ((imm (loop with result = 0
                                  for i from 1 to imm-size
                                  for pos = (+ pc i)
                                  when (< pos len)
                                  do (setf result (logior (ash result 8)
                                                          (aref code pos)))
                                  finally (return result))))
                   (push (list pc name imm) result))
                 (push (list pc name) result))
             (incf pc (+ 1 imm-size)))
    (nreverse result)))
