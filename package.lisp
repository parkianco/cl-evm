;;;; package.lisp - Package definitions for cl-evm
;;;;
;;;; Standalone EVM bytecode interpreter.

(defpackage #:cl-evm
  (:use #:cl)
  (:export
   ;; Main execution
   #:execute-bytecode
   #:step-instruction
   #:create-execution-context
   #:make-execution-context
   #:disassemble-bytecode

   ;; Execution context accessors
   #:context-pc
   #:context-stack
   #:context-memory
   #:context-storage
   #:context-gas-meter
   #:context-code
   #:context-call-data
   #:context-call-value
   #:context-caller
   #:context-origin
   #:context-address
   #:context-gas-price
   #:context-block-info
   #:context-return-data
   #:context-halted-p
   #:context-reverted-p
   #:context-return-value
   #:context-logs
   #:context-call-depth

   ;; Gas meter
   #:make-gas-meter
   #:gas-meter-limit
   #:gas-meter-used
   #:gas-meter-remaining
   #:gas-meter-refund
   #:charge-gas
   #:refund-gas
   #:check-gas

   ;; Stack operations
   #:make-evm-stack
   #:stack-push
   #:stack-pop
   #:stack-peek
   #:stack-dup
   #:stack-swap
   #:stack-depth
   #:stack-clear

   ;; Memory operations
   #:make-evm-memory
   #:memory-load
   #:memory-store
   #:memory-load-byte
   #:memory-store-byte
   #:memory-size
   #:memory-expand
   #:memory-copy

   ;; Storage operations
   #:make-evm-storage
   #:storage-load
   #:storage-store
   #:storage-get-original
   #:storage-is-warm-p
   #:storage-mark-warm
   #:storage-clear

   ;; Block info
   #:make-block-info
   #:block-info-number
   #:block-info-timestamp
   #:block-info-coinbase
   #:block-info-difficulty
   #:block-info-gas-limit
   #:block-info-base-fee
   #:block-info-chain-id
   #:block-info-blockhash-fn

   ;; Opcodes
   #:opcode-name
   #:opcode-gas-cost
   #:opcode-info
   #:get-opcode-info

   ;; Errors
   #:evm-error
   #:out-of-gas-error
   #:stack-underflow-error
   #:stack-overflow-error
   #:invalid-opcode-error
   #:invalid-jump-error
   #:write-protection-error
   #:call-depth-exceeded-error

   ;; Constants
   #:+word-size+
   #:+max-stack-depth+
   #:+max-call-depth+
   #:+max-code-size+

   ;; U256 arithmetic
   #:u256-add
   #:u256-sub
   #:u256-mul
   #:u256-div
   #:u256-mod
   #:u256-exp
   #:u256-sdiv
   #:u256-smod
   #:u256-addmod
   #:u256-mulmod
   #:u256-signextend
   #:to-signed
   #:to-unsigned

   ;; Utilities
   #:keccak256
   #:bytes-to-integer
   #:integer-to-bytes
   #:hex-to-bytes
   #:bytes-to-hex))
