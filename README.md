# cl-evm

A standalone Ethereum Virtual Machine (EVM) bytecode interpreter written in pure Common Lisp.

## Features

- Complete EVM opcode implementation (Constantinople+ compatible)
- 256-bit arithmetic with proper overflow handling
- EIP-2929 warm/cold access tracking for gas metering
- EIP-2200/3529 SSTORE gas semantics
- EIP-1153 transient storage (TLOAD/TSTORE)
- EIP-5656 memory copy (MCOPY)
- Native Keccak-256 implementation (SHA3 opcode)
- No external dependencies - pure SBCL

## Quick Start

```lisp
;; Load the system
(asdf:load-system :cl-evm)

;; Simple example: compute 5 + 3
;; Bytecode: PUSH1 5, PUSH1 3, ADD, STOP
(let* ((code #(#x60 #x05 #x60 #x03 #x01 #x00))
       (ctx (cl-evm:create-execution-context :code code)))
  (cl-evm:execute-bytecode ctx)
  (cl-evm:stack-peek (cl-evm:context-stack ctx)))
;; => 8
```

## API

### Execution Context

```lisp
(create-execution-context &key code gas-limit address caller origin
                               call-value call-data gas-price storage
                               block-info static-p)
```

Creates a new execution context for running EVM bytecode.

### Main Execution

```lisp
(execute-bytecode ctx)     ; Run until halt
(step-instruction ctx)     ; Execute single instruction
```

### Stack Operations

```lisp
(make-evm-stack)           ; Create stack
(stack-push stack value)   ; Push value
(stack-pop stack)          ; Pop value
(stack-peek stack &optional n)  ; Peek at nth item
(stack-dup stack n)        ; Duplicate nth item
(stack-swap stack n)       ; Swap top with nth
```

### Memory Operations

```lisp
(make-evm-memory)              ; Create memory
(memory-load memory offset)    ; Load 32-byte word
(memory-store memory offset value)
(memory-load-byte memory offset)
(memory-store-byte memory offset value)
(memory-load-range memory offset size)
(memory-store-range memory offset data)
```

### Storage Operations

```lisp
(make-evm-storage)         ; Create storage
(storage-load storage slot)
(storage-store storage slot value)
(storage-get-original storage slot)
```

### Gas Metering

```lisp
(make-gas-meter :limit gas-limit)
(charge-gas meter amount)
(gas-meter-remaining meter)
```

### Utilities

```lisp
(keccak256 data)           ; Compute Keccak-256 hash
(bytes-to-hex bytes)       ; Convert to hex string
(hex-to-bytes hex-string)  ; Parse hex string
(disassemble-bytecode code); Disassemble to readable form
```

## Supported Opcodes

### Arithmetic (0x00-0x0b)
STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND

### Comparison & Bitwise (0x10-0x1d)
LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE, SHL, SHR, SAR

### SHA3 (0x20)
SHA3 (Keccak-256)

### Environment (0x30-0x3f)
ADDRESS, BALANCE, ORIGIN, CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE,
CALLDATACOPY, CODESIZE, CODECOPY, GASPRICE, EXTCODESIZE, EXTCODECOPY,
RETURNDATASIZE, RETURNDATACOPY, EXTCODEHASH

### Block Info (0x40-0x48)
BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, PREVRANDAO, GASLIMIT, CHAINID,
SELFBALANCE, BASEFEE

### Stack/Memory/Storage (0x50-0x5e)
POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE, JUMP, JUMPI, PC, MSIZE, GAS,
JUMPDEST, TLOAD, TSTORE, MCOPY

### Push (0x5f-0x7f)
PUSH0, PUSH1-PUSH32

### Dup (0x80-0x8f)
DUP1-DUP16

### Swap (0x90-0x9f)
SWAP1-SWAP16

### Log (0xa0-0xa4)
LOG0-LOG4

### System (0xf0-0xff)
CREATE, CALL, CALLCODE, RETURN, DELEGATECALL, CREATE2, STATICCALL, REVERT,
INVALID, SELFDESTRUCT

Note: CALL/CREATE opcodes are stubbed to return failure. Full implementation
requires external state management.

## Testing

```lisp
(asdf:load-system :cl-evm/test)
(cl-evm/test:run-all-tests)
```

## License

MIT License. See LICENSE file.
