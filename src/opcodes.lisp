;;;; opcodes.lisp - EVM Opcode Definitions
;;;;
;;;; Complete EVM opcode table per Ethereum Yellow Paper.

(in-package #:cl-evm)

;;; ==========================================================================
;;; Opcode Info Structure
;;; ==========================================================================

(defstruct opcode-info
  "Information about an EVM opcode."
  (name nil :type symbol)
  (value 0 :type (unsigned-byte 8))
  (gas-cost 0 :type (unsigned-byte 32))
  (stack-in 0 :type (unsigned-byte 8))
  (stack-out 0 :type (unsigned-byte 8))
  (halts nil :type boolean)
  (writes-memory nil :type boolean)
  (writes-storage nil :type boolean)
  (immediate-size 0 :type (unsigned-byte 8)))

(defparameter *opcode-table*
  (make-array 256 :initial-element nil)
  "Table of opcode-info structures indexed by opcode value.")

(defparameter *opcode-names*
  (make-hash-table :test 'eq)
  "Hash table mapping opcode symbols to their values.")

(defmacro defopcode (name value &key (gas 0) (stack-in 0) (stack-out 0)
                                      halts writes-memory writes-storage
                                      (immediate-size 0))
  "Define an EVM opcode."
  `(progn
     (setf (aref *opcode-table* ,value)
           (make-opcode-info :name ',name
                            :value ,value
                            :gas-cost ,gas
                            :stack-in ,stack-in
                            :stack-out ,stack-out
                            :halts ,halts
                            :writes-memory ,writes-memory
                            :writes-storage ,writes-storage
                            :immediate-size ,immediate-size))
     (setf (gethash ',name *opcode-names*) ,value)
     ',name))

(defun get-opcode-info (opcode)
  "Get the opcode-info for an opcode value."
  (aref *opcode-table* opcode))

(defun opcode-name (opcode)
  "Get the name of an opcode."
  (let ((info (get-opcode-info opcode)))
    (if info (opcode-info-name info) :invalid)))

(defun opcode-gas-cost (opcode)
  "Get the base gas cost of an opcode."
  (let ((info (get-opcode-info opcode)))
    (if info (opcode-info-gas-cost info) 0)))

;;; ==========================================================================
;;; Opcode Definitions - Stop and Arithmetic
;;; ==========================================================================

(defopcode stop   #x00 :gas 0 :halts t)
(defopcode add    #x01 :gas 3 :stack-in 2 :stack-out 1)
(defopcode mul    #x02 :gas 5 :stack-in 2 :stack-out 1)
(defopcode sub    #x03 :gas 3 :stack-in 2 :stack-out 1)
(defopcode div    #x04 :gas 5 :stack-in 2 :stack-out 1)
(defopcode sdiv   #x05 :gas 5 :stack-in 2 :stack-out 1)
(defopcode mod    #x06 :gas 5 :stack-in 2 :stack-out 1)
(defopcode smod   #x07 :gas 5 :stack-in 2 :stack-out 1)
(defopcode addmod #x08 :gas 8 :stack-in 3 :stack-out 1)
(defopcode mulmod #x09 :gas 8 :stack-in 3 :stack-out 1)
(defopcode exp    #x0a :gas 10 :stack-in 2 :stack-out 1)  ; + dynamic
(defopcode signextend #x0b :gas 5 :stack-in 2 :stack-out 1)

;;; ==========================================================================
;;; Opcode Definitions - Comparison and Bitwise Logic
;;; ==========================================================================

(defopcode lt     #x10 :gas 3 :stack-in 2 :stack-out 1)
(defopcode gt     #x11 :gas 3 :stack-in 2 :stack-out 1)
(defopcode slt    #x12 :gas 3 :stack-in 2 :stack-out 1)
(defopcode sgt    #x13 :gas 3 :stack-in 2 :stack-out 1)
(defopcode eq     #x14 :gas 3 :stack-in 2 :stack-out 1)
(defopcode iszero #x15 :gas 3 :stack-in 1 :stack-out 1)
(defopcode and    #x16 :gas 3 :stack-in 2 :stack-out 1)
(defopcode or     #x17 :gas 3 :stack-in 2 :stack-out 1)
(defopcode xor    #x18 :gas 3 :stack-in 2 :stack-out 1)
(defopcode not    #x19 :gas 3 :stack-in 1 :stack-out 1)
(defopcode byte   #x1a :gas 3 :stack-in 2 :stack-out 1)
(defopcode shl    #x1b :gas 3 :stack-in 2 :stack-out 1)
(defopcode shr    #x1c :gas 3 :stack-in 2 :stack-out 1)
(defopcode sar    #x1d :gas 3 :stack-in 2 :stack-out 1)

;;; ==========================================================================
;;; Opcode Definitions - SHA3
;;; ==========================================================================

(defopcode sha3 #x20 :gas 30 :stack-in 2 :stack-out 1 :writes-memory nil)

;;; ==========================================================================
;;; Opcode Definitions - Environmental Information
;;; ==========================================================================

(defopcode address        #x30 :gas 2 :stack-out 1)
(defopcode balance        #x31 :gas 100 :stack-in 1 :stack-out 1)  ; + warm/cold
(defopcode origin         #x32 :gas 2 :stack-out 1)
(defopcode caller         #x33 :gas 2 :stack-out 1)
(defopcode callvalue      #x34 :gas 2 :stack-out 1)
(defopcode calldataload   #x35 :gas 3 :stack-in 1 :stack-out 1)
(defopcode calldatasize   #x36 :gas 2 :stack-out 1)
(defopcode calldatacopy   #x37 :gas 3 :stack-in 3 :writes-memory t)
(defopcode codesize       #x38 :gas 2 :stack-out 1)
(defopcode codecopy       #x39 :gas 3 :stack-in 3 :writes-memory t)
(defopcode gasprice       #x3a :gas 2 :stack-out 1)
(defopcode extcodesize    #x3b :gas 100 :stack-in 1 :stack-out 1)  ; + warm/cold
(defopcode extcodecopy    #x3c :gas 100 :stack-in 4 :writes-memory t)
(defopcode returndatasize #x3d :gas 2 :stack-out 1)
(defopcode returndatacopy #x3e :gas 3 :stack-in 3 :writes-memory t)
(defopcode extcodehash    #x3f :gas 100 :stack-in 1 :stack-out 1)  ; + warm/cold

;;; ==========================================================================
;;; Opcode Definitions - Block Information
;;; ==========================================================================

(defopcode blockhash  #x40 :gas 20 :stack-in 1 :stack-out 1)
(defopcode coinbase   #x41 :gas 2 :stack-out 1)
(defopcode timestamp  #x42 :gas 2 :stack-out 1)
(defopcode number     #x43 :gas 2 :stack-out 1)
(defopcode prevrandao #x44 :gas 2 :stack-out 1)  ; was DIFFICULTY
(defopcode gaslimit   #x45 :gas 2 :stack-out 1)
(defopcode chainid    #x46 :gas 2 :stack-out 1)
(defopcode selfbalance #x47 :gas 5 :stack-out 1)
(defopcode basefee    #x48 :gas 2 :stack-out 1)

;;; ==========================================================================
;;; Opcode Definitions - Stack, Memory, Storage, Flow
;;; ==========================================================================

(defopcode pop      #x50 :gas 2 :stack-in 1)
(defopcode mload    #x51 :gas 3 :stack-in 1 :stack-out 1)
(defopcode mstore   #x52 :gas 3 :stack-in 2 :writes-memory t)
(defopcode mstore8  #x53 :gas 3 :stack-in 2 :writes-memory t)
(defopcode sload    #x54 :gas 100 :stack-in 1 :stack-out 1)  ; + warm/cold
(defopcode sstore   #x55 :gas 100 :stack-in 2 :writes-storage t)  ; complex
(defopcode jump     #x56 :gas 8 :stack-in 1)
(defopcode jumpi    #x57 :gas 10 :stack-in 2)
(defopcode pc       #x58 :gas 2 :stack-out 1)
(defopcode msize    #x59 :gas 2 :stack-out 1)
(defopcode gas      #x5a :gas 2 :stack-out 1)
(defopcode jumpdest #x5b :gas 1)
(defopcode tload    #x5c :gas 100 :stack-in 1 :stack-out 1)  ; EIP-1153
(defopcode tstore   #x5d :gas 100 :stack-in 2)  ; EIP-1153
(defopcode mcopy    #x5e :gas 3 :stack-in 3 :writes-memory t)  ; EIP-5656

;;; ==========================================================================
;;; Opcode Definitions - Push Operations
;;; ==========================================================================

(defopcode push0  #x5f :gas 2 :stack-out 1)

(defopcode push1  #x60 :gas 3 :stack-out 1 :immediate-size 1)
(defopcode push2  #x61 :gas 3 :stack-out 1 :immediate-size 2)
(defopcode push3  #x62 :gas 3 :stack-out 1 :immediate-size 3)
(defopcode push4  #x63 :gas 3 :stack-out 1 :immediate-size 4)
(defopcode push5  #x64 :gas 3 :stack-out 1 :immediate-size 5)
(defopcode push6  #x65 :gas 3 :stack-out 1 :immediate-size 6)
(defopcode push7  #x66 :gas 3 :stack-out 1 :immediate-size 7)
(defopcode push8  #x67 :gas 3 :stack-out 1 :immediate-size 8)
(defopcode push9  #x68 :gas 3 :stack-out 1 :immediate-size 9)
(defopcode push10 #x69 :gas 3 :stack-out 1 :immediate-size 10)
(defopcode push11 #x6a :gas 3 :stack-out 1 :immediate-size 11)
(defopcode push12 #x6b :gas 3 :stack-out 1 :immediate-size 12)
(defopcode push13 #x6c :gas 3 :stack-out 1 :immediate-size 13)
(defopcode push14 #x6d :gas 3 :stack-out 1 :immediate-size 14)
(defopcode push15 #x6e :gas 3 :stack-out 1 :immediate-size 15)
(defopcode push16 #x6f :gas 3 :stack-out 1 :immediate-size 16)
(defopcode push17 #x70 :gas 3 :stack-out 1 :immediate-size 17)
(defopcode push18 #x71 :gas 3 :stack-out 1 :immediate-size 18)
(defopcode push19 #x72 :gas 3 :stack-out 1 :immediate-size 19)
(defopcode push20 #x73 :gas 3 :stack-out 1 :immediate-size 20)
(defopcode push21 #x74 :gas 3 :stack-out 1 :immediate-size 21)
(defopcode push22 #x75 :gas 3 :stack-out 1 :immediate-size 22)
(defopcode push23 #x76 :gas 3 :stack-out 1 :immediate-size 23)
(defopcode push24 #x77 :gas 3 :stack-out 1 :immediate-size 24)
(defopcode push25 #x78 :gas 3 :stack-out 1 :immediate-size 25)
(defopcode push26 #x79 :gas 3 :stack-out 1 :immediate-size 26)
(defopcode push27 #x7a :gas 3 :stack-out 1 :immediate-size 27)
(defopcode push28 #x7b :gas 3 :stack-out 1 :immediate-size 28)
(defopcode push29 #x7c :gas 3 :stack-out 1 :immediate-size 29)
(defopcode push30 #x7d :gas 3 :stack-out 1 :immediate-size 30)
(defopcode push31 #x7e :gas 3 :stack-out 1 :immediate-size 31)
(defopcode push32 #x7f :gas 3 :stack-out 1 :immediate-size 32)

;;; ==========================================================================
;;; Opcode Definitions - Dup Operations
;;; ==========================================================================

(defopcode dup1  #x80 :gas 3 :stack-in 1 :stack-out 2)
(defopcode dup2  #x81 :gas 3 :stack-in 2 :stack-out 3)
(defopcode dup3  #x82 :gas 3 :stack-in 3 :stack-out 4)
(defopcode dup4  #x83 :gas 3 :stack-in 4 :stack-out 5)
(defopcode dup5  #x84 :gas 3 :stack-in 5 :stack-out 6)
(defopcode dup6  #x85 :gas 3 :stack-in 6 :stack-out 7)
(defopcode dup7  #x86 :gas 3 :stack-in 7 :stack-out 8)
(defopcode dup8  #x87 :gas 3 :stack-in 8 :stack-out 9)
(defopcode dup9  #x88 :gas 3 :stack-in 9 :stack-out 10)
(defopcode dup10 #x89 :gas 3 :stack-in 10 :stack-out 11)
(defopcode dup11 #x8a :gas 3 :stack-in 11 :stack-out 12)
(defopcode dup12 #x8b :gas 3 :stack-in 12 :stack-out 13)
(defopcode dup13 #x8c :gas 3 :stack-in 13 :stack-out 14)
(defopcode dup14 #x8d :gas 3 :stack-in 14 :stack-out 15)
(defopcode dup15 #x8e :gas 3 :stack-in 15 :stack-out 16)
(defopcode dup16 #x8f :gas 3 :stack-in 16 :stack-out 17)

;;; ==========================================================================
;;; Opcode Definitions - Swap Operations
;;; ==========================================================================

(defopcode swap1  #x90 :gas 3 :stack-in 2 :stack-out 2)
(defopcode swap2  #x91 :gas 3 :stack-in 3 :stack-out 3)
(defopcode swap3  #x92 :gas 3 :stack-in 4 :stack-out 4)
(defopcode swap4  #x93 :gas 3 :stack-in 5 :stack-out 5)
(defopcode swap5  #x94 :gas 3 :stack-in 6 :stack-out 6)
(defopcode swap6  #x95 :gas 3 :stack-in 7 :stack-out 7)
(defopcode swap7  #x96 :gas 3 :stack-in 8 :stack-out 8)
(defopcode swap8  #x97 :gas 3 :stack-in 9 :stack-out 9)
(defopcode swap9  #x98 :gas 3 :stack-in 10 :stack-out 10)
(defopcode swap10 #x99 :gas 3 :stack-in 11 :stack-out 11)
(defopcode swap11 #x9a :gas 3 :stack-in 12 :stack-out 12)
(defopcode swap12 #x9b :gas 3 :stack-in 13 :stack-out 13)
(defopcode swap13 #x9c :gas 3 :stack-in 14 :stack-out 14)
(defopcode swap14 #x9d :gas 3 :stack-in 15 :stack-out 15)
(defopcode swap15 #x9e :gas 3 :stack-in 16 :stack-out 16)
(defopcode swap16 #x9f :gas 3 :stack-in 17 :stack-out 17)

;;; ==========================================================================
;;; Opcode Definitions - Log Operations
;;; ==========================================================================

(defopcode log0 #xa0 :gas 375 :stack-in 2)
(defopcode log1 #xa1 :gas 750 :stack-in 3)
(defopcode log2 #xa2 :gas 1125 :stack-in 4)
(defopcode log3 #xa3 :gas 1500 :stack-in 5)
(defopcode log4 #xa4 :gas 1875 :stack-in 6)

;;; ==========================================================================
;;; Opcode Definitions - System Operations
;;; ==========================================================================

(defopcode create       #xf0 :gas 32000 :stack-in 3 :stack-out 1 :writes-storage t)
(defopcode call         #xf1 :gas 100 :stack-in 7 :stack-out 1)
(defopcode callcode     #xf2 :gas 100 :stack-in 7 :stack-out 1)
(defopcode return       #xf3 :gas 0 :stack-in 2 :halts t)
(defopcode delegatecall #xf4 :gas 100 :stack-in 6 :stack-out 1)
(defopcode create2      #xf5 :gas 32000 :stack-in 4 :stack-out 1 :writes-storage t)
(defopcode staticcall   #xfa :gas 100 :stack-in 6 :stack-out 1)
(defopcode revert       #xfd :gas 0 :stack-in 2 :halts t)
(defopcode invalid      #xfe :gas 0 :halts t)
(defopcode selfdestruct #xff :gas 5000 :stack-in 1 :halts t :writes-storage t)
