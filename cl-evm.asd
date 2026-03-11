;;;; cl-evm.asd - Standalone EVM Bytecode Interpreter
;;;;
;;;; A pure Common Lisp implementation of the Ethereum Virtual Machine.
;;;; No external dependencies beyond SBCL.

(asdf:defsystem #:cl-evm
  :description "Standalone EVM bytecode interpreter in pure Common Lisp"
  :author "Parkian Company LLC"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "util")
                             (:file "opcodes")
                             (:file "gas")
                             (:file "stack")
                             (:file "memory")
                             (:file "storage")
                             (:file "interpreter")))))

(asdf:defsystem #:cl-evm/test
  :description "Tests for cl-evm"
  :depends-on (#:cl-evm)
  :components ((:module "test"
                :components ((:file "test-evm")))))
