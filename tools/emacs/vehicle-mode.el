;;; vehicle-mode.el -- Syntax highlighting for the Vehicle language

;; Copyright (c) 2021 Wen Kokke

;; Author: Wen Kokke
;; Version: 0.0.1
;; Created: 3 Mar 2021
;; Keywords: languages, tools
;; Homepage: https://github.com/wenkokke/vehicle

;; This file is not part of GNU Emacs.

;;; Commentary:

;; vehicle-mode provides syntax highlighting for the Vehicle language in Emacs.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vehicle-mode))

(defconst vehicle-mode-syntax-table
  (let ((table (make-syntax-table)))
		(modify-syntax-entry ?_  "w"     table)
		(modify-syntax-entry ?\{ "( 1c"  table)
    (modify-syntax-entry ?\} ") 4c"  table)
    (modify-syntax-entry ?-  ". 123" table)
		(modify-syntax-entry ?\n ">"     table)
		table)
	"Syntax table for Vehicle mode")

(defconst vehicle-keywords
	'( "@network"
		 "@dataset"
		 "@parameter"
		 "@property"
		 "@postulate"
		 "@noinline"
		 "forallT"
		 "let" "in"
		 "type")
	"Keywords for Vehicle language")

(defconst vehicle-builtins
	'( "True"
		 "False"
		 "nil"
		 "if" "then" "else"
		 "forall"
		 "exists"
		 "foreach"
		 "and"
		 "or"
		 "map"
		 "fold"
		 "dfold"
		 "indices"
		 "fromNat"
		 "fromInt")
	"Builtin constructors for Vehicle language")

(defconst vehicle-types
	'( "Type"
     "Unit"
     "Bool"
     "Nat"
     "Int"
     "Rat"
     "Vector"
     "List"
     "Index"
 		 "HasEq"
 		 "HasNotEq"
 		 "HasAdd"
 		 "HasSub"
 		 "HasMul"
 		 "HasFold"
 		 "HasMap")
	"Builtin types and kinds for Vehicle language")

(defvar vehicle-font-lock-keywords
	(append
		`( (,(regexp-opt vehicle-keywords 'symbols) . font-lock-keyword-face)
			 (,(regexp-opt vehicle-builtins 'symbols) . font-lock-builtin-face)
			 (,(regexp-opt vehicle-types    'symbols) . font-lock-type-face)
		))
	"Font Lock configuration for Vehicle mode")

;;;###autoload
(define-derived-mode vehicle-mode fundamental-mode "Vehicle"
	"Major mode for editing Vehicle"
	:syntax-table vehicle-mode-syntax-table
  (setq-local font-lock-defauts '(vehicle-font-lock-keywords)))

(provide 'vehicle-mode)
;;; vehicle-mode.el ends here
