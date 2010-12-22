;;; cg-mode.el --- Simple cc-mode extension for dealing with Cg files.
;;
;; Author: Kenneth Moreland <kmorel@sandia.gov>
;; Version: 0.0  2002/10/22
;; Keywords: Cg
;; $Id: cg-mode.el,v 1.1 2002-10-22 22:21:00 kmorel Exp $
;;
;; Just a simple mode for handling Cg files.  It is almost identical to C
;; mode, but with a different name.  In fact, what little is here is
;; basically just stripped from cc-mode.el This allows for slightly
;; different coloring (which I define elsewhere).
;;

(require 'cc-mode)


;;;###autoload
(defun cg-mode ()
  "Major mode for editing Cg code.
This mode inherits and gets its functionality from CC Mode.  To see what
version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `cg-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'cg-mode
	mode-name "Cg"
	local-abbrev-table c-mode-abbrev-table)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq comment-start "/* "
	comment-end   " */"
	c-conditional-key c-C-conditional-key
	c-class-key c-C-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-C++-comment-start-regexp
	imenu-generic-expression cc-imenu-c-generic-expression
	imenu-case-fold-search nil
	)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'cg-mode-hook)
  (c-update-modeline))

(provide 'cg-mode)
