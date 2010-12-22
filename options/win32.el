; win32.el
;
; This file sets up stuff specific for running NTemacs.

;Only run this stuff on windows.
(cond ((eq window-system 'w32)
       ;; Use Cygwin bash as shell.
       (defun my-shell-setup()
	 "For bash (cygwin 20) under Emacs 20"
	 (setq comint-scroll-show-maximum-output 'this)
	 (setq comint-completion-addsuffix t)
	 ; (setq comint-process-echoes t)
	 (setq comint-eol-on-send t)
	 (make-variable-buffer-local 'comint-completion-addsuffix))
       (setq shell-file-name "bash")
       (setq explicit-shell-file-name shell-file-name)
       (setq explicit-sh-args '("-login" "-i"))
       (setq shell-command-switch "-c")
       (setenv "SHELL" shell-file-name)
       (setenv "PID" nil)
       (setq w32-quote-process-args ?\")
       (setq shell-mode-hook 'my-shell-setup)
       (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
       (setq process-coding-system-alist (cons '("bash" . raw-text-unix)
					       process-coding-system-alist))

       ;;Handle cygwin pathnames and symbolic links.
       (require 'cygwin-mount)
       (cygwin-mount-activate)
       (defun follow-cygwin-symlink ()
	 (save-excursion
	   (goto-char 0)
	   (if (looking-at "!<symlink>")
	       (progn
		 (re-search-forward "!<symlink>\\(.*\\)\0")
		 (find-alternate-file (match-string 1)))
	     )))
       (add-hook 'find-file-hooks 'follow-cygwin-symlink)
       ))

