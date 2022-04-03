;Various "other" options

;Enables the eval-expression command ("\M-:")
(put 'eval-expression 'disabled nil)

;Enables the erase-buffer command
(put 'erase-buffer 'disabled nil)

;Highlight matching parenthesis
(show-paren-mode 1)

;Enable minibuffer resizing on demand
(if (< emacs-major-version 22)
    (resize-minibuffer-mode)
  )

;Turn off the startup screen
(setq inhibit-startup-screen t)

;Disable the creation of list of files that were auto-saved.
(setq auto-save-list-file-prefix nil)

;Makes find-file ignore the case of filenames and directories
(setq completion-ignore-case t)

;Interactively shows completions in the minibuffer.
(icomplete-mode)

;icomplet-mode can make for very large minibuffers.  Limit the size since
;large minibuffers are not very helpful.
(setq resize-minibuffer-window-max-height 3)

;I dislike having the down arrow modifying my document.
(custom-set-variables '(next-line-add-newlines nil))

;I have gotten into the habit of using one space after a period instead of
;two.
(setq sentence-end-double-space nil)

;I don't like it when emacs auto-indents things with tabs. You get
;inconsistent spacing because everyone uses different tab widths.
(setq-default indent-tabs-mode nil)

;Make a functions to tabify and untabify entire buffers (as
;opposed to regions)
(defun untabify-buffer ()
  "Replaces tabs with spaces for the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))
(defun tabify-buffer ()
  "Replaces spaces with tabs where applicable for the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(setq line-number-mode t)
(setq column-number-mode t)
(setq auto-save-timeout 10)
(setq display-time t)
(setq-default auto-fill-mode t)
(setq-default fill-column 75)

;Causes C-' to scroll the window down and C-/ to scroll the window up.
(global-set-key [?\^'] (lambda () "Scroll the window down."
			 (interactive) (scroll-down 1)))
(global-set-key [?\^/] (lambda () "Scroll the window up."
			 (interactive) (scroll-up 1)))

;Forces cursur to be at back of text strings when scrolling through history
;in the minibuffer
(defadvice next-history-element (after end-nhe act comp)
  "Forces point to be at the end instead of at the beginning of prompt."
  (goto-char (point-max)))

; Makes *compilation* buffer scroll automatically
(defadvice compile-internal (after compile-auto-scroll activate compile)
  "Forces compile buffer to scroll.  See around line 363 in compile.el"
  (let ((curbuf (current-buffer)))
    (select-window (get-buffer-window (compilation-find-buffer)))
    (goto-char (point-max))
    (select-window (get-buffer-window curbuf))))

;Will refuse to let you switch to a non-existant buffer with the use
;of \C-xb unless you use a prefix argument (i.e. \U-\C-xb).
(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only
unless given a prefix argument"
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

;When you hit C-x C-r, the buffer is renamed to filename<dirname>.
(require 'filefun)
(global-set-key "\C-x\C-r" 'name-buffer-by-dir)

(defun screen-alert-colors ()
  "
Causes the screen to display in reverse colors to attract the user's attention.
"
  (set-foreground-color (cdr (assoc 'background-color default-frame-alist)))
  (set-background-color (cdr (assoc 'foreground-color default-frame-alist)))
  (set-cursor-color "Red"))

;(add-hook 'minibuffer-setup-hook 'screen-alert-colors)

(defun restore-screen-colors ()
  "Restores the screen colors to those listed in \"default-frame-alist\"."
  (set-foreground-color (cdr (assoc 'foreground-color default-frame-alist)))
  (set-background-color (cdr (assoc 'background-color default-frame-alist)))
  (set-cursor-color (cdr (assoc 'cursor-color default-frame-alist))))

;(add-hook 'minibuffer-exit-hook 'restore-screen-colors)
