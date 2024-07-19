; Some customizations I use for the macports version of emacs.

(load (concat elispdir "/init.el"))

;(load "~/Library/Preferences/Aquamacs Emacs/customizations.el")

; The suspend-frame does not properly minimize the frame currently.  Use
; iconify-frame instead.
(global-set-key "\C-x\C-z" 'iconify-frame)

; Bind Meta-z to undo, since that is what it is bound to for most Mac
; applications.  It would be nice to do the same to \M-x, \M-c, and \M-v in
; a way similar to cua-mode.
(global-set-key "\M-z" 'undo)

(cond
 ((file-exists-p "/opt/homebrew/bin/aspell")
  (setq-default ispell-program-name "/opt/homebrew/bin/aspell"))
 ((file-exists-p "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))
 ((file-exists-p "/opt/local/bin/aspell")
  (setq-default ispell-program-name "/opt/local/bin/aspell"))
 )

(setcdr (assoc 'font default-frame-alist)
	"Menlo")

; I am having troubles with the box cursor: it covers up the character so
; that you cannot see it and it leaves annoying "ghosts" behind sometimes.
; Use the bar, which does not hide things behind it.
(setcdr (assoc 'cursor-type default-frame-alist) 'bar)

; Add support for mouse wheel scrolling.  This comes from some guy named
; Rodney on the internet.
;; Enable wheelmouse support by default
(require 'mwheel)

;;Define the mouse scroll wheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;;Support for scrolling Griffin powermate
(global-set-key [wheel-down] 'up-slightly)
(global-set-key [wheel-up] 'down-slightly)
(global-set-key [double-wheel-down] 'up-slightly)
(global-set-key [double-wheel-up] 'down-slightly)
(global-set-key [triple-wheel-down] 'up-slightly)
(global-set-key [triple-wheel-up] 'down-slightly)

; Make the "command" key behave like the meta modifier.
(setq ns-command-modifier (quote meta))
