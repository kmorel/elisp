;;; .emacs
;; This file is a .emacs that loads all the setup files in the subdirectories.
;; Rather than copy the file and modify it it is best to load it in your
;; real .emacs and then make changes.
;;
;; Here is how the beginning of your .emacs should look, where <elisp> is
;; the directory that this file is in (such as ~/local/elisp).
;;
;;     (setq elispdir "<elisp>")
;;     (load "<elisp>/init.el")
;;
;;     <Site specific setup>
;;
;; In order for this to work, you must run the install.sh script in this
;; directory.
;;

;Set the default place to find elisp files.
(defvar elispdir "~/elisp")

;Add local repository for elisp packages.
(load-file (concat elispdir "/elisp-dirs.el"))

;-----This stuff requires added packages.--------
;A simple package to update all buffers
(load "revbufs")

;Adds entries in Edit menu for changing EOL encodings.
(require 'eol-conversion)

;Sets up calculator mode.
(autoload 'calculator "calculator"
  "Run the pocket calculator." t)
;(global-set-key [(control return)] 'calculator)
(global-set-key [(control ?=)] 'calculator)
(custom-set-variables
 '(calculator-number-exp-llimit 1e-015)
 '(calculator-number-format "%f")
 '(calculator-user-registers (quote ((112 . 3.141592653589793) (101 . 2.718281828459045)))))
(setq calculator-user-operators
      '(("lg" log10	(log X 10)			x 6)
	("ln" ln 	log				x 6)
	("l2" log2 	(log X 2)			x 6)
	("R"  Nroot	(expt X (/ 1 Y))		2 7)
	("v" avg	(/ (apply '+ L) (length L))	0 8)
	("t" tot	(apply '+ L)			0 8)
	))

;Readies igrep
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
;(autoload 'dired-do-igrep "igrep"
;  "*Run `grep` on the marked (or next prefix ARG) files." t)
;(autoload 'dired-do-igrep-find "igrep"
;  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
;(autoload 'Buffer-menu-igrep "igrep"
;  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'egrep "igrep"
  "*Run `egrep`..." t)
(autoload 'fgrep "igrep"
  "*Run `fgrep`..." t)
(autoload 'egrep-find "igrep"
  "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep"
  "*Run `fgrep` via `find`..." t)

;Enable "windows-like" selecting, cutting, copying and pasting.
(if (require 'cua-base nil t)
    (cua-mode t)
  (load "cua")
  (CUA-mode t))

(custom-set-variables
 '(CUA-mode-normal-cursor-color "white")
 '(CUA-mode-read-only-cursor-color "green"))

;-----end added packages.------

;eshell setup
(defun myeshell/pushd (&rest args)
  "My version of pushd that echos the pushable paths."
  (eval (cons 'eshell/pushd args))
  (eshell-echo (cons (eshell/pwd) eshell-dirstack))
  )
(defun myeshell/popd (&rest args)
  "My version of popd that echos the pushable paths."
  (eval (cons 'eshell/popd args))
  (eshell-echo (cons (eshell/pwd) eshell-dirstack))
  )
(custom-set-variables
; '(eshell-prefer-to-shell t nil (eshell))
 '(eshell-plain-echo-behavior t)
 '(eshell-cmpl-ignore-case t)
 )

;Loads miscellaneous options
(load "general_setup")
(load "win32")
(load "unix")

;Sets up my keyboard
(load "keyboard_setup")

;Allows Windows to edit stuff in a currently running emacs
(load "server_setup")
(require 'framefun)

;Sets up all my programming edit functions.
(load "programming_setup")

;Sets up other textual editing.
(load "text_setup")

;Make backups go to the directory "~/.backups/emacs"
;(require 'backups)
;(move-backups t)
(setq backup-directory-alist '((".*" . "~/.backups/emacs")))
;Makes backups for version control also go to backup directory
(setq version-control 'never)
;Prevent annoying backup-like files from being created by vc.
(setq vc-cvs-stay-local nil)

;
; Startup stuff that should only be run if emacs is first started (not when I
; do eval-current-buffer).
;
(defvar have-run-emacs nil)
(if have-run-emacs
    (message "File .emacs evaluated successfully")
  (progn
    (message "Emacs started!")
    (setq have-run-emacs t)))

(put 'erase-buffer 'disabled nil)

;Set Frame sizes, fonts, etc (made for Win NT)
;Use (current-frame-configuration) to get current values for a given frame
;(Type into scratch buffer and hit C-j).
;You can also use (insert (prin1-to-string (x-list-fonts "*"))) to get
;a list of all possible fonts.
;A good way to change any of these values is
;  (setcdr (assoc '<attrib> default-frame-alist) <value>)
;For example, to change the font you could use:
;  (setcdr (assoc 'font default-frame-alist)
;	    "-*-Monotype.com-normal-r-*-*-11-82-*-*-c-*-*-ansi-")
(setq default-frame-alist
        '((top . 5) (left . 350)
          (width . 80) (height . 45)
          (cursor-color . "ivory")
          (cursor-type . box)
          (foreground-color . "ivory")
          (background-color . "black")
          (font . "7x13")))
 
(setq initial-frame-alist '((top . 25) (left . 30)))

; Sets the background mode as dark.
(custom-set-variables
 '(frame-background-mode (quote dark)))

(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "ivory")

(set-face-foreground 'default "ivory")
(set-face-background 'default "black")

; Get rid of stupid blinking cursor
(custom-set-variables
 '(blink-cursor nil))
