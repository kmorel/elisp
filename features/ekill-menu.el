;;; ekill-menu.el --- electric-kill-ring mode

;;; Author: Kenneth Moreland <kmorel@sandia.gov>
;;; Version: 1.0

;;; Commentary:

;;; This code was modified from the ebuff-mode.el file that is
;;; distributed with emacs 19.34


;;; Code:

(require 'electric)

(defun show-kill-ring ()
  "Display the contents of the kill-ring.  The kill-ring
is displayed in a buffer named `*Kill Ring*'."
  (interactive)
  (display-buffer
   (let ((standard-output standard-output))
     (save-excursion
       (set-buffer (get-buffer-create "*Kill Ring*"))
       (setq buffer-read-only nil)
       (erase-buffer)
       (setq standard-output (current-buffer))
       (princ "\
  Lines Text
  ----- ----
")
       (let ((kr kill-ring))
	 (while kr
	   (let ((text (car kr))
		 (this-text-line-start (point))
		 this-text-line-count)
	     (princ text)
	     (setq this-text-line-count
		   (count-lines this-text-line-start (point)))
	     (goto-char this-text-line-start)
	     (replace-string "\n" "\\")
	     (goto-char (point-max))
	     (princ "\n")
	     (goto-char this-text-line-start)
	     (indent-to 2)
	     (princ (format "%-5d" this-text-line-count))
	     (indent-to 8 1)
	     (goto-char (point-max)))
	   (setq kr (cdr kr))))
       (goto-char (point-min))
       (next-line 2)
       (setq buffer-read-only t)
       (current-buffer)))))

(defun Kill-menu-text (error-if-non-existent-p)
  "Return kill text described by this line of kill menu."
  (let* ((where (save-excursion
		  (beginning-of-line)
		  (count-lines (point-min) (point))))
	 (index (- where 2)))
    (if (and (>= index 0) (< index (length kill-ring)))
	(elt kill-ring index)
      (if error-if-non-existent-p
	  (error "No kill text on this line")
	nil))))

(defvar electric-kill-menu-mode-map nil)

;;;###autoload
(defun electric-kill-ring ()
  "Pops up a buffer describing the current kill ring.

If the very next character typed is a space then the kill ring
window disappears.  Otherwise, one may move around in the kill ring
window, selecting killed strings to be inserted.

To exit and insert kill-ring text, type a space when the cursor is on
the appropriate line of the kill-ring window.

Calls value of `electric-kill-menu-mode-hook' on entry if non-nil.

\\{electric-kill-menu-mode-map}" 
  (interactive)
  (let ((targetbuffer (current-buffer))
	select buffer)
    (save-window-excursion
      (save-window-excursion (show-kill-ring))
      (setq buffer (window-buffer (Electric-pop-up-window "*Kill Ring*")))
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (Electric-kill-menu-mode)
	    (setq select
		  (catch 'electric-kill-menu-select
		    (message "<<< Press Return to bury the kill ring >>>")
		    (if (eq (setq unread-command-events (list (read-event)))
			    ?\ )
			(progn (setq unread-command-events nil)
			       (throw 'electric-kill-menu-select nil)))
		    (let ((start-point (point))
			  (first (progn (goto-char (point-min))
					(forward-line 2)
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      ;; Use start-point if it is meaningful.
		      (goto-char (if (or (< start-point first)
					 (> start-point last))
				     first
				   start-point))
		      (Electric-command-loop 'electric-kill-menu-select
					     nil
					     t
					     'electric-kill-menu-looper
					     (cons first last))))))
	(set-buffer buffer)
	(message "")))
;This is the code stripped from ebuff-menu.el.  It was designed to handle
;many selections such as saving and deleting buffers.  Currently ekill
;only handles inserting text at the selected point, so let's just do that
;for now.  This code is left for refernece if needed later.
;    (if select
;	(progn (set-buffer buffer)
;	       (let ((opoint (point-marker)))
;		 (Buffer-menu-execute)
;		 (goto-char (point-min))
;		 (if (prog1 (search-forward "\n>" nil t)
;		       (goto-char opoint) (set-marker opoint nil))
;		     (Buffer-menu-select)
;		     (switch-to-buffer (Buffer-menu-buffer t))))))))
    (if select
	(let ((text (save-window-excursion
		      (set-buffer buffer)
		      (Kill-menu-text t))))
	  (insert text)))
    (kill-buffer buffer)))

(defun electric-kill-menu-looper (state condition)
  (cond ((and condition
	      (not (memq (car condition) '(buffer-read-only
					   end-of-buffer
					   beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (forward-line 2))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1)))))

(put 'Electric-kill-menu-mode 'mode-class 'special)
(defun Electric-kill-menu-mode ()
  "Major mode for editing a listing of the kill ring.
Each line describes one of the available strings in the kill ring.
Letters do not insert themselves; instead, they are commands.
\\<electric-buffer-menu-mode-map>
\\[keyboard-quit] or \\[Electric-buffer-menu-quit] -- exit kill menu, returning without inserting any text in
  the current buffer.  If the very first character typed is a space,
  it also has this effect.
\\[Electric-buffer-menu-select] -- insert text of line point is on.

\\{electric-buffer-menu-mode-map}

Entry to this mode via command electric-kill-ring calls the value of
electric-kill-menu-mode-hook if it is non-nil."
  (kill-all-local-variables)
  (use-local-map electric-kill-menu-mode-map)
  (setq mode-name "Electric Kill Menu")
  (setq mode-line-buffer-identification "Electric Kill Ring")
  (make-local-variable 'Helper-return-blurb)
  (setq Helper-return-blurb "return to buffer editing")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'Electric-kill-menu-mode)
  (goto-char (point-min))
  (if (search-forward "\n." nil t) (forward-char -1))
  (run-hooks 'electric-kill-menu-mode-hook))

;;;;End of edited code

(put 'Electric-kill-menu-undefined 'suppress-keymap t)
(if electric-kill-menu-mode-map
    nil
  (let ((map (make-keymap)) (submap (make-keymap)))
    (fillarray (car (cdr map)) 'Electric-kill-menu-undefined)
    (define-key map "\e" submap)
    (fillarray (car (cdr submap)) 'Electric-kill-menu-undefined)
    (define-key map "\C-z" 'suspend-emacs)
    (define-key map "v" 'Electric-kill-menu-mode-view-buffer)
    (define-key map (char-to-string help-char) 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'Electric-kill-menu-quit)
    (define-key map "\C-]" 'Electric-kill-menu-quit)
    (define-key map "q" 'Electric-kill-menu-quit)
    (define-key map " " 'Electric-kill-menu-select)
    (define-key map "\C-m" 'Electric-kill-menu-select)
    (define-key map "\C-l" 'recenter)
    ;(define-key map "s" 'Buffer-menu-save)
    ;(define-key map "d" 'Buffer-menu-delete)
    ;(define-key map "k" 'Buffer-menu-delete)
    ;(define-key map "\C-d" 'Buffer-menu-delete-backwards)
    ;(define-key map "\C-k" 'Buffer-menu-delete)
    ;(define-key map "\177" 'Buffer-menu-backup-unmark)
    ;(define-key map "~" 'Buffer-menu-not-modified)
    ;(define-key map "u" 'Buffer-menu-unmark)
    (let ((i ?0))
      (while (<= i ?9)
	(define-key map (char-to-string i) 'digit-argument)
	(define-key map (concat "\e" (char-to-string i)) 'digit-argument)
	(setq i (1+ i))))
    (define-key map "-" 'negative-argument)
    (define-key map "\e-" 'negative-argument)
    ;(define-key map "m" 'Buffer-menu-mark)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\ev" 'scroll-down)
    (define-key map ">" 'scroll-right)
    (define-key map "<" 'scroll-left)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (define-key map "\e\e" nil)
    (define-key map "\e\e\e" 'Electric-kill-menu-quit)
    (define-key map [escape escape escape] 'Electric-kill-menu-quit)
    (define-key map [mouse-2] 'Electric-kill-menu-mouse-select)
    (setq electric-kill-menu-mode-map map)))
 
(defun Electric-kill-menu-select ()
  "Leave Electric Kill Menu, inserting text at point."
  (interactive)
  (throw 'electric-kill-menu-select (point)))

(defun Electric-kill-menu-mouse-select (event)
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (set-buffer (window-buffer (selected-window)))
  (goto-char (posn-point (event-end event)))
  (throw 'electric-kill-menu-select (point)))

(defun Electric-kill-menu-quit ()
  "Leave Electric Kill Menu withoug doing anything."
  (interactive)
  (throw 'electric-kill-menu-select nil))

(defun Electric-kill-menu-undefined ()
  (interactive)
  (ding)
  (message "%s"
	   (if (and (eq (key-binding "\C-c\C-c") 'Electric-kill-menu-quit)
		    (eq (key-binding " ") 'Electric-kill-menu-select)
		    (eq (key-binding (char-to-string help-char)) 'Helper-help)
		    (eq (key-binding "?") 'Helper-describe-bindings))
	       (substitute-command-keys "Type C-c C-c to exit, Space to select, \\[Helper-help] for help, ? for commands")
	     (substitute-command-keys "\
Type \\[Electric-buffer-menu-quit] to exit, \
\\[Electric-buffer-menu-select] to select, \
\\[Helper-help] for help, \\[Helper-describe-bindings] for commands.")))
  (sit-for 4))

(autoload 'view-mode-enter "view")

(defun Electric-kill-menu-mode-view-buffer ()
  "View kill text on current line in Electric Buffer Menu.
Returns to Electric Kill Ring when done."
  (interactive)
  (let ((this-buf (current-buffer))
	(new-buf (generate-new-buffer "*Kill Text*"))
	(text (Kill-menu-text nil)))
    (cond (text (switch-to-buffer new-buf)
		(insert text)
		(view-mode-enter (list this-buf) 'kill-buffer))
	  (t (ding)
	     (message "Text does not exist!")
	     (sit-for 4)))))

(provide 'ekill-menu)

;;; ekill-menu.el ends here
