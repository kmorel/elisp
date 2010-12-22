;This file provides a minor mode called refill-mode.  This mode causes text
;to be "filled" as you type.  That is, words will be set into lines so that
;all the lines in a paragraph will fill an entire line as best a possible.

(require 'point)

(defvar refill-mode nil
  "Mode variable for refill minor mode.")
(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
  "
(refill-mode &optional arg)

Turns the refill minor mode on or off.  If no argument is given, refill
mode is toggled.  If a non-nil argument is given, refill-mode is turned
on.  It is turned off otherwise.

Refill mode causes the buffer to fill paragraphs (that is, make the lines
fit the width of the window) with every edit.

Since this makes extensive use of paragraph filling, you better make sure
that the variables \"paragraph-start\" and \"paragraph-separate\" are correctly
set for the current mode.
"
  (interactive "P")
  (setq refill-mode
	(if (null arg)
	    (not refill-mode)
	  (> (prefix-numeric-value arg) 0))
	)
;  (if refill-mode
;      (add-hook 'after-change-functions 'refill nil t)
;    (remove-hook 'after-change-functions 'refill t))
  )

(add-hook 'after-change-functions 'refill nil t)

(if (not (assq 'refill-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(refill-mode " Refill")
		minor-mode-alist))
  )

(defun refill (start end len)
  "
To be called after a text change.  It refills the current paragraph when
appropriate.
"
  (if (not refill-mode)
      nil
    (let ((left (if (zerop len)
		    start
		  (save-point
		   (max (progn
			  (goto-char start)
			  (beginning-of-line 0)
			  (point))
			(progn
			  (goto-char start)
			  (backward-paragraph 1)
			  (point))
			)
		   )
		  )
		)
	  )
      (if (or (and (zerop len)
		   (same-line-p start end)
		   (short-line-p end))
	      (and (eq (char-syntax (preceding-char))
		       ?\ )
		   (looking-at "\\s *$"))
	      )
	  nil
	(save-point
	 (fill-region left end nil nil t)))
      )
    )
  )

(defun same-line-p (start end)
  "Are START and END on the same line?"
  (save-point
   (goto-char start)
   (end-of-line)
   (<= end (point)))
  )

(defun short-line-p (pos)
  "Does the line containing POS stay within 'fill-column'?"
  (save-point
   (goto-char pos)
   (end-of-line)
   (<= (current-column) fill-column))
  )

(provide 'refill)
