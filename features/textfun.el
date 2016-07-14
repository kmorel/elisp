; Functions to help with text editing.

(defun delete-leading-line-whitespace ()
  "Delete all the trailing whitespace across the current buffer.
All whitespace before the first non-whitespace character in this line is
deleted."
  (interactive "*")
  (save-match-data
    (save-excursion
      (end-of-line)
      (if (re-search-backward "^\\s-+" (point-at-bol) t)
	  (replace-match "")
	)
      )
    )
  )

(defun right-justify-current-line ()
  "Right justifies the current line.  The last character of the line will
be at the column defined by `fill-column'.  If the line (minus the leading
spaces) is longer than `fill-column', then the line is left justified."
  (interactive "*")
  (delete-leading-line-whitespace)
  (let ((save-adaptive-fill-mode adaptive-fill-mode))
    (setq adaptive-fill-mode nil)
    (justify-current-line 'right)
    (setq adaptive-fill-mode save-adaptive-fill-mode)
    )
  )

(defun toggle-word-wrap-mode ()
  "Toggles whether word wrap is performed with auto-fill-mode, which inserts
actual line breaks in the document, or with visual-line-mode, which visually
breaks lines around words but has no actual newline in the document."
  (interactive)
  (if visual-line-mode
      (progn (message "Wrap lines with actual newline characters.")
	     (auto-fill-mode 1)
	     (visual-line-mode 0))
    (message "Display wrapped lines that have no newlines.")
    (auto-fill-mode 0)
    (visual-line-mode 1)
    )
  )


(provide 'textfun)
