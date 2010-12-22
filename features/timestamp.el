;Some functions for writing timestamps

(defvar writestamp-format "%e %B %Y"
  "*Format for writestamps (same format as function 'format-time-string').
If set to nil, writestamps will not occur.")

(defvar writestamp-prefix nil
  "*Unique string identifying start of writestamp.  If set to nil, writestamps
will not occur.")

(defvar writestamp-suffix nil
  "*String that terminates a writestamp.  If set to nil, writestamps will not
occur.")

(make-variable-buffer-local 'writestamp-format)
(make-variable-buffer-local 'writestamp-prefix)
(make-variable-buffer-local 'writestamp-suffix)

(add-hook 'write-file-hooks 'update-writestamps)
(defun update-writestamps ()
  "Find writestamps and replace them with the current time."
  (if (and writestamp-prefix writestamp-suffix writestamp-format)
      (save-excursion
	(save-restriction
	  (save-match-data
	    (widen)
	    (goto-char (point-min))
	    (while (search-forward-regexp writestamp-prefix nil t)
	      (let ((start (point)))
		(search-forward-regexp writestamp-suffix)
		(delete-region start (match-beginning 0))
		(goto-char start)
		(insert (format-time-string writestamp-format (current-time))))
	      )
	    nil
	    )
	  )
	)
    )
  )

(defun bind-html-timestamp ()
  "
Causes the function \"html-timestamp\" to be called any time this buffer is
saved.  The \"html-timestamp\" updates the buffer with the current date
(see that function's documentation for more details on how to do this).
To use \"html-timestamp\", put this in your .emacs section:

  (add-hook 'html-mode-hook 'bind-html-timestamp)
"
  (add-hook 'write-contents-hooks 'html-timestamp))

(defun html-timestamp ()
  "
Update the timestamp in an HTML buffer.

This function will look for

  <!-- mod-date=\"FORMAT\"-->DATE

and replace DATE with the current date.  FORMAT is anything acceptable
to `format-time-string.'  Note that DATE includes everything up to the
end of the line.  (Any text before the `<!--' is unchanged.)

Example:
  <i><font size=-1>Last modified: <!-- mod-date=\"%B %e, %Y\"-->January 8, 1998
  </font></i>

The function \"bind-html-timestamp\" can be used to enable this function
whenever an html buffer is loaded.  So you can enable this function with
the follwing in your .emacs file:

  (add-hook 'html-mode-hook 'bind-html-timestamp)

However, if \"bind-html-timestamp\" does not work because you do not have
an html-mode, you can use:

  (add-hook 'write-contents-hooks 'html-timestamp)

to call this function whenever any file is saved.

"
  (save-excursion
    (save-restriction
      (save-match-data
	(goto-char (point-min))
	(while (re-search-forward
		"<!-- *mod-date=\"\\([^\"\n]+\\)\" *-->\\(.*\\)$"
		nil t)
	  (replace-match (format-time-string (match-string 1) (current-time))
			 t t nil 2))
	)
      )
    )
  nil)

(defun bind-tex-timestamp ()
  "
Causes the function \"tex-timestamp\" to be called any time this buffer is
saved.  The \"tex-timestamp\" updates the buffer with the current date
(see that function's documentation for more details on how to do this).
To use \"tex-timestamp\", put this in your .emacs section:

  (add-hook 'tex-mode-hook 'bind-tex-timestamp)
"
  (add-hook 'write-contents-hooks 'tex-timestamp))

(defun tex-timestamp ()
  "
Update the timestamp in a tex (or latex) buffer.

This function will look for

  %TIMESTAMP=`FORMAT'
  \\date{DATE}

and replace DATE with the current date.  FORMAT is anything acceptable
to `format-time-string.'  Note that there should be nothing on the line
after the \\date command since it will be deleted.

Example:
  %TIMESTAMP=`%B %e, %Y'
  \\date{January 21, 2002}

The function \"bind-tex-timestamp\" can be used to enable this function
whenever a TeX or LaTeX buffer is loaded.  So you can enable this function
with the follwing in your .emacs file:

  (add-hook 'tex-mode-hook 'bind-tex-timestamp)

However, if \"bind-tex-timestamp\" does not work because you do not have
a tex-mode, you can use:

  (add-hook 'write-contents-hooks 'tex-timestamp)

to call this function whenever any file is saved.

"
  (save-excursion
    (save-restriction
      (save-match-data
	(goto-char (point-min))
	(while (re-search-forward
		" *%TIMESTAMP=`\\([^'\n]+\\)' *\n *\\\\date{\\([^}]*\\)}"
		nil t)
	  (replace-match (format-time-string (match-string 1) (current-time))
			 t t nil 2))
	)
      )
    )
  nil)

(provide 'timestamp)
