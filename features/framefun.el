;These are functions that make dealing with frames easier.

;I originally wrote this for use with gnuserve, but it very useful for work
;with frames in general.
(defun kill-buffer-delete-frame ()
  "
Kills the buffer in the current frame and deletes the frame.
Useful when used in conjunction with gnuclient.

Also makes a very good replacement for C-x C-c keybinding.  Often when you
deal with multiple frames you wish to think of them as their own instance
and kill them as such, but you never want to exit emacs.  This function
will kill the buffer and delete the frame as if it was its own instance
\(like notepad.exe for MS Windows\), but will ask you before closing the
only remaining frame.
"
  (interactive)
  (if (eq (cdr (frame-list)) nil)
      (if (y-or-n-p
	   "Only one frame remains.  Do you wish to leave emacs? ")
	  (save-buffers-kill-emacs)
	)
    (if (cond ((not (buffer-modified-p)) nil)
	      ((char-equal (elt (buffer-name) 0) ?*) nil)
	      ((y-or-n-p (format "%s is not saved.  Save now? " (buffer-name)))
	       (save-buffer))
	      ((yes-or-no-p (format "%s is modified.  Kill anyway? "
				    (buffer-name)))
	       (set-buffer-modified-p nil))
	      (t t))
	nil
      (kill-this-buffer)
      (delete-frame)
      )
    )
  )

(global-set-key "\C-x\M-\C-c" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-c" 'kill-buffer-delete-frame)

(provide 'framefun)
