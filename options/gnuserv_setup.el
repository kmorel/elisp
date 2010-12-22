;These are features that I find useful when using the (wonderful) gnuserv
;package.  It causes any window made by gnuserv to be raised when created.
;It also remaps C-x C-c to delete the current frame and exit, simulating as
;if the created frame was its own instance of emacs (and helps prevent me
;from quiting emacs when I don't really want to).

(cond
 (window-system
  (condition-case nil
      (progn
	(require 'framefun)
	(require 'gnuserv)
	(defun server-make-window-visible()
	  "Try to make this window even more visible."
	  (and (boundp 'window-system)
	       (cond ((fboundp 'raise-frame)
		      (raise-frame (selected-frame)))
		     ((fboundp 'deiconify-screen)
		      (deiconify-screen (selected-screen))
		      (raise-screen (selected-screen)))
		     ((fboundp 'mapraised-screen)
		      (mapraised-screen))
		     ((fboundp 'x-remap-window)
		      (x-remap-window)
		      ;;give window change to re-display text
		      (accept-process-output)))))
	(gnuserv-start)
	)
    (error (message "GnuServ not avaliable."))
    )
  )
 )

