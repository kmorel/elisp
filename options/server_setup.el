;The use of gnuserv is more-or-less depricated for use of emacsclient
;which comes packaged with most versions of emacs since version 22.
;If we are using an older version of emacs, use the older gnuserv_setup
;to use gnuserv/gnuclient.  Otherwise set things up with emacsclient.
;Also add the frame functions for use with the server: remap C-x C-c to
;delete the current frame rather than exit.  This will simulate the frame
;being created as its own emacs instance and also prevents accidentally
;shutting down emacs.

(cond
 ((< emacs-major-version 22) (load "gnuserv_setup"))
 (window-system
  (condition-case nil
      (progn
	(require 'framefun)
	(server-start)
	)
    (error (message "Trouble starting server."))
    )
  )
 )
