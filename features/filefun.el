;Functions for making using files and buffers easier.

(defun name-buffer-by-dir (num-dirs)
  "
Renames the current buffer as FILENAME<DIRNAME>.  It helps when working
with lots of files with the same name (such as makefiles).  If given a
number argument, DIRNAME will contain that many directories in it.
"
  (interactive "p")
  (setq path (buffer-file-name))
  (setq index (- (length path) 1))
  (while (not (= (elt path index) ?/))
    (setq index (- index 1))
    )
  (setq fname (substring path (+ index 1)))
  (setq path (substring path 0 index))
  (if (< num-dirs 1)
      (rename-buffer fname)	; Special case: restore buffer name.
    (setq count 0)
    (while (< count num-dirs)
      (setq index (- index 1))
      (while (and (> index -1)
		  (not (= (elt path index) ?/)))
	(setq index (- index 1))
	)
      (setq count (+ count 1))
      )
    (rename-buffer (format "%s<%s>" fname (substring path (+ index 1))))
    )
  )

(provide 'filefun)
