;; Code collapse minor mode for emacs
;;
;; This minor mode is like outline-mode in that it will hide sections
;; of a buffer without really deleting them.

;;
;;Necessary evils.  All the things required to make this a minor mode.
;;

;Variable used to define if collapse mode is in use.
(defvar collapse-mode nil
  "Mode variable for collapse minor mode.")
(make-variable-buffer-local 'collapse-mode)

;Function to turn collapse mode on and off
(defun collapse-mode (&optional arg)
  "
collapse-mode: Turn the code collapse minor mode on or off.

If arg is null, toggle collapse mode.  If arg is not null, collapse mode is
turned if arg's value is greater than 0, turned off otherwise.

\(collapse-mode &optional arg\)
"
  (interactive "P")
  (if (if (null arg)
	  (not collapse-mode)
	(> (prefix-numeric-value arg) 0))
      (collapse-mode-on)
    (collapse-mode-off))
  )

;Add this mode to the minor-mode-alist
(if (not (assq 'collapse-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(collapse-mode " >|c|<") minor-mode-alist))
  )

;;
;; Variables to use to edit the behavour of collapse mode
;;

(defvar collapse-block-begin "{"
  "*Header for a collapsible section of code.

This string is used by collapse-mode as the header for a collapsable
section of code.  Any section of code between a collapse-block-begin and
a collapse-block-end can be \"collapsed\" or temporarily removed from
view.")
(make-variable-buffer-local 'collapse-block-begin)

(defvar collapse-block-end "}"
  "*Footer for a collapsible section of code.

This string is used by collapse-mode as the footer for a collapsable
section of code.  Any section of code between a collapse-block-begin and
a collapse-block-end can be \"collapsed\" or temporarily removed from
view.")
(make-variable-buffer-local 'collapse-block-end)

;;
;; Startup and shutdown collapse mode
;;

(defun collapse-mode-on ()
  (setq line-move-ignore-invisible t)
  (setq buffer-invisibility-spec t)
  (setq collapse-mode t))

(defun collapse-mode-off ()
  (expand-buffer)
  (setq collapse-mode nil))

;;
;; collapse and expand functions
;;

(defun collapse-area (start-point end-point)
  "
Checks the area between start-point and end-point and collapse anything
between a collapse-block-begin and collapse-block-end set that is not
nested.

\(collapse-area start-point end-point\)
"
  (if (not collapse-mode)
      (error "Not in collapse-mode.")
    )
  (save-excursion
    (save-match-data
      (save-modification
       (goto-char start-point)
       (let ((begin-hide nil)
	     (end-hide nil)
	     )
	 (while (find-collapse-begin end-point)
	   (setq begin-hide (match-end 0))
	   (if (not (find-collapse-end end-point))
	       (error "Found block begin, '%s', with no matching end, '%s'"
		      collapse-block-begin collapse-block-end)
	     )
	   (setq end-hide (match-beginning 0))
	   (put-text-property begin-hide end-hide 'invisible t)
	   )
	 nil)
       )
      )
    )
  )

(defun collapse-buffer ()
  "Collapses the entire buffer."
  (interactive)
  (collapse-area (point-min) (point-max))
  )

(defun collapse-region ()
  "
Collapses everything in the current region.  This function will fail if the region
contains a collapse-block-begin without a matching collapse-block-end.
"
  (collapse-area (region-beginning) (region-end))
  )

(defun collapse-below ()
  "
Collapses the first block below the current pointer.  If there is not a
next block within the current block, an error is returned.
"
  (interactive)
  (save-match-data
    (let ((save-point (point))
	  (block-start nil)
	  (block-end nil))
      (unwind-protect
	  (progn
	    (if (not (find-collapse-begin (point-max)))
		(error ("Next block not found."))
	      )
	    (setq block-start (match-beginning 0))
	    (if (not (find-collapse-end (point-max)))
		(error ("Next block not closed."))
	      )
	    (setq block-end (match-end 0))
	    (goto-char save-point)
	    (if (find-collapse-end block-start)
		(error ("No more blocks within current block."))
	      )
	    (collapse-area block-start block-end)
	    )
	(goto-char save-point)
	)
      )
    )
  nil
  )

(defun collapse-this ()
  "
Collapses the block that the pointer is currently in.  If the pointer is
not in a block, an error is returned.
"
  (interactive)
  (if (not collapse-mode)
      (error "Not in collapse-mode.")
    )
  (save-match-data
    (let ((save-point (point))
	  (block-end nil))
      (unwind-protect
	  (progn
	    (if (not (find-collapse-end (point-max)))
		(error "Not in a block.")
	      )
	    (setq block-end (match-end 0))
	    (goto-char save-point)
	    (if (not (reverse-find-collapse-begin (point-min)))
		(error "Not in a block.")
	      )
	    ;Since point is in collapsed block, we now want to save above block.
	    (setq save-point (match-beginning 0))
	    (collapse-area save-point block-end)
	    )
	(goto-char save-point)
	)
      )
    )
  )

;;;;;;;; needs more work
(defun do-expand-at-cursor (expand-function)
  "
Will perform some sort of expansion at the next block if the point is
not in a collapsed area, or the expansion of the area the cursor is in.

Used to implement expand-here and telescope out.
"
  (if (not collapse-mode)
      (error "Not in collapse-mode")
    )
  (save-match-data
    (let ((save-point (point))
	  (start-point nil)
	  (end-point nil)
	  (spoint nil)
	  )
      (unwind-protect
	  (if (get-text-property (point) 'invisible)  ;;;;;;here!!!
	      (progn
		(setq start-point
		      (previous-single-property-change (point) 'invisible))
		(setq end-point
		      (previous-single-property-change (point) 'invisible))
		(funcall expand-function start-point end-point)
		)
	    (if (not (find-collapse-begin (point-max)))
		(error "Next block not found.")
	      )
	    (setq start-point (match-beginning 0))
	    (setq spoint (match-end 0))
	    (if (not (find-collapse-end (point-max)))
		(error "Next block not closed.")
	      )
	    (setq end-point (match-end 0))
	    (funcall expand-function start-point end-point)
	    (setq save-point spoint)
	    )
	(goto-char save-point)
	)
      )
    )
  )

(defun expand-here ()
  "
If the point is inside a collapsed area, the area is expanded.  If the
point is not inside a collapsed area, the next block and everything inside
it is expanded.
"
  (interactive)
  (save-excursion
    (do-expand-at-cursor 'expand-area)
    )
  )

(defun telescope-out ()
  "
Telescopes the block after the cursor.
"
  (interactive)
  (do-expand-at-cursor 'telescope-area)
  )

(defun telescope-area (start-point end-point)
  "
Expands all blocks within the given region and collapses all blocks within
them.
"
  (save-excursion
    (save-match-data
      (expand-area start-point end-point)
      (goto-char start-point)
      (while (find-collapse-begin end-point)
	(let ((block-start (match-end 0))
	      (block-end nil))
	  (if (not (find-collapse-end end-point))
	      nil
	    (setq block-end (match-beginning 0))
	    (collapse-area block-start block-end)
	    )
	  )
	)
      )
    )
  )

(defun expand-area (start-point end-point)
  "
Expands the entire area between start-point and end-point.  If start-point
or end-point is in the middle of a collapsed segement, then only the part
of the segment which is in the given area will be expanded.
"
  (if (not collapse-mode)
      (error "Not in collapse-mode")
    )
  (save-modification
   (remove-text-properties start-point end-point '(invisible nil))
   nil)
  )

(defun expand-buffer ()
  "Expands the entire buffer."
  (interactive)
  (expand-area (point-min) (point-max))
  )
(defun expand-region ()
  "
Expands the entire area in the region.  If the start or end of the region
is in the middle of a collapsed segement, then only the part of the segment
which is in the given area will be expanded.
"
  (interactive)
  (expand-area (region-beginning) (region-end))
  )

(defun find-collapse-begin (end-point)
  "Finds the first occurence of collapse-block-begin and puts the point
at the end of it, and point is returned.  On failure point is put at end-point
and nil is returned."
  (search-forward collapse-block-begin end-point t)
  )

(defun find-collapse-end (end-point)
  "
Find the first occurence of collapse-block-end that does not match a
collapse-block-begin.  If such a string cannot be found, nil is returned.
Otherwise, the current point is returned.

This function is typically called after (find-collapse-begin).
"
  (catch 'collapse-block-end
    (let ((last-cbegin (point))
	  (last-cend (point))
	  )
      (while (and (<= last-cbegin last-cend)
		  (< last-cbegin (point-max))
		  )
	(goto-char last-cbegin)
	(if (search-forward collapse-block-begin end-point t)
	    (setq last-cbegin (point))
	  (setq last-cbegin (point-max))
	  )
	(goto-char last-cend)
	(if (not (search-forward collapse-block-end end-point t))
	    (throw 'collapse-block-end nil)
	  )
	(setq last-cend (point))
	)
      (point))
    )
  )

(defun reverse-find-collapse-begin (start-point)
  "
Find the first occurence of collapse-block-begin behind the point that
does not match a collapse-block end.  On success, point is returned.  On
failure, nil is returned.
"
  (catch 'collapse-block-begin
    (let ((last-cbegin (point))
	  (last-cend (point))
	  )
      (while (and (<= last-cbegin last-cend)
		  (> last-cend (point-min))
		  )
	(goto-char last-cend)
	(if (search-backward collapse-block-end start-point t)
	    (setq last-cend (point))
	  (setq last-cend (point-min))
	  )
	(goto-char last-cbegin)
	(if (not (search-backward collapse-block-begin start-point t))
	    (throw 'collapse-block-begin nil)
	  )
	(setq last-cbegin (point))
	)
      (point))
    )
  )

(defun reverse-find-collapse-end (start-point)
  "Finds the first occurence of collapse-block-end behind the point and puts
the point at the beginning of it, and point is returned.  On failure point
is put at end-point and nil is returned."
  (search-backward collapse-block-end start-point t)
  )

;;
;; Movement around collapsible areas
;;

(defun goto-collapse-begin ()
  "
Moves the current point to the beginning of the collapsible block it resides in.
Emits an error if not in a collapsible block.
"
  (interactive)
  (save-match-data
    (let ((save-point (point)))
      (if (reverse-find-collapse-begin (point-min))
	  (point)
	(goto-char save-point)
	(error "Not in collapsible block.")
	)
      )
    )
  )

(defun goto-collapse-end ()
  "
Moves the current point to the end of the collapsible block it resides in.
Emits an error if not in a collapsible block.
"
  (interactive)
  (save-match-data
    (let ((save-point (point)))
      (if (find-collapse-end (point-max))
	  (point)
	(goto-char save-point)
	(error "Not in collapsible block.")
	)
      )
    )
  )

(defun goto-next-collapse ()
  "
Moves the current point to the beginning of the next collapsible block.  If
the current block does not have a next collapsible block, the cursor moves to
the next block outside of this one.  An error is emited if a next block
cannot be found.
"
  (interactive)
  (save-match-data
    (let ((save-point (point)))
      (cond ((not (find-collapse-begin (point-max)))
	     (goto-char save-point)
	     (error "Next block does not exist."))
	    ((not (= save-point (match-beginning 0)))
	     (goto-char (match-beginning 0))
	     )
	    ((not (find-collapse-end (point-max)))
	     (goto-char save-point)
	     (error "Block not closed"))
	    ((not (find-collapse-begin (point-max)))
	     (goto-char save-point)
	     (error "Next block does not exist."))
	    (t (goto-char (match-beginning 0))
	       )
	  )
      )
    )
  )

(defun goto-previous-collapse ()
  "
Moves the current point to the beginning of the next collapsible block.  If
the current block does not have a next collapsible block, the cursor moves to
the next block outside of this one.  An error is emited if a next block
cannot be found.
"
  (interactive)
  (save-match-data
    (let ((save-point (point)))
      (cond ((not (reverse-find-collapse-end (point-min)))
	     (goto-char save-point)
	     (error "Previous block does not exist."))
	    ((not (reverse-find-collapse-begin (point-min)))
	     (goto-char save-point)
	     (error "Previous block malformed."))
	    (t (goto-char (match-beginning 0))
	       )
	  )
      )
    )
  )

;;
;; Miscellaneous functions used elseware in this feature.
;;

(defmacro save-modification (&rest to-execute)
  "
\(save-modification BODY...\)

Saves the modification of the buffer and restores to modification after the BODY
expressions finish executing.  Useful for functions that technically change the
buffer, but do no actual damage.  An example would be changing the color of some text.
This does not actually change the file, but would ordinarily change the buffer's
modification
"
  (let ((save-mod-sym (make-symbol "save-mod")))
    `(let ((,save-mod-sym (buffer-modified-p)))
       (unwind-protect (progn ,@to-execute)
	 (set-buffer-modified-p ,save-mod-sym))
       )
    )
  )

(provide 'collapse)
