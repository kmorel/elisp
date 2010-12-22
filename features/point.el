;This feature contains functions that help when using the pointer.

(defmacro save-point (&rest to-execute)
  "
\(save-point BODY...\)

Like save-excursion, saves the positions of the current point, evaluates
all its arguments, and restores the current point.
"
  (let ((origin-point-sym (make-symbol "origin-point")))
    `(let ((,origin-point-sym (point-marker)))
       (unwind-protect (progn ,@to-execute)
	 (goto-char ,origin-point-sym))
       )
    )
  )

(provide 'point)