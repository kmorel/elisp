; This file provides a minor mode called one-sentence-per-line-mode. This
; mode allows you to more easily edit text files with one sentence per
; line. It is helpful when editing files like LaTeX where a subsequent
; parser will later refill the text. This mode is a modification posted to
; stackexchange:
; http://emacs.stackexchange.com/questions/443/editing-files-with-one-sentence-per-line

(define-minor-mode one-sentence-per-line-mode
  "One Sentence Per Line Mode

This mode makes it easier to edit document where you have one
sentence per line. This is helpful when editing documents that
will later be refilled by a document parser (such as LaTeX or
perhaps HTML) and has revision control."
  :init-value nil
  :lighter " 1s/l"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-w") 'one-sentence-per-line-fill-paragraph)
	    map)

  (if one-sentence-per-line-mode
      (visual-line-mode 1)
    (visual-line-mode -1))
  )

(defun one-sentence-per-line-unfill-paragraph ()
  "Un-fill the paragraph at point.

This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (interactive)
  (forward-paragraph 1)
  (forward-paragraph -1)
  (while (looking-at paragraph-separate)
    (forward-line 1))
  (let ((beg (point)))
    (forward-paragraph 1)
    (backward-char 1)
    (while (> (point) beg)
      (join-line)
      (beginning-of-line))))

(defun one-sentence-per-line-fill-paragraph ()
  "Fills the current paragraph until there is one sentence per line.

This un-fills the paragraph and then places hard line breaks
after each sentence.

Note that this function does not behave correctly with comments."
  (interactive)
  (save-excursion
    ;(fill-paragraph)         ; takes care of fixing spaces if needed
    (one-sentence-per-line-unfill-paragraph)  ; remove hard line breaks

    ;; insert line breaks again
    (let ((end-of-paragraph (make-marker)))
      (save-excursion
        (forward-paragraph)
        (backward-sentence)
        (forward-sentence)
        (set-marker end-of-paragraph (point)))
      (forward-sentence) 
      (while (< (point) end-of-paragraph)
        (just-one-space)
        (delete-backward-char 1)
        (newline-and-indent)
        (forward-sentence))
      (set-marker end-of-paragraph nil))))


(provide 'one-sentence-per-line)
