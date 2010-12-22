;;; chunk-fill.el
;; $Id: chunk-fill.el,v 1.1.1.1 2001-12-06 01:40:09 kmorel Exp $
;;
;; This simple minor mode expands the utility of fill-paragraph and other
;; fill-* functions.  Normally, these functions will split on every
;; whitespace character.  This is usually what you want, but sometimes you
;; have groups of words that you wish to keep together for readability.
;; This mode chunks such related words togther and forces the fill-*
;; functions to keep all of these words on the same line.
;;
;; I originally wrote this file to handle html editing.  I use fill mode a
;; lot because I generally don't care how each paragraph is broken into
;; lines, but do want them to be readable in text form.  Filling does this
;; well but often breaks tags.  I find the tags much more readable when
;; they are exclusively on one line.  In particular, tags such as <a
;; href="http://www.host.com/~user"> were being split such that the first
;; two characters were separated from the rest.  As you can see, that tag
;; isn't as readable as <a href="http://www.host.com/~user">.
;;
;; To use chunk-fill-mode, set chunk-fill-delimiters and then call
;; chunk-fill-mode.  An example of something you might put in your
;; html-mode-hook to fix the above proble is:
;;
;;     (setq chunk-fill-delimiters '( ("<[^!]" . ">") ))
;;     (chunk-fill-mode 1)
;;
;; Author: Kenneth Moreland
;; email: kmorel@sandia.gov
;;

(defvar chunk-fill-mode nil
  "Mode variable for chunk-fill minor mode.")
(make-variable-buffer-local 'chunk-fill-mode)

(defun chunk-fill-mode (&optional arg)
  "
Chunk-fill minor mode.

Chunk-fill mode is a fairly passive mode.  It only affects the behavior of
when fill-paragraph.  When this function is called, the some words in the
target paragraph are grouped into \"chunks.\"  All the text within these
chunks is guaranteed to be located on the same line.
"
  (interactive "P")
  (setq chunk-fill-mode
	(if (null arg)
	    (not chunk-fill-mode)
	  (> (prefix-numeric-value arg) 0))))

(defvar chunk-fill-delimiters nil
  "
A list of chunk delimiters.  This list is of the form:

    (DELIM DELIM DELIM ...)

Each DELIM is a cons cell containing two strings.  The car is a regexp that
will match the beginning of a chunk.  The cdr is a regexp that will match
the end of a chunk.  Here is an example of a valid value that will chunk
together any makefile macros:

    ( (\"\\\\$(\" . \")\") (\"\\\\${\" . \"}\") )

This will cause makefiles macros like $(patsubst %.cc,%.o,${SRCS}) and
${MACRO NAME} to not split across lines if fill-paragraph were called (not
that you would want to call fill-paragraph in a makefile).
")
(make-variable-buffer-local 'chunk-fill-delimiters)

(if (not (assq 'chunk-fill-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(chunk-fill-mode " Chunk")
		minor-mode-alist)))

(defadvice fill-region-as-paragraph (around chunk-fill activate compile)
  "
When chunk-fill-mode is on, groups up chunks that should be on single
lines and replace them with dummy values.  Then runs fill-region-as-paragraph
and finally restores the original text.
"
  (if (not chunk-fill-mode)
      ad-do-it
    (let* ((from (ad-get-arg 0))
	   (to (ad-get-arg 1))
	   (restore (hide-chunks from to chunk-fill-delimiters)))
      (unwind-protect
	  ad-do-it
	(restore-chunks restore)))))

(defun hide-chunks (from to delimiters)
  (let (replacements)
    (save-restriction
      (save-excursion
	;; Only deal with region we are looking at.
	(narrow-to-region (min from to) (max from to))
	;; Look at each delimiter cons cell.
	(while delimiters
	  (goto-char (point-min))
	  (let ((delimit (car delimiters))
		d-begin
		d-end)
	    (while (re-search-forward (car delimit) (point-max) 0)
	      (setq d-begin (match-beginning 0))
	      (if (null (re-search-forward (cdr delimit) (point-max) 0))
		  nil
		;; OK.  We have found a chunk.
		(setq d-end (match-end 0))
		;; Remove any newlines (possible created with auto-fill)
		(goto-char d-begin)
		(while (search-forward "\n" d-end 0)
		  (replace-match " "))
		(let ((original-text (buffer-substring d-begin d-end)))
		  ;; Replace the region with bogus text.
		  (delete-region d-begin d-end)
		  (insert-char ?- (- d-end d-begin))
		  ;; Save data for later restoration (in restore-chunks).
		  (setq replacements (cons (list (copy-marker d-begin t)
						 (copy-marker d-end nil)
						 original-text)
					   replacements))))))
	  (setq delimiters (cdr delimiters)))
	replacements))))

(defun restore-chunks (replacements)
  (save-excursion
    (while replacements
      (let* ((rep (car replacements))
	     (r-begin (marker-position (nth 0 rep)))
	     (r-end (marker-position (nth 1 rep)))
	     (r-text (nth 2 rep)))
	(delete-region r-begin r-end)
	(goto-char r-begin)
	(insert r-text))
      (setq replacements (cdr replacements)))))

(provide 'chunk-fill)
