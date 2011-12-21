;This is the setup I like to use whenever programming.  Because of its
;enormous size I took this out of my .emacs file.  It sets up the
;indentation features I like as well has my preferred hilighting.

;; (if (eq window-system 'w32)
;;     (setq compile-command "nmake ")
;;   (setq compile-command "make ")
;;   )
(setq compile-command "makeme ")

;; I have not done Java programming in a while.  Back in the day I used
;; jde, but that project seems to be outdated.  For now, comment out the
;; jde stuff and then resetup Java mode if I need to start using it again.
;; (autoload 'jde-mode "jde")

;; Load all the generic modes.
(setq generic-define-mswindows-modes t)
(setq generic-define-unix-modes t)
(condition-case e
    (require 'generic-x)
  (error (message "Couldn't load generic-x.")))

;; (defvar running-jde-mode nil)
;; (make-variable-buffer-local 'running-jde-mode)

(autoload 'python-mode "python")

(autoload 'cg-mode "cg-mode")

(autoload 'cuda-mode "cuda-mode")

(autoload 'lua-mode "lua-mode")

(require 'git)
(require 'git-blame)

;; (defadvice java-mode (around call-jde activate compile)
;;   "Call jde-mode instead of java-mode."
;;   (if running-jde-mode
;;       ad-do-it
;;     (setq running-jde-mode t)
;;     (jde-mode)
;;     (setq running-jde-mode nil)))

;; (defadvice jde-mode (around call-jde activate compile)
;;   "Call jde-mode instead of java-mode.
;; This works with a similar advice for java-mode."
;;   (setq running-jde-mode t)
;;   ad-do-it
;;   (setq running-jde-mode nil))

(load "programming_setup_standard")
(load "programming_setup_vtk")

(make-variable-buffer-local 'c-basic-offset)
(make-variable-buffer-local 'c-comment-only-line-offset)
(make-variable-buffer-local 'c-indent-comments-syntatically)
(make-variable-buffer-local 'c-electric-pound-behavior)
(make-variable-buffer-local 'c-offsets-alist)

(setq c-tab-always-indent nil)

(defun cc-mode-init ()
;;   (setq c-delete-function 'delete-backward-char)
  (setq c-backspace-function 'backward-delete-char)

;;   (c-set-indent-standard)
  (c-set-indent-vtk)

;  (setq imenu-sort-function 'imenu--sort-by-name)
;  (imenu-add-to-menubar "Functions")

  (require 'collapse)
  (collapse-mode t)
  (local-set-key [M-right] 'telescope-out)
  (local-set-key [M-left] 'collapse-this)
  (local-set-key [M-up] 'collapse-below)
  (local-set-key [M-down] 'expand-here)
  (local-set-key [M-prior] 'goto-previous-collapse)
  (local-set-key [M-next] 'goto-next-collapse)
  (local-set-key "\M-t" 'toggle-c-tab-always-indent)

  (require 'textfun)
  (local-set-key "\C-c\C-r" 'right-justify-current-line)
  (setq fill-column 80)

  (setq c-backspace-function 'backward-delete-char)
  )

(defun c++-mode-init ()
  (setq imenu-sort-function 'imenu--sort-by-name)
  (imenu-add-to-menubar "Functions")
  )

(defun java-mode-init ()
  (c-set-offset 'knr-argdecl -5)
  )

;; (defun jde-mode-init ()
;;   (let ((buffer-dir (file-name-directory buffer-file-name)))
;;     (if (not (member buffer-dir jde-db-source-directories))
;; 	(custom-set-variables
;; 	 `(jde-db-source-directories
;; 	   (quote ,(cons buffer-dir jde-db-source-directories)) t))))
;;   )

(defun toggle-c-tab-always-indent ()
  (interactive)
  (setq c-tab-always-indent (not c-tab-always-indent))
  (if c-tab-always-indent
      (message "Tab will always indent.")
    (message "Tab only indents when cursor is at beginning of line.")
    )
  )

(add-hook 'c-mode-common-hook 'cc-mode-init)

(add-hook 'c++-mode-hook 'c++-mode-init)

(add-hook 'java-mode-hook 'cc-mode-init)
(add-hook 'java-mode-hook 'java-mode-init)
;; (add-hook 'jde-mode-hook  'jde-mode-init)

(load "make-regexp")
(defun make-regexp-word (word-list)
  (concat "\\<\\(" (make-regexp word-list) "\\)\\>"))

(defun font-lock-recenter ()
  "Recenters, then let font lock change the font colors."
  (interactive)
  (recenter)
  (font-lock-fontify-buffer))

(defvar define-face 'define-face)
(defvar include-face 'include-face)
(defvar modifier-face 'modifier-face)
(defvar class-keyword-face 'class-keyword-face)
(defvar integer-face 'integer-face)
(defvar float-face 'float-face)
(defvar tag-face 'tag-face)

(cond
 (window-system
  (require 'font-lock)
  (setq font-lock-maximum-size nil)
  (setq font-lock-maximum-decoration t)
  (add-hook 'font-lock-mode-hook
	    (lambda () (local-set-key "\C-l" 'font-lock-recenter)))
  (global-font-lock-mode t)
  (setq font-lock-verbose 500)
;;;   ;;This is the old Emacs 19.x stuff.
;;;   (setq font-lock-face-attributes
;;; 	'((font-lock-comment-face "firebrick" nil nil t)
;;; 	  (font-lock-string-face "dark orchid")
;;; 	  (font-lock-keyword-face "ForestGreen")
;;; 	  (font-lock-function-name-face "blue" nil t)
;;; 	  (font-lock-variable-name-face "purple" nil t)
;;; 	  (font-lock-type-face "RoyalBlue" nil t)
;;; 	  (font-lock-reference-face "ForestGreen" nil t)
;;; 	  (define-face		"ForestGreen" nil t)
;;; 	  (include-face		"purple")
;;; 	  (modifier-face	"blue" nil t)
;;; 	  (class-keyword-face	"RoyalBlue")
;;; 	  (integer-face		"magenta")
;;; 	  (float-face		"magenta" nil t)
;;; 	  (tag-face		"blue" t t)))
  ;;This is the new Emacs 20.x customize stuff.
  (custom-set-faces
   '(font-lock-comment-face	((((class color) (background dark))
				  (:foreground "salmon"))))
   '(font-lock-string-face	((((class color) (background dark))
				  (:foreground "BurlyWood"))))
   '(font-lock-keyword-face	((((class color) (background dark))
				  (:foreground "green"))))
   '(font-lock-warning-face	((((class color) (background dark))
				  (:bold t :underline t :foreground "Pink"))))
   '(font-lock-constant-face	((((class color) (background dark))
				  (:bold t :foreground "Aquamarine"))))
   '(font-lock-type-face	((((class color) (background dark))
				  (:foreground "cyan"))))
   '(font-lock-variable-name-face ((((class color) (background dark))
				    (:foreground "magenta"))))
   '(font-lock-function-name-face ((((class color) (background dark))
				    (:bold t :foreground "DeepSkyBlue"))))
   '(font-lock-builtin-face	((((class color) (background dark))
				  (:bold t :foreground "LightSteelBlue"))))
   '(paren-face-match-light	((t (:background "#202020"))))
   '(region			((t (:background "DarkBlue"))))
   )
  (custom-declare-face 'define-face
		       '((((class color) (background dark))
			  (:bold t :foreground "green")))
		       "Face used for preprocessor macro definitions.")
  (custom-declare-face 'include-face
		       '((((class color) (background dark))
			  (:bold t :foreground "magenta")))
		       "Face used for include code lines.")
  (custom-declare-face 'modifier-face
		       '((((class color) (background dark))
			  (:foreground "DeepSkyBlue")))
		       "Face used for keywords that modify types (i.e private)")
  (custom-declare-face 'class-keyword-face
		       '((((class color) (background dark))
			  (:foreground "thistle")))
		       "Face used for keywords that specify classes (i.e. class, this).")
  (custom-declare-face 'integer-face
		       '((((class color) (background dark))
			  (:foreground "violet")))
		       "Face used for descrete (i.e. integer) litererals.")
  (custom-declare-face 'float-face
		       '((((class color) (background dark))
			  (:bold t :foreground "DeepPink")))
		       "Face used for real (i.e. float) literals.")
  (custom-declare-face 'tag-face
		       '((((class color) (background dark))
			  (:foreground "DeepSkyBlue")))
		       "Face used for html tags.")
  (let ((preprocessor1	'("^\\(#.*\\)$" 0 include-face t))
	(preprocessor2	'("^\\(#[ \t]*\\(undef\\|define\\)\\(\\\\\n\\|.\\)*\\)$"
			  0 define-face t))
	(c-type-mod	(cons (make-regexp-word '("static" "extern" "auto"
						  "typedef" "const" "register"
						  "signed" "unsigned"))
			      'modifier-face))
	(cg-type-mod	(cons (make-regexp-word '("static" "extern" "auto"
						  "typedef" "const" "register"
						  "signed" "unsigned"
						  "in" "out" "inout" "uniform"
						  "varying"
						  "packed" "unpacked"))
			      'modifier-face))
	(c++-type-mod	(cons (make-regexp-word
			       '("static" "extern" "auto" "typedef" "const"
				 "register" "signed" "unsigned" "public"
				 "private" "protected" "virtual" "inline"
				 "explicit" "export" "mutable"
				 "operator"))
			      'modifier-face))
	(java-packages	'("^\\(package\\|import\\).*;$" . include-face))
	(java-type-mod	(cons (make-regexp-word
			       '("static" "transient" "public" "private"
				 "protected" "abstract" "final" "native"
				 "synchronized" "throws"))
			      'modifier-face))
	(c++-class	(cons (make-regexp-word '("class" "this" "new" "delete"
						  "friend" "template"
						  "using" "namespace"
						  "typename"))
			      'class-keyword-face))
	(java-class	(cons (make-regexp-word
			       '("class" "interface" "extends" "implements"
				 "super" "this" "new"))
			      'class-keyword-face))
	(c-base-var	(cons (make-regexp-word '("float" "double" "void" "char"
						  "int" "long" "short" "struct"
						  "enum" "union" "wchar_t"))
			      'font-lock-type-face))
	(cg-base-var	(cons (make-regexp-word '("float" "double" "void" "char"
						  "int" "long" "short" "struct"
						  "enum" "union" "wchar_t"
						  "bool" "bool[1-4]"
						  "float[1-4]"
						  "float[1-4]x[1-4]"
						  "half" "half[1-4]"
						  "half[1-4]x[1-4]"
						  "fixed" "fixed[1-4]"
						  "fixed[1-4]x[1-4]"
						  "sampler[a-zA-Z0-9_]*"
						  "cfloat" "cint" "clampu1"))
			      'font-lock-type-face))
	(c++-base-var	(cons (make-regexp-word '("float" "double" "void" "char"
						  "int" "long" "short" "struct"
						  "enum" "union" "wchar_t"
						  "bool"))
			      'font-lock-type-face))
	(java-base-var	(cons (make-regexp-word '("float" "double" "void"
						  "boolean" "char" "byte" "int"
						  "long" "short" "String"))
			      'font-lock-type-face))
	(integers	'("\\<[0-9]+\\>" 0 integer-face t))
	(hexnums	'("\\<0x[0-9a-fA-F]+\\>" 0 integer-face t))
	(characters	'("'\\\\?.'" 0 integer-face t))
	(c-val		(cons (make-regexp-word '("NULL")) 'integer-face))
	(cg-val		(cons (make-regexp-word '("true" "false"))
			      'integer-face))
	(c++-val	(cons (make-regexp-word '("NULL" "true" "false"))
			      'integer-face))
	(java-val	(cons (make-regexp-word '("true" "false" "null"))
			      'integer-face))
	(floats1	'("\\<[0-9]+\\." 0 float-face t))
	(floats2	'("\\.[0-9]+\\([eE][+-]?[0-9]+\\)?" 0 float-face t))
	(c-keywords	(cons (make-regexp-word
			       '("return" "if" "else" "case" "default" "switch"
				 "break" "continue" "while" "do" "for" "goto"
				 "sizeof"))
			      'font-lock-keyword-face))
	(c++-keywords	(cons (make-regexp-word
			       '("return" "if" "else" "case" "default" "switch"
				 "break" "continue" "while" "do" "for" "goto"
				 "sizeof"
				 "try" "catch" "throw" "asm" "const_cast"
				 "dynamic_cast" "reinterpret_cast"
				 "static_cast"))
			      'font-lock-keyword-face))
	(java-keywords	(cons (make-regexp-word
			       '("return" "if" "else" "case" "default" "switch"
				 "break" "continue" "while" "do" "for" "try"
				 "catch" "throw" "finally" "instanceof"))
			       'font-lock-keyword-face)))
    (setq c-keyword-faces
	  (list preprocessor1 preprocessor2 c-type-mod c-base-var integers
		hexnums characters c-val floats1 floats2 c-keywords))
    (setq cg-keyword-faces
	  (list preprocessor1 preprocessor2 cg-type-mod cg-base-var integers
		hexnums characters cg-val floats1 floats2 c-keywords))
    (setq c++-keyword-faces
	  (list preprocessor1 preprocessor2 c++-type-mod c++-class c++-base-var
		integers hexnums characters c++-val floats1 floats2
		c++-keywords))
    (setq java-keyword-faces
	  (list java-packages java-type-mod java-class java-base-var
		integers hexnums characters java-val floats1 floats2
		java-keywords)))
  (let ((tags		'("<\\([^>\"'\\\\]\\|\\\\\\(.\\|\n\\)\\|\"\\([^\"\\\\]\\|\\\\\\(.\\|\n\\)\\)*\"\\|'\\([^\"\\\\]\\|\\\\\\(.\\|\n\\)\\)*'\\)*>" . tag-face))
	(characters	'("[&%][-.A-Za-z0-9]+;?" . integer-face))
	(comments	'("<!--\\([^>]\\|[^-]-?>\\)*-->"
			  0 font-lock-comment-face t)))
    (setq html-keyword-faces (list tags characters comments)))
  (let ((jargon-entry	'("^\\* [^:]+:+" . font-lock-function-name-face))
	(jargon-xref1	'("\\*[Nn]ote\\b[^:]+:+" . font-lock-variable-name-face))
	(jargon-xref2	'("  \\(\\(Next\\|Prev\\|Up\\): [^,\n]*\\)\\($\\|,\\)" 1 font-lock-variable-name-face))
	(jargon-keyword '("- \\(Variable\\|Function\\|Macro\\|Command\\|Special Form\\|User Option\\):.*$" . font-lock-keyword-face)))
    (setq Info-keyword-faces (list jargon-entry jargon-xref1
				   jargon-xref2 jargon-keyword)))
  (let ((major-comment	'("^\\[\\[\\[.*]]]$" . font-lock-function-name-face))
	(minor-comment	'("^\\[\\[[^[].*]]$" . font-lock-type-face)))
    (require 'compile)
    (setq compilation-keyword-faces
	  (nconc (list major-comment minor-comment)
		 (compilation-mode-font-lock-keywords))))
  (let ((c-mode-defaults
	 '(c-keyword-faces
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(cg-mode-defaults
	 '(cg-keyword-faces
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(cuda-mode-defaults
	 '(cuda-keyword-faces
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(c++-mode-defaults
	 '(c++-keyword-faces
	   nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(java-mode-defaults
	 '(java-keyword-faces
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-comment-start-regexp . "/[*/]")
	   (font-lock-mark-block-function . mark-defun)))
	(html-mode-defaults
	 '(html-keyword-faces
	   t t nil backward-paragraph))
	(Info-mode-defaults
	 '(Info-keyword-faces nil nil nil backward-paragraph))
	(compilation-mode-defaults
	 '(compilation-keyword-faces t))
	)
    (setq font-lock-defaults-alist
	  (append (list (cons 'c-mode c-mode-defaults)
			(cons 'cg-mode cg-mode-defaults)
			(cons 'cuda-mode cuda-mode-defaults)
			(cons 'c++-mode c++-mode-defaults)
			(cons 'java-mode java-mode-defaults)
			;; (cons 'jde-mode java-mode-defaults)
			(cons 'html-mode html-mode-defaults)
			(cons 'Info-mode Info-mode-defaults))
		  font-lock-defaults-alist))
    (setq my-html-mode-font-lock-defaults html-mode-defaults)
    (setq my-compilation-mode-font-lock-defaults compilation-mode-defaults))
  (add-hook 'html-mode-hook
	    (lambda ()
	      (setq font-lock-defaults my-html-mode-font-lock-defaults)))
  (add-hook 'compilation-mode-hook
	    (lambda ()
	      (setq font-lock-defaults my-compilation-mode-font-lock-defaults)))
;  (setq hilit-mode-enable-list	'(Info-mode)
;	hilit-background-mode	'light
;	hilit-inhibit-hooks	nil
;	hilit-inhibit-rebinding	nil)
;  (require 'hilit19)))
  ))

;; (custom-set-variables
;; ;I currently don't use jde projects, so for god's sake, don't try to load
;; ;one every time I load a java file.
;;  '(jde-project-context-switching-enabled-p nil)
;; ;Parsing the buffer isn't that imporantant to me, and the default interval
;; ;of 10 seconds is really annoying
;;  '(jde-auto-parse-buffer-interval 60)
;; ;Sort the contents of the imenu.  I have the imenu so I can find it.  If I
;; ;knew the order, I wouldn't need imenu.
;;  '(jde-imenu-sort (quote asc)))

;I usually make script files for sh even though I don't use this for my
;terminal shell.  So by default let's always edit sh scripts.
;If you are in a buffer, you should be able to change this with `sh-shell'.
(setq sh-shell-file "/bin/sh")

(require 'cmake-mode)

(setq auto-mode-alist
      (append
       '(("\\.c$"  . c++-mode)
	 ("\\.h$"  . c++-mode)
	 ("\\.cc$" . c++-mode)
	 ("\\.C$" . c++-mode)
	 ;; ("\\.java$" . jde-mode)
	 ("\\.java$" . java-mode)
	 ("Makefile.*" . makefile-mode)
	 ("makefile.*" . makefile-mode)
	 ("\\.mak$" . makefile-mode)
	 ("\\.mk$" . makefile-mode)
	 ("\\.py$" . python-mode)
	 ("\\.cg$" . cg-mode)
	 ("\\.cu$" . cuda-mode)
	 ("\\.lua$" . lua-mode)
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)	 )
       auto-mode-alist))

(if (eq window-system 'w32)
    (setq auto-mode-alist
	  (append
	   '(("\\.bat$" . bat-generic-mode)
	     ("\\.ini$" . ini-generic-mode)
	     ("\\.inf$" . inf-generic-mode)
	     )
	   auto-mode-alist)))

;;; I no longer use this function.  c-lineup-arglist-intro-after-paren, which
;;; just places each line right after the opening paren, seems to do what I
;;; want.
;; (defun c-lineup-operator-arglist (langelem)
;;   "
;; This function is much like c-lineup-arglist (in fact most of the code was
;; taken right from it).  They both are used in c-offsets-alist to indent code
;; within parentheses.  The difference is that this function also allows the
;; allignment of expressions with operators at the beginning of lines.  For
;; example, the following indentation is supported:

;;     if (   cond1
;;         || cond2
;;         || cond3) ...
;; "
;;   (save-excursion
;;     (let* ((containing-sexp
;; 	    (save-excursion
;; 	      ;; arglist-cont-nonempty gives relpos ==
;; 	      ;; to boi of containing-sexp paren. This
;; 	      ;; is good when offset is +, but bad
;; 	      ;; when it is c-lineup-arglist, so we
;; 	      ;; have to special case a kludge here.
;; 	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
;; 		  (progn
;; 		    (beginning-of-line)
;; 		    (backward-up-list 1)
;; 		    (skip-chars-forward " \t" (c-point 'eol)))
;; 		(goto-char (cdr langelem)))
;; 	      (point)))
;; 	   (langelem-col (c-langelem-col langelem t))
;; 	   (operator-length (save-excursion
;; 			      (beginning-of-line)
;; 			      (skip-chars-forward " \t" (c-point 'eol))
;; 			      (+ (skip-chars-forward "^ \t" (c-point 'eol))
;; 				 (skip-chars-forward " \t" (c-point 'eol))))))
;;       (if (save-excursion
;; 	    (beginning-of-line)
;; 	    (looking-at "[ \t]*)"))
;; 	  (progn (goto-char (match-end 0))
;; 		 (c-forward-sexp -1)
;; 		 (forward-char 1)
;; 		 (c-forward-syntactic-ws)
;; 		 (- (current-column) langelem-col))
;; 	(goto-char containing-sexp)
;; 	(or (eolp)
;; 	    (not (memq (char-after) '(?{ ?\( )))
;; 	    (let ((eol (c-point 'eol))
;; 		  (here (progn
;; 			  (forward-char 1)
;; 			  (skip-chars-forward " \t")
;; 			  (point))))
;; 	      (c-forward-syntactic-ws)
;; 	      (if (< (point) eol)
;; 		  (goto-char here))))
;; 	(if (< operator-length (- (point) containing-sexp))
;; 	    (backward-char operator-length))
;; 	(- (current-column) langelem-col)
;; 	))))
