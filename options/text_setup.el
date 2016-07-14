;Options that set up various simple text editing modes.

;(require 'refill)

;(add-hook 'text-mode-hook 'refill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(require 'timestamp)
(add-hook 'html-mode-hook 'bind-html-timestamp)
(add-hook 'tex-mode-hook 'bind-tex-timestamp)

(require 'chunk-fill)
(add-hook 'html-mode-hook
	  (lambda ()
	    (setq chunk-fill-delimiters '( ("<[^!]" . ">") ))
	    (chunk-fill-mode 1)))

(require 'one-sentence-per-line)
(add-hook 'tex-mode-hook
	  (lambda()
	    (auto-fill-mode -1)
	    (one-sentence-per-line-mode 1)))

(add-hook 'bibtex-mode-hook
	  (lambda ()
	    (local-set-key [tab] 'indent-for-tab-command)))

(setq TeX-newline-function 'newline-and-indent)

(autoload 'xml-lite-mode "xml-lite" "Minor mode for editing XML files." t)

(add-hook 'xml-lite-mode-hook
	  (lambda()
	    (setq xml-lite-indent-offset 2)
	    (setq indent-tabs-mode nil)
	    (auto-fill-mode nil)
	    )
	  )

(autoload 'longlines-mode "longlines"
  "Minor mode for automatically wrapping long lines." t)

(setq auto-mode-alist
      (append
       '(("^README" . text-mode)
	 ("\\.md$" . text-mode)
	 ("\\.xml$" . xml-lite-mode)
	 ("\\.qrc$" . xml-lite-mode)
	 )
       auto-mode-alist))
