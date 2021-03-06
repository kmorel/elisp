(defun c-set-indent-standard ()
  (interactive)

  (setq c-basic-offset 4)
  (setq c-comment-only-line-offset 0)
  (setq c-indent-comments-syntactically-p nil)
  (setq c-electric-pound-behavior '(alignleft))
  (setq indent-tabs-mode nil)

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '*)
  (c-set-offset 'label '*)
  (c-set-offset 'topmost-intro-cont 4)

  (c-set-offset 'string 'c-lineup-dont-change)
  (c-set-offset 'c 'c-lineup-C-comments)
  (c-set-offset 'defun-open 0)
  (c-set-offset 'defun-close 0)
  (c-set-offset 'defun-block-intro '+)
  (c-set-offset 'class-open 0)
  (c-set-offset 'class-close 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'inline-close 0)
  (c-set-offset 'func-decl-cont '+)
  (c-set-offset 'knr-argdecl-intro 5)
  (c-set-offset 'knr-argdecl 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'member-init-intro '+)
  (c-set-offset 'member-init-cont 0)
  (c-set-offset 'inher-intro '+)
  (c-set-offset 'inher-cont 'c-lineup-multi-inher)
  (c-set-offset 'block-open 0)
  (c-set-offset 'block-close 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-close 0)
  (c-set-offset 'brace-list-intro '+)
  (c-set-offset 'brace-list-entry 0)
  (c-set-offset 'brace-entry-open 0)
  (c-set-offset 'statement 0)
  (c-set-offset 'statement-cont '+)
  (c-set-offset 'statement-block-intro '+)
  (c-set-offset 'statement-case-intro '+)
  (c-set-offset 'statement-case-open '+)
  (c-set-offset 'substatement '+)
  (c-set-offset 'access-label '/)
  (c-set-offset 'do-while-closure 0)
  (c-set-offset 'else-clause 0)
  (c-set-offset 'catch-clause 0)
  (c-set-offset 'comment-intro 'c-lineup-comment)
  (c-set-offset 'arglist-intro 'c-lineup-arglist-intro-after-paren)
  (c-set-offset 'arglist-cont 'c-lineup-arglist-intro-after-paren)
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist-intro-after-paren)
  (c-set-offset 'arglist-close 'c-lineup-arglist)
  (c-set-offset 'stream-op 'c-lineup-streamop)
  (c-set-offset 'inclass '+)
  (c-set-offset 'cpp-macro -1000)
  (c-set-offset 'cpp-macro-cont 'c-lineup-dont-change)
  (c-set-offset 'friend 0)
  (c-set-offset 'objc-method-intro -1000)
  (c-set-offset 'objc-method-args-cont 'c-lineup-ObjC-method-args)
  (c-set-offset 'objc-method-call-cont 'c-lineup-ObjC-method-call)
  (c-set-offset 'extern-lang-open 0)
  (c-set-offset 'extern-lang-close 0)
  (c-set-offset 'inextern-lang '+)
  (c-set-offset 'namespace-open 0)
  (c-set-offset 'namespace-close 0)
  (c-set-offset 'innamespace '+)
  (c-set-offset 'template-args-cont '+)
  (c-set-offset 'inlambda 'c-lineup-inexpr-block)
  (c-set-offset 'lambda-intro-cont '+)
  (c-set-offset 'inexpr-statement 0)
  (c-set-offset 'inexpr-class '+)

  (message "Standard indentation."))

(add-hook 'c-mode-common-hook (lambda () "Set standard indent."
				(local-set-key "\C-c\C-i\C-s"
					       'c-set-indent-standard)))
