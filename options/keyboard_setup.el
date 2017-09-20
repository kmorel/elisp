;This file sets up the "general-purpose" key commands that I like to use.

(global-set-key "\C-c=" 'what-line)
(global-set-key "\C-cc" 'compile)
(global-set-key "\M-c" 'indent-for-comment)
(global-set-key "\M-r" 'isearch-backward-regexp)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-ce" 'eval-current-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-visual-line)
(global-set-key [end] 'end-of-visual-line)
(global-set-key "\C-x\C-b" `electric-buffer-list)

;Allows you to quickly specify the exact width and height of the current frame.
(require 'framefun)
(global-set-key "\C-x5w" 'set-frame-width-interactive)
(global-set-key "\C-x5h" 'set-frame-height-interactive)

;Allows you to quickly toggle between wrapping words with or without actual
;newlines.
(require 'textfun)
(global-set-key "\C-w" 'toggle-word-wrap-mode)
  
;Provides apropos for both functions and variables
(global-set-key "\C-h\C-a" 'apropos)

;Provides an electric kill ring
(require 'ekill-menu)
(global-set-key "\M-y" 'electric-kill-ring)

;Makes the meta I,K,J,L key sequences move up, down, left, right,
;respectively.  Also binds meta H and ; to begining of line, end
;of line and meta o and m to page-up and page-down.
;Moves (downcase-word) from M-l to C-=.  For consistancy, C-+ is
;also bound to (upcase-word) but is still available by M-u.
;(global-set-key [?\^=] 'downcase-word)
;(global-set-key [?\^+] 'upcase-word)
;(global-set-key "\M-i" 'previous-line)
;(global-set-key "\M-k" 'next-line)
;(global-set-key "\M-j" 'backward-char)
;(global-set-key "\M-l" 'forward-char)
;(global-set-key "\M-h" 'beginning-of-line)
;(global-set-key "\M-o" 'scroll-down)
;(global-set-key "\M-m" 'scroll-up)
;(global-set-key "\M-;" 'end-of-line)

