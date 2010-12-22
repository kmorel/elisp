;
; This elisp code sets up printing.
;

(cond ((string-equal window-system "w32")
       (setq lpr-command "print")
       (setq ps-lpr-command "print")
       (setq lpr-destination '("/D:\\\\csu807\\nlleishnt\\Nancy'sH"))
       (setq ps-lpr-destination '("/D:\\\\csu807\\nlleishnt\\Nancy'sH"))
       (setq ps-print-use-gs t)
       (setq gs-print-command "C:\\PROGRA~1\\gstools\\gs4.03\\gswin32.exe")
       (setq gs-print-switches '("-q -dNOPAUSE"))
       (setq gs-view-command "C:\\PROGRA~1\\gstools\\gs4.03\\gswin32.exe")
       (require 'print-nt)
       (global-set-key "\M-p" 'nt-ps-print-buffer)
       )
      )

