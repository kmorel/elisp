;;; revbufs.el -- reverts all out-of-date buffers safely

;; Copyright (C) 1997-1999 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <nwv@acm.org>
;; Created:  22-Mar-1997
;; Version:  1.0
;; Keywords: revert
;; X-URL:    http://nwv.www.media.mit.edu/people/nwv/projects/revbufs/
;; X-RCS:    $Id: revbufs.el,v 1.1.1.1 2001-12-06 01:40:07 kmorel Exp $ GMT

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; `revbufs' is a little package I wrote for situations in which both humans
;; and compilers directly modify the same files.  After running `make' to have
;; the tool modify the files, I do `M-x revbufs RET' to update all my buffers.
;;
;; `revbufs' won't revert a buffer that has been modified (what it calls
;; "conflicts"), and will tell you if any files disappeared out from under your
;; buffers ("orphans").

;;; Change Log:

;; [Version 1.0, 04-Sep-1999, nwv@acm.org] Initial release.

;;; Code:

(defun revbufs ()
  (interactive)
  (let ((conflicts  '())
        (orphans    '())
        (reverts    '())
        (report-buf (get-buffer-create "*revbufs*")))

    ;; Process the buffers.
    (mapcar (function
             (lambda (buf)
	       (let ((file-name (buffer-file-name buf)))
                 (cond
                  ;; If buf is the report buf, ignore it.
                  ((eq buf report-buf) nil)
                  ;; If buf is not a file buf, ignore it.
                  ((not file-name) nil)
                  ;; If buf file doesn't exist, buf is an orphan.
                  ((not (file-exists-p file-name))
                   (setq orphans (nconc orphans (list buf))))
                  ;; If file modified since buf visit, buf is either a conflict
                  ;; (if it's modified) or we should revert it.
                  ((not (verify-visited-file-modtime buf))
                   (if (buffer-modified-p buf)
                       (setq conflicts (nconc conflicts (list buf)))
                     (save-excursion
                       (set-buffer buf)
                       (revert-buffer t t))
                     (setq reverts (nconc reverts (list buf)))))))))
            (copy-sequence (buffer-list)))

    ;; Prepare the report buffer.
    (save-excursion
      (set-buffer report-buf)
      (setq buffer-read-only nil
            truncate-lines   t)
      (delete-region (point-min) (point-max))
      (insert (revbufs-format-list conflicts "CONFLICTS")
              (revbufs-format-list orphans   "ORPHANS")
              (revbufs-format-list reverts   "REVERTS"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (bury-buffer report-buf)

    ;; Print message in echo area.
    (if (or conflicts orphans)
        (progn
          (display-buffer report-buf)
          (message
	   (concat
	    (format "Reverted %s with"
		    (revbufs-quantity (length reverts) "buffer"))
	    (if conflicts 
		(format " %s%s"
			(revbufs-quantity (length conflicts) "conflict")
			(if orphans " and" "")))
	    (if orphans
		(format " %s"
			(revbufs-quantity (length orphans) "orphan"))))))
      (if reverts
          (message "Reverted %s." (revbufs-quantity (length reverts) "buffer"))
        (message "No buffers need reverting.")))))

(defun revbufs-format-list (list label)
  (if list
      (concat label
              (format " (%s):\n" (length list))
              (mapconcat 
               (function
                (lambda (buf)
                  (format "  %-20s %s\n"
                          (buffer-name buf)
                          (buffer-file-name buf))))
               list
               ""))
    ""))

(defun revbufs-quantity (num what)
  (format "%d %s%s" num what (if (= num 1) "" "s")))

;; revbufs.el ends here
