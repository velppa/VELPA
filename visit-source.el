;;; visit-source.el --- Open file at provided position.

;; Copyright (C) 2020 u/yxhuvud

;; Licensed under the same terms as Emacs.

;; Author: u/yxhuvud
;; Maintainer: Pavel Popov (https://github.com/velppa)
;; Created: July 2 2020
;; Version: 0.1
;; Keywords: find-file, os-integration
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://www.reddit.com/r/emacs/comments/7wjyhy/emacs_tip_findfileatpoint/du35fh4/

;;; Code:

(defun visit-source (&optional name)
  "Open file under current line.  If NAME provided, use `find-file'.

If the current line contains text like './src/program.rb:34:',
visit that file in the other window and position point on that
line. A file must either have a / or . in the filename to be
recognized.
     # ./app/views/interfaces/edit.html.erb:3:fdsin
     # ./app/views/interfaces/edit.html.erb:3:in
     # ./app/views/interfaces/edit.html.erb:3
     # ./app:3
     # bar/foo"
  (interactive)
  (if name
      (find-file name)
    (let* ((chars "^  \"\t\n`'=|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
           (path (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (let (p0 p1 p2)
                     (setq p0 (point))
                     ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                     (skip-chars-backward chars)
                     (setq p1 (point))
                     (goto-char p0)
                     (skip-chars-forward chars)
                     (setq p2 (point))
                     (goto-char p0)
                     (buffer-substring-no-properties p1 p2))))
           (match (and path (string-match "^\\([^: ]*[/.][^: ]*\\)\\(:[0-9]+\\)?\\(:[0-9]+\\)?:?" path)))
           (fpath (if match (match-string 1 path) path))
           (line-no (if (and match (match-string 2 path)) (string-to-number (substring (match-string 2 path) 1)) 0))
           (col-no (if (match-string 3 path) (string-to-number (substring (match-string 3 path) 1)) 0)))
      (and
       match
       (file-exists-p fpath)
       (find-file-other-window fpath)
       (when line-no
         (goto-char (point-min)) ;; goto-line is only for interactive use
         (forward-line (1- line-no))
         (when (> col-no 0) (forward-char (1- col-no)))
         t)
       t))))

(provide 'visit-source)
;;; visit-source.el ends here
