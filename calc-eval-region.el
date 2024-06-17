;;; calc-eval-region.el --- Eval region using GNU Emacs Calculator -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun calc-eval-region (beg end)
  "Calculate the region BEG and END and display the result in the echo area.
If i is pressed after, insert the result at the end of region.
If r is pressed replace the text with the result"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-at-bol) (point-at-eol))))
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr))
         (my-beg beg)
         (my-end end) map)
    (message "%s = %s" expr result)
    (when (not (listp result))
      (setq map (make-sparse-keymap))
      (define-key map "i"
        (lambda () (interactive) (goto-char my-end) (insert " = " result)))
      (define-key map "r"
        (lambda ()
          (interactive)
          (kill-region my-beg my-end)
          (goto-char my-beg)
          (insert result)))
      (set-transient-map map))))

(provide 'calc-eval-region)
;;; calc-eval-region.el ends here
