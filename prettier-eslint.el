;;; prettier-js.el --- Minor mode to format JS code on file save
;; Version: 0.1.0
;;; Commentary:
;; Formats your JavaScript code using 'prettier' on file save.
;;; Code:

(defun prettier-eslint/binary ()
  "Return the full path to prettier-eslint binary."
  (or
    (executable-find "prettier-eslint")
    ;; give up
    (error "Couldn't find a prettier-eslint executable")))

(defun prettier-eslint ()
  "Format the current file with ESLint."
  (interactive)
  (progn
    (message "starting prettier-eslint")
    (let ((eslint-path-config (concat (prettier-eslint/binary) "/../../.eslintrc")))
        (message "eslint-path-config: %s" eslint-path-config)
        (call-process (prettier-eslint/binary)
          nil "*Prettier-ESLint Errors*" nil
          buffer-file-name
          "--eslint-path-config" eslint-path-config)
     )
     (revert-buffer t t t)))


(provide 'prettier-eslint)
;;; prettier-eslint.el ends here
