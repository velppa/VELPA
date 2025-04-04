;;; echo-keys.el --- Display pressed keys -*- lexical-binding: t; -*-
;;; URL: https://www.emacswiki.org/emacs/EchoKeyPresses

;;; CODE
(defvar echo-keys-last nil "Last command processed by `echo-keys'.")

(defvar echo-keys-buffer-name "*echo-key*")

(defun echo-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (with-current-buffer (get-buffer-create echo-keys-buffer-name)
        (goto-char (point-max))
        ;; self  self
        ;; self  other \n
        ;; other self  \n
        ;; other other \n
        (unless (and (eq 'self-insert-command echo-keys-last)
                     (eq 'self-insert-command this-command))
          (insert "\n"))
        (if (eql this-command 'self-insert-command)
            (let ((desc (key-description (this-command-keys))))
              (if (= 1 (length desc))
                  (insert desc)
                (insert " " desc " ")))
          (insert (key-description (this-command-keys))))
        (setf echo-keys-last this-command)
        (dolist (window (window-list))
          (when (eq (window-buffer window) (current-buffer))
            ;; We need to use both to get the effect.
            (set-window-point window (point))
            (end-of-buffer)))))))

(defun echo-keys-toggle ()
  (interactive)
  (if (member 'echo-keys  pre-command-hook)
      (progn
        (remove-hook 'pre-command-hook 'echo-keys)
        (dolist (window (window-list))
          (when (eq (window-buffer window) (get-buffer echo-keys-buffer-name))
            (delete-window window))))
    (progn
      (add-hook    'pre-command-hook 'echo-keys)
      (delete-other-windows)
      (split-window nil (- (window-width) 32) t)
      (other-window 1)
      (switch-to-buffer (get-buffer-create echo-keys-buffer-name))
      (set-window-dedicated-p (selected-window) t)
      (other-window 1))))

(provide 'echo-keys)
