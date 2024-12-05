;;; aerospace.el --- Utilities to work with AeroSpace  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/velpa/tree/main/aerospace.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities to work with AeroSpace.
;;; Code:

(require 'tomelr)

(defvar aerospace-config-file-path
  "~/.config/aerospace/aerospace.toml"
  "Path to AeroSpace config file.")

(defvar aerospace-gap 10 "Gap between windows.")

(defvar aerospace-root-config
  `(;;
    (start-at-login . t)
    (enable-normalization-flatten-containers . t)
    (enable-normalization-opposite-orientation-for-nested-containers . t)
    (accordion-padding . 30)
    (default-root-container-layout . "tiles")
    (default-root-container-orientation . "auto")
    (key-mapping.preset . "qwerty")
    (on-focused-monitor-changed . ("move-mouse monitor-lazy-center")))
  "Root AeroSpace configuration")

(defvar aerospace-modes-config
  `((mode.main.binding
     . ,(append '((cmd-shift-7 . "layout h_tiles")
                  (cmd-shift-6 . "layout h_accordion")
                  (cmd-shift-9 . "focus left")
                  (cmd-shift-0 . "focus right")
                  ;;
                  (alt-tab . "workspace-back-and-forth")
                  (alt-shift-tab . "move-workspace-to-monitor --wrap-around next")
                  ;;
                  (cmd-left . "focus left")
                  (cmd-down . "focus down")
                  (cmd-up . "focus up")
                  (cmd-right . "focus right")
                  ;;
                  (f9 . "mode keys")
                  (f13 . "mode keys")
                  (f14 . "move-workspace-to-monitor --wrap-around next")
                  ;; (tick . "mode keys")
                  )
                (mapcar (lambda (x)
                          (cons (make-symbol (format "alt-%s" x))
                                (format "workspace %s" x)))
                        (number-sequence 0 9))))
    (mode.keys.binding
     . ,(append '((esc . "mode main")
                  (f9 . "mode main")
                  (f13 . "mode main")
                  (g . "mode main")
                  (tab . "mode main")
                  ;;
                  (period . "layout tiles horizontal vertical")
                  (comma . "layout accordion horizontal vertical")
                  ;;
                  (m . "mode move")
                  ;;
                  (h . "focus left")
                  (j . "focus down")
                  (k . "focus up")
                  (l . "focus right")
                  ;;
                  (minus . "resize smart -50")
                  (equal . "resize smart +50")
                  ;;
                  (f . "flatten-workspace-tree")
                  (r . "reload-config")
                  (t . "layout floating tiling")
                  (backspace . "close-all-windows-but-current")
                  ;;
                  (left . ("join-with left" "mode main"))
                  (down . ("join-with down" "mode main"))
                  (up . ("join-with up" "mode main"))
                  (right . ("join-with right" "mode main")))
                (mapcar (lambda (x)
                          (cons (make-symbol (format "%s" x))
                                (format "workspace %s" x)))
                        (number-sequence 0 9))))
    (mode.move.binding
     . ,(append '((h . ("move left" "mode main"))
                  (j . ("move down" "mode main"))
                  (k . ("move up" "mode main"))
                  (l . ("move right" "mode main"))
                  (m . ("move-workspace-to-monitor --wrap-around next" "mode main")))
                (mapcar (lambda (x)
                          (cons (make-symbol (format "%s" x))
                                (list (format "move-node-to-workspace %s" x) "mode main")))
                        (number-sequence 0 9)))))
  "AeroSpace mode bindings configuration")


(defun aerospace-gap-block (size)
  "Return uniform gap configuration block of SIZE."
  `(gaps
    . ((inner.horizontal . ,size)
       (inner.vertical . ,size)
       (outer.left . ,size)
       (outer.bottom . ,size)
       (outer.top . ,size)
       (outer.right . ,size))))

(comment
 (aerospace-gap 10)
 ;; (gaps (inner.horizontal . 10) (inner.vertical . 10) (outer.left . 10) (outer.bottom . 10) (outer.top . 10) (outer.right . 10))
 )
;;

;;;; Commands

(defun aerospace-encode-config ()
  (tomelr-encode
   (append
    aerospace-root-config
    aerospace-modes-config
    (list (aerospace-gap-block aerospace-gap)))))

;;;###autoload
(defun aerospace-apply ()
  "Write current AeroSpace configuration into a file and reload-config."
  (interactive)
  (with-temp-file aerospace-config-file-path
    (insert (aerospace-encode-config)))
  (start-process "aerospace" nil "aerospace" "reload-config")
  (message "AeroSpace config reloaded."))

;;;###autoload
(defun aerospace-set-gap (n)
  "Set gap between windows in AeroSpace to N pixels."
  (interactive "nGap in pixels: ")
  (setq aerospace-gap n)
  (aerospace-apply))

;;;; Footer

(provide 'aerospace)

;;; aerospace.el ends here
