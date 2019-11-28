;;; arduino-cli.el --- arduino-cli command wrapper -*- lexical-binding: t -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/arduino-cli-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Created: 2019-11-16
;; Keywords: extensions processes arduino

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains a wrapper for the official Arduino command line utility.
;; It aims to be as simple as possible, while still providing useful conveniences.
;;
;; The structure of this package was inspired by ZachMassia's PlatformIO-Mode:
;; https://github.com/ZachMassia/PlatformIO-Mode/
;;
;; For more information on the wrapper, see the readme at https://github.com/motform/emacs-arduino-cli
;; For more information on arduino-cli itself, see https://github.com/arduino/arduino-cli

;;; Code:

(require 'compile)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

;;; Customization
(defgroup arduino-cli nil
  "Arduino-cli functions and settings."
  :group 'tools
  :prefix "arduino-cli-")

(defcustom arduino-cli-mode-keymap-prefix (kbd "C-c C-a")
  "Arduino-cli keymap prefix."
  :group 'arduino-cli
  :type 'string)

(defcustom arduino-cli-mode-verify nil
  "Verify uploaded binary after the upload."
  :group 'arduino-cli
  :type 'boolean)


;;; Internal functions
(define-compilation-mode arduino-cli-compilation-mode "arduino-cli-compilation"
  "Arduino-cli specific `compilation-mode' derivative."
  (setq-local compilation-scroll-output t)
  (require 'ansi-color))

(defun arduino-cli--compile (cmd)
  "Run arduino-cli CMD in 'arduino-cli-compilation-mode."
  (let ((cmd (concat "arduino-cli " cmd " " default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda () default-directory))
    (compilation-start cmd 'arduino-cli-compilation-mode)))

(defun arduino-cli--message (cmd &rest path)
  "Run arduino-cli CMD in PATH (if provided) and print as message."
  (let* ((default-directory (when path (car path)))
         (cmd (concat "arduino-cli " cmd))
         (out (shell-command-to-string cmd)))
    (message out)))

(defun arduino-cli--arduino? (usb-device)
  "Return USB-DEVICE it is an Arduino, nil otherwise."
  (assoc 'boards usb-device))

;; # NOTE This leaves 'boards in final map, causing
;; insignificant, but ugly, duplication
(defun arduino-cli--get-board ()
  (let* ((usb-devices (thread-first "arduino-cli board list --format json"
                        shell-command-to-string
                        json-read-from-string))
         (board (car (seq-filter #'arduino-cli--arduino? usb-devices)))
         (board-info (thread-first (assoc 'boards board) cdr (seq-elt 0))))
    (map-merge 'list board board-info)))

;;; User commands
(defun arduino-cli-compile ()
  "Compile Arduino project."
  (interactive)
  (let* ((board (arduino-cli--get-board))
         (fqbn (cdr (assoc 'FQBN board)))
         (cmd (concat "compile --fqbn " fqbn)))
    (arduino-cli--compile cmd)))

(defun arduino-cli-compile-and-upload ()
  "Compile and upload Arduino project."
  (interactive)
  (let* ((board (arduino-cli--get-board))
         (fqbn (cdr (assoc 'FQBN board)))
         (port (cdr (assoc 'address board)))
         (cmd (concat "compile --fqbn " fqbn " --port " port " --upload")))
    (arduino-cli--compile cmd)))

(defun arduino-cli-upload ()
  "Upload Arduino project."
  (interactive)
  (let* ((board (arduino-cli--get-board))
         (fqbn (cdr (assoc 'FQBN board)))
         (port (cdr (assoc 'address board)))
         (cmd (concat "upload --fqbn " fqbn " --port " port)))
    (arduino-cli--compile cmd)))

(defun arduino-cli-board-list ()
  "Show list of connected boards."
  (interactive)
  (arduino-cli--message "board list"))

(defun arduino-cli-new-sketch ()
  "Create a new Arduino sketch."
  (interactive)
  (let* ((name (read-string "Sketch name: "))
         (path (read-directory-name "Sketch path: "))
         (cmd (concat "sketch new " name)))
    (arduino-cli--message cmd path)))

;;; Minor mode
(defvar arduino-cli-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'arduino-cli-compile)
    (define-key map (kbd "b") #'arduino-cli-compile-and-upload) ; # TODO find a better key
    (define-key map (kbd "u") #'arduino-cli-upload)
    (define-key map (kbd "l") #'arduino-cli-board-list)
    (define-key map (kbd "n") #'arduino-cli-new-sketch)
    map)
  "Keymap for arduino-cli mode commands after `arduino-cli-mode-keymap-prefix'.")
(fset 'arduino-cli-command-map arduino-cli-command-map)

(defvar arduino-cli-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map arduino-cli-mode-keymap-prefix 'arduino-cli-command-map)
    map)
  "Keymap for arduino-cli mode.")

(easy-menu-change
 '("Tools") "arduino-cli"
 '(["Compile Project" arduino-cli-build]
   ["Upload Project" arduino-cli-compile-and-upload]
   ["Compile and Upload Project" arduino-cli-upload]
   "--"
   ["Board list" arduino-cli-board-list]
   ["New sketch" arduino-cli-new-sketch]))

;;;###autoload
(define-minor-mode arduino-cli-mode
  "arduino-cli integration for Emacs."
  :lighter " arduino-cli"
  :keymap arduino-cli-mode-map
  :group 'arduino-cli
  :require 'arduino-cli)

(provide 'arduino-cli-mode)
;;; arduino-cli.el ends here
