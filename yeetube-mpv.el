;;; yeetube-mpv.el --- Provide yeetube mpv functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions youtube videos invidious
;; URL: https://git.thanosapollo.com/yeetube

;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a yeetube extension, to start an mpv process &
;; remotely control it.

;;; Code:

(defcustom yeetube-mpv-disable-video nil
  "Add no-video flag when using mpv."
  :type 'boolean
  :group 'yeetube)

(defvar yeetube-mpv-path (executable-find "mpv")
  "Path for mpv executable.")

(defun yeetube-mpv-process (command)
  "Start yeetube process for shell COMMAND."
  (unless yeetube-mpv-path
    (error "Mpv not found.  Install mpv or change the value of yeetube-player"))
  (let ((yeetube-mpv-process "yeetube"))
    (dolist (process (process-list))
      (when (string-match yeetube-mpv-process (process-name process))
	(kill-process process)))
    (sit-for 0.1)
    (unless (get-process yeetube-mpv-process)
      (start-process-shell-command
       "yeetube" nil command))))

(defun yeetube-mpv (url)
  "Start yeetube process to play URL using mpv."
  (yeetube-mpv-process
   (if yeetube-mpv-disable-video
       (format "%s --no-video '%s'" yeetube-mpv-path url)
     (format "%s '%s'" yeetube-mpv-path url)))
  (message "yeetube: starting mpv process"))

(defun yeetube-mpv-toggle-no-video-flag ()
  "Toggle no video flag for mpv player."
  (interactive)
  (if yeetube-mpv-disable-video
      (progn (setf yeetube-mpv-disable-video nil)
	     (message "yeetube: mpv removed no-video flag"))
    (setf yeetube-mpv-disable-video t)
    (message "yeetube: mpv added no-video flag")))

(defun yeetube-mpv-send-keypress (key)
  "Send KEY to yeetube-mpv-process."
  (interactive "sKey: ")
  (process-send-string "yeetube" key))

(defun yeetube-mpv-toggle-pause ()
  "Toggle pause mpv."
  (interactive)
  (yeetube-mpv-send-keypress "p")
  (message "yeetube: toggle pause"))

(defun yeetube-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (yeetube-mpv-send-keypress "f")
  (message "toggle fullscreen"))

(defun yeetube-mpv-toggle-video ()
  "Toggle video mpv."
  (interactive)
  (yeetube-mpv-send-keypress "_")
  (message "yeetube: toggle video"))

(provide 'yeetube-mpv)
;;; yeetube-mpv.el ends here
