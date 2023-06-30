;;; org-yt.el --- Watch & Download Videos  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions

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

;; 

;;; Code:

(require 'org-element)

;; TODO: Make a defcustom for video player
(defun yt-play ()
  "Open the link at point in an `'org-mode buffer with `'mpv."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (async-shell-command (format "mpv %s" url))
      (message "Opening %s with mpv" url))))



;; TODO: Search Youtube videos play them using a video player



;; TODO: Download videos using yt-dlp



(provide 'org-yt)
;;; org-yt.el ends here
