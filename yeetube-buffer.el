;;; yeetube-buffer.el --- Yeetube Buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.com/yeetube
;; Version: 0.0.1

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

;; This package provides yeetube-buffer functionality

;; TODO: Fix titles with emojis (ruin formatting)

;;; Code:

(require 'yeetube-face)

(defun yeetube-buffer-fix-title (title)
  "Adjust TITLE."
  (let ((replacements '(("&amp;" . "&")
                        ("&quot;" . "\"")
                        ("&#39;" . "'")
			("u0026" . "&")
			("\\\\" . ""))))
    (mapc (lambda (replacement)
            (setf title (replace-regexp-in-string (car replacement) (cdr replacement) title)))
          replacements)
    title))

(defun yeetube-buffer-fix-view-count (view-count)
  "Fix VIEW-COUNT display issues."
  (replace-regexp-in-string "[^0-9]" "" view-count))

(defun yeetube-buffer-view-count-add-commas (string)
  "Add commas for STRING."
  (let ((result "")
        (len (length string)))
    (dotimes (i len)
      (setf result (concat (substring string (- len i 1) (- len i)) result))
      (when (and (> (- len (1+ i)) 0)
                 (= (% (1+ i) 3) 0))
        (setf result (concat "," result))))
    result))

(defun yeetube-buffer-render-header (query)
  "Render header for *yeetube* buffer for QUERY."
  (setf header-line-format
	(concat
	 (format "Yeetube Search: %s" (propertize query 'face 'yeetube-face-header-query)))))

;; Inspired from ytel
(defun yeetube-buffer--format-title (title)
  "Format a video TITLE to be inserted in the *yeetube* buffer."
  (let* ((n (string-width title))
	 (extra-chars (- n 60))
	 (formatted-string (if (<= extra-chars 0)
			       (concat title
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq title 0 60)
				     " "))))
    (propertize formatted-string 'face 'yeetube-face-title)))

(defun yeetube-buffer--format-view-count (view-count)
  "Format a video VIEW-COUNT to be inserted in the *yeetube* buffer."
  (let* ((n (string-width view-count))
	 (extra-chars (- n 20))
	 (formatted-string (if (<= extra-chars 0)
			       (concat view-count
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq view-count 0 20)
				     "..."))))
    (propertize formatted-string 'face 'yeetube-face-view-count)))

(defun yeetube-buffer--format-video-duration (video-duration)
  "Format a video VIDEO-DURATION to be inserted in the *yeetube* buffer."
  (let* ((n (string-width video-duration))
	 (extra-chars (- n 20))
	 (formatted-string (if (<= extra-chars 0)
			       (concat video-duration
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq video-duration 0 20)
				     "..."))))
    (propertize formatted-string 'face 'yeetube-face-duration)))

(defun yeetube-buffer-create (query content buffer-mode)
  "Create *yeetube* buffer with BUFFER-MODE for QUERY, displaying CONTENT."
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*yeetube*"))
    (funcall buffer-mode)
    (erase-buffer)
    (pop content) ;; Remove filtes
    (yeetube-buffer-render-header query)
    (dolist (info (reverse content))
		  (let ((title (yeetube-buffer-fix-title (car info)))
			(view-count (caddr info))
			(video-duration (cadddr info)))
		    (insert (yeetube-buffer--format-title
			     (format "%s "
				    (propertize title 'face 'message-header-subject))))
		    ;; Add commas, using this in yeetube-buffer-fix-view-count
		    ;; causes display issues with unicode characters
		    (end-of-line)
		    (insert  (format "| %s"
			     (yeetube-buffer--format-view-count
				    (if (< (length view-count) 20)
					 (yeetube-buffer-view-count-add-commas
					  (yeetube-buffer-fix-view-count view-count))
				      "nil")))
			     (format "%s\n"
				     (yeetube-buffer--format-video-duration
				      (if (string-match-p "^[0-9:]+$" video-duration)
					  video-duration
					"nil"))))))))

(provide 'yeetube-buffer)
;;; yeetube-buffer.el ends here
