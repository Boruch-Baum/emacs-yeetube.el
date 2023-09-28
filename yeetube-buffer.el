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

;;; Code:

(require 'yeetube-face)

;; TODO: Fix titles with emojis (ruin formatting)
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

;; Formatting inspired from ytel
(defun yeetube-buffer--format-header-title (query)
  "Format header for QUERY."
  (let* ((n (string-width query))
	 (extra-chars (- n 53))
	 (formatted-string
	  (if (<= extra-chars 0)
	      (concat query
		      (make-string (abs extra-chars) ?\ )
		      " ")
	    (concat (seq-subseq query 0 53)
		    "... " ))))
    (propertize formatted-string 'face 'yeetube-face-header-query)))

(defun yeetube-buffer--format-header (query)
  "Render header for *yeetube* buffer for QUERY."
  (setf header-line-format
	(concat
	 (concat
	  "Search: " (yeetube-buffer--format-header-title query)
	  (yeetube-buffer--format-view-count "Views")
	  (yeetube-buffer--format-video-duration "Duration")
	  (yeetube-buffer--format-channel "Channel")))))

(defun yeetube-buffer--format-title (title)
  "Format a video TITLE to be inserted in the *yeetube* buffer."
  (let* ((n (string-width title))
	 (extra-chars (- n 60))
	 (formatted-string
	  (if (<= extra-chars 0)
	      (concat title
		      (make-string (abs extra-chars) ?\ )
		      " ")
	    (concat (seq-subseq title 0 57)
		    "... " ))))
    (propertize formatted-string 'face 'yeetube-face-title)))

(defun yeetube-buffer--format-view-count (view-count)
  "Format a video VIEW-COUNT to be inserted in the *yeetube* buffer."
  (let* ((n (string-width view-count))
	 (extra-chars (- n 13))
	 (formatted-string
	  (if (<= extra-chars 0)
	      (concat view-count
		      (make-string (abs extra-chars) ?\ )
		      " ")
	    (concat (seq-subseq view-count 0 10)
		    "..."))))
    (propertize formatted-string 'face 'yeetube-face-view-count)))

(defun yeetube-buffer--format-video-duration (video-duration)
  "Format a video VIDEO-DURATION to be inserted in the *yeetube* buffer."
  (let* ((n (string-width video-duration))
	 (extra-chars (- n 10))
	 (formatted-string (if (<= extra-chars 0)
			       (concat video-duration
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq video-duration 0 20)
				     "..."))))
    (propertize formatted-string 'face 'yeetube-face-duration)))

(defun yeetube-buffer--format-video-duration (video-duration)
  "Format a video VIDEO-DURATION to be inserted in the *yeetube* buffer."
  (let* ((n (string-width video-duration))
	 (extra-chars (- n 7))
	 (formatted-string (if (<= extra-chars 0)
			       (concat video-duration
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq video-duration 0 7)
				     "..."))))
    (propertize formatted-string 'face 'yeetube-face-duration)))

(defun yeetube-buffer--format-channel (channel)
  "Format a video CHANNEL to be inserted in the *yeetube* buffer."
  (let* ((n (string-width channel))
	 (extra-chars (- n 15))
	 (formatted-string
	  (if (<= extra-chars 0)
	      (concat channel
		      (make-string (abs extra-chars) ?\ )
		      " ")
	    (concat (seq-subseq channel 0 11)
		    "... " ))))
    (propertize formatted-string 'face 'yeetube-face-channel)))

(defun yeetube-buffer-create (query content buffer-mode)
  "Create *yeetube* buffer with BUFFER-MODE for QUERY, displaying CONTENT."
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*yeetube*"))
    (funcall buffer-mode)
    (erase-buffer)
    (yeetube-buffer--format-header query)
    (dolist (info (reverse content))
      (let ((title (yeetube-buffer-fix-title (car info)))
	    (view-count (caddr info))
	    (video-duration (cadddr info))
	    (channel-name (nth 4 info)))
	(insert
	 (yeetube-buffer--format-title title)
	 (yeetube-buffer--format-view-count
	  (if (< (length view-count) 20)
	      (yeetube-buffer-view-count-add-commas
	       (yeetube-buffer-fix-view-count view-count))
	    "nil"))
	 (yeetube-buffer--format-video-duration
	  (if (string-match-p "^[0-9:]+$" video-duration)
	      video-duration
	    "nil"))
	 (yeetube-buffer--format-channel channel-name)
	 "\n")))))

(add-hook #'yeetube-buffer-create #'yeetube-mode)
(provide 'yeetube-buffer)
;;; yeetube-buffer.el ends here
