;;; yeetube-buffer.el --- Yeetube Buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.org/yeetube
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

;; This package is an extension for yeetube, to create a custom
;; *yeetube* buffer.

;;; Code:

(require 'cl-lib)

(defcustom yeetube-buffer-display-emojis nil
  "Display video title's emojis.

Emojis cause formatting issues, this should be off by default."
  :type 'boolean
  :group 'yeetube)

(defgroup yeetube-buffer-faces nil
  "Faces used by yeetube."
  :group 'yeetube
  :tag "Yeetube Faces"
  :prefix 'yeetube-face)

(defface yeetube-buffer-face-header-query
  '((t :inherit font-lock-function-name-face))
  "Face used for the video published date."
  :group 'yeetube-buffer-faces)

(defface yeetube-buffer-face-duration
  '((t :inherit font-lock-string-face))
  "Face used for the video duration."
  :group 'yeetube-buffer-faces)

(defface yeetube-buffer-face-view-count
  '((t :inherit font-lock-keyword-face))
  "Face used for the video view count."
  :group 'yeetube-buffer-faces)

(defface yeetube-buffer-face-title
  '((t :inherit font-lock-variable-use-face))
  "Face used for video title."
  :group 'yeetube-buffer-faces)

(defface yeetube-buffer-face-channel
  '((t :inherit font-lock-function-call-face))
  "Face used for video channel name."
  :group 'yeetube-buffer-faces)

(defun yeetube-buffer-strip-emojis (str)
  "Remove characters which are part of the `emoji' script from STR."
  (cl-remove-if (lambda (c)
                  (equal (aref char-script-table c) 'emoji))
                str))

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

;;; Formatting inspired from ytel
(defun yeetube-buffer--format-header-title (query)
  "Format header for QUERY."
  (let* ((n (string-width query))
	 (extra-chars (- n 53))
	 (formatted-string
	  (if (<= extra-chars 0)
	      (concat query
		      (make-string (abs extra-chars) ?\ )
		      " ")
	    (concat (seq-subseq query 0 50)
		    "... " ))))
    (propertize formatted-string 'face 'yeetube-buffer-face-header-query)))

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
    (propertize formatted-string 'face 'yeetube-buffer-face-title)))

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
    (propertize formatted-string 'face 'yeetube-buffer-face-view-count)))

(defun yeetube-buffer--format-video-duration (video-duration)
  "Format a video VIDEO-DURATION to be inserted in the *yeetube* buffer."
  (let* ((n (string-width video-duration))
	 (extra-chars (- n 13))
	 (formatted-string (if (<= extra-chars 0)
			       (concat video-duration
				       (make-string (abs extra-chars) ?\ )
				       " ")
			     (concat (seq-subseq video-duration 0 10)
				     "..."))))
    (propertize formatted-string 'face 'yeetube-buffer-face-duration)))

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
    (propertize formatted-string 'face 'yeetube-buffer-face-channel)))

(defun yeetube-buffer--format-header (query)
  "Render header for *yeetube* buffer for QUERY."
  (setf header-line-format
	 (concat
	  "Search: " (yeetube-buffer--format-header-title query)
	  (yeetube-buffer--format-view-count "Views")
	  (yeetube-buffer--format-video-duration "Duration")
	  (yeetube-buffer--format-channel "Channel"))))

(defun yeetube-buffer-insert-content (content)
  "Insert formatted CONTENT."
  (insert
   (concat
    (yeetube-buffer--format-title  (cl-getf content :title))
    (yeetube-buffer--format-view-count (yeetube-buffer-view-count-add-commas
					(yeetube-buffer-fix-view-count
					 (cl-getf content :view-count))))
    (yeetube-buffer--format-video-duration (cl-getf content :duration))
    (yeetube-buffer--format-channel (cl-getf content :channel))
    "\n")))

;;;###autoload
(defun yeetube-buffer-create (query content buffer-mode)
  "Create *yeetube* buffer with BUFFER-MODE for search QUERY, displaying CONTENT."
  (with-current-buffer
      (switch-to-buffer (get-buffer-create "*yeetube*"))
    (funcall buffer-mode)
    (setf buffer-read-only nil)
    (erase-buffer)
    (yeetube-buffer--format-header query)
    (cl-loop for result in (reverse content)
	     do (yeetube-buffer-insert-content result))
    (delete-char -1)
    (goto-char (point-min))
    (setf buffer-read-only t)))

(provide 'yeetube-buffer)
;;; yeetube-buffer.el ends here
