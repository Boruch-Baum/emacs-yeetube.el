;;; yeetube.el --- YouTube Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.com/yeetube
;; Version: 2.0.4
(defvar yeetube--version '2.0.4)

;; Package-Requires: ((emacs "29.1"))

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

;; This package provides a YouTube front-end for Emacs.

;;; Code:

(require 'url)
(require 'cl-lib)
(require 'yeetube-buffer)
(require 'yeetube-mpv)

(defgroup yeetube nil
  "Youtube Front-end."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 15
  "Define a limit for search results."
  :type 'number
  :group 'yeetube)

(defcustom yeetube-player #'yeetube-mpv
  "Select media player function."
  :type 'function
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Select download video as audio FORMAT.
If nil download output will be the default format.

Example Usage:
 (setf yeetube-download-audio-format \"m4a\")"
  :type 'string
  :group 'yeetube)

(defcustom yeetube-download-directory "~/Downloads"
  "Default directory to downlaod videos."
  :type 'string
  :group 'yeetube)

(defvar yeetube-content nil
  "Scraped content.")

(defvar yeetube-saved-videos nil
  "Saved/bookmarked video urls.")

(defvar yeetube-last-played nil
  "Last played url.")

(defun yeetube-get-url ()
  "Get url for subject in *yeetube* buffer at point."
  (let ((video-url (concat "https://youtube.com/watch?v="
			   (cadr (nth (- (line-number-at-pos) 1) (reverse yeetube-content))))))
    video-url))

(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (funcall yeetube-player (yeetube-get-url)))

(defun yeetube-load-saved-videos ()
  "Load saved videos."
  (interactive)
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (if (file-exists-p file-path)
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (let ((contents (read (current-buffer))))
	    (setf yeetube-saved-videos contents)))
      (write-region "nil" nil file-path))))

(defun yeetube-save-video ()
  "Save url at point."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((name (read-string "Save as: "))
	(url (yeetube-get-url)))
    (push (cons name url) yeetube-saved-videos)))

(defun yeetube-play-saved-video ()
  "Select & Play a saved video."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (funcall yeetube-player (cdr (assoc video yeetube-saved-videos)))))

(defun yeetube-remove-saved-video ()
  "Select video to remove from saved videos."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (setf yeetube-saved-videos (remove (assoc video yeetube-saved-videos) yeetube-saved-videos))))

(defun yeetube-remove-all-saved-videos ()
  "Clear yeetube saved."
  (interactive)
  (let ((clear-saved (y-or-n-p "Delete saved?")))
    (when clear-saved
      (setf yeetube-saved-videos nil))))

(defun yeetube-update-saved-videos-list (_symbol new-value _where _environment)
  "Updated saved videos.

SYMBOL-NAME is the name of the symbol to update.
NEW-VALUE is the new value for the symbol.
OPERATION is the operation to perform.
WHERE indicates where in the buffer the update should happen."
  (with-temp-buffer (find-file (concat user-emacs-directory "yeetube"))
		    (erase-buffer)
		    (setf yeetube-saved-videos new-value)
		    (insert (pp-to-string yeetube-saved-videos))
		    (save-buffer)
		    (kill-buffer)))

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "https://youtube.com/search?q="
	       (replace-regexp-in-string " " "+" query)
	       ;; Filter parameter to remove live videos.
	       "&sp=EgQQASAB")
       t t 30)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    (toggle-enable-multibyte-characters)
    (yeetube-get-content)
    (yeetube-buffer-create query yeetube-content 'yeetube-mode)))

(defun yeetube-browse-url ()
  "Open URL in browser."
  (interactive)
  (browse-url (yeetube-get-url)))

(defun yeetube-get-item (query)
  "Get item from youtube results for QUERY.

Video result starts with videorenderer.
Search back to videorenderer (start of video results),
then for item."
  (search-backward "videorenderer" nil t)
  (search-forward query nil t)
  (search-forward "text" nil t))

(defun yeetube-get-content ()
  "Get content from youtube."
  (setf yeetube-content nil)
  (while (and (< (length yeetube-content) yeetube-results-limit)
	      (search-forward "videorenderer" nil t))
    (search-forward "videoid")
    (let ((videoid (buffer-substring (+ (point) 3) (- (search-forward ",") 2))))
      (unless (member videoid (car yeetube-content))
	(yeetube-get-item "title") ;; Video Title
        (let ((title (buffer-substring (+ (point) 3) (- (search-forward ",\"") 5))))
	  (unless (member title (car yeetube-content))
	    (yeetube-get-item "viewcounttext") ;; View Count
	    (let ((view-count (buffer-substring (+ (point) 3) (- (search-forward " ") 0))))
	      (yeetube-get-item "lengthtext") ;; Video Duration
	      (let ((video-duration (buffer-substring (+ (point) 3) (- (search-forward "},") 3))))
		(yeetube-get-item "longbylinetext") ;; Channel Name
		(let ((channel (buffer-substring (+ (point) 3) (- (search-forward ",") 2))))
		  (push
		   `(,title ,videoid ,view-count ,video-duration ,channel)
		   yeetube-content))))))))))

(defun yeetube-version ()
  "Show Yeetube Version."
  (interactive)
  (message "Yeetube Version: %s" yeetube--version))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

;; Yeetube Downlaod:

(defvar yeetube-ytdlp (executable-find "yt-dlp")
  "Path for yt-dlp executable.")

(defun yeetube-download-change-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

(defun yeetube-download-change-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setf yeetube-download-audio-format nil)))

(defun yeetube-download-ytdlp (url &optional name audio-format)
  "Download URL using yt-dlp.

Optional values:
 NAME to specify file.
 AUDIO-FORMAT to extract and keep contents as specified audio-format only."
  (unless yeetube-ytdlp
    (error "Executable for yt-dlp not found.  Please install yt-dlp"))
  (call-process-shell-command
   (concat "yt-dlp " (shell-quote-argument url)
	   (when name
	     " -o "(shell-quote-argument name))
	   (when audio-format
	     " --extract-audio --audio-format " (shell-quote-argument audio-format)))
   nil 0))

;;;###autoload
(defun yeetube-download-video ()
  "Download entry at point in *yeetube* buffer with yt-dlp."
  (interactive)
  (let ((url (yeetube-get-url)))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (yeetube-download-ytdlp url nil yeetube-download-audio-format)
        (message "Downloading %s " url)))))

;;;###autoload
(defun yeetube-download-videos ()
  "Download one or multiple videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your videos,
then run this command interactively.  You can leave the 'Custom name:'
prompt blank to keep the default name."
  (interactive)
  (let ((url "")
	(name "")
	(download-counter 1))
    (while (not (string= url "q"))
      (setf url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
	(setf name (read-string (format "Custom name (download counter: %d) " download-counter)))
	(setf download-counter (1+ download-counter))
	(yeetube-download-ytdlp url name yeetube-download-audio-format)))))

;; Yeetube Mode
(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "d" #'yeetube-download-video
  "D" #'yeetube-change-download-directory
  "a" #'yeetube-change-download-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "P" #'yeetube-play-saved-video
  "q" #'quit-window)

(define-derived-mode yeetube-mode special-mode "Yeetube"
  "Yeetube mode."
  :interactive t
  (abbrev-mode 0)
  (display-line-numbers-mode 0)
  :lighter " yeetube-mode"
  :keymap yeetube-mode-map)

(provide 'yeetube)
;;; yeetube.el ends here
