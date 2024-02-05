;;; yeetube.el --- YouTube Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.org/yeetube
;; Version: 2.0.7


;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; This package provides the ability to scrape YouTube, with the results
;; displayed in a proced-like buffer.
;;
;; Basic functionality includes:
;;
;; - Query YouTube
;; - Play video url by default using mpv
;; - Bookmark/Save video url
;; - Download video using yt-dlp
;; - A minimal yt-dlp front-end, which is independent of the rest YouTube functionality, to download multiple urls.

;;; Code:

(require 'compat)
(require 'url)
(require 'tabulated-list)
(require 'cl-lib)
(require 'yeetube-mpv)

(defgroup yeetube nil
  "Youtube Front-end."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 20
  "Define a limit for search results."
  :type 'number
  :group 'yeetube)

(defcustom yeetube-player #'yeetube-mpv-play
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

(defcustom yeetube-default-sort-column "Title"
  "Which column to sort the search results table."
  :type '(radio (const "Title")
                (const "Views")
                (const "Duration")
                (const "When")
                (const "Channel"))
  :group 'yeetube)

(defcustom yeetube-default-sort-ascending nil
  "Whether to sort the search results in ascending order."
  :type 'boolean
  :group 'yeetube)

(defgroup yeetube-faces nil
  "Faces used by yeetube."
  :group 'yeetube
  :tag "Yeetube Faces"
  :prefix 'yeetube-face)

(defface yeetube-face-header-query
  '((t :inherit font-lock-function-name-face))
  "Face used for the video published date."
  :group 'yeetube-faces)

(defface yeetube-face-duration
  '((t :inherit font-lock-string-face))
  "Face used for the video duration."
  :group 'yeetube-faces)

(defface yeetube-face-view-count
  '((t :inherit font-lock-keyword-face))
  "Face used for the video view count."
  :group 'yeetube-faces)

(defface yeetube-face-title
  '((t :inherit font-lock-variable-use-face))
  "Face used for video title."
  :group 'yeetube-faces)

(defface yeetube-face-when
  '((t :inherit font-lock-variable-use-face))
  "Face used for video age."
  :group 'yeetube-faces)

(defface yeetube-face-channel
  '((t :inherit font-lock-function-call-face))
  "Face used for video channel name."
  :group 'yeetube-faces)

(defvar yeetube-invidious-instances
  '("vid.puffyan.us"
    "invidious.flokinet.to"
    "yt.artemislena.eu"
    "invidious.privacydev.net"
    "onion.tube"
    "yewtu.be")
  "List of invidious instaces.")

(defvar yeetube-content nil
  "Scraped content.")

(defvar yeetube-saved-videos nil
  "Saved/bookmarked video urls.")

(defvar yeetube-history nil
  "Stored urls & titles of recently played content.")

(defvar yeetube-url "https://youtube.com/watch?v="
  "URL used to play videos from.

You can change the value to an invidious instance.")

(defun yeetube-get (keyword)
  "Retrieve KEYWORD value for entry at point.

Retrieve keyword value for entry at point, from `yeetube-content', in
*yeetube* buffer.

Keywords:
- :title
- :videoid
- :view-count
- :published-time
- :duration
- :channel"
  (unless (keywordp keyword)
    (error "Value `%s' is not a keyword" keyword))
  (cl-getf (tabulated-list-get-id) keyword))

(defun yeetube-get-url ()
  "Get video url."
  (let ((video-url (concat yeetube-url (yeetube-get :videoid))))
    video-url))

;;;###autoload
(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (let ((video-url (yeetube-get-url))
	(video-title (yeetube-get :title)))
    (funcall yeetube-player video-url)
    (push (list :url video-url :title video-title) yeetube-history)
    (message "Playing: %s" video-title)))

;;;###autoload
(defun yeetube-replay ()
  "Select entry from history to replay.

Select entry title from yeetube-history and play corresponding URL."
  (interactive)
  (let* ((titles (mapcar (lambda (entry) (cl-getf entry :title)) yeetube-history))
         (selected (completing-read "Replay: " titles))
         (selected-entry (cl-find-if (lambda (entry) (string= selected (cl-getf entry :title))) yeetube-history))
         (url (cl-getf selected-entry :url)))
    (funcall yeetube-player url)
    (message "Replaying: %s" selected)))

(defun yeetube-load-saved-videos ()
  "Load saved videos."
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (if (file-exists-p file-path)
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (let ((contents (read (current-buffer))))
	    (setf yeetube-saved-videos contents)))
      (write-region "nil" nil file-path))))

;;;###autoload
(defun yeetube-save-video ()
  "Save url at point."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((name (read-string "Save as: "))
	(url (yeetube-get-url)))
    (push (cons name url) yeetube-saved-videos)))

;; We could use keywords here, but it would break users saved videos
;; from previous versions.
;;;###autoload
(defun yeetube-play-saved-video ()
  "Select & Play a saved video."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (funcall yeetube-player (cdr (assoc video yeetube-saved-videos)))
    (message "Playing: %s" (car (assoc video yeetube-saved-videos)))))

;;;###autoload
(defun yeetube-remove-saved-video ()
  "Select video to remove from saved videos."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (setf yeetube-saved-videos (remove (assoc video yeetube-saved-videos) yeetube-saved-videos))))

;;;###autoload
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

;; TODO: Find a way to display thumbnails in tabulated list
(cl-defun yeetube-get-thumbnails (content)
  "Download thumbnails for CONTENT using `wget'.

This is used to download thumbnails from `yeetube-content', within
`yeetube-search'. We can't as of now use images with tabulated-list."
  (interactive)
  (let ((wget-exec (executable-find "wget"))
	(default-directory temporary-file-directory))
    (unless wget-exec
      (error "Please install `wget', to download thumbnails"))
    (cl-loop for item in content
	     do (let ((title (plist-get item :title))
		      (thumbnail (plist-get item :thumbnail)))
		  (call-process-shell-command
		   (concat "wget " (shell-quote-argument thumbnail) " -O" (shell-quote-argument title))
		   nil 0)))))

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
       'silent 'inhibit-cookies 30)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    (toggle-enable-multibyte-characters)
    (yeetube-get-content)
    ;; (yeetube-get-thumbnails yeetube-content) ;; download thumbnails
    ;; unfortunately can't use images them with tabulated list
    )
  (with-current-buffer
      (switch-to-buffer (get-buffer-create (concat "*yeetube*")))
    (yeetube-mode)))

;;;###autoload
(defun yeetube-browse-url ()
  "Open URL for video at point, using an invidious instance."
  (interactive)
  (let ((invidious-instance (+ 1 (random (length yeetube-invidious-instances)))))
    (browse-url
     (replace-regexp-in-string "youtube.com"
			       (nth invidious-instance yeetube-invidious-instances)
			       (yeetube-get-url)))))

(cl-defun yeetube-scrape-item (&key item (item-start "text") item-end (substring-start 3) substring-end)
  "Scrape ITEM from YouTube.com.

Video result starts with videorenderer.
Search back to videorenderer (start of video results),
then for item.

ITEM-START is the start of the information for item.
ITEM-END is the end of the item information.
SUBSTRING-START is the start of the string to return, integer.
SUBSTRING-END is the end of the string to return, interger."
  (search-backward "videorenderer" nil t)
  (search-forward item nil t)
  (search-forward item-start nil t)
  (let ((item (buffer-substring (+ (point) substring-start)
				(- (search-forward item-end) substring-end))))
    item))

(defun yeetube-view-count-format (string)
  "Add commas for STRING."
  (let* ((string (replace-regexp-in-string "[^0-9]" "" string))
         (len (length string))
         (result ""))
    (cl-loop for i from 0 to (1- len)
             do (setf result (concat (substring string (- len i 1) (- len i)) result))
             if (and (> (- len (1+ i)) 0)
                     (= (% (1+ i) 3) 0))
             do (setf result (concat "," result)))
    result))

(defun yeetube-get-content ()
  "Get content from youtube."
  (setf yeetube-content nil)
  (while (and (< (length yeetube-content) yeetube-results-limit)
	      (search-forward "videorenderer" nil t))
    (search-forward "videoid")
    (let ((videoid (buffer-substring (+ (point) 3)
				     (- (search-forward ",") 2))))
      (unless (member videoid (car yeetube-content))
	(let ((title (yeetube-scrape-item :item "title" :item-end ",\"" :substring-end 5))
	      (view-count (yeetube-scrape-item :item "viewcounttext" :item-end " " :substring-end 0))
	      (video-duration (yeetube-scrape-item :item "lengthtext" :item-end "}," :substring-end 3))
              (published-time (replace-regexp-in-string "Streamed " "" (yeetube-scrape-item :item "publishedtimetext" :item-end ",\"" :substring-end 4)))
	      (channel (yeetube-scrape-item :item "longbylinetext" :item-end "," :substring-end 2))
	      (thumbnail (yeetube-scrape-item :item "thumbnail" :item-start "url" :item-end ",\"" :substring-end 5)))
	  (push (list :title title
		      :videoid videoid
		      :view-count (yeetube-view-count-format view-count)
		      :duration video-duration
                      :published-time published-time
		      :channel channel
		      :thumbnail thumbnail)
		yeetube-content))))))

(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

;; View thumbnail using eww
(defun yeetube-view-thumbnail ()
  "Open URL using eww in a new buffer."
  (interactive)
  (eww-browse-url (yeetube-get :thumbnail)))


;; Yeetube Downlaod:

(defvar yeetube-ytdlp (executable-find "yt-dlp")
  "Path for yt-dlp executable.")

;;;###autoload
(defun yeetube-download-change-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

;;;###autoload
(defun yeetube-download-change-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setf yeetube-download-audio-format nil)))

(defun yeetube-download--ytdlp (url &optional name audio-format)
  "Download URL using yt-dlp.

Optional values:
 NAME for custom file name.
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

(defun yeetube-download--ffmpeg (url name)
  "Download URL as NAME using ffmpeg."
  (let ((ffmpeg (executable-find "ffmpeg")))
    (unless ffmpeg
      (error "Executable ffmpeg not found.  Please install ffmpeg"))
    (call-process-shell-command
     (concat ffmpeg
	     " -protocol_whitelist file,crypto,data,https,tls,tcp -stats -i "
	     (shell-quote-argument url)
	     " -codec copy "
	     name))))

;;;###autoload
(defun yeetube-download-video ()
  "Download entry at point in *yeetube* buffer with yt-dlp."
  (interactive)
  (let ((url (yeetube-get-url)))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (yeetube-download--ytdlp url nil yeetube-download-audio-format)
        (message "Downloading: '%s' at '%s'"
		 (yeetube-get :title) yeetube-download-directory)))))

;; TODO: Add option to use ffmpeg
;;;###autoload
(defun yeetube-download-videos ()
  "Bulk download videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your
videos, then run this command interactively.  You can leave the name
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
	(yeetube-download--ytdlp url name yeetube-download-audio-format)))))

(defun yeetube-propertize-vector (content &rest fields-face-pairs)
  "Create a vector with each item propertized with its corresponding face.

CONTENT is a list of strings.
FIELDS-FACE-PAIRS is a list of fields and faces."
  (apply #'vector
         (cl-loop for (field face) on fields-face-pairs by #'cddr
                  collect (propertize (cl-getf content field) 'face face))))

;; Yeetube Mode
(defvar-keymap yeetube-mode-map
  :doc "Keymap for yeetube commands"
  "RET" #'yeetube-play
  "M-RET" #'yeetube-search
  "b" #'yeetube-browse-url
  "d" #'yeetube-download-video
  "D" #'yeetube-download-change-directory
  "a" #'yeetube-download-change-audio-format
  "p" #'yeetube-mpv-toggle-pause
  "v" #'yeetube-mpv-toggle-video
  "V" #'yeetube-mpv-toggle-no-video-flag
  "s" #'yeetube-save-video
  "P" #'yeetube-play-saved-video
  "r" #'yeetube-replay
  "t" #'yeetube-view-thumbnail
  "q" #'quit-window)

(defun yeetube--sort-views (a b)
  "PREDICATE for function 'sort'.
Used by variable 'tabulated-list-format' to sort the \"Views\"
column."
  (< (string-to-number (replace-regexp-in-string "," "" (aref (cadr a) 1)))
     (string-to-number (replace-regexp-in-string "," "" (aref (cadr b) 1)))))

(defun yeetube--sort-duration (a b)
  "PREDICATE for function 'sort'.
Used by variable 'tabulated-list-format' to sort the \"Duration\"
column."
  (< (string-to-number (replace-regexp-in-string ":" "" (aref (cadr a) 2)))
     (string-to-number (replace-regexp-in-string ":" "" (aref (cadr b) 2)))))

(defun yeetube--sort-when (a b)
  "PREDICATE for function 'sort'.
Used by variable 'tabulated-list-format' to sort the \"When\"
column."
  (let* ((intervals '("econd" "minute" "hour" "day" "week" "month" "year"))
         (split-a (split-string (replace-regexp-in-string "s" "" (aref (cadr a) 3))))
         (split-b (split-string (replace-regexp-in-string "s" "" (aref (cadr b) 3))))
         (units-a (length (member (nth 1 split-a) intervals)))
         (units-b (length (member (nth 1 split-b) intervals))))
    (if (= units-a units-b)
      (< (string-to-number (nth 0 split-a)) (string-to-number (nth 0 split-b)))
     (> units-a units-b))))

(define-derived-mode yeetube-mode tabulated-list-mode "Yeetube"
  "Yeetube mode."
  :keymap yeetube-mode-map
  (setf tabulated-list-format
        [("Title" 60 t)
         ("Views" 12 yeetube--sort-views)
         ("Duration" 9 yeetube--sort-duration)
         ("When" 13 yeetube--sort-when)
         ("Channel" 12 t)]
	tabulated-list-entries
	(cl-map 'list
		(lambda (content)
                  (list content
			(yeetube-propertize-vector content
                                                   :title 'yeetube-face-title
                                                   :view-count 'yeetube-face-view-count
                                                   :duration 'yeetube-face-duration
                                                   :published-time 'yeetube-face-when
                                                   :channel 'yeetube-face-channel)))
		yeetube-content)
	tabulated-list-sort-key (cons yeetube-default-sort-column
                                      yeetube-default-sort-ascending))
  (display-line-numbers-mode 0)
  (tabulated-list-init-header)
  (tabulated-list-print))

(provide 'yeetube)
;;; yeetube.el ends here
