;;; yeetube.el --- YouTube Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions youtube videos
;; URL: https://git.thanosapollo.com/yeetube
;; Version: 2.0.2
(defvar yeetube--version '2.0.2)

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

;; This package provides a YouTube front-end for Emacs.

;;; Code:

(require 'url)
(require 'org-element)
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
  :safe #'numberp
  :group 'yeetube)

(defcustom yeetube-results-prefix "+"
  "Define prefix to display results with."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-query-url "https://www.youtube.com"
  "Search URL."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-download-audio-format nil
  "Select download video as audio FORMAT.
If nil download output will be the default format.

Example Usage:
 (setf yeetube-download-audio-format \"m4a\")"
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-display-info-keys t
  "Display default keybindings."
  :type 'boolean
  :safe #'booleanp
  :group 'yeetube)

(defcustom yeetube-player #'yeetube-mpv
  "Select media player function."
  :type 'function
  :safe #'function
  :group 'yeetube)

(defcustom yeetube-download-directory "~/Downloads"
  "Default directory to downlaod videos."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-display-view-count t
  "Show video view count."
  :type 'boolean
  :safe #'booleanp
  :group 'yeetube)

(define-minor-mode yeetube-mode
  "Yeetube mode."
  :init-value nil
  :lighter " yeetube-mode"
  :keymap (let ((yeetube-mode-map (make-sparse-keymap)))
            (define-key yeetube-mode-map (kbd "RET") 'yeetube-play)
            (define-key yeetube-mode-map (kbd "d") 'yeetube-download-video)
            (define-key yeetube-mode-map (kbd "u") 'yeetube-change-platform)
            (define-key yeetube-mode-map (kbd "q") 'kill-current-buffer)
            (define-key yeetube-mode-map (kbd "D") 'yeetube-change-download-directory)
            (define-key yeetube-mode-map (kbd "a") 'yeetube-change-download-audio-format)
            (define-key yeetube-mode-map (kbd "p") 'yeetube-mpv-toggle-pause)
            (define-key yeetube-mode-map (kbd "v") 'yeetube-mpv-toggle-video)
	    (define-key yeetube-mode-map (kbd "V") 'yeetube-mpv-toggle-no-video-flag)
	    (define-key yeetube-mode-map (kbd "s") 'yeetube-save-video)
	    (define-key yeetube-mode-map (kbd "P") 'yeetube-play-saved-video)
            yeetube-mode-map))

(defvar yeetube-yt-dlp (executable-find "yt-dlp"))

(defvar yeetube-content nil)

(defvar yeetube-saved-videos nil)

(defvar yeetube-last-played nil)

(defun yeetube-youtube-p (url)
  "Check if it's a youtube URL."
  (if (string-match-p "youtube" url)
      t
    nil))

(defun yeetube-play ()
  "Play video at point in *yeetube* buffer."
  (interactive)
  (let ((item-num (line-number-at-pos)))
    (funcall yeetube-player
	     (concat "https://youtube.com/watch?v="
		     (cadr (nth (- item-num 1) (reverse yeetube-content)))))))

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
	(url (org-element-property
	      :raw-link (org-element-context))))
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

;; Usually titles from youtube get messed up,
;; This should fix some of the common issues.
(defun yeetube-fix-title (title)
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

;;;###autoload
(defun yeetube-search (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (let ((is-youtube? (yeetube-youtube-p yeetube-query-url)))
    (with-current-buffer
	(url-retrieve-synchronously
	 (concat yeetube-query-url
		 "/search?q="
		 (replace-regexp-in-string " " "+" query)
		 "&type=video")
	 t t 30)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (toggle-enable-multibyte-characters)
      (yeetube-get-content-youtube)
      (yeetube-buffer-create query yeetube-content 'yeetube-mode))))

(defun yeetube-get-content-youtube ()
  "Get content from youtube."
  (setf yeetube-content nil)
  ;; we define these temp lists to keep tract of video-ids and
  ;; video-titles, ensuring we push only unique ones to
  ;; yeetube-content
  (let ((video-ids nil)
	(video-titles nil))
    (while (and (< (length video-ids) yeetube-results-limit)
		(search-forward "videoId" nil t))
      (let* ((videoid-start (point))
             (videoid-end (search-forward ","))
             (videoid (buffer-substring
                       (+ videoid-start 3)
                       (- videoid-end 2))))
	(unless (and (member videoid video-ids)
		     (not (and (>= (length videoid) 9)
			       (<= (length videoid) 13)
			       (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
          (push videoid video-ids)
	  ;; (search-backward "videoid")
	  (search-forward "\"title\":")
          (search-forward "text")
          (let* ((title-start (point))
		 (title-end (search-forward ",\""))
		 (title (buffer-substring
			 (+ title-start 3)
			 (- title-end 5))))
	    (unless (member title video-titles)
	      (push title video-titles)
	      ;; (search-backward "videoid")
	      (search-forward "viewCountText" nil t)
	      (search-forward "text" nil t)
	      (let* ((view-count-start (point))
		     (view-count-end (search-forward " "))
		     (view-count (buffer-substring
				  (+ view-count-start 3)
				  (- view-count-end 0))))
		;; Get video duration
		(search-forward "lengthText" nil t)
		(search-forward "text" nil t)
		(let* ((video-duration-start (point))
		       (video-duration-end (search-forward "},"))
		       (video-duration (buffer-substring
					(+ video-duration-start 3)
					(- video-duration-end 3))))
		  (search-backward "videoid")
		  (push `(,title ,videoid ,view-count ,video-duration) yeetube-content))))))))))

;;;###autoload
(defun yeetube-download-video ()
  "Download using link at point in an `'org-mode buffer with yt-dlp."
  (interactive)
  (let ((url (org-element-property
              :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (call-process-shell-command
         (if yeetube-download-audio-format
             (format "%s '%s' --extract-audio --audio-format %s" (executable-find "yt-dlp") url yeetube-download-audio-format)
	   (format "%s '%s'" (executable-find "yt-dlp") url))
	 nil 0)
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
        (download-counter 1)
	(stored-contents nil))
    ;; Read links and names until "q" is entered
    (while (not (string= url "q"))
      (setf url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
        (setf name (read-string (format "Custom name (download counter: %d) " download-counter)))
	(push (cons url name) stored-contents)
        (setf download-counter (1+ download-counter))))
    ;; Process the collected links and names
    (dolist (pair stored-contents)
      (let ((url (car pair))
            (name (cdr pair)))
        (call-process-shell-command
         (format "%s '%s' -o %s" (executable-find "yt-dlp") url name)
	 nil 0)))))

(defun yeetube-insert-info ()
  "Insert default keybindings at *Yeetube Search* buffer."
  (insert
   "\n\n** Info"
   (format "\nDownload Directory: %s" yeetube-download-directory)
   (format "\nDownload as audio format: %s" yeetube-download-audio-format))
  (when yeetube-display-info-keys
    (insert
     "\n\n*** Keybindings"
     "\n"
     "\n~RET~     -> Play Video\n"
     "\n~v~       -> Toggle Video\n"
     "\n~V~       -> Toggle no-video flag\n"
     "\n~p~       -> Toggle Pause\n"
     "\n~C-c C-o~ -> Open In Browser\n"
     "\n~d~       -> Download\n"
     "\n~D~       -> Change Download Directory\n"
     "\n~a~       -> Change Download (Audio) Format\n"
     "\n~q~       -> Quit\n"
     "\n~s~       -> Save video\n"
     "\n~P~       -> Play Saved Video")))

(defun yeetube-change-download-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

(defun yeetube-change-download-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setf yeetube-download-audio-format nil)))

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

(defun yeetube-version ()
  "Show Yeetube Version."
  (interactive)
  (message "Yeetube Version: %s" yeetube--version))

(provide 'yeetube)
;;; yeetube.el ends here
