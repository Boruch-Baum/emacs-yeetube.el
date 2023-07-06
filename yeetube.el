;;; yeetube.el --- YouTube/Invidious Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions
;; URL: https://git.sr.ht/~thanosapollo/yeetube.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

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

;; Search, play and downlaod videos from YouTube or any Invidious instance,
;; including localhost.

;; This package does not use YouTube/Invidious API.

;;; Code:

(require 'url)
(require 'org-element)
(require 'cl-lib)


(defgroup yeetube nil
  "Search, Play & Download videos."
  :group 'external
  :prefix "yeetube-")

(defcustom yeetube-results-limit 10
  "Define the amount of search results.

Note:
If you are using an invidious instance, it's recommended to keep it
below 15."
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
 (setq yeetube-download-audio-format \"m4a\")"
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-display-info-keys t
  "Display default keybindings."
  :type 'boolean
  :safe #'booleanp
  :group 'yeetube)


(defcustom yeetube-player (unless (eq (executable-find "mpv") nil)
			    "mpv --input-ipc-server=/tmp/mpvsocket")
  "Select default video player.

It's recommended you keep it as the default value."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-download-directory "~/Downloads"
  "Default directory to downlaod videos."
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(define-minor-mode yeetube-mode
  "Yeetube mode."
  :init-value nil
  :lighter " yeetube-mode"
  :keymap (let ((yeetube-mode-map (make-sparse-keymap)))
	    (define-key yeetube-mode-map (kbd "RET") 'yeetube-play)
	    (define-key yeetube-mode-map (kbd "d") 'yeetube-download-video)
	    (define-key yeetube-mode-map (kbd "u") 'yeetube-change-query-url)
	    (define-key yeetube-mode-map (kbd "q") 'kill-current-buffer)
	    (define-key yeetube-mode-map (kbd "C") 'yeetube-change-download-directory)
	    (define-key yeetube-mode-map (kbd "a") 'yeetube-change-download-audio-format)
            yeetube-mode-map))

(defun yeetube-check-if-youtube (url)
  "Check if URL contain youtube."
  (if (string-match-p "youtube" url)
      t
    nil))

(defun yeetube-play ()
  "Open the url at point in an `'org-mode buffer using 'yeetube-player'."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (shell-command (format "pkill -9 -f %s" (shell-quote-argument yeetube-player)))
    (when (string-prefix-p "http" url)
      (call-process-shell-command
       (format "%s %s" yeetube-player url) nil 0)
      (message "Opening with %s" yeetube-player))))

(defun yeetube-toggle-video-mpv ()
  "Toggle video on/off for mpv player."
  (interactive)
  (when yeetube-player
    (setq yeetube-player
	  (if (equal yeetube-player "mpv --input-ipc-server=/tmp/mpvsocket")
	      "mpv --no-video --input-ipc-server=/tmp/mpvsocket"
	    "mpv --input-ipc-server=/tmp/mpvsocket"))))
;; we should use something like
;; (decode-coding-region (point-min) (point-max) 'utf-8
;;                        (get-buffer-create "decoded"))
;; in yeetube-search to make sure titles are always correct
;; this is a quick "duck-tape" fix.
(defun yeetube-fix-title (title)
  "Adjust TITLE."
  (replace-regexp-in-string "&#39;" "'"
			    (replace-regexp-in-string "&quot;" "\"" title)))

(defun yeetube-insert-content (prefix url video-titles video-ids)
  "Insert video links with titles into the buffer.

Arguments:
- PREFIX: The prefix string for each link.
- URL: The base URL for the YouTube links.
- VIDEO-TITLES: A list of video titles.
- VIDEO-IDS: A list of video IDs.

For each video ID and video title, inserts a link into the buffer in the format:
PREFIX [[URL/watch?v=VIDEOID][VIDEOTITLE ]]"
  (cl-loop for (video-id . video-title) in
	   (cl-mapcar #'cons (reverse video-ids) (reverse video-titles))
           do (insert (format "%s [[%s/watch?v=%s][%s ]]\n"
			      prefix url video-id
			      (yeetube-fix-title video-title)))))

(defun yeetube-search (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (let ((video-ids '())
        (video-titles '())
	(is-youtube? (yeetube-check-if-youtube query)))
    (with-current-buffer
	(url-retrieve-synchronously
	 (concat yeetube-query-url
		 "/search?q="
		 (replace-regexp-in-string " " "+" query))
	 t t)
      (goto-char (point-min))
      (toggle-enable-multibyte-characters)
      (while (< (length video-ids) yeetube-results-limit)
	(if is-youtube?
	    (search-forward "videoId")
	  (search-forward "watch?v"))
        (let* ((start (point))
               (end (if is-youtube?
			(search-forward ",")
		      (search-forward ">")))
               (videoid (buffer-substring
			 (if is-youtube?
			     (+ start 3)
			   (+ start 1))
			 ;; They are the same in both cases,
			 ;; /but/ for debugging/adding more sites
			 ;; it's easier this way.
			 (if is-youtube?
			     (- end 2)
			   (- end 2)))))
          (unless (or (member videoid video-ids)
                      (not (and (>= (length videoid) 9)
                                (<= (length videoid) 13)
                                (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
            (push videoid video-ids)
            (if is-youtube?
		(search-forward "text")
	      (search-forward "\"auto\">"))
            (let* ((start (point))
                   (end (if is-youtube?
			    (search-forward ",\"")
			  (search-forward ">")))
                   (title (buffer-substring
			   (if is-youtube?
			       (+ start 3)
			     (+ start 0))
			   (if is-youtube?
			       (- end 5)
			     (- end 4)))))
	      (if (string-match-p "vssLoggingContext" title)
		  (pop video-ids)
		(push title video-titles)))))))
    (with-current-buffer
	(switch-to-buffer
         (get-buffer-create "*Yeetube Search*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (org-mode)
      (insert
       (format "searching: %s\nfor: %s \n* Search Results: \n \n" yeetube-query-url query))
      (yeetube-insert-content
       yeetube-results-prefix yeetube-query-url
       video-titles video-ids)
      (yeetube-insert-info)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (search-forward yeetube-results-prefix)
      (yeetube-mode))))

(defun yeetube-download-video ()
  "Download using link at point in an `'org-mode buffer with yt-dlp."
  (interactive)
  (let ((url (org-element-property
	      :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
	(async-shell-command
	 (if yeetube-download-audio-format
	     (format "yt-dlp %s --extract-audio --audio-format %s"
		     (shell-quote-argument url)
		     (shell-quote-argument yeetube-download-audio-format))
	   (format "yt-dlp %s" (shell-quote-argument url)))
	 (message "Downloading %s " url))))))

(defun yeetube-download-videos ()
  "Download one or multiple videos using yt-dlp.

This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your videos,
then run this command interactively."
  (interactive)
  (let ((links '())
        (names '())
        (url "")
        (name "")
        (buffer-counter 1)
        (name-counter 1))
    ;; Read links and names until "q" is entered
    (while (not (string= url "q"))
      (setq url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
        (setq links (cons url links))
        (setq name (read-string (format "Enter name (%d-NAME): " name-counter)))
        (while (get-buffer (format "download-video-%d" buffer-counter))
          (setq buffer-counter (1+ buffer-counter)))
        (setq names (cons name names))
        (setq buffer-counter (1+ buffer-counter))
        (setq name-counter (1+ name-counter))))
    ;; Process the collected links and names
    (setq links (reverse links))
    (setq names (reverse names))
    (dolist (pair (cl-mapcar 'cons links names))
      (let ((url (car pair))
            (name (cdr pair))
            (buffer-name (format "download-video-%d" buffer-counter)))
        (async-shell-command
	 (if yeetube-download-audio-format
	     (format "yt-dlp %s --extract-audio --audio-format %s -o %s"
		     (shell-quote-argument url)
		     (shell-quote-argument yeetube-download-audio-format)
		     (shell-quote-argument name))
	   (format "yt-dlp %s -o %s"
		   (shell-quote-argument url)
		   (shell-quote-argument name)))
	 buffer-name)
        (setq buffer-counter (1+ buffer-counter))))))

(defun yeetube-insert-info ()
  "Insert default keybindings at *Yeetube Search* buffer."
  (insert
   "\n\n** Info"
   (format "\nDownload Directory: %s" yeetube-download-directory)
   (format "\nDownload as audio format: %s" yeetube-download-audio-format)
   (format "\nYeetube Player: %s" yeetube-player))
  (when yeetube-display-info-keys
    (insert
     "\n\n*** Keybindings"
     "\n"
     "\n~RET~     -> Play Video\n"
     "\n~d~       -> Download\n"
     "\n~C-c C-o~ -> Open In Browser\n"
     "\n~u~       -> Change search URL (youtube.com, any invidious instance or localhost)\n"
     "\n~C~       -> Change Download Directory\n"
     "\n~a~       -> Change Download (Audio) Format\n"
     "\n~q~       -> Quit\n")))

(defun yeetube-change-download-directory ()
  "Change download directory."
  (interactive)
  (setq yeetube-download-directory
	(read-directory-name "Select a directory: ")))

(defun yeetube-change-download-audio-format ()
  "Change download audio format."
  (interactive)
  (setq yeetube-download-audio-format
	(read-string "Specify audio format(no for nil): "))
  (when (equal yeetube-download-audio-format "no")
    (setq yeetube-download-audio-format nil)))

(defun yeetube-change-query-url ()
  "Change `yeetube-query-url'."
  (interactive)
  (setq yeetube-query-url (read-string "URL: "))
  (when (string-prefix-p "localhost" yeetube-query-url)
    (setq yeetube-query-url (concat "http://localhost:" (read-string "Port: "))))
  (unless (or (string-prefix-p "http://" yeetube-query-url)
	      (string-prefix-p "https://" yeetube-query-url))
    (setq yeetube-query-url (concat "https://" yeetube-query-url)))
  (when (string-suffix-p "/" yeetube-query-url)
    (setq yeetube-query-url (substring yeetube-query-url 0 -1))))

(defun yeetube-update-info (symbol-name new-value _operation _where)
  "Update information for SYMBOL-NAME with NEW-VALUE.

SYMBOL-NAME is the name of the symbol to update.
NEW-VALUE is the new value for the symbol.
OPERATION is the operation to perform (e.g., insert or replace).
WHERE indicates where in the buffer the update should happen.

OPERATION & WHERE are required to work with 'add-variable-watcher."
  (when (get-buffer "*Yeetube Search*")
    (push-mark)
    (let ((to-change
	   (pcase symbol-name
	     ('yeetube-player "Yeetube Player:")
	     ('yeetube-download-directory "Download Directory:")
	     ('yeetube-download-audio-format "Download as audio format:")
	     ('yeetube-query-url "searching:")))
	  (buffer-cur (buffer-name)))
      (switch-to-buffer (get-buffer "*Yeetube Search*"))
      (setq-local buffer-read-only nil)
      (goto-char (point-min))
      (search-forward to-change)
      (beginning-of-visual-line)
      (kill-region (point) (line-end-position))
      (insert
       (format "%s %s" to-change new-value))
      (setq-local buffer-read-only t)
      (switch-to-buffer buffer-cur))
    (goto-char (mark))))

(defun yeetube-read-documentation ()
  "Read yeetube.el Documentation."
  (interactive)
  (find-file (expand-file-name "documentation.org")))

;; Variable to watch
(add-variable-watcher 'yeetube-download-directory #'yeetube-update-info)
(add-variable-watcher 'yeetube-player #'yeetube-update-info)
(add-variable-watcher 'yeetube-download-audio-format #'yeetube-update-info)
(add-variable-watcher 'yeetube-query-url #'yeetube-update-info)

(provide 'yeetube)
;;; yeetube.el ends here
