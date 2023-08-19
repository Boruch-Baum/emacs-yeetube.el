;;; yeetube.el --- YouTube & Invidious Front End  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.com>
;; Keywords: extensions youtube videos invidious
;; URL: https://git.sr.ht/~thanosapollo/yeetube.el
;; Version: 1.5.0
(defvar yeetube--version '1.5.0)

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

;; This package provides the ability to scrape YouTube or any Invidious
;; instance, with the results displayed in a read-only org-mode buffer.

;; Key features:
;;  - Search video query
;;  - Play video URL, by default with mpv
;;  - Save video URL with a custom name/label
;;  - Download video, this package serves also as a front-end for
;;    yt-dlp, thus supporting platforms beyond Youtube & Invidious.

;;; Code:

(require 'url)
(require 'org-element)
(require 'cl-lib)


(defgroup yeetube nil
  "Youtube & Invidious Front-end."
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
 (setq yeetube-download-audio-format \"m4a\")"
  :type 'string
  :safe #'stringp
  :group 'yeetube)

(defcustom yeetube-display-info-keys t
  "Display default keybindings."
  :type 'boolean
  :safe #'booleanp
  :group 'yeetube)

(defcustom yeetube-player 'mpv
  "Select video player."
  :type 'symbol
  :safe #'symbolp
  :group 'yeetube)

(defcustom yeetube-mpv-disable-video nil
  "Add no-video flag when using mpv."
  :type 'boolean
  :safe #'booleanp
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
            (define-key yeetube-mode-map (kbd "p") 'yeetube-toggle-pause-mpv)
            (define-key yeetube-mode-map (kbd "v") 'yeetube-toggle-video-mpv)
	    (define-key yeetube-mode-map (kbd "s") 'yeetube-save-video)
	    (define-key yeetube-mode-map (kbd "P") 'yeetube-play-saved-video)
            yeetube-mode-map))

(defvar yeetube-yt-dlp (executable-find "yt-dlp"))

(defvar yeetube-content nil)

(defvar yeetube-saved-videos nil)

(defvar yeetube-last-played nil)

(defvar yeetube-invidious-instances
  '(("https://yewtu.be")
    ("https://vid.puffyan.us")
    ("https://yt.artemislena.eu")
    ("https://invidious.flokinet.to")
    ("https://invidious.projectsegfau.lt")
    ("https://invidious.tiekoetter.com")
    ("https://invidious.slipfox.xyz")
    ("https://inv.pistasjis.net")
    ("https://invidious.privacydev.net")
    ("https://vid.priv.au")))

(defun yeetube-youtube-p (url)
  "Check if it's a youtube URL."
  (if (string-match-p "youtube" url)
      t
    nil))

(defun yeetube-play-url (url)
  "Open URL using yeetube-player."
  (let ((media-player (executable-find (symbol-name yeetube-player))))
    (unless media-player
      (error (format "%s not found." media-player)))
    (when (string-prefix-p "http" url)
      (setq yeetube-last-played url)
      (if (eq yeetube-player 'mpv)
	  (yeetube-start-mpv-process url)
        (yeetube-start-process
	 (format "%s %s"  media-player url))))))

(defun yeetube-play ()
  "Open the url at point in an `'org-mode buffer using ='yeetube-player'."
  (interactive)
  (let ((url (org-element-property
              :raw-link (org-element-context))))
    (yeetube-play-url url)))

(defun yeetube-load-saved-videos ()
  "Load saved videos."
  (interactive)
  (let ((file-path (concat user-emacs-directory "yeetube")))
    (if (file-exists-p file-path)
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (goto-char (point-min))
	  (let ((contents (read (current-buffer))))
	    (setq yeetube-saved-videos contents)))
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
    (yeetube-play-url (cdr (assoc video yeetube-saved-videos)))))

(defun yeetube-remove-saved-video ()
  "Select video to remove from saved videos."
  (interactive)
  (yeetube-load-saved-videos)
  (let ((video (completing-read "Select video: " yeetube-saved-videos nil t)))
    (setq yeetube-saved-videos (remove (assoc video yeetube-saved-videos) yeetube-saved-videos))))

(defun yeetube-remove-all-saved-videos ()
  "Clear yeetube saved."
  (interactive)
  (let ((clear-saved (y-or-n-p "Delete saved?")))
    (when clear-saved
      (setq yeetube-saved-videos nil))))

(defun yeetube-start-process (command)
  "Start yeetube process for shell COMMAND."
  (let ((process-name "yeetube"))
    (dolist (process (process-list))
      (when (string-match process-name (process-name process))
	(kill-process process)))
    (sit-for 0.1)
    (unless (get-process process-name)
      (start-process-shell-command
       "yeetube" nil command))))

(defun yeetube-start-mpv-process (url)
  "Start yeetube process to play URL using mpv."
  (yeetube-start-process
   (if yeetube-mpv-disable-video
       (format "%s --no-video %s" (executable-find "mpv") url)
     (format "%s %s" (executable-find "mpv") url))))
(defun yeetube-mpv-toggle-no-video-flag ()
  "Toggle no video flag for mpv player."
  (interactive)
  (if yeetube-mpv-disable-video
      (progn (setq yeetube-mpv-disable-video nil)
	     (message "yeetube: mpv removed no-video flag"))
    (setq yeetube-mpv-disable-video t)
    (message "yeetube: mpv added no-video flag")))

(defun yeetube-mpv-toggle-pause ()
  "Toggle play/pause mpv."
  (interactive)
  (process-send-string "yeetube" "p")
  (message "yeetube: toggle pause"))

(defun yeetube-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (process-send-string "yeetube" "f")
  (message "toggle fullscreen"))

(defun yeetube-mpv-send-keypress (key)
  "Send KEY to mpv process."
  (interactive "sKeypress: ")
  (process-send-string "yeetube" key))

(defun yeetube-mpv-toggle-video ()
  "Toggle video on/off mpv."
  (interactive)
  (process-send-string "yeetube" "_")
  (message "yeetube: toggle video"))


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
            (setq title (replace-regexp-in-string (car replacement) (cdr replacement) title)))
          replacements)
    title))

(defun yeetube-create-buffer (query content)
  "Create *Yeetube-Search* buffer for QUERY, using CONTENT."
  (with-temp-buffer
    (switch-to-buffer
     (get-buffer-create "*Yeetube Search*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (insert
     (format "searching: %s\nfor: %s \n* Search Results: \n \n" yeetube-query-url query))
    (dolist (info (reverse content))
      (let ((videoid (car info))
	    (title (yeetube-fix-title (cadr info)))
	    (view-count (caddr info)))
	;; gap [%s ] for titles that end with ]
	(if yeetube-display-view-count
	    (insert (format "%s [[%s/watch?v=%s][%s ]] =%s=\n"
			yeetube-results-prefix yeetube-query-url
			videoid title view-count))
	  (insert (format "%s [[%s/watch?v=%s][%s ]]\n"
			yeetube-results-prefix yeetube-query-url
			videoid title)))))
    (yeetube-insert-info)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (search-forward yeetube-results-prefix)
    (yeetube-mode)))

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
      (if is-youtube?
	  (yeetube-get-content-youtube)
	(yeetube-get-content-invidious))
      (yeetube-create-buffer query yeetube-content))))

(defun yeetube-get-content-youtube ()
  "Get content from youtube."
  (setq yeetube-content nil)
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
	(unless (or (member videoid video-ids)
                    (not (and (>= (length videoid) 9)
                              (<= (length videoid) 13)
                              (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
          (push videoid video-ids)
          (search-forward "text")
          (let* ((title-start (point))
		 (title-end (search-forward ",\""))
		 (title (buffer-substring
			 (+ title-start 3)
			 (- title-end 5))))
            (if (string-match-p "vssLoggingContext" title)
		(pop video-ids)
              (push title video-titles)
	      (search-forward "viewCountText")
	      (search-forward "text")
	      (let* ((view-count-start (point))
		     (view-count-end (search-forward " "))
		     (view-count (buffer-substring
				  (+ view-count-start 3)
				  (- view-count-end 1))))
		;; Don't remove this! It'll make it easier to scrape more
		;; info in the future
		(search-backward "videoid")
		;; show livestreams views as nil
		(if (string-match-p "text" view-count)
		    (push `(,videoid ,title "nil") yeetube-content)
		  (push `(,videoid ,title ,view-count) yeetube-content))))))))))

;; same as youtube but with different values, it's easier this way
;; even though it's "wrong".
(defun yeetube-get-content-invidious ()
  "Get content from an invidious instance."
  (setq yeetube-content nil)
  (let ((video-ids nil)
	(video-titles nil))
    (while (and (< (length video-ids) yeetube-results-limit)
		(search-forward "watch?v" nil t))
      (let* ((videoid-start (point))
             (videoid-end (search-forward ">"))
             (videoid (buffer-substring
                       (+ videoid-start 1)
                       (- videoid-end 2))))
	(unless (or (member videoid video-ids)
                    (not (and (>= (length videoid) 9)
                              (<= (length videoid) 13)
                              (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
          (push videoid video-ids)
          (search-forward "\"auto\">")
          (let* ((title-start (point))
		 (title-end (search-forward ">"))
		 (title (buffer-substring
			 (+ title-start 0)
			 (- title-end 4))))
            (push title video-titles)
	    (search-forward "views")
	    (let* ((view-count-start (search-backward ">"))
		   (view-count-end (search-forward " "))
		   (view-count (buffer-substring
				(+ view-count-start 1)
				(- view-count-end 1))))
	    (push `(,videoid ,title ,view-count) yeetube-content))))))))

;;;###autoload
(defun yeetube-download-video ()
  "Download using link at point in an `'org-mode buffer with yt-dlp."
  (interactive)
  (let ((url (org-element-property
              :raw-link (org-element-context))))
    (when (string-prefix-p "http" url)
      (let ((default-directory yeetube-download-directory))
        (async-shell-command
         (if yeetube-download-audio-format
             (format "%s %s --extract-audio --audio-format %s"
                     (shell-quote-argument yeetube-yt-dlp)
                     (shell-quote-argument url)
                     (shell-quote-argument yeetube-download-audio-format))
           (format "%s %s"
                   (shell-quote-argument yeetube-yt-dlp)
                   (shell-quote-argument url)))
         (message "Downloading %s " url))))))

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
	(audio-only-p (y-or-n-p "Download videos as audio only format?"))
	(stored-contents nil))
    (if audio-only-p
	(yeetube-change-download-audio-format (read-string "Specify audio format: "))
      (yeetube-change-download-audio-format nil))
    ;; Read links and names until "q" is entered
    (while (not (string= url "q"))
      (setq url (read-string "Enter URL (q to quit): "))
      (unless (string= url "q")
        (setq name (read-string (format "Custom name (download counter: %d) " download-counter)))
	(push (cons url name) stored-contents)
        (setq download-counter (1+ download-counter))))
    ;; Process the collected links and names
    (dolist (pair stored-contents)
      (let ((url (car pair))
            (name (cdr pair)))
        (call-process-shell-command
         (if yeetube-download-audio-format
             (format "%s %s --extract-audio --audio-format %s -o %s"
                     (shell-quote-argument yeetube-yt-dlp)
                     (shell-quote-argument url)
                     (shell-quote-argument yeetube-download-audio-format)
                     (shell-quote-argument name))
           (format "%s %s -o %s"
                   (shell-quote-argument yeetube-yt-dlp)
                   (shell-quote-argument url)
                   (shell-quote-argument name)))
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
     "\n~v~       -> Toggle Video on/off\n"
     "\n~p~       -> Toggle Pause/Play\n"
     "\n~C-c C-o~ -> Open In Browser\n"
     "\n~d~       -> Download\n"
     "\n~D~       -> Change Download Directory\n"
     "\n~a~       -> Change Download (Audio) Format\n"
     "\n~u~       -> Change Video Platform (YouTube, Invidious, Localhost, Custom)\n"
     "\n~q~       -> Quit\n"
     "\n~s~       -> Save video\n"
     "\n~P~       -> Play Saved Video")))

(defun yeetube-change-download-directory ()
  "Change download directory."
  (interactive)
  (setq yeetube-download-directory
        (read-directory-name "Select a directory: ")))

(defun yeetube-change-download-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setq yeetube-download-audio-format audio-format)
  (when (equal yeetube-download-audio-format "no")
    (setq yeetube-download-audio-format nil)))


(defun yeetube-change-platform ()
  "Change video platform."
  (interactive)
  (let ((platform (completing-read "Choose video platform: "
				   '("YouTube" "Invidious" "Localhost" "Custom"))))
    (pcase platform
      ("Invidious" (setq yeetube-query-url
			 (completing-read "Select Instance: " yeetube-invidious-instances)))
      ("Localhost" (setq yeetube-query-url "localhost"))
      ("YouTube" (setq yeetube-query-url "youtube.com"))
      ("Custom" (setq yeetube-query-url (read-string "URL: ")))))
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
OPERATION is the operation to perform.
WHERE indicates where in the buffer the update should happen.
OPERATION & WHERE are required to work with ='add-variable-watcher."
  (let ((to-change
	 (pcase symbol-name
	   ('yeetube-download-directory "Download Directory:")
	   ('yeetube-download-audio-format "Download as audio format:")
	   ('yeetube-query-url "searching:")))
	(buffer-cur (buffer-name)))
    (when (get-buffer "*Yeetube Search*")
      (push-mark)
      (switch-to-buffer (get-buffer "*Yeetube Search*"))
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (search-forward to-change)
      (beginning-of-visual-line)
      (kill-region (point) (line-end-position))
      (insert
       (format "%s %s" to-change new-value))
      (setq-local buffer-read-only t)
      (switch-to-buffer buffer-cur)
      (goto-char (mark)))))

(defun yeetube-update-saved-videos-list (_symbol new-value _where _environment)
  "Updated saved videos.

SYMBOL-NAME is the name of the symbol to update.
NEW-VALUE is the new value for the symbol.
OPERATION is the operation to perform.
WHERE indicates where in the buffer the update should happen."
  (with-temp-buffer (find-file (concat user-emacs-directory "yeetube"))
		    (erase-buffer)
		    (setq yeetube-saved-videos new-value)
		    (insert (pp-to-string yeetube-saved-videos))
		    (save-buffer)
		    (kill-buffer)))

(defun yeetube-version ()
  "Show Yeetube Version."
  (interactive)
  (message "Yeetube Version: %s" yeetube--version))

;; Variable to watch
(add-variable-watcher 'yeetube-download-directory #'yeetube-update-info)
(add-variable-watcher 'yeetube-download-audio-format #'yeetube-update-info)
(add-variable-watcher 'yeetube-query-url #'yeetube-update-info)
(add-variable-watcher 'yeetube-saved-videos #'yeetube-update-saved-videos-list)

(provide 'yeetube)
;;; yeetube.el ends here
