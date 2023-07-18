
(defvar yeetube--video-ids '())
(defvar yeetube--video-titles '())

(defun yt-search2 (query)
  "Search for QUERY."
  (interactive "sYeetube Search: ")
  (with-current-buffer
      (url-retrieve-synchronously
       (concat yeetube-query-url
               "/search?q="
               (replace-regexp-in-string " " "+" query))
       t t)
    (goto-char (point-min))
    (toggle-enable-multibyte-characters)
    (yeetube--get-content-youtube)
    (yeetube--draw-buffer query yeetube--video-titles yeetube--video-ids)))


(defun yeetube--get-content-youtube ()
  (setq yeetube--video-ids nil)
  (setq yeetube--video-titles nil)
  (while (and (< (length yeetube--video-ids) yeetube-results-limit)
              (search-forward "videoId" nil t))
    (let* ((start (point))
           (end (search-forward ","))
           (videoid (buffer-substring
                     (+ start 3)
                     (- end 2))))
      (unless (or (member videoid yeetube--video-ids)
                  (not (and (>= (length videoid) 9)
                            (<= (length videoid) 13)
                            (string-match-p "^[a-zA-Z0-9_-]*$" videoid))))
        (push videoid yeetube--video-ids)
        (search-forward "text")
        (let* ((start (point))
               (end (search-forward ",\""))
               (title (buffer-substring
                       (+ start 3)
                       (- end 5))))
          (if (string-match-p "vssLoggingContext" title)
              (pop yeetube--video-ids)
            (push title yeetube--video-titles)))))))

(setq yeetube-results-limit 15)
