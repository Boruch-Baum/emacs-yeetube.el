;;; yeetube-face.el --- Yeetube Face  -*- lexical-binding: t; -*-

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

;; This package is a yeetube extension, providing custom faces for
;; yeetube variables

;;; Code:

(defface yeetube-face-header-query
  '((((class color) (background light)) (:foreground "#b6e63e"))
    (((class color) (background dark))  (:foreground "#b6e63e")))
  "Face used for the video published date.")

(defface yeetube-face-duration
  '((((class color) (background light)) (:foreground "#e2c770"))
    (((class color) (background dark))  (:foreground "#e2c770")))
  "Face used for the video duration.")

(defface yeetube-face-view-count
  '((((class color) (background light)) (:foreground "#fb2874"))
    (((class color) (background dark))  (:foreground "#fb2874")))
  "Face used for the video view count.")

(defface yeetube-face-title
  '((((class color) (background light)) (:foreground "#fd971f"))
    (((class color) (background dark))  (:foreground "#fd971f")))
  "Face used for video title.")

(defface yeetube-face-channel
  '((((class color) (background light)) (:foreground "#b6e63e"))
    (((class color) (background dark))  (:foreground "#b6e63e")))
  "Face used for video title.")

(provide 'yeetube-face)

;;; yeetube-face.el ends here
