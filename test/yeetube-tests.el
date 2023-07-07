;;; yeetube-tests.el --- tests for yeetube.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Thanos Apollo

;; Author: Thanos Apollo <thanosapollo@proton.me>
;; Keywords: 

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

;; 

;;; Code:
(require 'ert)
(load-file "../yeetube.el")
(require 'yeetube)

(ert-deftest test-is-youtube? ()
  (should (equal (yeetube-check-if-youtube "youtube.com") t))
  (should (equal (yeetube-check-if-youtube "localhost") nil))
  (should (equal (yeetube-check-if-youtube "yewtu.be") nil)))

(ert-deftest test-download-audio-format ()
  (should (equal yeetube-download-audio-format nil))
  (yeetube-change-download-audio-format "m4a")
  (should (equal yeetube-download-audio-format "m4a")))


(ert-run-tests-batch-and-exit)

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
