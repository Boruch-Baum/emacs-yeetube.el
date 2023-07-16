;;; Code:
(load-file "../yeetube.el")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'ert)
(require 'yeetube)


(package-install 'package-lint)
(require 'package-lint)


(ert-deftest test-is-youtube? ()
  (should (equal (yeetube-check-if-youtube "youtube.com") t))
  (should (equal (yeetube-check-if-youtube "localhost") nil))
  (should (equal (yeetube-check-if-youtube "yewtu.be") nil)))

(ert-deftest test-download-audio-format ()
  (should (equal yeetube-download-audio-format nil))
  (yeetube-change-download-audio-format "m4a")
  (should (equal yeetube-download-audio-format "m4a")))

(ert-deftest test-package-lint ()
  (let ((package-lint-errors (package-lint-buffer (find-file-noselect "../yeetube.el"))))
    (should (equal package-lint-errors nil))))

(ert-run-tests-batch-and-exit)

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
