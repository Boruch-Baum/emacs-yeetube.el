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
  (should (equal (yeetube-youtube-p "youtube.com") t))
  (should (equal (yeetube-youtube-p "localhost") nil))
  (should (equal (yeetube-youtube-p "yewtu.be") nil)))

(ert-deftest test-package-lint ()
  (let ((package-lint-errors (package-lint-buffer (find-file-noselect "../yeetube.el"))))
    (should (equal package-lint-errors nil))))

(ert-run-tests-batch-and-exit)

(provide 'yeetube-tests)
;;; yeetube-tests.el ends here
