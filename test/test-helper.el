;; test-helper.el --- Test helpers for gotest.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@chmouel.com>
;; Homepage: https://github.com/nlamirault/gotest.el

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'f)
(require 'undercover)


(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

;; (let ((undercover-force-coverage t))
;;   (undercover "gotest.el"))

(defconst go-test-testsuite-dir (f-parent (f-this-file))
  "The testsuite directory.")

(defconst go-test-source-dir
  (f-parent go-test-testsuite-dir)
  "The gotest.el source directory.")

(message "Running tests on Emacs %s" emacs-version)

(message "Load gotest : %s" go-test-source-dir)
(load (s-concat go-test-source-dir "/gotest.elc"))


;; (defadvice undercover--report-on-kill (around self-report activate)
;;   (let ((undercover--files (list (file-truename "gotest.el"))))
;;     ad-do-it))


(provide 'test-helper)
;;; test-helper.el ends here
