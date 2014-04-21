;;; gotest-test.el --- Tests for gotest.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(defconst testsuite-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(defconst testsuite-buffer-name
  (concat testsuite-dir "go_test.go")
  "File name for testing.")


(load (expand-file-name "../gotest" testsuite-dir) nil :no-message)
(load (expand-file-name "test-helper.el" testsuite-dir) nil :no-message)


(defun go-test-command (&rest arg)
  (apply 's-concat "go test " arg))

;; Files

(ert-deftest test-go-test-get-current-file ()
  (with-current-buffer (find-file-noselect testsuite-buffer-name)
    (should (string= testsuite-buffer-name
                     (go-test-get-current-file "go_test.go")))))

;; Arguments

(ert-deftest test-go-test-get-program-without-args ()
  (should (string= (go-test-command)
		   (go-test-get-program (go-test-arguments "")))))

(ert-deftest test-go-test-add-verbose-argument ()
  (let ((go-test-verbose t))
    (should (string= (go-test-command " -v")
		     (go-test-get-program (go-test-arguments ""))))))

;; Find

(ert-deftest test-go-test-get-current-test ()
  (with-current-buffer (find-file-noselect testsuite-buffer-name)
    (save-excursion
      (re-search-forward "logFoo")
      (should (string= "TestFoo" (go-test-get-current-test))))))

(provide 'gotest-test)
;;; gotest-test.el ends here
