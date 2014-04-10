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


(require 'test-helper)
(require 'gotest)


(defun go-test-command (&rest arg)
  (apply 's-concat "go test " arg))


;; FIXME
;; (ert-deftest test-go-test-get-current-file ()
;;   (should (string= "/tmp/foo/go_test.go"
;; 		   (go-test-get-current-file "/tmp/foo/go_test.go"))))

;; Arguments

(ert-deftest test-go-test-get-program-without-args ()
  (should (string= (go-test-command)
		   (go-test-get-program (go-test-arguments "")))))

(ert-deftest test-go-test-add-verbose-argument ()
  (let ((go-test-verbose-mode t))
    (should (string= (go-test-command " -v")
		     (go-test-get-program (go-test-arguments ""))))))


(provide 'gotest-test)
;;; gotest-test.el ends here
