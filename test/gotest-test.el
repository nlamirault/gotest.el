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

;; (defconst testsuite-dir
;;   (if load-file-name
;;       (file-name-directory load-file-name)
;;     ;; Fall back to default directory (in case of M-x eval-buffer)
;;     default-directory)
;;   "Directory of the test suite.")

;; (defconst testsuite-buffer-name
;;   (concat testsuite-dir "go_test.go")
;;   "File name for testing.")

(defconst testsuite-buffer-name
  (f-join go-test-testsuite-dir "go_test.go")
  "File name for testing.")

;; (load (expand-file-name "../gotest" testsuite-dir) nil :no-message)
;; (load (expand-file-name "test-helper.el" testsuite-dir) nil :no-message)

(defun go-test-command (&rest arg)
  (apply 's-concat "go test " arg))

(defun go-run-command (&rest arg)
  (apply 's-concat "go run " arg))


;; Arguments

(ert-deftest test-go-test-get-program-without-args ()
  :tags '(arguments)
  (with-test-sandbox
   (should (string= (go-test-command)
                    (go-test--get-program (go-test--arguments ""))))))

(ert-deftest test-go-test-get-program-with-args ()
  :tags '(arguments)
  (with-test-sandbox
   (let ((go-test-args "-race"))
     (should (string= (go-test-command " -race")
                      (go-test--get-program (go-test--arguments "")))))))

(ert-deftest test-go-test-add-verbose-argument ()
  :tags '(arguments current)
  (with-test-sandbox
   (let ((go-test-verbose t))
     (should (string= (go-test-command " -v")
                      (go-test--get-program (go-test--arguments ""))))
     (let ((go-test-args "-race"))
       (should (string= (go-test-command " -v -race")
                        (go-test--get-program (go-test--arguments ""))))))))

(ert-deftest test-go-run-command-without-args ()
  :tags '(arguments)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (should (string= (go-run-command testsuite-buffer-name)
                      (go-test--go-run-get-program (go-test--go-run-arguments)))))))

(ert-deftest test-go-run-command-with-args ()
  :tags '(arguments)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (let ((go-run-args "-foo -bar=x"))
       (should (string= (go-run-command
                         (s-concat testsuite-buffer-name " -foo -bar=x"))
                        (go-test--go-run-get-program (go-test--go-run-arguments))))))))

(ert-deftest test-go-run-command-with-prefix ()
  :tags '(arguments)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (let ((current-prefix-arg '-)
           (go-run-args "-baz")
           (go-run-history (mapcar (lambda (arg)
                                     (s-concat buffer-file-name " " arg))
                                   '("-foo" "-bar" "-biz"))))
       (should (string= (go-run-command
                         (s-concat testsuite-buffer-name " -foo"))
                        (go-test--go-run-get-program (go-test--go-run-arguments))))))))

;; Find

(ert-deftest test-go-test-get-current-test ()
  :tags '(find)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (save-excursion
       (re-search-forward "logFoo")
       (should (string= "TestFoo" (go-test--get-current-test)))))))

(ert-deftest test-go-test-get-current-test-when-suite-test ()
  :tags '(find)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (save-excursion
       (re-search-forward "logSuite")
       (should (string= "TestSuite" (go-test--get-current-test)))))))

(ert-deftest test-go-test-get-current-file-tests ()
  :tags '(find)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect testsuite-buffer-name)
     (should (string= "TestFoo|TestBar|Test_Baz"
                      (go-test--get-current-file-tests))))))

;; when current buffer 'go.go', Test names should be found in 'go_test.go'
(ert-deftest test-go-test-get-current-file-tests-other ()
  :tags '(find)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect (f-join go-test-testsuite-dir "go.go"))
     (should (string= "TestFoo|TestBar|Test_Baz"
                      (go-test--get-current-file-tests))))))

;; 'no_test.go' does not exist, should-error
(ert-deftest test-go-test-get-current-file-tests-other-enoent ()
  :tags '(find)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect
                         (concat go-test-testsuite-dir "no.go"))
     (should-error (go-test--get-current-file-tests)))))

;; Error Regexp

;; (ert-deftest test-go-test-compilation-error-regexp-matches ()
;;   :tags '(regexp)
;;   (with-test-sandbox
;;    (let ((tests
;;           '((go-test-testing . ("	foo_test.go:146: some message"))
;;             (go-test-testify . ("	Location:	foo_test.go:66"))
;;             (go-test-gopanic . ("	/some/path/foo_test.go:266 +0xb6"))
;;             (go-test-compile . ("foo_test.go:51:50: expected operand, found '}'"))
;;             (go-test-linkage . ("./foo_test.go:174: undefined: foo.Symbol")))))
;;      (dolist (item tests)
;;        (let* ((type (car item))
;;               (regex (nth 1 (assq type
;;                                   go-test-compilation-error-regexp-alist-alist))))
;;          (dolist (msg (cdr item))
;;            (should (string-match regex msg))))))))

;; (ert-deftest test-go-test-compilation-error-regexp-matches-not ()
;;   :tags '(regexp)
;;   (with-test-sandbox
;;    (let ((tests
;;           '((go-test-testing . ("	foo_test.go some message"
;;                                 "./foo_test.go:174: undefined: foo.Symbol"))
;;             (go-test-testify . ("	Location:	foo_test.go"))
;;             (go-test-gopanic . ("	/usr/local/go/src/pkg/runtime/panic.c:266 +0xb6"))
;;             (go-test-compile . ("./foo_test.go:174: undefined: foo.Symbol"))
;;             (go-test-linkage . ("./foo_test.go:174: redefined: foo.Symbol")))))
;;      (dolist (item tests)
;;        (let* ((type (car item))
;;               (regex (nth 1 (assq type
;;                                   go-test-compilation-error-regexp-alist-alist))))
;;          (dolist (msg (cdr item))
;;            (should (not (string-match regex msg)))))))))

(provide 'gotest-test)
;;; gotest-test.el ends here
