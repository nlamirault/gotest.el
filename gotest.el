;;; gotest.el --- Launch GO unit tests

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/gotest.el
;; Version: 0.1.0
;; Keywords: go, tests

;; Package-Requires: ((s "1.9.0") (f "0.11.0") (go-mode))

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;; Thanks to tox.el(https://github.com/chmouel/tox.el) from Chmouel Boudjnah.

;;; Code:

(require 's)
(require 'f)
(require 'go-mode)


(defgroup gotest nil
  "GoTest utility"
  :group 'go)

(defcustom go-test-program go-command ;"go"
  "GO binary path."
  :type 'file
  :group 'go)

(defcustom go-test-args ""
  "Argument to pass to go."
  :type 'string
  :group 'go)

(defcustom go-test-verbose-mode nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'go)


;; Commands
;; -----------

(defun go-test-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to go command line arguments."
  (s-concat go-test-program " test "
            ;;(go-test-get-root-directory)
            args))

(defun go-test-get-root-directory()
  "Return the root directory to run tests."
  (let ((filename (buffer-file-name)))
    (when filename
      (file-truename (or (locate-dominating-file filename "Makefile")
                         "./")))))

(defun go-test-get-current-file (&optional file)
  "Return the filename of the go test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    (f-long (f-filename file))))


(defun go-test-get-current-test ()
  (let ((start (point))
	test-name)
    (save-excursion
      (end-of-line)
      (unless (and
               (search-backward-regexp "^[[:space:]]*func[[:space:]]*Test" nil t)
               (save-excursion (go-end-of-defun) (< start (point))))
        (error "Unable to find a test"))
      (save-excursion
        (search-forward "Test")
        (setq test-name (thing-at-point 'word))))
    test-name))


(defun go-test-arguments (args)
  (let ((opts args))
    (when go-test-verbose-mode
      (setq opts (s-concat opts " -v")))
    opts))


(defun go-test-run (args)
  (compile (go-test-get-program (go-test-arguments args))))


; API
;; ----


;;;###autoload
(defun go-test-current-test ()
  "Launch go test on curent test."
  (interactive)
  (let ((test-name (go-test-get-current-test)))
    (when test-name
      (let ((args (s-concat " -run " test-name)))
      (go-test-run args)))))


;;;###autoload
(defun go-test-current-file ()
  "Launch go test on file."
  (interactive)
  (let ((args (s-concat " -file=" (go-test-get-current-file))))
    (go-test-run args)))


;;;###autoload
(defun go-test-current-project ()
  "Launch go test on project."
  (interactive)
  (go-test-run ""))


(provide 'gotest)
;;; gotest.el ends here
