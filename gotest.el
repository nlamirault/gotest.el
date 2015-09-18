;;; gotest.el --- Launch GO unit tests

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/gotest.el
;; Version: 0.4.0
;; Keywords: languages, go, tests

;; Package-Requires: ((emacs "24.3") (s "1.9.0") (f "0.17.2") (go-mode "1.0.0"))

;; Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

(defvar-local go-test-args nil
  "Arguments to pass to go test.
  This variable is buffer-local, set using .dir-locals.el for example.")

(defvar-local go-run-args nil
  "Arguments to pass to go run.
  This variable is buffer-local, set using .dir-locals.el for example.")

(defvar go-test-history nil
  "History list for go test command arguments.")

(defvar go-run-history nil
  "History list for go run command arguments.")

(defcustom go-test-verbose nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'gotest)

(defvar go-test-compilation-error-regexp-alist-alist
  '((go-test-testing . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): .*$" 1 2)) ;; stdlib package testing
    (go-test-testify . ("^\tLocation:\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\)$" 1 2)) ;; testify package assert
    (go-test-gopanic . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\) \\+0x\\(?:[0-9a-f]+\\)" 1 2)) ;; panic()
    (go-test-compile . ("^\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\):\\([0-9]+\\): .*$" 1 2 3)) ;; go compiler
    (go-test-linkage . ("^\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): undefined: .*$" 1 2))) ;; go linker
  "Alist of values for `go-test-compilation-error-regexp-alist'.
See also: `compilation-error-regexp-alist-alist'.")

(defcustom go-test-compilation-error-regexp-alist
  '(go-test-testing
    go-test-testify
    go-test-gopanic
    go-test-compile
    go-test-linkage)
  "Alist that specifies how to match errors in go test output.
The default set of regexps should only match the output of the
standard `go' tool, which includes compile, link, stacktrace (panic)
and package testing.  There is support for matching error output
from other packages, such as `testify'.

Only file names ending in `.go' will be matched by default.

Instead of an alist element, you can use a symbol, which is
looked up in `go-testcompilation-error-regexp-alist-alist'.

See also: `compilation-error-regexp-alist'.
"
  :type '(repeat (choice (symbol :tag "Predefined symbol")
			 (sexp :tag "Error specification")))
  :group 'gotest)


;; Commands
;; -----------

(defun go-test-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to go command line arguments."
  (s-concat go-command " test "
            ;;(go-test-get-root-directory)
            args))


(defun go-test-get-arguments (defaults history)
  "Get optional arguments for go test or go run.
DEFAULTS will be used when there is no prefix argument.
When a prefix argument of '- is given, use the most recent HISTORY item.
When any other prefix argument is given, prompt for arguments using HISTORY."
  (if current-prefix-arg
      (if (equal current-prefix-arg '-)
          (car (symbol-value history))
        (let* ((name (nth 1 (s-split "-" (symbol-name history))))
               (prompt (s-concat "go " name " args: ")))
          (read-shell-command prompt defaults history)))
    defaults))


(defun go-test-get-root-directory()
  "Return the root directory to run tests."
  (let ((filename (buffer-file-name)))
    (when filename
      (file-truename (or (locate-dominating-file filename "Makefile")
                         "./")))))


(defun go-test-get-current-buffer ()
  "Return the test buffer for the current `buffer-file-name'.
If `buffer-file-name' ends with `_test.go', `current-buffer' is returned.
Otherwise, `ff-other-file-name' is used to find the test buffer.
For example, if the current buffer is `foo.go', the buffer for
`foo_test.go' is returned."
  (if (string-match "_test\.go$" buffer-file-name)
      (current-buffer)
    (let ((ff-always-try-to-create nil))
      (let ((filename (ff-other-file-name)))
        (message "File :%s" filename)
        (find-file-noselect filename)))))
;      (find-file-noselect (ff-other-file-name)))))


(defun go-test-get-current-test ()
  (let ((start (point))
        test-prefix
        test-name)
    (save-excursion
      (end-of-line)
      (setq test-prefix "Test")
      (unless (and
               (or
                (search-backward-regexp "^[[:space:]]*func[[:space:]]*Test" nil t)
                (and
                 (setq test-prefix "Example")
                 (search-backward-regexp "^[[:space:]]*func[[:space:]]*Example" nil t)))
               (save-excursion (go-end-of-defun) (< start (point))))
        (error "Unable to find a test"))
      (save-excursion
        (search-forward test-prefix)
        (setq test-name (thing-at-point 'word))))
    test-name))


(defun go-test-get-current-file-tests ()
  "Generate regexp to match tests in the current buffer."
  (with-current-buffer (go-test-get-current-buffer)
    (save-excursion
      (goto-char (point-min))
      (if (string-match "\.go$" buffer-file-name)
          (let (tests)
            (while (or
                    (re-search-forward
                     "^[[:space:]]*func[[:space:]]*\\(Test[^(]+\\)" nil t)
                    (re-search-forward
                     "^[[:space:]]*func[[:space:]]*\\(Example[^(]+\\)" nil t))
              (let ((test (buffer-substring-no-properties
                           (match-beginning 1) (match-end 1))))
                (setq tests (append tests (list test)))))
            (mapconcat 'identity tests "|"))))))


(defun go-test-arguments (args)
  (let ((opts args))
    (when go-test-verbose
      (setq opts (s-concat opts " -v")))
    (when go-test-args
      (setq opts (s-concat opts " " go-test-args)))
    (go-test-get-arguments opts 'go-test-history)))


(defun go-test-compilation-hook (p)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       go-test-compilation-error-regexp-alist-alist)
  (set (make-local-variable 'compilation-error-regexp-alist)
       go-test-compilation-error-regexp-alist))


(defun go-test-run (args)
  (add-hook 'compilation-start-hook 'go-test-compilation-hook)
  (compile (go-test-get-program (go-test-arguments args)))
  (remove-hook 'compilation-start-hook 'go-test-compilation-hook))


(defun go-run-get-program (args)
  "Return the command to launch go run.
`ARGS' corresponds to go command line arguments."
  (s-concat go-command " run " args))


(defun go-run-arguments ()
  "Arguments for go run."
  (let ((opts (if go-run-args
                  (s-concat (shell-quote-argument (buffer-file-name)) " " go-run-args)
                (shell-quote-argument (buffer-file-name)))))
    (go-test-get-arguments opts 'go-run-history)))


; API
;; ----


;;;###autoload
(defun go-test-current-test ()
  "Launch go test on the current test."
  (interactive)
  (let ((test-name (go-test-get-current-test)))
    (when test-name
      (let ((args (s-concat "-run " test-name)))
      (go-test-run args)))))


;;;###autoload
(defun go-test-current-file ()
  "Launch go test on the current buffer file."
  (interactive)
  (let ((args (s-concat "-run='" (go-test-get-current-file-tests) "'")))
    (go-test-run args)))


;;;###autoload
(defun go-test-current-project ()
  "Launch go test on the current project."
  (interactive)
  (go-test-run "./..."))


;;;###autoload
(defun go-test-current-coverage ()
  "Launch go test coverage on the current project."
  (interactive)
  (let ((args (s-concat
               "--coverprofile="
               (expand-file-name (read-file-name "Coverage file" nil "cover.out")) " ./...")))
    (go-test-run args)))


;;;###autoload
(defun go-run ()
  "Launch go run on current buffer file."
  (interactive)
  (add-hook 'compilation-start-hook 'go-test-compilation-hook)
  (compile (go-run-get-program (go-run-arguments)))
  (remove-hook 'compilation-start-hook 'go-test-compilation-hook))

(provide 'gotest)
;;; gotest.el ends here
