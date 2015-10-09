;;; gotest-version-test.el --- Tests for version information

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

(ert-deftest phpunit-mode-library-version ()
  :expected-result (if (executable-find "cask") :passed :failed)
  (let* ((cask-version (car (process-lines "cask" "version"))))
    (message "gotest.el Cask version: %s" cask-version)
    (should (string= "0.5.0" cask-version))))


(provide 'gotest-version-test)
;;; gotest-version-test.el ends here
