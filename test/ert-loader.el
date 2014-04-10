;;; ert-loader.el --- Load Ert if not included in Emacs

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

;;; Code:


(require 'f)
(eval-when-compile
  (require 'cl))


(defvar go-test-root-path
  (f-parent (f-parent load-file-name))
  "Path to root.")

(defvar go-test-vendor-path
  (f-expand "vendor" go-test-root-path)
  "Path to vendor.")

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" go-test-vendor-path)))


(provide 'ert-loader.el)
;;; ert-loader.el ends here
