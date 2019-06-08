;;; deploy-ghpages.el --- Export README.org to index.html -*- lexical-binding: t -*-

;; Copyright (C) 2019  Mahito TANNO

;; This program is free software: you can redistribute it and/or modify
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
;; This is a minimal configuration in order to export `README.org' to
;; a HTML file via Travis CI.  See also `deploy-ghpages.el'.

;;; Code:
;;; Install packages.
;; Add melpa.org and orgmode.org to `package-archives'.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

;; Initialize `package.el'.
(package-initialize)
(package-refresh-contents)

;; Declare packages should be installed.
(defvar my--favorite-package-list
  '(
    org
    ;; org-plus-contrib                    ; if needed
    htmlize
    )
  "List of packages should be installed.")

;; Installed packages listed in `my--favorite-package-list'.
(dolist (package-name my--favorite-package-list)
  (unless (package-installed-p package-name)
    (package-install package-name)))

;;; Setup for export HTML
;; Separat CSS(eval-after-load "org"
(with-eval-after-load "ox-html"
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>")
  (setq org-html-htmlize-output-type
        'css))

(provide 'deploy-ghpages)
;;; deploy-ghpages.el ends here
