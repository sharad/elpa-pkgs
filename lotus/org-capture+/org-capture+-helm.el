;;; org-capture+-helm.el --- org capture+ helm       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad
;; Keywords: convenience

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

;;

;;; Code:

(provide 'org-capture+-helm)


;; (require 'cl)
;; (eval-when-compile
;;   (require 'cl))
(require 'dash)
(require 'helm)


(defvar org-capture+-helm-templates-alist nil)

(defun org-capture+-helm-template-add (scope heading template)
  (unless (assoc heading org-capture+-helm-templates-alist)
    (cl-pushnew (list heading) org-capture+-helm-templates-alist))
  (cl-pushnew template
           (cl-rest (assoc heading org-capture+-helm-templates-alist))))

(org-capture+-helm-template-add 'test "TODO"    "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "TODO"    "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


;;;###autoload
(defun org-capture+-build-helm-template-source (name attrib-list &rest templates)
  `((name . ,name)
    (multiline)
    (candidates ,@templates)
    ,@attrib-list))

;;;###autoload
(defun org-capture+-build-helm-template-sources (attrib-list alist)
  (mapcar
   #'(lambda (e)
       (apply #'org-capture+-build-helm-template-source
              (cl-first e)
              attrib-list
              (cl-rest e)))
   alist))

;;;###autoload
(defun org-capture+-helm-select-template (&optional attrib-list alist)
  (let ((attrib-list (or attrib-list '((action . identity))))
        (alist       (or alist       org-capture+-helm-templates-alist)))
    (helm :sources
          (org-capture+-build-helm-template-sources attrib-list alist))))

;; (org-capture+-helm-select-template)

;;; org-capture+-helm.el ends here
