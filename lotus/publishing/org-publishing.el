;;; org-publishing.el --- org publishing

;; Copyright (C) 2015  sharad

;; Author: s <>
;; Keywords:convenience, data, hypermedia, wp

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

;; Publishing related configuration.

;;; Code:

(provide 'org-publishing)


(require 'cl)
(require 'publishing)
(require 'org)
(when (featurep 'org-compact)
 (require 'org-compact))
(require 'ox-publish)


;;;###autoload
(defun org-publishing-created-contents-path (&optional path)
  "thisandthat."
  (publishing-created-contents-path 'org path))

;;;###autoload
(defun org-publishing-generated-contents-path (&optional path)
  "thisandthat."
  (publishing-generated-contents-path 'org path))

;;;###autoload
(defun org-publishing-website-address (&optional localpath)
  (publishing-website-address 'org localpath))


;;;###autoload
(defun* add-org-project (&rest project-spec)
  "Add org project."
  (interactive (let ((project-spec
                      (read-org-project-spec)))))
  (when (and (member (cl-first project-spec)
                     (mapcar #'car org-publish-project-alist))
             (or (not (called-interactively-p 'interactive))
                 (y-or-n-p (format "project %s already present, do you want to overwrite it?: " (cl-first project-spec))))
             (remove-org-project project-spec)))
  (add-to-list 'org-publish-project-alist project-spec t))

;;;###autoload
(defun remove-org-project (&rest project-spec)
  (interactive (let ((project (ido-completing-read "Project: "
                                                   (mapcar 'car org-publish-project-alist))))
                 (if project
                     (list project)
                   (error "No project %s present" project))))
  (let ((project (cond ((and (consp project-spec)
                             (stringp (cl-first project-spec)))
                        (cl-first project-spec))
                       ((stringp project-spec) project-spec)
                       (t nil))))
    (if project
        (setq org-publish-project-alist
              (delete* project org-publish-project-alist
                       :key 'car
                       :test 'string-equal)))))


;; (defun* make-org-style-spec (org-dir publishing-path publishing-style publishing-url publishing-options)
;;   (interactive
;;    (let* ((org-dir (read-directory-name "Org Project Directory: " (org-publishing-created-contents-path)))
;;           (publishing-path
;;            (read-directory-name
;;             "Org Project Directory: "
;;             (org-publishing-generated-contents-path (replace-regexp-in-string (org-publishing-created-contents-path) "" org-dir))
;;           (publishing-style
;;            (ido-completing-read "Org Publishing Style: " (mapcar 'car org-publishing-styles)))
;;           (publishing-url (read-from-minibuffer "Publishing Base URL: "))
;;           (publishing-options nil))
;;      (list org-dir publishing-path publishing-style publishing-url publishing-options)))
;;   (apply
;;    'org-publish-project-alist-styles
;;    (append
;;     (list
;;      org-dir
;;      publishing-path)
;;     (list publishing-style)
;;     (if publishing-url
;;         (list :base-url publishing-url))
;;     publishing-options)))

;;;###autoload
(defun* make-org-project-spec (name
                               &rest
                               publishing-options)
  (interactive (let* ((name    (read-from-minibuffer "Org Project Name: "))
                      (org-dir (read-directory-name "Org Project Directory: "
                                                    (org-publishing-created-contents-path name)))
                      (publishing-path (read-directory-name "Org Project Publishing Directory: "
                                                            (org-publishing-generated-contents-path
                                                             (replace-regexp-in-string (org-publishing-created-contents-path ) ""
                                                                                       (if (consp org-dir) (cl-first org-dir) org-dir)))))
                      (publishing-options nil))
                 (list name
                       :base-directory org-dir
                       :publishing-directory publishing-path
                       publishing-options)))
  (cons name publishing-options))

;; (defun* read-org-style-spec ()
;;   (let* ((org-dir (read-directory-name "Org Project Directory: " (org-publishing-created-contents-path )))
;;          (publishing-path
;;           (read-directory-name
;;            "Org Project Directory: "
;;            (org-publishing-generated-contents-path  (replace-regexp-in-string (org-publishing-created-contents-path ) "" org-dir))))
;;          (publishing-style
;;           (ido-completing-read "Org Publishing Style: " (mapcar 'car org-publishing-styles)))
;;          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
;;          (publishing-options nil))
;;     (list org-dir publishing-path publishing-style publishing-url publishing-options)))

;;;###autoload
(defun* read-org-project-spec ()
  (let* ((name    (read-from-minibuffer "Org Project Name: "))
         (org-dir (read-directory-name "Org Project Directory: "
                                       (org-publishing-created-contents-path name)))
         (publishing-path (read-directory-name "Org Project Publishing Directory: "
                                               (org-publishing-generated-contents-path (replace-regexp-in-string (org-publishing-created-contents-path)
                                                                                                                 ""
                                                                                                                 (if (consp org-dir) (cl-first org-dir) org-dir)))))
         (publishing-options nil))
    `(,name
      :base-directory ,org-dir
      :publishing-directory ,publishing-path
      ,@publishing-options)))


;; (mapcar 'car org-publishing-styles)
;; (org-publish-project-alist-styles
;;  (org-publishing-created-contents-path "doc/priv")
;;  (org-publishing-generated-contents-path "/doc/pdf/doc/priv/pdf")
;;  "pdf")

;; (org-publish-project-alist-styles
;;  (org-publishing-created-contents-path "web/site/blog")
;;  (org-publishing-generated-contents-path "/web/site/blog/pdf")
;;  "ikiwiki"
;;  :base-url (org-publishing-website-address "/blog/"))


;;;###autoload
(defun org-publish-get-attribute (project extention attrib)
  (let ((proj-alist (assoc project org-publish-project-alist)))
    (or (plist-get  (cl-rest proj-alist) attrib)
        (plist-get  (cl-rest (cl-first (remove-if-not #'(lambda (p)
                                                          (string-match (plist-get (cl-rest p) :base-extension)
                                                                        extention))
                                                      (org-publish-expand-projects (list proj-alist)))))
                    attrib))))

;;;###autoload
(defun org-publish-get-attribute (project extention attrib)
  ;; TODO: IMPROVE further
  (let ((proj-alist (assoc project org-publish-project-alist)))
    (or (plist-get (cl-rest proj-alist) attrib)
        (let* ((projects (mapcar #'car
                                 (remove-if-not (lambda (p)
                                                  (string-match
                                                   (plist-get (cl-rest p) :base-extension)
                                                   extention))
                                                (org-publish-expand-projects
                                                 (list proj-alist)))))
               (project (find-if #'(lambda (p) (org-publish-get-attribute p
                                                                          extention
                                                                          attrib))
                                 projects)))
          (when project
            (org-publish-get-attribute project
                                       extention
                                       attrib))))))

;; (org-publish-get-attribute "tasks" "org" :base-directory)

;;; org-publishing.el ends here
