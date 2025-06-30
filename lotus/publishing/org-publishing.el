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


(defun org-publish-generate-tags-file (&optional dir output-file)
  "Generate a tags index page."
  (interactive
   (list (read-directory-name "Directory to generate Tags: ")))
  (let* ((dir (or dir
                  (org-publishing-created-contents-path)))
         (output-file (or output-file
                          (expand-file-name "tags.org" dir)))
         (files (directory-files-recursively dir "\\.org$"))
         (tags-table (make-hash-table :test 'equal)))
    ;; Collect tags
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((tags (if (re-search-forward "^#\\+TAGS: \\(.*\\)" nil t)
                        (split-string (match-string 1))
                      nil)))
          (when tags
            (dolist (tag tags)
              (puthash tag
                       (cons (list (file-name-base file) (file-name-nondirectory file))
                             (gethash tag tags-table))
                       tags-table))))))
    ;; Write tags.org
    (with-current-buffer (find-file-noselect output-file)
      (erase-buffer)
      (insert "#+TITLE: Tags\n#+OPTIONS: toc:nil\n\n")
      (maphash
       (lambda (tag files)
         (insert (format "* %s\n" tag))
         (dolist (file files)
           (let ((title (car file))
                 (fname (cadr file)))
             (insert (format "- [[file:%s][%s]]\n" fname title)))))
       tags-table)
      (save-buffer))))


(defun org-publish-generate-calendar (&optional dir output-file)
  "Generate a calendar archive org file from org files in your drafts."
  (interactive
   (list (read-directory-name "Directory to generate Calendar: ")))
  (let* ((dir (or dir
                  (org-publishing-created-contents-path)))
         (output-file (or output-file
                          (expand-file-name "calendar.org" dir)))
         (files (directory-files-recursively dir "\\.org$"))
         (entries '()))
    ;; Collect files with dates
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((date-str (if (re-search-forward "^#\\+DATE: \\(.*\\)" nil t)
                            (match-string 1)
                          (format-time-string "%Y-%m-%d"
                                              (nth 5 (file-attributes file))))))
          (when (string-match "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" date-str)
            (let ((year (match-string 1 date-str))
                  (month (match-string 2 date-str))
                  (title (file-name-base file)))
              (push (list year month title (file-name-nondirectory file)) entries))))))
    ;; Sort and group
    (setq entries (sort entries (lambda (a b)
                                  (string> (concat (car a) (cadr a) (caddr a))
                                           (concat (car b) (cadr b) (caddr b))))))
    ;; Write to calendar.org
    (with-current-buffer (find-file-noselect output-file)
      (erase-buffer)
      (insert "#+TITLE: Archives\n\n")
      (let ((current-year nil)
            (current-month nil))
        (dolist (entry entries)
          (let ((year (nth 0 entry))
                (month (nth 1 entry))
                (title (nth 2 entry))
                (file (nth 3 entry)))
            (unless (equal year current-year)
              (setq current-year year)
              (insert (format "* %s\n" year)))
            (unless (equal month current-month)
              (setq current-month month)
              (insert (format "** %s\n" (format-time-string "%B" (encode-time 0 0 0 1 (string-to-number month) (string-to-number year))))))
            (insert (format "- [[file:%s][%s]]\n" file title)))))
      (save-buffer))))


(provide 'org-publishing)

;;; org-publishing.el ends here
