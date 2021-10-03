;;; occ-print.el --- occ print                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'occ-print)


(require 'lotus-utils)
(require 's)
(eval-when-compile
  (require 'org-macs))


(cl-defmethod occ-obj-uniquify-file ((tsk occ-tsk))
  (let* ((filename (occ-obj-get-property tsk 'file))
         (basename (file-name-nondirectory filename))
         (files (occ-obj-files)))))
    ;; (uniquify-buffer-file-name)

;; (file-name-nondirectory "/aaa/aaa/aaa")


(defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")
;; (defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")

(cl-defgeneric occ-obj-fontify-like-in-org-mode (obj)
  "occ-obj-fontify-like-in-org-mode")

(cl-defmethod occ-obj-fontify-like-in-org-mode ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (when (occ-obj-buffer obj)
    (with-current-buffer (org-base-buffer (occ-obj-buffer obj))
      (org-with-wide-buffer
       (progn
         (goto-char obj)
         (let* ((cat         (org-get-category))
                (heading     (org-get-heading 'notags))
                (prefix      (save-excursion
                               (org-back-to-heading t)
                               (looking-at org-outline-regexp)
                               (match-string 0)))
                (org-heading (substring
                              (org-fontify-like-in-org-mode
                               (concat prefix heading)
                               org-odd-levels-only)
                              (length prefix))))

           org-heading))))))

(cl-defmethod occ-obj-fontify-like-in-org-mode ((obj occ-tsk))
  (let* ((level               (or (occ-obj-get-property obj 'level) 0))
         (subtree-level       (or (occ-obj-get-property obj 'subtree-level) 1))
         (filename            (occ-obj-format-file obj))
         ;; (filename-prefix     (concat " " (make-string (+ level subtree-level) occ-fontify-like-org-file-bullet) " "))
         (heading-prop        (occ-obj-get-property obj 'heading-prop))
         (heading             (if (eq heading-prop 'noheading)
                                  (concat (make-string 1 occ-fontify-like-org-file-bullet) " " filename)
                                heading-prop))
         (heading-prefix      " ")
         (prefix              (concat (make-string (+ subtree-level level) ?\*) " "))
         (org-heading-noprop  (concat prefix heading))
         (org-heading         (org-fontify-like-in-org-mode org-heading-noprop org-odd-levels-only))
         (display-org-heading (concat heading-prefix org-heading)))
    ;; (occ-debug "fontify: %s subtree-level=%s" heading subtree-level)
    display-org-heading))

(cl-defmethod occ-obj-build-format-string ((obj occ-tsk))
  (occ-obj-fontify-like-in-org-mode obj))


(cl-defmethod occ-obj-build-format-file ((obj occ-tsk))
  (let ((filename (occ-obj-get-property obj 'file))
        (lcp      (apply #'s-lcp (occ-obj-files))))
    (s-chop-prefix lcp filename)))


(defun occ-case (case title)
  (if (fboundp case)
      (funcall case title)
    title))

(cl-defgeneric occ-obj-title (obj
                              case)
  "occ-obj-title")

(cl-defmethod occ-obj-title (obj
                             case)
  (occ-case case
            (occ-obj-class-name obj)))

(cl-defmethod occ-obj-title ((obj  marker)
                             (case symbol))
  (occ-case case
            (occ-obj-class-name obj)))

(cl-defmethod occ-obj-title ((obj  occ-obj)
                             (case symbol))
  (occ-case case
            (occ-obj-class-name obj)))

(defun occ-obj-Title (obj)
  (occ-case 'capitalize
            (occ-obj-class-name obj)))


(defun occ-obj-TITLE (obj)
  (occ-case 'upcase
            (occ-obj-class-name obj)))


(fmakunbound 'occ-obj-format)

(cl-defgeneric occ-obj-format (obj
                               &optional
                               case
                               rank
                               no-curr-clock)
  "occ-obj-format")

(cl-defmethod occ-obj-format (obj
                              &optional
                              case
                              rank
                              no-curr-clock)
  (concat (when case
            (concat (occ-obj-title obj
                               case)
                    ": "))
          (format "%s" obj)))

(cl-defmethod occ-obj-format ((obj marker)
                              &optional
                              case
                              rank
                              no-curr-clock)
  (concat (when case (concat (occ-obj-title obj case) ": "))
          (occ-obj-fontify-like-in-org-mode obj)))

(defvar occ-obj-format-tsk-tag-alignment 100 "occ-obj-format-tsk-tag-alignment")

(cl-defmethod occ-obj-format ((obj occ-tsk)
                              &optional
                              case
                              rank
                              no-curr-clock)
  (let* ((align      occ-obj-format-tsk-tag-alignment)
         (heading    (occ-obj-format-string obj))
         (headinglen (length heading))
         (tags       (occ-obj-get-property obj 'tags))
         (tagstr     (if tags
                         (concat ":" (mapconcat #'identity tags ":") ":")
                       "")))
    (concat (when case (concat (occ-obj-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-tsk-rank obj) -128)))
            (format (format (if tags "%%-%ds         %%s" "%%s")
                            align
                            (if (< headinglen align) (- align headinglen) 0))
                    heading tagstr))))

(cl-defmethod occ-obj-format ((obj occ-ctx)
                              &optional
                              case
                              rank
                              no-curr-clock)
  (format "%s" obj))

(cl-defmethod occ-obj-format ((obj occ-obj-ctx-tsk)
                              &optional
                              case
                              rank
                              no-curr-clock)
  (let ((tsk (occ-ctsk-tsk obj)))
    (concat (when case (concat (occ-obj-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-obj-rank obj) -128)))
            (occ-obj-format tsk case nil)
            (unless no-curr-clock
              (when (occ-obj-current-p obj) "          🕑")))))

(cl-defmethod occ-obj-format ((obj occ-ctxual-tsk) &optional
                                                   case
                                                   rank
                                                   no-curr-clock)
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    (concat (when case (concat (occ-obj-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-obj-rank obj) -128)))
            (format "%s" (occ-obj-format tsk case rank))
            (unless no-curr-clock
              (when (occ-obj-current-p obj) "          🕑")))))


(defun occ-obj-Format (obj &optional
                           rank
                           no-curr-clock)
  (occ-obj-format obj 'capitalize
                  rank
                  no-curr-clock))

(defun occ-obj-FORMAT (obj &optional
                           rank
                           no-curr-clock)
  (occ-obj-format obj 'upcase
              rank
              no-curr-clock))


(cl-defgeneric occ-obj-display (obj)
  "Neatly Output OBJ with its properties")

(cl-defmethod occ-obj-display ((obj occ-obj-tsk))
  (occ-error "Implement it: neatly output OBJ with its properties"))

;;; occ-print.el ends here
