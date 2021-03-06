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


(cl-defmethod occ-uniquify-file ((tsk occ-tsk))
  (let* ((filename (occ-get-property tsk 'file))
         (basename (file-name-nondirectory filename))
         (files (occ-files)))))
    ;; (uniquify-buffer-file-name)

;; (file-name-nondirectory "/aaa/aaa/aaa")


(defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")
;; (defvar occ-fontify-like-org-file-bullet ?\▆ "occ-fontify-like-org-file-bullet")

(cl-defgeneric occ-fontify-like-in-org-mode (obj)
  "occ-fontify-like-in-org-mode")

(cl-defmethod occ-fontify-like-in-org-mode ((obj marker))
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

(cl-defmethod occ-fontify-like-in-org-mode ((obj occ-tsk))
  (let* ((level               (or (occ-get-property obj 'level) 0))
         (subtree-level       (or (occ-get-property obj 'subtree-level) 1))
         (filename            (occ-format-file obj))
         ;; (filename-prefix     (concat " " (make-string (+ level subtree-level) occ-fontify-like-org-file-bullet) " "))
         (heading-prop        (occ-get-property obj 'heading-prop))
         (heading             (if (eq heading-prop 'noheading)
                                  (concat (make-string 1 occ-fontify-like-org-file-bullet) " " filename)
                                heading-prop))
         (heading-prefix      " ")
         (prefix              (concat (make-string (+ subtree-level level) ?\*) " "))
         (org-heading-noprop  (concat prefix heading))
         (org-heading         (org-fontify-like-in-org-mode org-heading-noprop org-odd-levels-only))
         (display-org-heading (concat heading-prefix org-heading)))
    ;; (occ-debug :debug "fontify: %s subtree-level=%s" heading subtree-level)
    display-org-heading))

(cl-defmethod occ-build-format-string ((obj occ-tsk))
  (occ-fontify-like-in-org-mode obj))


(cl-defmethod occ-build-format-file ((obj occ-tsk))
  (let ((filename (occ-get-property obj 'file))
        (lcp      (apply #'s-lcp (occ-files))))
    (s-chop-prefix lcp filename)))


(cl-defgeneric occ-format (obj
                           &optional case rank)
  "occ-format")

(cl-defmethod occ-format (obj
                          &optional case rank)
  (concat (when case (concat (occ-title obj case) ": "))
          (format "%s" obj)))

(cl-defmethod occ-format ((obj marker)
                          &optional case rank)
  (concat (when case (concat (occ-title obj case) ": "))
          (occ-fontify-like-in-org-mode obj)))

(defvar occ-format-tsk-tag-alignment 100 "occ-format-tsk-tag-alignment")

(cl-defmethod occ-format ((obj occ-tsk)
                          &optional case rank)
  (let* ((align      occ-format-tsk-tag-alignment)
         (heading    (occ-format-string obj))
         (headinglen (length heading))
         (tags       (occ-get-property obj 'tags))
         (tagstr     (if tags
                         (concat ":" (mapconcat #'identity tags ":") ":")
                       "")))
    (concat (when case (concat (occ-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-tsk-rank obj) -128)))
            (format
             (format (if tags "%%-%ds         %%s" "%%s")
                     align
                     (if (< headinglen align) (- align headinglen) 0))
             heading tagstr))))

(cl-defmethod occ-format ((obj occ-ctx)
                          &optional case)
  (format "%s" obj))

(cl-defmethod occ-format ((obj occ-obj-ctx-tsk)
                          &optional case rank)
  (let ((tsk (occ-ctsk-tsk obj)))
    (concat (when case (concat (occ-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-rank obj) -128)))
            (occ-format tsk case nil))))

(cl-defmethod occ-format ((obj occ-ctxual-tsk)
                          &optional case rank)
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    (concat (when case (concat (occ-title obj case) ": "))
            (when rank (format "[%4d] " (or (occ-rank obj) -128)))
            (format "%s" (occ-format tsk case rank)))))


;; (cl-defmethod occ-print-rank ((obj occ-tsk))
;;   (occ-message "Rank for %s is %d"
;;                (occ-format obj 'capitalize)
;;                (occ-rank obj)))

;; (cl-defmethod occ-print-rank ((obj occ-obj-ctx-tsk))
;;   (occ-message "Rank for %s is %d"
;;                (occ-format obj 'capitalize)
;;                (occ-rank obj)))

(cl-defmethod occ-print-rank ((obj occ-obj-tsk))
  (occ-message "Rank for %s is %d"
               (occ-format obj 'capitalize)
               (occ-rank obj)))

;;; occ-print.el ends here
