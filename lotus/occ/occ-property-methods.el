;;; occ-property-methods.el --- org capture plus         -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author:
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

;;; Code:

;; * Provide this file

;; [[file:occ-property-methods.org::*Provide this file][Provide this file:1]]
(provide 'occ-property-methods)

;; Provide this file:1 ends here

;; Required libraries

;; [[file:occ-property-methods.org::*Required libraries][Required libraries:1]]
(require 'org)

(require 'occ-impl-utils)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-obj-accessor)
(require 'occ-debug-method)
(require 'occ-prop-intf)
(require 'occ-rank)
(require 'occ-intf)

;; * Required libraries:1 ends here
;;
;;
;; 
;; Current File property of task
;;
;; [[file:occ-property-methods.org::*Current File property of task][Current File property of task:1]]
;;{{ currfile
(cl-defmethod occ-obj-impl-rank ((obj occ-obj-ctx-tsk)
                                 (prop (eql currfile))) ;; do not use (prop (eql file)) that is another property which represent file in which task defined.
  ;; file in which tsk aka org entry exists.
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-aggregate-rank tsk-currfile 'currfile tsk #'+
      (if (occ-pu-files-same-p tsk-currfile (occ-ctx-file ctx))
          (occ-rank-percentage 100)     ;Obsolete: as exact match to files giving double matching points.
        (occ-rank-percentage 0)))))
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql currfile))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (occ-debug "calling occ-obj-impl-get(ctx occ-ctx)")
  (let ((currfile (occ-ctx-file ctx)))
    ;; (occ-message "currfile %s" currfile)
    currfile))

(cl-defmethod occ-obj-impl-list-p ((prop (eql currfile)))
  (ignore prop)
  t)

(cl-defmethod occ-obj-impl-to-org ((prop (eql currfile))
                                   value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-impl-from-org ((prop (eql currfile))
                                     value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql currfile))
                                (obj occ-obj-ctx-tsk))
  "currfile property for tsk aka org entry"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
           (ctx-dir      (when (stringp ctx-currfile)
                           (file-name-directory ctx-currfile)))
           (prompt       (concat (symbol-name prop) ": ")))
      (ido-read-file-name prompt ctx-dir ctx-currfile))))

(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql currfile)))
  (let* ((tsk        (occ-obj-tsk      obj))
         (files      (occ-obj-get-property tsk prop))
         (first-file (cl-first files)))
       (if first-file
           (find-file first-file)
         (occ-debug "occ-do-impl-checkout: %s value ruturned for prop %s" first-file prop))))
      ;;}}

;; Current File property of task:1 ends here

;; Root dir property of task

;; [[file:occ-property-methods.org::*Root dir property of task][Root dir property of task:1]]
;;{{ root
(cl-defmethod occ-obj-impl-rank ((obj occ-obj-ctx-tsk)
                                 (prop (eql root)))
  "RANK Predicate funtion to check if ctx matches to tsk's ROOT attribute."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-nodisplay "rankprop(%s, %s): begin"
               (occ-obj-format tsk 'capitalize)
               (occ-obj-format ctx 'capitalize)
               prop)
    (occ-aggregate-rank tsk-root 'root tsk #'max
      (if (occ-pu-file-in-dir-p tsk-root (occ-ctx-file ctx))
          (occ-rank-percentage 100)
        (occ-rank-percentage 0)))))
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql root))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (let ((file (occ-ctx-file ctx)))
      (when file (dirname-of-file file))))

(cl-defmethod occ-obj-impl-list-p ((prop (eql root)))
  (ignore prop)
  t)

(cl-defmethod occ-obj-impl-to-org ((prop (eql root))
                                   value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-impl-from-org ((prop (eql root))
                                     value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql root))
                                (obj occ-obj-ctx-tsk))
  "READ"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
           (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
           (prompt     (concat (symbol-name prop) ": ")))
      (ido-read-directory-name prompt ctx-dir ctx-dir))))

(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql root)))
  (let* ((tsk       (occ-obj-tsk      obj))
         (dirs      (occ-obj-get-property tsk prop))
         (first-dir (cl-first dirs)))
    (if first-dir
        (find-file first-dir)
      (occ-debug "occ-do-impl-checkout: %s value ruturned for prop %s" first-dir prop))))
      ;;}}

;; Root dir property of task:1 ends here

;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time

;; [[file:occ-property-methods.org::*Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time][Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1]]
(cl-defmethod occ-obj-impl-rank ((obj occ-tsk)
                                 (prop (eql timebeing)))
  (ignore prop)
  (let ((tsk (occ-obj-tsk obj)))
      (let ((timebeing (occ-obj-get-property tsk
                                         'timebeing)))
        (let ((timebeing-time (if timebeing
                                  (org-duration-to-minutes timebeing)
                                (occ-rank-percentage 0)))
              (clocked-time   (occ-obj-get-property tsk
                                                'clock-sum)))
          (if (and (numberp clocked-time)
                   (numberp timebeing-time)
                   (> timebeing-time clocked-time))
              (/ (* (occ-rank-percentage 100)
                  (- timebeing-time
                   clocked-time))
                 timebeing-time)
            (occ-rank-percentage 0))))))

(cl-defmethod occ-obj-impl-list-p ((prop (eql timebeing)))
  (ignore prop)
  nil)

(cl-defmethod occ-obj-impl-to-org ((prop (eql timebeing))
                                   value)
  (ignore prop)
  (if (numberp value)
      (number-to-string value)
    ""))

(cl-defmethod occ-obj-impl-from-org ((prop (eql timebeing))
                                     value)
  (ignore prop)
  (if (stringp value)
      (or (string-to-number value)
          0)
    0))

(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql timebeing))
                                (obj occ-tsk))
  "READ"
  (let ((tsk (occ-obj-tsk obj)))
    (ignore tsk)
    (let* ((prompt     (concat (symbol-name prop)
                               ": ")))
      (ignore prompt)
      (read-number "Timebeing mins: "))))

(cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                      (operation (eql increment))
                                      (prop (eql timebeing))
                                      values)
  (ignore operation)
  (ignore prop)
  (ignore values)
  (occ-obj-current-p obj))

(cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                    (prop (eql timebeing))
                                    (operation (eql increment)))
  (ignore prop)
  (ignore operation)
  (when (occ-obj-current-p obj)
    10))

(cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
                                     (operation (eql increment))
                                     (prop (eql timebeing))
                                     values)
  (ignore operation)
  (ignore values)
  (let ((tsk    (occ-obj-tsk obj)))
    (ignore tsk)
    (if (occ-obj-intf-list-p prop)
        (occ-error "Implement it.")
      (occ-error "Implement it."))))

(cl-defmethod occ-do-org-operation ((obj occ-obj-tsk)
                                    (operation (eql increment))
                                    (prop (eql timebeing))
                                    values)
  (ignore obj)
  (ignore operation)
  (ignore values)
  (let ((prop-string (symbol-name prop)))
    (ignore prop-string)
    (if (occ-obj-intf-list-p prop
            (occ-error "Implement it.")
          (occ-error "Implement it.")))))


(cl-defmethod occ-obj-valid-p ((prop      (eql timebeing))
                               (operation (eql increment)))
  (ignore prop)
  (ignore operation)
  t)

;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1 ends here


;; STATUS property of task

;; [[file:occ-property-methods.org::*STATUS property of task][STATUS property of task:1]]
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (prop (eql status)))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (ignore prop)
  (let ((todo-type (occ-obj-get-property obj 'todo-type))
        (closed    (occ-obj-get-property obj 'closed))
        (status    (occ-obj-get-property obj 'todo-keyword)))
    (if (or closed
            (eql todo-type 'done)
            (string= status "HOLD"))
        -(occ-rank-percentage 100)
      (occ-rank-percentage 0))))

;; STATUS property of task:1 ends here

;; Key property of task for setting arbitrary rank

;; [[file:occ-property-methods.org::*Key property of task for setting arbitrary rank][Key property of task for setting arbitrary rank:1]]
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (prop (eql key)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (ignore prop)
  (let* ((key (occ-obj-get-property obj 'KEY)))
      (if key
          (let ((nkey (string-to-number key)))
            (if (> nkey (occ-rank-percentage 100))
                (occ-rank-percentage 100)
              nkey))
        (occ-rank-percentage 0))))

;; Key property of task for setting arbitrary rank:1 ends here

;; Current clock status property of task (will rank based on task is currently clocking-in or not)

;; [[file:occ-property-methods.org::*Current clock status property of task (will rank based on task is currently clocking-in or not)][Current clock status property of task (will rank based on task is currently clocking-in or not):1]]
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (prop (eql current-clock)))
  (ignore prop)
  (let* ((tsk-marker (occ-obj-get-property obj 'marker)))
    (ignore tsk-marker)
    (if (and org-clock-marker
             (occ-obj-marker= obj org-clock-marker))
        (occ-rank-percentage 100)
      (occ-rank-percentage 0))))

;; Current clock status property of task (will rank based on task is currently clocking-in or not):1 ends here

;; SubtreeFile property of task

;; [[file:occ-property-methods.org::*SubtreeFile property of task][SubtreeFile property of task:1]]
;;{{ sub-tree
(cl-defmethod occ-obj-readprop ((obj occ-obj-ctx-tsk)
                                (prop (eql subtree)))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (ignore ctx)
    (let* ((prompt (concat (symbol-name prop) ": "))
           ;; org-iread-file-name
           (filename (ido-read-file-name prompt
                                         default-directory ;DIR
                                         nil               ;DEFAULT-FILENAME
                                         t                 ;MUSTMATCH
                                         nil               ;PREDICATE
                                         #'(lambda (f)     ;INITIAL
                                             (string-match "\.\+.org" f)))))
      (file-relative-name filename
                          default-directory))))
;;}}

;; SubtreeFile property of task:1 ends here



;; Git branch property of task

;; [[file:occ-property-methods.org::*Git branch property of task][Git branch property of task:1]]
;;{{ git-branch
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql git-branch))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (let ((buff (occ-ctx-buffer ctx)))
    ;; (occ-message "occ-obj-impl-get: git-branch: buff = %s" buff)
    (when buff
      (with-current-buffer buff
        (let ((branch (magit-get-current-branch)))
          ;; (occ-message "occ-obj-impl-get: branch = %s" branch)
          branch)))))
(cl-defmethod occ-obj-impl-get ((user occ-user-agent)
                                (prop (eql git-branch))
                                (obj occ-obj-ctx-tsk))
  "Read value of list of elements if (occ-obj-intf-list-p PROPERTY)
        else element for property PROPERTY from user for OCC-TSK OBJ,
        must return ORG compatible value."
  (magit-read-branch "Git branch"))

(cl-defmethod occ-do-impl-checkout ((obj occ-obj-tsk)
                                    (prop (eql git-branch)))
  (let* ((tsk        (occ-obj-tsk      obj))
         (rootdir    (occ-obj-get-property tsk 'root))
         (git-branch      (occ-obj-get-property tsk prop)))
    (if (and rootdir
             git-branch)
        (progn
          ;; (occ-message "branch %s" git-branch)
          (magit-checkout git-branch))
      (occ-debug "occ-do-impl-checkout: %s value ruturned for prop %s" first-file prop))))

(cl-defmethod occ-obj-impl-rank ((obj occ-obj-ctx-tsk)
                                 (prop (eql git-branch)))
  "Return the RANK (number) for OCC-TSK based on the property GIT-BRANCH"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((ctx-branch (occ-obj-impl-get ctx prop nil))
          (tsk-branch (occ-obj-get-property tsk prop)))
      (if (and (and ctx-branch
                    tsk-branch)
               (string= ctx-branch
                        tsk-branch))
          (occ-rank-percentage 100)
        (occ-rank-percentage 0)))))
(cl-defmethod occ-obj-impl-has-p ((obj occ-obj-tsk)
                                  (prop (eql git-branch))
                                  value)
  "OBJ has property PROPERTY"
  (let* ((tsk            (occ-obj-tsk obj))
         (tsk-prop-value (occ-obj-get-property tsk prop)))
    (if (and value tsk-prop-value)
        (string= value tsk-prop-value))))

(cl-defmethod occ-obj-impl-format ((obj occ-obj-tsk)
                                   (prop (eql git-branch))
                                   value)
  "Return format printable value of property PROPERTY."
  value)
(cl-defmethod occ-obj-impl-list-p ((prop (eql git-branch)))
  "Is the property GIT-BRANCH has VALUES in list, Method tell
         property represent list or not."
  t)
(cl-defmethod occ-obj-impl-to-org ((prop (eql git-branch))
                                   value)
  "Return string representation for property GIT-BRANCH, Method
      convert value VALUE of property PROPERTY from occ to org string
      representation."
  (format "%s" value))
(cl-defmethod occ-obj-org-from-imp ((prop (eql git-branch)
                                     value))
  "Return the Actual Object representation for property
      GIT-BRANCH, Method convert value VALUE of property PROPERTY from
      org string to occ representation."
  (unless (string= value "")
    value))
(cl-defmethod occ-obj-impl-require-p ((obj occ-obj-tsk)
                                      (operation (eql _operation_))
                                      (prop (eql git-branch))
                                      values)
  "Used by OCC-OBJ-GEN-EDIT-IF-REQUIRED to decide for this property
      GIT-BRANCH if CALLABLE (helm method) should be generated."
  nil)
(cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                    (prop (eql git-branch))
                                    (operation (eql _operation_)))
  "Return a default VALUE of property GIT-BRANCH."
  nil)
(cl-defmethod occ-obj-impl-default ((obj occ-obj-tsk)
                                    (prop (eql git-branch))
                                    (operation (eql _operation_)))
  "Return a default VALUE of property GIT-BRANCH."
  nil)
(cl-defmethod occ-do-impl-operation ((obj occ-obj-tsk)
                                     (operation (eql _operation_))
                                     (prop (eql git-branch))
                                     values)
  "Do the actual _OPERATION_."
  nil)

;; Git branch property of task:1 ends here


;; _template1_ property of task


;; _template1_ property of task:1 ends here



;; File Ends Here

;; [[file:occ-property-methods.org::*File Ends Here][File Ends Here:1]]
;;; occ-property-methods.el ends here
;; File Ends Here:1 ends here

