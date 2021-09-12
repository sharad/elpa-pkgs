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

;; Provide this file

;; [[file:occ-property-methods.org::*Provide this file][Provide this file:1]]
(provide 'occ-property-methods)

;; Provide this file:1 ends here

;; Required libraries

;; [[file:occ-property-methods.org::*Required libraries][Required libraries:1]]
(require 'occ-prop-intf)

;; Required libraries:1 ends here

;; Template property of task

;; [[file:occ-property-methods.org::*Template property of task][Template property of task:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql _template2_)))
  "Return the RANK (number) for OCC-TSK based on the property _TEMPLATE2_"
  (let ((tsk (occ-obj-tsk obj)))
    (let ((_template2_ (occ-get-property tsk
                                         '_template2_)))
      0)))
(cl-defmethod occ-has-p ((obj occ-obj-tsk)
                         (prop (eql _template2_))
                         value)
  "OBJ-has-property PROPERTY")
(cl-defmethod occ-get-property-value-from-ctx ((obj occ-ctx)
                                               (prop (eql _template2_)))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ.")

(cl-defmethod occ-format-prop ((obj occ-obj-tsk)
                               (property symbol)
                               value)
  "Return format printable value of property PROPERTY."
  value)
(cl-defmethod occ-list-p ((prop (eql _template2_)))
  "Is the property _TEMPLATE2_ has VALUES in list, Method tell
   property represent list or not."
  nil)

(cl-defmethod occ-prop-to-org   ((prop (eql _template2_))
                                 value)
  "Return string representation for property _TEMPLATE2_, Method
convert value VALUE of property PROPERTY from occ to org string
representation."
  nil)

(cl-defmethod occ-prop-from-org ((prop (eql _template2_))
                                 value)
  "Return the Actual Object representation for property
_TEMPLATE2_, Method convert value VALUE of property PROPERTY from
org string to occ representation."
  nil)

(cl-defmethod occ-readprop-from-user ((obj occ-tsk)
                                      (prop (eql _template2_)))
  "READ the value for property _TEMPLATE2_, Read value of element
of list for property PROPERTY from user for OCC-TSK OBJ, must
return ORG compatible value."
  (let ((tsk (occ-obj-tsk obj)))
    (let* ((prompt     (concat (symbol-name prop)
                               ": ")))
      (read-number (format "%s: " prompt)))))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql _operation_))
                             (prop (eql _template2_))
                             values)
  "Used by OCC-GEN-EDIT-IF-REQUIRED to decide for this property
_TEMPLATE2_ if CALLABLE (helm method) should be generated."
  (occ-current-p obj))

(cl-defmethod occ-prop-default-value ((obj occ-obj-tsk)
                                      (prop (eql _template2_))
                                      (operation (eql _operation_)))
  "Return a default VALUE of property _TEMPLATE2_."
  (if (occ-current-p obj)
      10
    0))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql _operation_))
                             (prop (eql _template2_))
                             values)
  "Do the actual _OPERATION_."
  (let ((tsk (occ-obj-tsk obj)))
      (if (occ-list-p prop)
          (occ-error "Implement it.")
        (occ-error "Implement it."))))

(cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                 (prop (eql _template2_)))
  "Checkout property _TEMPLATE2_ in case of force clock-in.")

;; Template property of task:1 ends here

;; Current File property of task

;; [[file:occ-property-methods.org::*Current File property of task][Current File property of task:1]]
;;{{ currfile
(cl-defmethod occ-rankprop ((obj occ-obj-ctx-tsk)
                            (prop (eql currfile))) ;; do not use (prop (eql file)) that is another property which represent file in which task defined.
  ;; file in which tsk aka org entry exists.
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-debug :debug "occ-rankprop-with: currfile begin")
    (occ-aggrigate-list-rank tsk-currfile (occ-get-property tsk prop) #'+
      (let* ((tsk-currfile (if tsk-currfile (file-truename tsk-currfile)))
             (ctx-file     (occ-ctx-file ctx))
             (ctx-file     (if ctx-file (file-truename ctx-file))))
        (if tsk-currfile
            (progn
              (occ-debug :nodisplay "tsk %s tsk-currfile %s" (occ-format tsk 'capitalize) tsk-currfile)
              (occ-debug :nodisplay "tsk %s ctx-file %s"     (occ-format tsk 'capitalize) ctx-file))
          (occ-debug :nodisplay "tsk %s tsk-currfile %s not present."
                     (occ-format tsk 'capitalize)
                     tsk-currfile))
        (if (and tsk-currfile ctx-file
                 (string= tsk-currfile ctx-file))
            (* 2 (length tsk-currfile))     ;as exact match to files giving double matching points.
          0)))))

(cl-defmethod occ-get-property-value-from-ctx ((ctx occ-ctx)
                                               (prop (eql currfile)))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (occ-debug :debug "calling occ-get-property-value-from-ctx(ctx occ-ctx)")
  (let ((currfile (occ-ctx-file ctx)))
    currfile))

(cl-defmethod occ-list-p ((prop (eql currfile)))
  t)

(cl-defmethod occ-prop-to-org   ((prop (eql currfile))
                                 value)
  value)

(cl-defmethod occ-prop-from-org ((prop (eql currfile))
                                 value)
  value)

(cl-defmethod occ-readprop-from-user ((obj occ-obj-ctx-tsk)
                                      (prop (eql currfile)))
  "currfile property for tsk aka org entry"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
           (ctx-dir      (when (stringp ctx-currfile)
                           (file-name-directory ctx-currfile)))
           (prompt       (concat (symbol-name prop) ": ")))
      (ido-read-file-name prompt ctx-dir ctx-currfile))))

(cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                 (prop (eql currfile)))
  (let* ((files      (occ-get-property obj))
         (first-file (first files)))
    (find-file first-file)))

;;}}

;; Current File property of task:1 ends here

;; Root dir property of task

;; [[file:occ-property-methods.org::*Root dir property of task][Root dir property of task:1]]
;;{{ root
(cl-defmethod occ-rankprop ((obj occ-obj-ctx-tsk)
                            (prop (eql root)))
  "RANK Predicate funtion to check if ctx matches to tsk's ROOT attribute."
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-debug :nodisplay "rankprop(%s, %s): begin"
               (occ-format tsk 'capitalize)
               (occ-format ctx 'capitalize)
               prop)
    (occ-aggrigate-list-rank tsk-root (occ-get-property tsk prop) #'+
      (let* ((tsk-root (car (occ-get-property tsk prop)))
             (tsk-root (when tsk-root (file-truename tsk-root)))
             (ctx-file (occ-ctx-file ctx))
             ;; (ctx-file (when ctx-file (file-truename ctx-file)))
             (ctx-dir  (when (stringp ctx-file) (file-name-directory ctx-file)))
             (ctx-dir  (when (stringp ctx-file) (file-truename ctx-dir))))
        (if tsk-root
            (progn
              (occ-debug :nodisplay "tsk %s tsk-root: %s" (occ-format tsk 'capitalize) tsk-root)
              (occ-debug :nodisplay "tsk %s ctx-dir:  %s" (occ-format tsk 'capitalize) ctx-dir))
          (occ-debug :nodisplay "tsk %s tsk-root %s not present."
                     (occ-format tsk 'capitalize) tsk-root))
        (if (and tsk-root ctx-dir
                 (string-match tsk-root ctx-dir))
            (length tsk-root)
          0)))))

(cl-defmethod occ-get-property-value-from-ctx ((ctx occ-ctx)
                                               (prop (eql root)))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (let ((file (occ-ctx-file ctx)))
    (when file (dirname-of-file file))))

(cl-defmethod occ-list-p ((prop (eql root)))
  t)

(cl-defmethod occ-prop-to-org   ((prop (eql root))
                                 value)
  value)

(cl-defmethod occ-prop-from-org ((prop (eql root))
                                 value)
  value)

(cl-defmethod occ-readprop-from-user ((obj occ-obj-ctx-tsk)
                                      (prop (eql root)))
  "READ"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
           (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
           (prompt     (concat (symbol-name prop) ": ")))
      (ido-read-directory-name prompt ctx-dir ctx-dir))))

(cl-defmethod occ-checkout-prop ((obj occ-obj-tsk)
                                 (prop (eql root)))
  (let* ((dirs      (occ-get-property obj))
         (first-dir (first dirs)))
    (find-file first-dir)))
;;}}

;; Root dir property of task:1 ends here

;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time

;; [[file:occ-property-methods.org::*Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time][Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql timebeing)))
  (let ((tsk (occ-obj-tsk obj)))
    (let ((timebeing (occ-get-property tsk
                                       'timebeing)))
      (let ((timebeing-time (if timebeing
                                (org-duration-string-to-minutes timebeing)
                              0))
            (clocked-time   (occ-get-property tsk
                                              'clock-sum)))
        (if (and (numberp clocked-time)
                 (numberp timebeing-time)
                 (> timebeing-time clocked-time))
            (- timebeing-time
               clocked-time)
          0)))))

(cl-defmethod occ-list-p ((prop (eql timebeing)))
   nil)

(cl-defmethod occ-prop-to-org   ((prop (eql timebeing))
                                 value)
  (if (numberp value)
      (number-to-string value)
    ""))

(cl-defmethod occ-prop-from-org ((prop (eql timebeing))
                                 value)
  (if (stringp value)
      (or (string-to-number value)
          0)
    0))

(cl-defmethod occ-readprop-from-user ((obj occ-tsk)
                                      (prop (eql timebeing)))
  "READ"
  (let ((tsk (occ-obj-tsk obj)))
    (let* ((prompt     (concat (symbol-name prop)
                               ": ")))
      (read-number "Timebeing mins: "))))

(cl-defmethod occ-require-p ((obj occ-obj-tsk)
                             (operation (eql increment))
                             (prop (eql timebeing))
                             values)
  (occ-current-p obj))

(cl-defmethod occ-prop-default-value ((obj occ-obj-tsk)
                                      (prop (eql timebeing))
                                      (operation (eql increment)))
  (when (occ-current-p obj)
    10))

(cl-defmethod occ-operation ((obj occ-obj-tsk)
                             (operation (eql increment))
                             (prop (eql timebeing))
                             values)
  (let ((tsk    (occ-obj-tsk obj)))
    (if (occ-list-p prop)
        (occ-error "Implement it.")
      (occ-error "Implement it."))))

(cl-defmethod occ-org-operation ((obj occ-obj-tsk)
                                 (operation (eql increment))
                                 (prop (eql timebeing))
                                 values)
  (let ((prop-string (symbol-name prop)))
    (if (occ-list-p prop)
        (occ-error "Implement it.")
      (occ-error "Implement it."))))


;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1 ends here

;; Git branch property of task

;; [[file:occ-property-methods.org::*Git branch property of task][Git branch property of task:1]]
;;{{ git-branch
(cl-defmethod occ-get-property-value-from-ctx ((ctx occ-ctx)
                                               (prop (eql git-branch)))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (let ((file (occ-ctx-file ctx)))
    file))

;; Git branch property of task:1 ends here

;; STATUS property of task

;; [[file:occ-property-methods.org::*STATUS property of task][STATUS property of task:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql status)))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (let ((todo-type (occ-get-property obj 'todo-type))
        (closed    (occ-get-property obj 'closed))
        (status    (occ-get-property obj 'todo-keyword)))
    (if (or closed
            (eql todo-type 'done)
            (string= status "HOLD"))
        -30 0)))

;; STATUS property of task:1 ends here

;; Key property of task for setting arbitrary rank

;; [[file:occ-property-methods.org::*Key property of task for setting arbitrary rank][Key property of task for setting arbitrary rank:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql key)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((key (occ-get-property obj 'KEY)))
    (if key (string-to-number key) 0)))

;; Key property of task for setting arbitrary rank:1 ends here

;; Heading level property of task

;; [[file:occ-property-methods.org::*Heading level property of task][Heading level property of task:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql heading-level)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((level (occ-get-property obj 'level)))
    (if level level 0)))

;; Heading level property of task:1 ends here

;; Current clock status proprty of task (will rank based on task is currently clocking-in or not

;; [[file:occ-property-methods.org::*Current clock status proprty of task (will rank based on task is currently clocking-in or not][Current clock status proprty of task (will rank based on task is currently clocking-in or not:1]]
(cl-defmethod occ-rankprop ((obj occ-tsk)
                            (prop (eql current-clock)))
  (let* ((tsk-marker (occ-get-property obj 'marker)))
    (if (occ-marker= obj org-clock-marker)
        100
      0)))

;; Current clock status proprty of task (will rank based on task is currently clocking-in or not:1 ends here

;; SubtreeFile property of task

;; [[file:occ-property-methods.org::*SubtreeFile property of task][SubtreeFile property of task:1]]
;;{{ sub-tree
(cl-defmethod occ-readprop ((obj occ-obj-ctx-tsk)
                            (prop (eql subtree)))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((prompt (concat (symbol-name prop) ": ")))
      (file-relative-name
       (ido-read-file-name ;; org-iread-file-name
        prompt
        default-directory default-directory)))))
;;}}

;; SubtreeFile property of task:1 ends here

;; File Ends Here

;; [[file:occ-property-methods.org::*File Ends Here][File Ends Here:1]]
;;; occ-property-methods.el ends here
;; File Ends Here:1 ends here
