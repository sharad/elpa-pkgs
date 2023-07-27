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

(require 'occ-print)
(require 'occ-predicate)
(require 'occ-obj-accessor)
(require 'occ-debug-method)
(require 'occ-prop-intf)

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
    (occ-debug "occ-obj-impl-rank-with: currfile begin")
    (occ-aggregate-rank tsk-currfile (occ-obj-get-property tsk prop) #'+
      (let* ((tsk-currfile (if tsk-currfile (file-truename tsk-currfile)))
             (ctx-file     (occ-ctx-file ctx))
             (ctx-file     (if ctx-file (file-truename ctx-file))))
        (if tsk-currfile
            (progn
              (occ-nodisplay "tsk %s tsk-currfile %s" (occ-obj-format tsk 'capitalize) tsk-currfile)
              (occ-nodisplay "tsk %s ctx-file %s"     (occ-obj-format tsk 'capitalize) ctx-file))
          (occ-nodisplay "tsk %s tsk-currfile %s not present."
                     (occ-obj-format tsk 'capitalize)
                     tsk-currfile))
        (if (and tsk-currfile ctx-file
                 (string= tsk-currfile ctx-file))
            (* 2 (length tsk-currfile))     ;as exact match to files giving double matching points.
          0)))))

(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql currfile))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (occ-debug "calling occ-obj-impl-get(ctx occ-ctx)"
    (let ((currfile (occ-ctx-file ctx)))
      currfile)))

(cl-defmethod occ-obj-list-p ((prop (eql currfile)))
  (ignore prop)
  t)

(cl-defmethod occ-obj-prop-to-org ((prop (eql currfile))
                                   value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-prop-from-org ((prop (eql currfile))
                                     value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-readprop-from-user ((obj occ-obj-ctx-tsk)
                                          (prop (eql currfile)))
  "currfile property for tsk aka org entry"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
           (ctx-dir      (when (stringp ctx-currfile)
                           (file-name-directory ctx-currfile)))
           (prompt       (concat (symbol-name prop) ": ")))
      (ido-read-file-name prompt ctx-dir ctx-currfile))))

(cl-defmethod occ-do-checkout-prop ((obj occ-obj-tsk)
                                    (prop (eql currfile)))
  (let* ((tsk        (occ-obj-tsk      obj))
         (files      (occ-obj-get-property tsk prop))
         (first-file (cl-first files)))
       (if first-file
           (find-file first-file)
         (occ-debug "occ-do-checkout-prop: %s value ruturned for prop %s" first-file prop))))
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
    (occ-aggregate-rank tsk-root (occ-obj-get-property tsk prop) #'+
      (let* ((tsk-root (cl-first (occ-obj-get-property tsk prop)))
             (tsk-root (when tsk-root (file-truename tsk-root)))
             (ctx-file (occ-ctx-file ctx))
             ;; (ctx-file (when ctx-file (file-truename ctx-file)))
             (ctx-dir  (when (stringp ctx-file) (file-name-directory ctx-file)))
             (ctx-dir  (when (stringp ctx-file) (file-truename ctx-dir))))
        (if tsk-root
            (progn
              (occ-nodisplay "tsk %s tsk-root: %s" (occ-obj-format tsk 'capitalize) tsk-root)
              (occ-nodisplay "tsk %s ctx-dir:  %s" (occ-obj-format tsk 'capitalize) ctx-dir))
          (occ-nodisplay "tsk %s tsk-root %s not present."
                     (occ-obj-format tsk 'capitalize) tsk-root))
        (if (and tsk-root ctx-dir
                 (string-match tsk-root ctx-dir))
            (length tsk-root)
          0)))))

(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql root))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (let ((file (occ-ctx-file ctx)))
      (when file (dirname-of-file file))))

(cl-defmethod occ-obj-list-p ((prop (eql root)))
  (ignore prop)
  t)

(cl-defmethod occ-obj-prop-to-org ((prop (eql root))
                                   value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-prop-from-org ((prop (eql root))
                                     value)
  (ignore prop)
  value)

(cl-defmethod occ-obj-readprop-from-user ((obj occ-obj-ctx-tsk)
                                          (prop (eql root)))
  "READ"
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
           (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
           (prompt     (concat (symbol-name prop) ": ")))
      (ido-read-directory-name prompt ctx-dir ctx-dir))))

(cl-defmethod occ-do-checkout-prop ((obj occ-obj-tsk)
                                    (prop (eql root)))
  (let* ((tsk       (occ-obj-tsk      obj))
         (dirs      (occ-obj-get-property tsk prop))
         (first-dir (cl-first dirs)))
    (if first-dir
        (find-file first-dir)
      (occ-debug "occ-do-checkout-prop: %s value ruturned for prop %s" first-dir prop))))
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
                                0))
              (clocked-time   (occ-obj-get-property tsk
                                                'clock-sum)))
          (if (and (numberp clocked-time)
                   (numberp timebeing-time)
                   (> timebeing-time clocked-time))
              (- timebeing-time
                 clocked-time)
            0)))))

(cl-defmethod occ-obj-list-p ((prop (eql timebeing)))
  (ignore prop)
  nil)

(cl-defmethod occ-obj-prop-to-org ((prop (eql timebeing))
                                   value)
  (ignore prop)
  (if (numberp value)
      (number-to-string value)
    ""))

(cl-defmethod occ-obj-prop-from-org ((prop (eql timebeing))
                                     value)
  (ignore prop)
  (if (stringp value)
      (or (string-to-number value)
          0)
    0))

(cl-defmethod occ-obj-readprop-from-user ((obj occ-tsk)
                                          (prop (eql timebeing)))
  "READ"
  (let ((tsk (occ-obj-tsk obj)))
    (ignore tsk)
    (let* ((prompt     (concat (symbol-name prop)
                               ": ")))
      (ignore prompt)
      (read-number "Timebeing mins: "))))

(cl-defmethod occ-obj-require-p ((obj occ-obj-tsk)
                                 (operation (eql increment))
                                 (prop (eql timebeing))
                                 values)
  (ignore operation)
  (ignore prop)
  (ignore values)
  (occ-obj-current-p obj))

(cl-defmethod occ-obj-prop-default-value ((obj occ-obj-tsk)
                                          (prop (eql timebeing))
                                          (operation (eql increment)))
  (ignore prop)
  (ignore operation)
  (when (occ-obj-current-p obj)
    10))

(cl-defmethod occ-obj-operation ((obj occ-obj-tsk)
                                 (operation (eql increment))
                                 (prop (eql timebeing))
                                 values)
  (ignore operation)
  (ignore values)
  (let ((tsk    (occ-obj-tsk obj)))
    (ignore tsk)
    (if (occ-obj-list-p prop)
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
    (if (occ-obj-list-p prop
            (occ-error "Implement it.")
          (occ-error "Implement it.")))))


(cl-defmethod occ-obj-valid-p ((prop      (eql timebeing))
                               (operation (eql increment)))
  (ignore prop)
  (ignore operation)
  t)

;; Timebeing property of task (not fully implemented) will use for keeping a task clocked in for given time:1 ends here

;; Git branch property of task

;; [[file:occ-property-methods.org::*Git branch property of task][Git branch property of task:1]]
;;{{ git-branch
(cl-defmethod occ-obj-impl-get ((ctx occ-ctx)
                                (prop (eql git-branch))
                                (arg null))
  "Return occ compatible value of property PROPERTY from OCC-CTX OBJ."
  (ignore prop)
  (let ((file (occ-ctx-file ctx)))
      file))

;; Git branch property of task:1 ends here

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
        -30 0)))

;; STATUS property of task:1 ends here

;; Key property of task for setting arbitrary rank

;; [[file:occ-property-methods.org::*Key property of task for setting arbitrary rank][Key property of task for setting arbitrary rank:1]]
(cl-defmethod occ-obj-impl-rank ((obj  occ-tsk)
                                 (prop (eql key)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (ignore prop)
  (let* ((key (occ-obj-get-property obj 'KEY)))
      (if key (string-to-number key) 0)))

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
        100
      0)))

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

;; File Ends Here

;; [[file:occ-property-methods.org::*File Ends Here][File Ends Here:1]]
;;; occ-property-methods.el ends here
;; File Ends Here:1 ends here
