;;; occ-rank.el --- occ rank                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-rank)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-util-common)
(require 'occ-obj-accessor)
(require 'occ-debug-method)
(require 'occ-prop-base)

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

(defvar occ-rank-max-range 100)
(defvar occ-rank-quanta 1)


(defun occ-rank-percentage (num)
  num)


(cl-defgeneric occ-obj-calculate-rank (obj)
  "occ-obj-rank")


(cl-defmethod occ-obj-calculate-rank ((obj occ-obj-tsk)
                                      (properties list))
  ;; too much output
  ;; (occ-obj-properties-to-calculate-rank obj)
  (occ-debug "occ-obj-calculate-rank((occ-tsk=%s))"
               (occ-obj-Format (occ-obj-tsk obj)))
  (occ-debug "occ-obj-calculate-rank(obj occ-tsk) %s" properties)
  (/ (cl-reduce #'+
                (mapcar #'(lambda (slot)
                            (let ((prop (downcase-sym slot)))
                              (occ-obj-priority-rank obj
                                                     prop)))
                        properties))
     occ-rank-quanta))


;; (cl-defmethod occ-obj-calculate-rank ((obj occ-obj-ctx-tsk))
;;   ;; too much output

;;   ;; (occ-debug "occ-obj-calculate-rank((occ-obj-ctx-tsk=%s))"
;;   ;;            (occ-obj-format (occ-obj-tsk obj) 'capitalize))
;;   (occ-debug "occ-obj-calculate-rank(obj occ-obj-ctx-tsk) %s" (occ-obj-properties-to-calculate-rank obj))
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (ignore tsk)
;;     (ignore ctx)
;;     (let* ((properties (occ-obj-properties-to-calculate-rank obj))
;;            (rank       (/ (cl-reduce #'+
;;                                    (mapcar #'(lambda (slot)
;;                                                (let ((prop (downcase-sym slot)))
;;                                                  (occ-obj-priority-rank obj
;;                                                                         prop)))
;;                                            properties))
;;                           occ-rank-quanta)))
;;       (occ-debug "occ-obj-calculate-rank(obj occ-obj-ctx-tsk): rank = %d" rank)
;;       rank)))



(cl-defmethod occ-obj-prop-rank ((obj  occ-ranktbl)
                                 (prop symbol))
  (let ((rplist (occ-rank-plist obj)))
    (plist-get rplist prop))) ;; -- add unless
(cl-defmethod occ-obj-rank-inheritable ((obj occ-ranktbl))
  (occ-rank-inheritable obj))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-ranktbl))
  (occ-rank-nonhereditable obj))
(cl-defmethod occ-obj-rank-acquired ((obj occ-ranktbl))
  (occ-rank-acquired obj))
(cl-defmethod occ-obj-rank ((obj occ-ranktbl))
  (occ-rank-value obj))

(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-ranktbl)
                                        (prop symbol))
  (let ((rplist (occ-rank-plist obj)))
    (setf (occ-rank-plist obj)
          (plist-put rplist prop rank))))
(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number) (obj occ-ranktbl))
  (setf (occ-rank-inheritable obj) rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number) (obj occ-ranktbl))
  (setf (occ-rank-nonhereditable obj) rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number) (obj occ-ranktbl))
  (setf (occ-rank-acquired obj) rank))
(cl-defmethod (setf occ-obj-rank) ((rank number) (obj occ-ranktbl))
  (setf (occ-rank-value obj) rank))


(cl-defmethod occ-obj-acc-parent-rank ((obj null) (label number))
  0)

(cl-defmethod occ-obj-acc-parent-rank ((obj occ-obj-tsk) (label number))
  (+ (occ-obj-rank-inheritable obj)
     (occ-obj-acc-parent-rank (occ-obj-tsk-parent obj) 0)))


(cl-defmethod occ-obj-acc-ctx-parent-rank ((ctx occ-obj-ctx) (tsk null) (label number))
  0)

(cl-defmethod occ-obj-acc-ctx-parent-rank ((ctx occ-obj-ctx) (tsk occ-obj-tsk) (label number))
  (+ (occ-obj-rank-with tsk ctx)
     (occ-obj-acc-parent-rank ctx (occ-obj-tsk-parent tsk) 0)))


;; (cl-defmethod occ-obj-tsk-prop-rank ((obj  occ-tsk)
;;                                      (prop symbol))
;;   (let ((rt (occ-obj-rt obj)))
;;    ;; -- add unless
;;     (occ-obj-prop-rank rt prop)))
;; (cl-defmethod (setf occ-obj-tsk-prop-rank) ((rank number)
;;                                             (obj  occ-tsk)
;;                                             (prop symbol))
;;   (let ((rt (occ-obj-rt obj)))
;;     (setf (occ-obj-prop-rank rt prop) rank)))


;; (cl-defmethod occ-obj-ctx-tsk-prop-rank ((ctx occ-obj-ctx)
;;                                          (tsk occ-obj-tsk)
;;                                          (property symbol))
;;   (let ((rt (occ-obj-ctx-tsk-rt ctx tsk)))
;;     ;; -- add unless
;;     (occ-obj-prop-rank rt property)))

;; (cl-defmethod (setf occ-obj-ctx-tsk-prop-rank) ((rank number)
;;                                                 (ctx occ-obj-ctx)
;;                                                 (tsk occ-obj-tsk)
;;                                                 (property symbol))
;;   (let ((rt (occ-obj-ctx-tsk-rt ctx tsk)))
;;     ;; -- add unless
;;     (setf (occ-obj-prop-rank rt property) rank)))


;; (cl-defmethod occ-obj-ctx-tsk-rank ((ctx occ-obj-ctx)
;;                                     (tsk occ-obj-tsk))
;;   (let ((rt (occ-obj-rt-with tsk
;;                              ctx)))
;;     (occ-obj-rank rt)))

;; (cl-defmethod (setf occ-obj-ctx-tsk-rank) (rank
;;                                            (ctx occ-obj-ctx)
;;                                            (tsk occ-obj-tsk))
;;   (let ((rt (occ-obj-rt-with tsk
;;                              ctx)))
;;     (setf (occ-obj-rank rt) rank)))

(cl-defmethod occ-obj-rank-with ((tsk occ-obj-tsk)
                                 (ctx occ-obj-ctx))
  (let ((rt (occ-obj-rt-with tsk
                             ctx)))
    (occ-obj-rank rt)))


(cl-defmethod (setf occ-obj-rank-with) ((rank number)
                                        (tsk occ-obj-tsk)
                                        (ctx occ-obj-ctx))
  (let ((rt (occ-obj-rt-with tsk
                             ctx)))
    (setf (occ-obj-rank rt) rank)))


(cl-defmethod occ-obj-prop-rank ((obj  occ-obj-tsk)
                                 (property symbol))
  (let ((rt (occ-obj-rt obj)))
    (unless (occ-obj-prop-rank rt prop)
      (setf (occ-obj-prop-rank rt prop) (occ-obj-intf-rank (occ-obj-tsk obj)
                                                           (occ-obj-ctx obj) 
                                                           prop)))
    (occ-obj-prop-rank rt prop)))

(cl-defmethod (setf occ-obj-prop-rank) ((rank number)
                                        (obj  occ-obj-tsk)
                                        (property symbol))
  (let ((rt (occ-obj-rt obj)))
    (setf (occ-obj-prop-rank rt prop) rank)))


(cl-defmethod occ-obj-rank-inheritable ((obj occ-obj-tsk))
  (unless (occ-obj-rank-inheritable (occ-obj-rt obj))
    (setf (occ-obj-rank-inheritable (occ-obj-rt obj))
          (occ-obj-calculate-rank obj (occ-obj-properties-to-calculate-rank obj))))
  (occ-obj-rank-inheritable (occ-obj-rt obj)))
(cl-defmethod occ-obj-rank-nonheritable ((obj occ-obj-tsk))
  (unless (occ-obj-rank-inheritable (occ-obj-rt obj))
    (setf (occ-obj-rank-inheritable (occ-obj-rt obj))
          (occ-obj-calculate-rank obj nil)))
  (occ-obj-rank-nonhereditable (occ-obj-rt obj)))
(cl-defmethod occ-obj-rank-acquired ((tsk occ-obj-tsk))
  (unless (occ-obj-rank-acquired (occ-obj-rt obj))
    (setf (occ-obj-rank-acquired (occ-obj-rt obj)) (+ (occ-obj-rank-inheritable obj)
                                                      (occ-obj-rank-nonheritable obj))))
  (occ-obj-rank-acquired (occ-obj-rt obj)))
(cl-defmethod occ-obj-rank ((obj occ-obj-tsk))
  (unless (occ-obj-rank (occ-obj-rt obj))
    ;; Add code for adding parent ranks
    (setf (occ-obj-rank (occ-obj-rt obj)) (+ (occ-obj-rank-acquired obj)
                                             (occ-obj-acc-parent-rank obj 0))))
  (occ-obj-rank (occ-obj-rt obj)))
(cl-defmethod occ-obj-rank ((obj occ-obj-ctx-tsk))
  (unless (occ-obj-rank (occ-obj-rt obj))
    ;; Add code for adding parent ranks
    (occ-message "name: %s Tree: %d - %s"
                 (occ-obj-format tsk)
                 (length (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk)))
                 (mapcar #'cdr (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk))))
    (setf (occ-obj-rank (occ-obj-rt obj))
          (+ (occ-obj-rank-acquired obj)
             (occ-obj-acc-ctx-parent-rank (occ-obj-ctx obj)
                                          (occ-obj-tsk obj)
                                          0))))
  (occ-assert (occ-obj-rank (occ-obj-rt obj)))
  (occ-obj-rank (occ-obj-rt obj)))
 

(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number) (obj occ-obj-tsk))
  (setf (occ-obj-rank-inheritable (occ-obj-rt obj)) rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number) (obj occ-obj-tsk))
  (setf (occ-obj-rank-nonhereditable (occ-obj-rt obj)) rank))
(cl-defmethod (setf occ-obj-rank-acquired) ((rank number) (obj occ-obj-tsk))
  (setf (occ-obj-rank-acquired (occ-obj-rt obj)) rank))
(cl-defmethod (setf occ-obj-rank) ((rank number) (obj occ-obj-tsk))
  (setf (occ-obj-rank (occ-obj-rt obj)) rank))


;; (cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
;;                                                (tsk occ-obj-tsk))
;;   (setf (occ-obj-tsk-rank-inheritable tsk) rank))
;; (cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
;;                                                (tsk occ-ctxual-tsk))
;;   (setf (occ-obj-ctx-tsk-rank (occ-obj-ctx tsk) (occ-obj-tsk tsk)) rank) ;insert into map for inheritance
;;   (setf (occ-obj-tsk-rank-inheritable tsk) rank))
;; (cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
;;                                                 (tsk occ-obj-tsk))
;;   (setf (occ-obj-tsk-rank-nonheritable tsk) rank))
;; (cl-defmethod occ-obj-rank-inheritable ((tsk occ-obj-tsk))
;;   (unless (occ-obj-tsk-rank-inheritable tsk)
;;     (setf (occ-obj-rank-inheritable tsk) (occ-obj-calculate-rank tsk (occ-obj-properties-to-calculate-rank tsk))))
;;   (occ-obj-tsk-rank-inheritable tsk))
;; (cl-defmethod occ-obj-rank-nonheritable ((tsk occ-obj-tsk))
;;   (unless (occ-obj-tsk-rank-nonheritable tsk)
;;     (setf (occ-obj-rank-nonheritable tsk) (occ-obj-calculate-rank tsk nil)))
;;   (occ-obj-tsk-rank-nonheritable tsk))


;; (cl-defmethod (setf occ-obj-rank-acquired) ((rank number) (tsk occ-obj-tsk))
;;   (setf (occ-obj-tsk-rank-acquired tsk) rank))
;; (cl-defmethod occ-obj-rank-acquired ((tsk occ-obj-tsk))
;;   (unless (occ-obj-tsk-rank-acquired tsk)
;;     (setf (occ-obj-rank-acquired tsk) (+ (occ-obj-rank-inheritable tsk)
;;                                          (occ-obj-rank-nonheritable tsk))))
;;   (occ-obj-tsk-rank-acquired tsk))


;; (cl-defmethod (setf occ-obj-rank) ((rank number) (tsk occ-obj-tsk))
;;   (setf (occ-obj-tsk-rank tsk) rank))

;; (cl-defmethod occ-obj-rank ((tsk occ-obj-tsk))
;;   (unless (occ-obj-tsk-rank tsk)
;;     ;; Add code for adding parent ranks
;;     (setf (occ-obj-tsk-rank tsk) (+ (occ-obj-rank-acquired tsk)
;;                                     (occ-obj-acc-parent-rank tsk 0))))
;;   (occ-assert (occ-obj-tsk-rank tsk))
;;   (occ-obj-tsk-rank tsk))

;; (cl-defmethod occ-obj-rank ((tsk occ-ctxual-tsk))
;;   (unless (occ-obj-tsk-rank tsk)
;;     ;; Add code for adding parent ranks
;;     (occ-message "name: %s Tree: %d - %s"
;;                  (occ-obj-format tsk)
;;                  (length (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk)))
;;                  (mapcar #'cdr (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk))))
;;     (setf (occ-obj-tsk-rank tsk)
;;           (+ (occ-obj-rank-acquired tsk)
;;              (occ-obj-acc-ctx-parent-rank (occ-obj-ctx tsk)
;;                                           (occ-obj-tsk tsk)
;;                                           0))))
;;   (occ-assert (occ-obj-tsk-rank tsk))
;;   (occ-obj-tsk-rank tsk))

;; ;; occ-ctsk - accessors
(cl-defmethod occ-obj-rank ((obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-obj-tsk obj)))
    (occ-assert (occ-obj-rank tsk))
    (occ-obj-rank tsk)))

(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-obj-tsk obj)))
    (setf (occ-obj-rank tsk) rank)))




















(cl-defmethod occ-obj-calculate-avgrank ((obj occ-ctx))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-avgrank(occ-ctx=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-list obj
                                  :builder #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         ;; BUG
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-ctx))
  ;; too much output
  (occ-debug "occ-obj-calculate-varirank(occ-ctx=%s)"
             (occ-obj-format obj))
  (let* ((objs      (occ-obj-list obj
                                  :builder #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         ;; BUG
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-collection))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-avgrank(occ-collection=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (avgrank   (occ-calculate-average rankslist)))
       avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-collection))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-varirank(occ-collection=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-list obj))
         (rankslist (mapcar #'occ-obj-rank
                            objs))
         (varirank  (occ-calculate-variance rankslist)))
      varirank))


;; (occ-obj-avgrank (occ-default-collection))
;; (occ-obj-varirank (occ-default-collection))

;;; occ-rank.el ends here
