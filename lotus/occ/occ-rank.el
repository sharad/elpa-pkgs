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



(cl-defmethod occ-obj-tsk-prop-rank ((obj  occ-tsk)
                                     (prop symbol))
  (let ((rplist (occ-obj-tsk-prop-ranks-plist obj)))
    (plist-get rplist prop))) ;; -- add unless
(cl-defmethod (setf occ-obj-tsk-prop-rank) ((rank number)
                                            (obj  occ-tsk)
                                            (prop symbol))
  (let ((rplist (occ-obj-tsk-prop-ranks-plist obj)))
    (setf (occ-obj-tsk-prop-ranks-plist obj)
          (plist-put rplist prop rank))))


(cl-defmethod occ-obj-ctx-tsk-plist ((ctx occ-obj-ctx)
                                     (tsk occ-obj-tsk))
  (cdr (assoc tsk (occ-obj-ctx-tsk-aplist ctx)))) ;; -- add unless

(cl-defmethod (setf occ-obj-ctx-tsk-plist) (plist
                                            (ctx occ-obj-ctx)
                                            (tsk occ-obj-tsk))
  (if (occ-obj-ctx-tsk-plist ctx tsk)
      (setf (cdr (assoc tsk (occ-obj-ctx-tsk-aplist ctx))) plist)
    (cl-pushnew (cons tsk plist)
                (occ-obj-ctx-tsk-aplist ctx))))

(cl-defmethod occ-obj-ctx-tsk-prop-rank ((ctx occ-obj-ctx)
                                         (tsk occ-obj-tsk)
                                         (property symbol))
  (plist-get (occ-obj-ctx-tsk-plist ctx tsk) ;; -- add unless
             property))

(cl-defmethod (setf occ-obj-ctx-tsk-prop-rank) ((rank number)
                                                (ctx occ-obj-ctx)
                                                (tsk occ-obj-tsk)
                                                (property symbol))
  (setf (occ-obj-ctx-tsk-plist ctx tsk)
        (plist-put (occ-obj-ctx-tsk-plist ctx tsk) property rank)))



(cl-defmethod occ-obj-ctx-tsk-rank ((ctx occ-obj-ctx)
                                    (tsk occ-obj-tsk))
  (unless (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx)))
    (occ-obj-rank-inheritable (occ-obj-make-ctxual-tsk-with tsk ctx)))
    ;; (setf (occ-obj-ctx-tsk-rank ctx tsk)
    ;;       ;; (TODO)
    ;;       (occ-obj-rank-acquired (occ-obj-make-ctxual-tsk-with tsk ctx)))
  (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx))))

(cl-defmethod (setf occ-obj-ctx-tsk-rank) (rank
                                           (ctx occ-obj-ctx)
                                           (tsk occ-obj-tsk))
  (if (occ-obj-ctx-tsk-rank ctx tsk)
      (setf (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx))) rank)
    (cl-pushnew (cons tsk rank)
                (occ-ctx-tsk-rank-alist ctx))))


(cl-defmethod occ-obj-prop-rank ((obj  occ-tsk)
                                 (property symbol))
  (unless (occ-obj-tsk-prop-rank obj property)
    (setf (occ-obj-tsk-prop-rank obj property)
          (occ-obj-intf-rank obj
                             prop)))
  (occ-obj-tsk-prop-rank obj property))

(cl-defmethod occ-obj-prop-rank ((obj  occ-obj-ctx-tsk)
                                 (property symbol))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (unless (occ-obj-ctx-tsk-prop-rank ctx tsk property)
      (setf (occ-obj-ctx-tsk-prop-rank ctx tsk property)
            (occ-obj-intf-rank obj
                               prop)))
    (occ-obj-ctx-tsk-prop-rank ctx tsk
                               property)))


(cl-defmethod occ-obj-tsk-rank-inheritable ((tsk occ-obj-tsk))
  (occ-tsk-rank-inheritable tsk))
(cl-defmethod occ-obj-tsk-rank-nonheritable ((tsk occ-obj-tsk))
  (occ-tsk-rank-nonhereditable tsk))
(cl-defmethod occ-obj-tsk-rank-acquired ((tsk occ-obj-tsk))
  (occ-tsk-rank-acquired tsk))
(cl-defmethod occ-obj-tsk-rank ((tsk occ-obj-tsk))
  (occ-tsk-rank tsk))

(cl-defmethod occ-obj-tsk-rank-inheritable ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-inheritable tsk))
(cl-defmethod occ-obj-tsk-rank-nonheritable ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-nonhereditable tsk))
(cl-defmethod occ-obj-tsk-rank-acquired ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-acquired tsk))
(cl-defmethod occ-obj-tsk-rank ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank tsk))

(cl-defmethod (setf occ-obj-tsk-rank-inheritable) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-nonheritable) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-nonhereditable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-acquired) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-acquired tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank tsk) rank))

(cl-defmethod (setf occ-obj-tsk-rank-inheritable) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-nonheritable) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-nonhereditable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-acquired) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-acquired tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank tsk) rank))


(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (tsk occ-obj-tsk))
  (setf (occ-obj-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-rank-inheritable) ((rank number)
                                               (tsk occ-ctxual-tsk))
  (setf (occ-obj-ctx-tsk-rank (occ-obj-ctx tsk) (occ-obj-tsk tsk)) rank) ;insert into map for inheritance
  (setf (occ-obj-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-rank-nonheritable) ((rank number)
                                                (tsk occ-obj-tsk))
  (setf (occ-obj-tsk-rank-nonheritable tsk) rank))
(cl-defmethod occ-obj-rank-inheritable ((tsk occ-obj-tsk))
  (unless (occ-obj-tsk-rank-inheritable tsk)
    (setf (occ-obj-rank-inheritable tsk) (occ-obj-calculate-rank tsk (occ-obj-properties-to-calculate-rank tsk))))
  (occ-obj-tsk-rank-inheritable tsk))
(cl-defmethod occ-obj-rank-nonheritable ((tsk occ-obj-tsk))
  (unless (occ-obj-tsk-rank-nonheritable tsk)
    (setf (occ-obj-rank-nonheritable tsk) (occ-obj-calculate-rank tsk nil)))
  (occ-obj-tsk-rank-nonheritable tsk))


(cl-defmethod (setf occ-obj-rank-acquired) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-obj-tsk-rank-acquired tsk) rank))
(cl-defmethod occ-obj-rank-acquired ((tsk occ-obj-tsk))
  (unless (occ-obj-tsk-rank-acquired tsk)
    (setf (occ-obj-rank-acquired tsk) (+ (occ-obj-rank-inheritable tsk)
                                         (occ-obj-rank-nonheritable tsk))))
  (occ-obj-tsk-rank-acquired tsk))


(cl-defmethod (setf occ-obj-rank) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-obj-tsk-rank tsk) rank))

(cl-defmethod occ-obj-rank ((tsk occ-obj-tsk))
  (unless (occ-obj-tsk-rank tsk)
    ;; Add code for adding parent ranks
    (setf (occ-obj-tsk-rank tsk) (+ (occ-obj-rank-acquired tsk)
                                    (occ-obj-acc-parent-rank tsk 0))))
  (occ-assert (occ-obj-tsk-rank tsk))
  (occ-obj-tsk-rank tsk))

(cl-defmethod occ-obj-rank ((tsk occ-ctxual-tsk))
  (unless (occ-obj-tsk-rank tsk)
    ;; Add code for adding parent ranks
    (occ-message "name: %s Tree: %d - %s"
                 (occ-obj-format tsk)
                 (length (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk)))
                 (mapcar #'cdr (occ-ctx-tsk-rank-alist (occ-obj-ctx tsk))))
    (setf (occ-obj-tsk-rank tsk)
          (+ (occ-obj-rank-acquired tsk)
             (occ-obj-acc-ctx-parent-rank (occ-obj-ctx tsk)
                                          (occ-obj-tsk tsk)
                                          0))))
  (occ-assert (occ-obj-tsk-rank tsk))
  (occ-obj-tsk-rank tsk))

;; ;; occ-ctsk - accessors
(cl-defmethod occ-obj-rank ((obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctsk-tsk obj)))
    (occ-assert (occ-obj-rank tsk))
    (occ-obj-rank tsk)))

(cl-defmethod (setf occ-obj-rank) ((rank number)
                                   (obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctsk-tsk obj)))
    (setf (occ-obj-rank tsk) rank)))


(cl-defmethod occ-obj-acc-parent-rank ((tsk null) (label number))
  0)

(cl-defmethod occ-obj-acc-parent-rank ((tsk occ-obj-tsk) (label number))
  (+ (occ-obj-rank-inheritable tsk)
     (occ-obj-acc-parent-rank (occ-obj-task-parent tsk) 0)))


(cl-defmethod occ-obj-acc-ctx-parent-rank ((ctx occ-obj-ctx) (tsk null) (label number))
  0)

(cl-defmethod occ-obj-acc-ctx-parent-rank ((ctx occ-obj-ctx) (tsk occ-obj-tsk) (label number))
  (+ (occ-obj-ctx-tsk-rank ctx tsk)
     (occ-obj-acc-parent-rank ctx (occ-obj-task-parent tsk) 0)))








(cl-defmethod occ-rank-prop-rank ((obj  occ-rank)
                                  (prop symbol))
  (let ((rplist (occ-rank-plist obj)))
    (plist-get rplist prop))) ;; -- add unless
(cl-defmethod (setf occ-rank-prop-rank) ((rank number)
                                         (obj  occ-tsk)
                                         (prop symbol))
  (let ((rplist (occ-rank-plist obj)))
    (setf (occ-rank-plist obj)
          (plist-put rplist prop rank))))


;; (cl-defmethod occ-obj-ctx-tsk-plist ((ctx occ-obj-ctx)
;;                                      (tsk occ-rank))
;;   (cdr (assoc tsk (occ-obj-ctx-tsk-aplist ctx)))) ;; -- add unless

;; (cl-defmethod (setf occ-obj-ctx-tsk-plist) (plist
;;                                             (ctx occ-obj-ctx)
;;                                             (tsk occ-rank))
;;   (if (occ-obj-ctx-tsk-plist ctx tsk)
;;       (setf (cdr (assoc tsk (occ-obj-ctx-tsk-aplist ctx))) plist)
;;     (cl-pushnew (cons tsk plist)
;;                 (occ-obj-ctx-tsk-aplist ctx))))

;; (cl-defmethod occ-obj-ctx-tsk-prop-rank ((ctx occ-obj-ctx)
;;                                          (tsk occ-rank)
;;                                          (property symbol))
;;   (plist-get (occ-obj-ctx-tsk-plist ctx tsk) ;; -- add unless
;;              property))

;; (cl-defmethod (setf occ-obj-ctx-tsk-prop-rank) ((rank number)
;;                                                 (ctx occ-obj-ctx)
;;                                                 (tsk occ-rank)
;;                                                 (property symbol))
;;   (setf (occ-obj-ctx-tsk-plist ctx tsk)
;;         (plist-put (occ-obj-ctx-tsk-plist ctx tsk) property rank)))



(cl-defmethod occ-obj-ctx-tsk-rank ((ctx occ-obj-ctx)
                                    (tsk occ-obj-tsk))
  (unless (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx)))
    (occ-obj-rank-inheritable (occ-obj-make-ctxual-tsk-with tsk ctx)))
    ;; (setf (occ-obj-ctx-tsk-rank ctx tsk)
    ;;       ;; (TODO)
    ;;       (occ-obj-rank-acquired (occ-obj-make-ctxual-tsk-with tsk ctx)))
  (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx))))

(cl-defmethod (setf occ-obj-ctx-tsk-rank) (rank
                                           (ctx occ-obj-ctx)
                                           (tsk occ-obj-tsk))
  (if (occ-obj-ctx-tsk-rank ctx tsk)
      (setf (cdr (assoc tsk (occ-ctx-tsk-rank-alist ctx))) rank)
    (cl-pushnew (cons tsk rank)
                (occ-ctx-tsk-rank-alist ctx))))


(cl-defmethod occ-obj-prop-rank ((obj  occ-rank)
                                 (property symbol))
  (unless (occ-obj-tsk-prop-rank obj property)
    (setf (occ-obj-tsk-prop-rank obj property)
          (occ-obj-intf-rank obj
                             prop)))
  (occ-obj-tsk-prop-rank obj property))

(cl-defmethod occ-obj-prop-rank ((obj  occ-obj-ctx-tsk)
                                 (property symbol))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (unless (occ-obj-ctx-tsk-prop-rank ctx tsk property)
      (setf (occ-obj-ctx-tsk-prop-rank ctx tsk property)
            (occ-obj-intf-rank obj
                               prop)))
    (occ-obj-ctx-tsk-prop-rank ctx tsk
                               property)))


(cl-defmethod occ-obj-tsk-rank-inheritable ((tsk occ-obj-tsk))
  (occ-tsk-rank-inheritable tsk))
(cl-defmethod occ-obj-tsk-rank-nonheritable ((tsk occ-obj-tsk))
  (occ-tsk-rank-nonhereditable tsk))
(cl-defmethod occ-obj-tsk-rank-acquired ((tsk occ-obj-tsk))
  (occ-tsk-rank-acquired tsk))
(cl-defmethod occ-obj-tsk-rank ((tsk occ-obj-tsk))
  (occ-tsk-rank tsk))

(cl-defmethod occ-obj-tsk-rank-inheritable ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-inheritable tsk))
(cl-defmethod occ-obj-tsk-rank-nonheritable ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-nonhereditable tsk))
(cl-defmethod occ-obj-tsk-rank-acquired ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank-acquired tsk))
(cl-defmethod occ-obj-tsk-rank ((tsk occ-ctxual-tsk))
  (occ-ctxual-tsk-rank tsk))

(cl-defmethod (setf occ-obj-tsk-rank-inheritable) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-nonheritable) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-nonhereditable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-acquired) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank-acquired tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank) ((rank number) (tsk occ-obj-tsk))
  (setf (occ-tsk-rank tsk) rank))

(cl-defmethod (setf occ-obj-tsk-rank-inheritable) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-inheritable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-nonheritable) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-nonhereditable tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank-acquired) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank-acquired tsk) rank))
(cl-defmethod (setf occ-obj-tsk-rank) ((rank number) (tsk occ-ctxual-tsk))
  (setf (occ-ctxual-tsk-rank tsk) rank))














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
