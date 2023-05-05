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
(require 'occ-prop-intf)
(require 'occ-normalize-ineqs)
(require 'occ-obj-accessor)
(require 'occ-debug-method)
(require 'occ-prop-base)

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

(cl-defgeneric occ-obj-calculate-rank (obj)
  "occ-obj-rank")


(cl-defmethod occ-obj-calculate-rank ((obj occ-tsk))
  ;; too much output
  (occ-debug "occ-obj-calculate-rank((occ-tsk=%s))"
             (occ-obj-Format (occ-obj-tsk obj)))
  (occ-debug "occ-obj-calculate-rank((occ-tsk=%s))"
               (occ-obj-Format (occ-obj-tsk obj)))
  (occ-debug "occ-obj-calculate-rank(obj occ-tsk) %s" (occ-obj-properties-to-calculate-rank obj))
  (let* ((properties (occ-obj-properties-to-calculate-rank obj))
         (rank       (cl-reduce #'+
                                (mapcar #'(lambda (slot)
                                            (let ((prop (downcase-sym slot)))
                                              (occ-obj-ineq-rankprop obj
                                                                     prop)))
                                        properties))))
    ;; (occ-debug "occ-obj-calculate-rank(obj occ-tsk): rank = %d" rank)
    rank))


(cl-defmethod occ-obj-calculate-rank ((obj occ-obj-ctx-tsk))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-rank((occ-obj-ctx-tsk=%s))"
  ;;            (occ-obj-format (occ-obj-tsk obj) 'capitalize))
  (occ-debug "occ-obj-calculate-rank(obj occ-obj-ctx-tsk) %s" (occ-obj-properties-to-calculate-rank obj))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (ignore tsk)
    (ignore ctx)
    (let* ((properties (occ-obj-properties-to-calculate-rank obj))
           (rank       (cl-reduce #'+
                                  (mapcar #'(lambda (slot)
                                              (let ((prop (downcase-sym slot)))
                                                (occ-obj-ineq-rankprop obj
                                                                       prop)))
                                          properties))))
      ;; (occ-debug "occ-obj-calculate-rank(obj occ-obj-ctx-tsk): rank = %d" rank)
      rank)))


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
             obj)
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
