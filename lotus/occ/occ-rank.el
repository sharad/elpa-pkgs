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


(require 'occ-macros)
(require 'occ-util-common)
(require 'occ-prop-intf)

;; TODO: graded ranking where ranking will be under priority of properties, where one can not go beyond above one, normally

(cl-defgeneric occ-obj-calculate-rank (obj)
  "occ-obj-rank")


(cl-defmethod occ-obj-calculate-rank ((obj occ-tsk))
  ;; too much output
  (occ-debug "occ-obj-calculate-rank((occ-tsk=%s))"
             (occ-obj-format (occ-obj-tsk obj) 'capitalize))
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot)
                             (occ-obj-rankprop obj (downcase-sym slot)))
                         (occ-obj-properties-to-calculate-rank obj)))))
    rank))


(cl-defmethod occ-obj-calculate-rank ((obj occ-obj-ctx-tsk))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-rank((occ-obj-ctx-tsk=%s))"
  ;;            (occ-obj-format (occ-obj-tsk obj) 'capitalize))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (reduce #'+
            (mapcar #'(lambda (slot)
                        (occ-obj-rankprop obj (downcase-sym slot)))
                    (occ-obj-properties-to-calculate-rank obj)))))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-ctx))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-avgrank(occ-ctx=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-list obj #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank objs))
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-ctx))
  ;; too much output
  (occ-debug "occ-obj-calculate-varirank(occ-ctx=%s)"
             obj)
  (let* ((objs      (occ-obj-list obj #'occ-obj-build-ctxual-tsk-with))
         (rankslist (mapcar #'occ-obj-rank objs))
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


(cl-defmethod occ-obj-calculate-avgrank ((obj occ-collection))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-avgrank(occ-collection=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-collect-list obj))
         (rankslist (mapcar #'occ-obj-rank objs))
         (avgrank   (occ-calculate-average rankslist)))
    avgrank))

(cl-defmethod occ-obj-calculate-varirank ((obj occ-collection))
  ;; too much output
  ;; (occ-debug "occ-obj-calculate-varirank(occ-collection=%s)"
  ;;            obj)
  (let* ((objs      (occ-obj-collect-list obj))
         (rankslist (mapcar #'occ-obj-rank objs))
         (varirank  (occ-calculate-variance rankslist)))
    varirank))


;; (occ-obj-avgrank (occ-obj-collection-object))
;; (occ-obj-varirank (occ-obj-collection-object))

;;; occ-rank.el ends here
