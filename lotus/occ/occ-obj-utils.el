;;; occ-obj-utils.el --- object utils                -*- lexical-binding: t; -*-

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

(provide 'occ-obj-utils)


(require 'occ-obj)


;; method
(fmakunbound 'occ-return-tranform)

(cl-defmethod occ-return-tranform ((ap-obj occ-ap-normal)
                                   (obj occ-obj))
  "Will make all action except first to return OCC-RETURN-SELECT-LABEL."
  ;; (occ-message "(occ-obj-ap-callables ap-obj obj) = %s" (car (occ-obj-ap-callables ap-obj obj)))
  (let* ((identity-sel-callable            (occ-make-callable-normal :select
                                                                     occ-return-select-name
                                                                     occ-return-select-function))
         (identity-sel-ret-lambda-callable (occ-build-return-lambda identity-sel-callable
                                                                    occ-return-select-label))
         (callables                        (occ-obj-ap-callables ap-obj obj))
         (new-callables                    (cons identity-sel-ret-lambda-callable
                                                 (mapcar #'occ-build-return-lambda
                                                         ;; #'(lambda (c)
                                                         ;;     (occ-build-return-lambda c occ-return-evaluate))
                                                         callables)))) ;;BUG to fix
    (cl-assert callables)
    (occ-build-ap-normal (cons :callables
                               new-callables))))

(cl-defmethod occ-return-tranform ((ap-transf-obj occ-ap-transf)
                                   (obj occ-obj))
  "Will make transformer fun to change action except first to return occ-return-label."
  (let ((new-transform #'(lambda (action
                                  candidate)
                           (let ((transform (occ-obj-ap-transform ap-transf-obj)))
                             (cl-assert transform)
                             (let* ((candidate-obj (occ-return-get-value candidate))
                                    (ap-normal-obj (funcall transform
                                                            action candidate-obj)))
                               (occ-debug :debug "occ-return-tranform: lambda: ap-normal-obj = %s" ap-normal-obj)
                               ;; (occ-return-tranform ap-normal-obj obj)
                               (occ-return-tranform ap-normal-obj candidate-obj))))))
    (occ-build-ap-transf (cons :transform
                               new-transform))))


(cl-defmethod occ-return-in-labels-p (retval &rest label)
  retval)

(cl-defmethod occ-return-in-labels-p ((retval occ-return) &rest label)
  (memq (occ-return-label retval)
        label))


(cl-defmethod occ-return-get-value (retval)
  retval)

(cl-defmethod occ-return-get-value ((retval occ-return))
  (occ-return-value retval))


(cl-defmethod occ-return-get-label (retval)
  retval)

(cl-defmethod occ-return-get-label ((retval occ-return))
  (occ-return-label retval))


(defun occ-specs ()
  (cl-method-param-case
   '(occ-make-tsk-collection (`((head ,val)) val))))

(defun occ-valid-spec-p (spec)
  t)

;;; occ-obj-utils.el ends here
