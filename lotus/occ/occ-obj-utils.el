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


(defvar occ-return-select-label     :occ-selected    "should not be null")
(defvar occ-return-quit-label       :occ-nocandidate "should not be null")
(defvar occ-return-nocndidate-label :occ-quitted     "should not be null")
(defvar occ-return-timeout-label    :occ-timeout     "should not be null") ;TODO: need to implement.
(defvar occ-return-true-label       :occ-true        "should not be null")
(defvar occ-return-false-label      :occ-false       "should not be null")

(cl-assert occ-return-select-label)
(cl-assert occ-return-quit-label)
(cl-assert occ-return-nocndidate-label)
(cl-assert occ-return-true-label)
(cl-assert occ-return-false-label)

(defvar occ-return-select-function #'identity)
(defvar occ-return-select-name     "Select")
(cl-assert occ-return-select-function )
(cl-assert occ-return-select-name     )


(occ-testing                            ;BAckup
 
 (defun occ-build-return-lambda (action &optional label)
   #'(lambda (candidate)
       (let* ((value (funcall action candidate))
              (label (or label
                         (if value occ-return-true-label occ-return-false-label))))
         (occ-make-return label value))))

 (defun occ-return-tranform (action)
   "Will make all action except first to return OCC-RETURN-SELECT-LABEL."
   (let ((identity-selector (cons occ-return-select-name ;add default select operation.
                                  (occ-build-return-lambda occ-return-select-function
                                                           occ-return-select-label)))
         (action-launcher   (mapcar #'(lambda (a)
                                        (if (consp a)
                                            (cons (car a)
                                                  (occ-build-return-lambda (cdr a)))
                                          (occ-build-return-lambda a)))
                                    action)))
     (cons identity-selector action-launcher)))

 (defun occ-return-tranformer-fun-transform (tranformer-fun)
   "Will make transformer fun to change action except first to return occ-return-label."
   #'(lambda (action
              candidate)
       (occ-return-tranform (funcall tranformer-fun
                                     action candidate)))))










(cl-defmethod occ-build-return-lambda ((callable occ-callable-normal)
                                       &optional label)
  (let ((newcallable #'(lambda (candidate)
                         (let ((fun (occ-callable-fun callable))))
                         (let* ((value (funcall fun candidate))
                                (label (or label
                                           (if value
                                               occ-return-true-label
                                             occ-return-false-label))))
                           (occ-make-return label value)))))
    (occ-make-callable-normal (occ-callable-keyword callable)
                              (occ-callable-name callable)
                              newcallable)))
(cl-defmethod occ-build-return-lambda ((callable occ-callable-transf)
                                       &optional label)
  (occ-error "Can not use occ-callable-transf %s" callable))

(cl-defmethod occ-return-tranform ((ap-obj occ-ap-normal))
  "Will make all action except first to return OCC-RETURN-SELECT-LABEL."
  (let* ((identity-sel-callable            (occ-make-callable-normal :select
                                                                     occ-return-select-name
                                                                     occ-return-select-function))
         (identity-sel-ret-lambda-callable (occ-build-return-lambda identity-sel-callable
                                                                    occ-return-select-label))
         (new-callables                    (cons identity-selector-ret-lambda-callable
                                                 (mapcar #'occ-build-return-lambda
                                                         (occ-obj-ap-callables ap-obj)))))
    (occ-make-ap-normal (cons :callables
                              new-callables))))

 (cl-defmethod occ-return-tranformer-fun-transform ((ap-transf-obj occ-ap-transf))
   "Will make transformer fun to change action except first to return occ-return-label."
   #'(lambda (action
              candidate)
       (let* ((fun           (occ-ap-transf-transform ap-transf-obj))
              (ap-normal-obj (funcall fun
                                      action candidate)))
         (occ-return-tranform ap-normal-obj))))

;; (cl-defmethod occ-return-operate-p (retval)
;;   retval)

;; (cl-defmethod occ-return-operate-p ((retval occ-return))
;;   (eq
;;    occ-return-operate-label
;;    (occ-return-label retval)))

(cl-defmethod occ-return-in-labels-p (retval &rest label)
  retval)

(cl-defmethod occ-return-in-labels-p ((retval occ-return) &rest label)
  (memq (occ-return-label retval)
        label))

;; (cl-defmethod occ-return-operate-p ((retval null))
;;   nil)

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
