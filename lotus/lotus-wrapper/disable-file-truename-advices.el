;;; disable-file-truename-advices.el --- file truename advices  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

;; Author: s <>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(provide 'disable-file-truename-advices)


(defvar disable-file-truename-advice-prefix "disable-file-truename--")

;;;###autoload
(defun disable-file-truename-ad--callers-define-around-advice (f)
  (let ((fun (intern (concat disable-file-truename-advice-prefix (symbol-name f)))))
    (unless (fboundp fun)
      (eval `(defun ,fun (oldfun &rest r)
               (cl-flet ((file-truename (&rest args) (apply #'identity args)))
                 (apply oldfun r)))))))
;;;###autoload
(defun disable-file-truename-ad--callers-add-around-advice (f)
  (let ((fun (intern (concat disable-file-truename-advice-prefix (symbol-name f)))))
    (eval `(add-function :around
                         (symbol-function ',f)
                         #',fun))))
;;;###autoload
(defun disable-file-truename-ad--callers-remove-around-advice (f)
  (let ((fun (intern (concat disable-file-truename-advice-prefix (symbol-name f)))))
    (eval `(remove-function (symbol-function ',f)
                            #',fun))))


;;;###autoload
(defun disable-file-truename-ad--set-advices (lib fns)
  (with-eval-after-load lib
    (dolist (fn fns)
      (disable-file-truename-ad--callers-define-around-advice fn)
      (disable-file-truename-ad--callers-add-around-advice fn))))

;;;###autoload
(defun disable-file-truename-ad--unset-advices (lib fns)
  (with-eval-after-load lib
    (dolist (fn fns)
      (disable-file-truename-ad--callers-remove-around-advice fn))))

;;; disable-file-truename-advices.el ends here
