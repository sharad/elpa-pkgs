;;; occ-obj-method.el --- tsk ctsk cxtual-tsk callable method action from helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(provide 'occ-obj-method)


(require 'occ-obj-clock-method)
(require 'occ-tsk)
(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-helm-config)
(require 'occ-prop)
(require 'occ-rank)


(cl-defmethod occ-checkout ((obj occ-obj-tsk))
  (occ-op-props-checkout obj))

(defun occ-util-read-sexp-from-minibuffer (prompt)
 (car (read-from-string (read-from-minibuffer prompt))))

(cl-defmethod occ-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj obj)
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (obj) ,exp-with-obj) obj)))))
    (funcall fun)))

(cl-defmethod occ-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj-name     (occ-util-read-sexp-from-minibuffer "obj name: ")) ;prefill with obj
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (,obj-name) ,exp-with-obj) obj)))))
    (funcall fun)))

(let ((occ-debug-object nil))
  (cl-defmethod occ-set-debug-obj ((obj occ-obj-tsk))
    (setq occ-debug-object obj)
    (occ-message "Use (occ-get-debug-obj) to access object."))
  (defun occ-describe-debug-obj ()
    (interactive)
    (occ-describe-obj occ-debug-object))
  (defun occ-get-debug-obj ()
    (interactive)
    occ-debug-object))

(cl-defmethod occ-describe-obj ((obj occ-obj-tsk))
  (let ((buf (get-buffer-create (format "*helpful occ-object: %s*"
                                        (occ-format obj)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setf (buffer-string) "")
        ;; (cl-prettyprint obj)
        (insert (format "Object: %s\n\n" (occ-Format obj)))
        (insert (pp-to-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))


;; do both fast and interactive editing.
;; (occ-properties-editor obj)
(cl-defmethod occ-properties-editor-combined ((obj occ-obj-ctx-tsk))
  (let ((prompt (format "%s fast edit" (occ-Format obj))))
    (let ((helm-fast-source     (helm-build-sync-source prompt
                                  :candidates (append (occ-obj-callable-helm-actions (occ-gen-each-prop-fast-edits obj)
                                                                                     obj)
                                                      (occ-obj-callable-helm-actions (occ-gen-each-prop-fast-edits (occ-obj-tsk obj))
                                                                                     obj))
                                  :action     (list (cons "Edit"
                                                          #'(lambda (candidate-fun)
                                                              (funcall candidate-fun obj))))))
          (helm-edit-source     (helm-build-sync-source "edit"
                                  :candidates (list (cons "Edit"
                                                          (occ-lambda-with-one-arg #'occ-properties-editor)))
                                  :action     (list (cons "Edit"
                                                          #'(lambda (candidate-fun)
                                                              (funcall candidate-fun obj))))))
          (helm-checkout-source (helm-build-sync-source "other"
                                  :candidates (list (cons "Continue"
                                                          #'(lambda (arg)
                                                              t)) ;to bypass three repeat cycle of (occ-try-until ) function
                                                    (cons "Checkout"
                                                          (occ-lambda-with-one-arg #'occ-checkout)))
                                  :action     (list (cons "Edit"
                                                          #'(lambda (candidate-fun)
                                                              (funcall candidate-fun obj)))))))
      (let* ((sources (list helm-fast-source
                            helm-edit-source
                            helm-checkout-source))
             (retval  (helm-timed occ-idle-timeout nil
                        (helm :sources sources))))
        (if (eq retval t)
            t)))))

;;; occ-obj-method.el ends here
