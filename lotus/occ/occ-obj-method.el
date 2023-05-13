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

;; Contains methods callable from helm selection menu for OCC-OBJ-TSK etc.

;;; Code:

(provide 'occ-obj-method)


(eval-when-compile
  (require 'occ-macros))
(require 'occ-obj-clock-method)
(require 'occ-tsk)
(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-helm-actions-config)
(require 'occ-prop)
(require 'occ-rank)


(cl-defmethod occ-do-checkout ((obj occ-obj-tsk))
  (occ-do-op-props-checkout obj))


(defun occ-util-read-sexp-from-minibuffer (prompt)
  ;; (cl-first (read-from-string (read-from-minibuffer prompt)))
  (read--expression prompt))



(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj obj)
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (obj) ,exp-with-obj) obj)))))
    (funcall fun)))

(cl-defmethod occ-do-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj-name     (occ-util-read-sexp-from-minibuffer "obj name: ")) ;prefill with obj
                   (exp-with-obj (occ-util-read-sexp-from-minibuffer "expression with obj: ")))
               #'(lambda ()
                   (funcall
                    `(lambda (,obj-name) ,exp-with-obj) obj)))))
    (funcall fun)))

(let ((occ-debug-object nil))
  (cl-defmethod occ-do-set-debug-obj ((obj occ-obj-tsk))
    (setq occ-debug-object obj)
    (occ-message "Use (occ-get-debug-obj) to access object."))
  (defun occ-describe-debug-obj ()
    (interactive)
    (occ-do-describe-obj occ-debug-object))
  (defun occ-get-debug-obj ()
    (interactive)
    occ-debug-object))


(cl-defmethod occ-do-describe-obj ((obj occ-obj-tsk))
  (let ((buf (get-buffer-create (format "*helpful occ-object: %s*"
                                        (occ-obj-format obj)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setf (buffer-string) "")
        ;; (cl-prettyprint obj)
        (insert (format "Object: %s\n\n" (occ-obj-Format obj)))
        (insert (pp-to-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))


(cl-defmethod occ-do-display-obj ((obj occ-obj-tsk))
  (let ((buf (get-buffer-create (format "*helpful occ-object: %s*"
                                        (occ-obj-format obj)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setf (buffer-string) "")
        (insert (format "Object: %s\n\n" (occ-obj-display obj)))
        (insert (pp-to-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))


;; do both fast and interactive editing.
;; (occ-do-properties-editor obj)
(cl-defmethod occ-do-properties-editor-combined ((obj occ-obj-ctx-tsk))
  (let ((prompt (format "%s fast edit" (occ-obj-Format obj))))
    (let ((helm-fast-source     (helm-build-sync-source prompt
                                  :candidates (occ-obj-callable-helm-actions (occ-obj-gen-each-prop-fast-edits obj)
                                                                             obj)
                                  :action     (list (cons "Edit"
                                                          (occ-clouser-call-cand-on-obj obj)))))
          (helm-edit-source     (helm-build-sync-source "edit"
                                  :candidates (list (cons "Edit"
                                                          (occ-clouser-call-obj-on-cand #'occ-do-properties-editor)))
                                  :action     (list (cons "Edit"
                                                          (occ-clouser-call-cand-on-obj obj)))))
          (helm-checkout-source (helm-build-sync-source "other"
                                  :candidates (list (cons (format "Continue with same clock task %s" (occ-obj-Format obj))
                                                          (occ-clouser-call-obj-on-cand 'skip))
                                                    (cons "Try another clocking"
                                                          (occ-clouser-call-obj-on-cand 'no-action)) ;to bypass three repeat cycle of (occ-try-until ) function
                                                    (cons "Checkout"
                                                          (occ-clouser-call-obj-on-cand #'occ-do-checkout)))
                                  :action     (list (cons "Edit"
                                                          (occ-clouser-call-cand-on-obj obj))))))
      (let ((sources (list helm-checkout-source
                           helm-fast-source
                           helm-edit-source)))
        (helm-timed occ-idle-timeout nil
          (helm :sources sources))))))


(cl-defmethod occ-do-print-rank ((obj occ-obj-tsk))
  (occ-debug "Rank for %s is %d"
               (occ-obj-Format obj)
               (occ-obj-rank obj)))


(cl-defmethod occ-do-close ((obj occ-obj-tsk))
  (occ-debug "ImplementIt: mark this %s %d tsk CLOSE "
               (occ-obj-Format obj)
               (occ-obj-rank obj)))


(cl-defmethod occ-do-force-clockin ((obj occ-obj-tsk))
  (occ-debug "ImplementIt: Force this %s %d tsk clock-in for a PERIOD with period property"
               (occ-obj-Format obj)
               (occ-obj-rank obj)))

;;; occ-obj-method.el ends here
