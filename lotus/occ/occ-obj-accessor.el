;;; occ-obj-accessor.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(provide 'occ-obj-accessor)


(require 'occ-tsk)
(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-helm-actions-config)
(require 'occ-prop)
(require 'occ-rank)


(cl-defmethod occ-obj-class-name (obj)
  "unknown")

(cl-defmethod occ-obj-class-name ((obj symbol))
  "symbol")

(cl-defmethod occ-obj-class-name ((obj null))
  "null")

(cl-defmethod occ-obj-class-name ((obj marker))
  "marker")

(cl-defmethod occ-obj-class-name ((obj occ-tsk))
  "task")

(cl-defmethod occ-obj-class-name ((obj occ-ctsk))
  "context task")

(cl-defmethod occ-obj-class-name ((obj occ-ctxual-tsk))
  "contextual task")


(cl-defmethod occ-obj-obj ((obj occ-obj))
  (occ-obj-return-get-value obj))

(cl-defmethod occ-obj-obj ((obj occ-return))
  (occ-obj-return-get-value obj))


(cl-defmethod occ-obj-obj ((obj null))
  nil)


(cl-defmethod occ-obj-tsk ((obj null))
  nil)

(cl-defmethod occ-obj-tsk ((obj occ-tsk))
  obj)

;; will also cover occ-ctxual-tsk
(cl-defmethod occ-obj-tsk ((obj occ-ctsk))
  (occ-ctsk-tsk obj))

(cl-defmethod occ-obj-tsk ((obj occ-obj-ctx))
  nil)


(cl-defmethod occ-obj-ctx ((obj null))
  nil)

(cl-defmethod occ-obj-ctx ((obj occ-ctx))
  obj)

;; will also cover occ-ctxual-tsk
(cl-defmethod occ-obj-ctx ((obj occ-ctsk))
  (occ-ctsk-ctx obj))


(cl-defmethod occ-obj-marker ((obj null))
  nil)

(cl-defmethod occ-obj-marker ((obj marker))
  obj)

(cl-defmethod occ-obj-marker ((obj occ-obj-tsk))
  (occ-tsk-marker (occ-obj-tsk obj)))


(cl-defmethod occ-obj-buffer ((obj null))
  nil)

(cl-defmethod occ-obj-buffer ((obj marker))
  (marker-buffer obj))

(cl-defmethod occ-obj-buffer ((obj occ-obj-tsk))
  (occ-obj-buffer (occ-tsk-marker (occ-obj-tsk obj))))


(cl-defmethod occ-obj-callable ((callable occ-callable))
  callable)

(cl-defmethod occ-obj-callable-internal ((callable list) (type symbol))
  (let ((callable-ctor (if (eq type :normal)
                           #'occ-obj-make-callable-normal
                         (if (eq type :generator)
                             #'occ-obj-make-callable-generator
                           (occ-error "occ-obj-callable-internal: type is not one of (:normal :generator)")))))
    (let ((keyword (nth 0 (callable)))
          (name    (nth 1 (callable)))
          (fun     (nth 2 (callable))))
      (funcall callable-ctor keyword name fun))))

(cl-defmethod occ-obj-callable-normal ((callable list))
  (occ-obj-callable-internal callable
                             :normal))

(cl-defmethod occ-obj-callable-generator ((callable list))
  (occ-obj-callable-internal callable
                             :generator))

(cl-defmethod occ-obj-callable ((callable list))
  (occ-obj-callable-normal callable))

;; TODO: Consider preparing
;; (cl-defmethod occ-obj-callable-normal (xyz)
;;   (occ-error "Implement it"))

;; (cl-defmethod occ-obj-callable-generator (xyz)
;;   (occ-error "Implement it"))


(cl-defmethod occ-callable-desc     ((callable occ-callable))
  (occ-callable-name callable))

(cl-defmethod occ-obj-callable-desc ((callable occ-callable))
  (occ-callable-desc callable))

(cl-defmethod occ-obj-callable-name ((callable occ-callable))
  (occ-callable-name callable))

;; methods

(cl-defmethod occ-obj-callables ((callable list)
                                 (obj      null))
  "Return list of ((NAME . FUN) ...)"
  (mapcar #'occ-obj-callable
          callable))

(cl-defmethod occ-obj-callables ((callable list)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (occ-obj-callables callable nil))

(cl-defmethod occ-obj-callables ((callable occ-callable-normal)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (list callable))

(cl-defmethod occ-obj-callables ((callable occ-callable-generator)
                                 (obj      occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (occ-debug "occ-obj-callables(callable occ-callable-generator): got %s" (occ-callable-desc callable))
  (let ((fun (occ-callable-fun callable)))
    (let ((callables (funcall fun obj
                              :param-only nil)))
      ;; (cl-assert callables)
      (dolist (x callables)
        (occ-debug "occ-obj-callables(callable occ-callable-generator): generated %s" (occ-callable-desc x)))
      (cl-assert (cl-every #'occ-callable-p
                           callables))
      (cl-assert (cl-notany #'occ-callable-generator-p
                            callables))
      callables)))

;; methods

(cl-defmethod occ-obj-callable-helm-action ((callable occ-callable))
  "Return pair or (NAME . FUN)"
  (cons (occ-callable-name callable)
        (occ-callable-fun  callable)))

;; methods

(cl-defmethod occ-obj-callable-helm-actions ((callables list)
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let ((callables-list (occ-obj-callables callables obj)))
    (cl-assert (cl-every #'occ-callable-normal-p callables-list))
    (cl-assert (cl-notany #'occ-callable-generator-p callables-list))
    (mapcar #'occ-obj-callable-helm-action
            callables-list)))

(cl-defmethod occ-obj-callable-helm-actions ((callable occ-callable)
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let ((callables (occ-obj-callables callable
                                      obj)))
    (occ-obj-callable-helm-actions callables)))

(cl-defmethod occ-obj-callable-helm-actions ((callables (head :callables))
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let ((callables (rest callables)))
    (occ-obj-callable-helm-actions callables)))

(cl-defmethod occ-obj-callable-helm-actions ((callables (head :keywords))
                                             (obj occ-obj))
  "Return list of ((NAME . FUN) ...)"
  (let* ((keywords  (rest callables))
         (callables (occ-helm-callables-get keywords)))
    (occ-obj-callable-helm-actions callables)))


;; TODO: Consider preparing
(cl-defmethod occ-obj-ap (xyz)
  (occ-error "Implement it"))

(cl-defmethod occ-obj-ap-normal (xyz)
  (occ-error "Implement it"))

(cl-defmethod occ-obj-ap-transf (xyz)
  (occ-error "Implement it"))


(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap))
  (occ-ap-tree-keybranch ap-obj))

(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap-normal))
  (let ((base (cl-call-next-method)))
    (or base
        (let ((callables (occ-ap-normal-callables ap-obj)))
          (when callables
            (cons :callables callables))))))

(cl-defmethod occ-obj-ap-base ((ap-obj occ-ap-transf))
  (let ((base (cl-call-next-method)))
    (or base
        (let ((transform (occ-ap-normal-transform ap-obj)))
          (when transform
            (cons :transform transform))))))

(cl-defmethod occ-obj-ap-tree-keybranch ((ap-obj occ-ap)
                                         (obj    occ-obj))
  (unless (occ-ap-tree-keybranch ap-obj)
    (occ-error "occ-ap obj %s missing tree-keybranch %s" ap-obj (occ-ap-tree-keybranch ap-obj)))
  (occ-ap-tree-keybranch ap-obj))

(cl-defmethod occ-obj-ap-callables ((ap-obj occ-ap-normal)
                                    (obj occ-obj))
  (occ-debug "occ-obj-ap-callables: ap-obj = %s" ap-obj)

  ;; NOTE:
  ;; If TREE-KEYBRANCH are present then callable must have to generated afresh every time
  ;; this method is called.
  (unless (and (not (occ-ap-tree-keybranch ap-obj))
               (occ-ap-normal-callables ap-obj))
    (occ-debug "Called")
    (let ((tree-keybranch (occ-obj-ap-tree-keybranch ap-obj obj)))
      (let* ((keywords-list (occ-get-keywords-list-from-tree tree-keybranch))
             (callables     (occ-obj-get-callables obj ;; ???
                                                   keywords-list)))
        (unless keywords-list
          (occ-error "for tree-keybranch %s empty keywords-list %s returned "
                     tree-keybranch
                     keywords-list))
        (when tree-keybranch
          (cl-assert callables)
          (cl-assert keywords-list))
        (setf (occ-ap-normal-callables ap-obj) callables))))
  (occ-ap-normal-callables ap-obj))

(cl-defmethod occ-obj-ap-transform ((ap-obj occ-ap-transf))
  "This return callables"
  (unless (occ-ap-transf-transform ap-obj)
    (let ((transform #'(lambda (action
                                candidate)
                         (occ-debug "occ-obj-ap-transform: lambda: ap-obj = %s" ap-obj)
                         (let* ((candidate-obj (occ-obj-obj candidate))
                                (callables (occ-obj-ap-callables ap-obj
                                                                 (occ-obj-obj candidate-obj))))
                           (cl-assert callables)
                           (occ-debug "occ-obj-ap-transform: lambda: transform: callables = %s" callables)
                           (occ-obj-make-ap-normal (cons :callables callables))))))
      (occ-debug "occ-obj-ap-transform: setting transform tp %s" transform)
      (setf (occ-ap-transf-transform ap-obj) transform)))
  (occ-ap-transf-transform ap-obj))


(cl-defmethod occ-obj-ap-helm-actions ((ap-obj list)
                                       (obj occ-obj))
  (let* ((ap-obj    (occ-obj-build-ap-normal ap-obj obj))
         (callables (occ-obj-ap-callables ap-obj obj)))
    (occ-obj-callable-helm-actions callables
                                   obj)))

(cl-defmethod occ-obj-ap-helm-actions ((ap-obj occ-ap-normal)
                                       (obj occ-obj))
  (let ((callables (occ-obj-ap-callables ap-obj obj)))
    (occ-obj-callable-helm-actions callables
                                   obj)))

(cl-defmethod occ-obj-ap-helm-actions ((ap-obj occ-ap-transf)
                                       (obj occ-obj))
  (occ-error "OCC-OBJ-AP-HELM-ACTIONS can not work for OCC-AP-TRANSF as it requires OCC-AP-NORMAL to run TRANSFORMATION function"))


(cl-defmethod occ-obj-ap-helm-transformation ((ap-obj occ-ap-transf))
  (let ((transform (occ-obj-ap-transform ap-obj)))
    (cl-assert transform)
    #'(lambda (action
               candidate)
        (occ-debug "occ-obj-ap-helm-transformation: lambda: transform = %s" transform)
        (cl-assert transform)
        (let* ((candidate-obj (occ-obj-obj candidate))
               (ap-normal-obj (funcall transform
                                       action
                                       candidate-obj)))
          (cl-assert (occ-ap-normal-p ap-normal-obj))
          (occ-debug "helm-transformation: got ap-normal-obj = %s" ap-normal-obj)
          (let ((helm-actions (occ-obj-ap-helm-actions ap-normal-obj
                                                       candidate-obj)))
            (cl-assert helm-actions)
            (dolist (a helm-actions)
              (occ-debug "occ-obj-ap-helm-transformation: helm-action: %s" (prin1-to-string a)))
            (when helm-actions
              (cl-assert (cl-every #'(lambda (x)
                                       (functionp (rest x)))
                                   helm-actions)))
            (occ-debug "occ-obj-ap-helm-transformation: lambda: helm-actions %s" helm-actions)
            helm-actions)))))


(cl-defmethod occ-obj-ap-helm-transformed-actions ((apn occ-ap-normal)
                                                   (apt occ-ap-transf)
                                                   (obj occ-obj))
  (let ((callables (occ-obj-callable-helm-actions (occ-obj-ap-callables apn obj)
                                                  obj))
        (fun       (occ-obj-ap-helm-transformation apt)))
    (funcall fun callables obj)))


(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn occ-ap-normal)
                                           (apt occ-ap-transf))
  (occ-obj-ap-helm-transformed-actions apn apt obj))

(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn occ-ap-normal)
                                           (apt null))
  (occ-obj-ap-helm-actions apn obj))

(cl-defmethod occ-obj-ap-helm-get-actions ((obj occ-obj)
                                           (apn null)
                                           (apt occ-ap-transf))
  (occ-error "test"))


(cl-defmethod occ-obj-ap-helm-item ((ap-obj occ-ap-normal)
                                    (obj occ-obj))
  "Return actions"
  (occ-obj-ap-helm-actions ap-obj obj))

(cl-defmethod occ-obj-ap-helm-item ((ap-obj occ-ap-transf)
                                    (obj occ-obj))
  "Return lambda function which do transformation on actions and return actions"
  (occ-obj-ap-helm-transformation ap-obj))


(cl-defmethod occ-obj-heading-marker ((obj null))
  (make-marker))

(cl-defmethod occ-obj-heading-marker ((obj marker))
  (save-excursion
    (with-current-buffer (marker-buffer obj)
      (goto-char obj)
      (end-of-line)
      (outline-previous-heading)
      (point-marker))))

(cl-defmethod occ-obj-heading-marker ((obj occ-obj-tsk))
  (occ-obj-heading-marker (occ-obj-marker obj)))


;; occ-tsk - accessors
(cl-defmethod occ-obj-rank ((obj occ-tsk))
  (occ-debug "occ-obj-rank(occ-tsk=%s)" (occ-obj-Format obj))
  (let ((rank (occ-tsk-rank obj)))
    (unless rank
      (setf (occ-tsk-rank obj) (occ-obj-calculate-rank obj)))
    (occ-debug "occ-obj-rank((obj occ-tsk)) rank = %s" rank)
    (occ-tsk-rank obj)))

(cl-defmethod (setf occ-obj-rank) (value (obj occ-tsk))
  (occ-debug "setf occ-obj-rank(occ-tsk=%s)" (occ-obj-Format obj))
  (setf (occ-tsk-rank obj) value))


;; occ-ctsk - accessors
(cl-defmethod occ-obj-rank ((obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctsk-tsk obj)))
    (occ-obj-rank tsk)))

(cl-defmethod (setf occ-obj-rank) (value (obj occ-ctsk))
  (occ-debug "occ-obj-rank(occ-ctsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctsk-tsk obj)))
    (setf (occ-obj-rank tsk) rank)))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-obj-rank ((obj occ-ctxual-tsk))
  (occ-debug "occ-obj-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((rank (occ-ctxual-tsk-rank obj)))
    (unless rank
      (setf (occ-ctxual-tsk-rank obj) (occ-obj-calculate-rank obj)))
    (occ-debug "occ-obj-rank((obj occ-ctxual-tsk)) rank = %s" rank)
    (occ-ctxual-tsk-rank obj)))

(cl-defmethod (setf occ-obj-rank) (value (obj occ-ctxual-tsk))
  (occ-debug "occ-obj-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (setf (occ-ctxual-tsk-rank obj) value))


(cl-defmethod occ-obj-member-tsk-rank ((obj occ-ctxual-tsk))
  (occ-debug "occ-obj-member-tsk-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (occ-debug "occ-obj-member-tsk-rank(occ-ctxual-tsk=%s)" (occ-obj-Format (occ-obj-tsk obj)))
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    (occ-obj-rank tsk)))


;; occ-tsk - accessors
(cl-defmethod occ-obj-format-string ((obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (let ((format-string (occ-tsk-format-string obj)))
    (unless format-string
      (setf (occ-tsk-format-string obj) (occ-obj-build-format-string obj)))
    (occ-tsk-format-string obj)))

(cl-defmethod (setf occ-obj-format-string) (value (obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-string obj) value))


;; occ-tsk - accessors
(cl-defmethod occ-obj-format-file ((obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (let ((format-file (occ-tsk-format-file obj)))
    (unless format-file
      (setf (occ-tsk-format-file obj) (occ-obj-build-format-file obj)))
    (occ-tsk-format-file obj)))

(cl-defmethod (setf occ-obj-format-file) (value (obj occ-tsk))
  ;; (occ-debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-file obj) value))


;; occ-ctx - accessors
(cl-defmethod occ-obj-avgrank ((obj occ-ctx))
  (occ-debug "occ-obj-avgrank(occ-ctx=%s)" obj)
  (let ((avgrank (occ-ctx-avgrank obj)))
    (unless avgrank
      (setf (occ-ctx-avgrank obj) (occ-obj-calculate-avgrank obj)))
    (occ-ctx-avgrank obj)))

(cl-defmethod (setf occ-obj-avgrank) (value (obj occ-ctx))
  (occ-debug "occ-obj-avgrank(occ-ctx=%s)" obj)
  (setf (occ-ctx-avgrank obj) value))


;; occ-ctx - accessors
(cl-defmethod occ-obj-varirank ((obj occ-ctx))
  (occ-debug "occ-obj-varirank(occ-ctx=%s)" obj)
  (let ((varirank (occ-ctx-varirank obj)))
    (unless varirank
      (setf (occ-ctx-varirank obj) (occ-obj-calculate-varirank obj)))
    (occ-ctx-varirank obj)))

(cl-defmethod (setf occ-obj-varirank) (value (obj occ-ctx))
  (occ-debug "occ-obj-varirank(occ-ctx=%s)" obj)
  (setf (occ-ctx-varirank obj) value))


;; occ-collection - accessors
(cl-defmethod occ-obj-avgrank ((obj occ-collection))
  (occ-debug "occ-obj-avgrank(occ-collection=%s)" obj)
  (let ((avgrank (occ-collection-avgrank obj)))
    (unless avgrank
      (setf (occ-collection-avgrank obj) (occ-obj-calculate-avgrank obj)))
    (occ-collection-avgrank obj)))

(cl-defmethod (setf occ-obj-avgrank) (value (obj occ-collection))
  (occ-debug "occ-obj-avgrank(occ-collection=%s)" obj)
  (setf (occ-collection-avgrank obj) value))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-obj-varirank ((obj occ-collection))
  (occ-debug "occ-obj-varirank(occ-collection=%s)" obj)
  (let ((varirank (occ-collection-varirank obj)))
    (unless varirank
      (setf (occ-collection-varirank obj) (occ-obj-calculate-varirank obj)))
    (occ-collection-varirank obj)))

(cl-defmethod (setf occ-obj-varirank) (value (obj occ-collection))
  (occ-debug "occ-obj-varirank(occ-collection=%s)" obj)
  (setf (occ-collection-varirank obj) value))


(cl-defgeneric occ-obj-candidate (obj)
  "occ-obj-candidate")

(cl-defmethod occ-obj-candidate
  ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (cons (occ-obj-format obj nil t)
        obj))

(cl-defmethod occ-obj-candidate ((obj occ-obj-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-obj-format obj nil t)
        obj))


;; find place to put these all function

(cl-defmethod occ-do-checkout ((obj occ-obj-tsk))
  (occ-do-op-props-checkout obj))

;; BUG  in Menu
;;           "Set debug obj"
;;           "Call with obj"
;;       occ-return getting passed

(defun occ-util-read-sexp-from-minibuffer (prompt)
 (first (read-from-string (read-from-minibuffer prompt))))

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
    (occ-debug "Use (occ-get-debug-obj) to access object."))
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
        (insert (format "Object: %s\n\n"
                        (occ-obj-Format obj)))
        (insert (pp-to-string obj)))
      (read-only-mode 1))
    (switch-to-buffer-other-window buf)))


(defun occ-current-ctxual-tsk (&optional occ-other-allowed)
  (let* ((ctxual-tsk (first *occ-clocked-ctxual-tsk-ctx-history*)))
    (let ((clock-marker    (occ-valid-marker org-clock-marker))
          (clock-hd-marker (occ-valid-marker org-clock-hd-marker)))
      (let ((clock (or clock-marker
                       clock-hd-marker)))
        (if (and clock
                 ctxual-tsk
                 (occ-obj-marker= ctxual-tsk
                                  clock))
            ctxual-tsk
          (when clock
            (let ((msg (if ctxual-tsk
                           (format "occ-current-ctxual-tsk: %s from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s"
                                   (occ-obj-Format ctxual-tsk nil t)
                                   (occ-obj-Format clock nil t))
                         (format "occ-current-ctxual-tsk: %s is outside of occ"
                                 (occ-obj-Format clock nil t)))))
              (if occ-other-allowed
                  (occ-debug :warning msg)
                (occ-error msg))
              (occ-obj-build-ctxual-tsk-with (and clock
                                                  (occ-obj-make-tsk clock))
                                             (occ-obj-make-ctx-at-point)))))))))

(defun occ-current-tsk (&optional occ-other-allowed)
  (let ((curr-ctxual-tsk (occ-current-ctxual-tsk occ-other-allowed))) ;recursion
    (when curr-ctxual-tsk
      (occ-obj-tsk curr-ctxual-tsk))))


(defun occ-current-tsk (&optional occ-other-allowed)
  (let* ((ctxual-tsk (first *occ-clocked-ctxual-tsk-ctx-history*)))
    (let ((clock-marker    (occ-valid-marker org-clock-marker))
          (clock-hd-marker (occ-valid-marker org-clock-hd-marker)))
      (let ((clock (or clock-marker
                       clock-hd-marker)))
        (if (and ctxual-tsk
                 clock
                 (occ-obj-marker= ctxual-tsk clock))
            (occ-obj-tsk ctxual-tsk)
          (when clock
            (let ((msg (if ctxual-tsk
                           (format "occ-current-ctxual-tsk: %s from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s"
                                   (occ-obj-Format ctxual-tsk nil t)
                                   (occ-obj-Format clock nil t))
                         (format "occ-current-ctxual-tsk: %s is outside of occ"
                                 (occ-obj-Format clock nil t)))))
              (if occ-other-allowed
                  (occ-debug :warning msg)
                (occ-error msg))
              (if clock
                   (occ-obj-make-tsk clock)))))))))


(defun occ-default-collection (&optional noerror)
  (let ((key (occ-collector-default-key)))
    (unless (occ-collector-roots key)
      (if (occ-collector-spec key)
          (progn
            ;; (occ-obj-make-collection (occ-collector-spec key))
            (occ-obj-tsks (occ-collector-get key) t))
        (progn
          (occ-uninsinuate)
          (occ-debug "(occ-collector-spec key) is nil, set it using M-x occ-obj-build-spec or set (occ-collector-spec key), disabled occ")
          (unless noerror
            (occ-error "(occ-collector-spec key) is nil, set it using M-x occ-obj-build-spec or set (occ-collector-spec key), disabled occ")))))
    (occ-collector-get key)))


(cl-defmethod occ-do-reset-tsks ((collection null))
  ;; (occ-message "null")
  nil)

(cl-defmethod occ-do-reset-tsks ((collection occ-tree-collection))
  ;; (occ-message "tree %s" (occ-tree-collection-list collection))
  (setf (occ-list-collection-list collection) nil)
  (setf (occ-tree-collection-tree collection) nil))

(cl-defmethod occ-do-reset-tsks ((collection occ-list-collection))
  (setf (occ-list-collection-list collection) nil))

;; To deprecate
;; global-object - accessors
(cl-defmethod occ-obj-collect-tsks (collection
                                    &optional
                                    force)
  (occ-error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

;; FIND: what it mean by tsks collection-tree are same ?
(cl-defmethod occ-obj-collect-tsks ((collection occ-tree-collection)
                                    &optional
                                    force)
  (unless (and (not force)
               (occ-tree-collection-tree collection))
    (setf (occ-tree-collection-tree collection) (occ-obj-build-tsks collection)))
  (occ-tree-collection-tree collection))

(cl-defmethod occ-obj-collect-tsks ((collection occ-list-collection)
                                    force)
  (unless (and (not force)
               (occ-list-collection-list collection))
    (setf (occ-list-collection-list collection) (occ-obj-build-tsks collection)))
  (occ-list-collection-list collection))
;; To deprecate

(cl-defmethod occ-obj-tsks (collection
                            &optional
                            force)
  (occ-error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

;; FIND: what it mean by tsks collection-tree are same ?
(cl-defmethod occ-obj-tsks ((collection occ-tree-collection)
                            &optional
                            force)
  (occ-obj-collect-tsks collection force))

(cl-defmethod occ-obj-tsks ((collection occ-list-collection)
                            force)
  (occ-obj-collect-tsks collection force))


(cl-defmethod occ-obj-collection-tsks ((collection occ-tree-collection))
  (unless (occ-tree-collection-tree collection)
    (occ-obj-tsks collection nil)
    (run-hooks '*occ-collection-change-hook*))
  (occ-tree-collection-tree collection))

(cl-defmethod occ-obj-collection-tsks ((collection occ-list-collection))
  (unless (occ-list-collection-list collection)
    (occ-obj-tsks collection nil)
    (run-hooks '*occ-collection-change-hook*))
  (occ-list-collection-list collection))





(cl-defmethod occ-obj-collect-files ((collection occ-tree-collection)
                                     &optional
                                     force)
  (unless (occ-tree-collection-files collection)
    ;; (occ-obj-collect-tsks collection nil)
    (let ((occ-files (let ((tsks  (occ-obj-collection-tsks collection))
                           (files '()))
                       (mapc #'(lambda (tsk)
                                 (occ-mapc-tree-tsks #'(lambda (tsk args)
                                                         (push (occ-tsk-file tsk) files))
                                                     tsk
                                                     nil))
                             tsks)
                       files)))
       (setf (occ-tree-collection-files collection) (remove nil (delete-dups occ-files)))))
  (occ-tree-collection-files collection))

(cl-defmethod occ-obj-collect-files ((collection occ-list-collection)
                                     &optional
                                     force)
  (unless (occ-list-collection-files collection)
    (setf
     (occ-list-collection-files collection)
     (occ-list-collection-roots collection)))
  (occ-list-collection-files collection))

(cl-defmethod occ-obj-files ()
  (occ-obj-collect-files (occ-default-collection)))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-obj-list (obj
                             &key
                             builder
                             obtrusive)
  "occ-obj-list")

(cl-defmethod occ-obj-list ((obj occ-ctx)
                            &key
                            builder
                            obtrusive)
  "return CTXUAL-TSKs container"
  (occ-obj-list-with obj
                     (occ-default-collection)
                     :builder   builder
                     :obtrusive obtrusive))

(cl-defmethod occ-obj-list ((obj null)
                            &key
                            builder
                            obtrusive)
  "return TSKs container"
  (occ-obj-list (occ-obj-make-ctx-at-point)
                :builder   builder
                :obtrusive obtrusive))

(cl-defmethod occ-obj-list ((collection occ-tree-collection)
                            &key
                            builder
                            obtrusive)
  (unless (occ-tree-collection-list collection)
    (let ((tsks     (occ-obj-collection-tsks collection))
          (tsk-list '()))
      (mapc #'(lambda (tsk)
                (occ-mapc-tree-tsks #'(lambda (subtsk args)
                                        (setf tsk-list (nconc tsk-list (list subtsk))))
                                    tsk
                                    nil))
            tsks)
      (setf (occ-tree-collection-list collection)
            tsk-list)))
  (occ-tree-collection-list collection))

(cl-defmethod occ-obj-list ((collection occ-list-collection)
                            &key
                            builder
                            obtrusive)
  (let ((tsks (occ-obj-collection-tsks collection)))
    tsks))

;; ;; to deprecate
;; (cl-defmethod occ-obj-collect-list ((collection occ-tree-collection))
;;   (unless (occ-tree-collection-list collection)
;;     (let ((tsks     (occ-obj-collection-tsks collection))
;;           (tsk-list '()))
;;       (mapc #'(lambda (tsk)
;;                 (occ-mapc-tree-tsks #'(lambda (subtsk args)
;;                                         (setf tsk-list (nconc tsk-list (list subtsk))))
;;                                     tsk
;;                                     nil))
;;             tsks)
;;       (setf (occ-tree-collection-list collection) tsk-list)))
;;   (occ-tree-collection-list collection))

;; (cl-defmethod occ-obj-collect-list ((collection occ-list-collection))
;;   (let ((tsks (occ-obj-collection-tsks collection)))
;;     tsks))




(cl-defmethod occ-obj-list-with ((obj        occ-ctx)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return CTSKs list"
  (let ((builder (or builder
                     #'occ-obj-build-ctsk-with)))
    (let ((ctsks (occ-run-unobtrusively obtrusive
                   (let ((tsks (occ-obj-list collection))) ;;????TODO
                     (when tsks
                       (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                               tsks))))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it tpo occ-select occ-do-clock-in
        (occ-debug "Busy user input `%s'"
                     (if (numberp last-input-event)
                         (single-key-description last-input-event)
                       last-input-event))
        ctsks))))

;; BUG: For now, facing some STRUCK issue with occ-run-unobtrusively
(cl-defmethod occ-obj-list-with ((obj        occ-ctx)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return CTSKs list"
  (let ((builder (or builder
                     #'occ-obj-build-ctsk-with)))
    (let ((ctsks (let ((tsks (occ-obj-list collection))) ;;????TODO
                   (when tsks
                     (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                             tsks)))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it top occ-select occ-do-clock-in
        (occ-debug "Busy user input `%s'"
                     (if (numberp last-input-event)
                         (single-key-description last-input-event)
                       last-input-event))
        ctsks))))

(cl-defmethod occ-obj-list-with ((obj null)
                                 (collection occ-collection)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  (occ-obj-list-with (occ-obj-make-ctx-at-point)
                     (occ-obj-collection collection)
                     :builder   builder
                     :obtrusive obtrusive))

(cl-defmethod occ-obj-list-with ((obj occ-ctx)
                                 (collection null)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  nil)

(cl-defmethod occ-obj-list-with ((obj null)
                                 (collection null)
                                 &key
                                 builder
                                 obtrusive)
  "return TSKs container"
  nil)

(cl-defmethod occ-obj-length ((collection symbol))
  (length (occ-obj-list (occ-obj-collection collection))))

(cl-defmethod occ-obj-length ((collection occ-collection))
  (length (occ-obj-list (occ-obj-collection collection))))

;; ;; To deprecate
;; (cl-defmethod occ-obj-collection-obj-list ((collection occ-collection)
;;                                            (obj occ-ctx)
;;                                            &key
;;                                            builder
;;                                            obtrusive)
;;   "return CTSKs list"
;;   (let ((builder (or builder
;;                      #'occ-obj-build-ctsk-with)))
;;     (let ((ctsks (occ-run-unobtrusively obtrusive
;;                    (let ((tsks (occ-obj-collect-list collection))) ;;????TODO
;;                      (when tsks
;;                        (mapcar #'(lambda (tsk) (funcall builder tsk obj))
;;                                tsks))))))
;;       (unless (eq t ctsks)
;;         ;; BUG: TODO: convey it tpo occ-select occ-do-clock-in
;;         (occ-debug "Busy user input `%s'"
;;                      (if (numberp last-input-event)
;;                          (single-key-description last-input-event)
;;                        last-input-event))
;;         ctsks))))

;; ;; FOr now.
;; (cl-defmethod occ-obj-collection-obj-list ((collection occ-collection)
;;                                            (obj occ-ctx)
;;                                            &key
;;                                            builder
;;                                            obtrusive)
;;   "return CTSKs list"
;;   (let ((builder (or builder
;;                      #'occ-obj-build-ctsk-with)))
;;     (let ((ctsks (let ((tsks (occ-obj-collect-list collection))) ;;????TODO
;;                    (when tsks
;;                      (mapcar #'(lambda (tsk) (funcall builder tsk obj))
;;                              tsks)))))
;;       (unless (eq t ctsks)
;;         ;; BUG: TODO: convey it top occ-select occ-do-clock-in
;;         (occ-debug "Busy user input `%s'"
;;                      (if (numberp last-input-event)
;;                          (single-key-description last-input-event)
;;                        last-input-event))
;;         ctsks))))

;; (cl-defmethod occ-obj-collection-obj-list ((collection occ-collection)
;;                                            (obj null)
;;                                            &key
;;                                            builder
;;                                            obtrusive)
;;   "return CTSKs list"
;;   (occ-obj-collection-obj-list collection
;;                                (occ-obj-make-ctx-at-point)
;;                                :builder   builder
;;                                :obtrusive obtrusive))


;; (cl-defmethod occ-obj-collection-obj-list ((collection null)
;;                                            (obj occ-ctx)
;;                                            &key
;;                                            builder
;;                                            obtrusive)
;;   "return CTSKs list"
;;   nil)


(cl-defmethod occ-obj-select-obj ((source null))
  nil)

(cl-defmethod occ-ob-select-obj ((source occ-hsrc))
  (occ-hsrc-obj source))

(cl-defmethod occ-obj-obj ((source occ-hsrc))
  (occ-hsrc-obj source))

;;; occ-obj-accessor.el ends here
