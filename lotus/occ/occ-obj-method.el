;;; occ-obj-method.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  Sharad Pratap

;; Author: Sharad Pratap
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

(provide 'occ-obj-method)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'timer-utils-lotus)


(require 'occ-cl-utils)
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-obj-simple)
(require 'occ-debug-method)
(require 'occ-unnamed)
(require 'occ-helm)


(defcustom *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar    *occ-buff-sel-timer*                nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)


(cl-defmethod occ-ctxual-current-tsk ((obj occ-ctx))
  (let ((curr-tsk (occ-current-tsk)))
    (when curr-tsk
      (occ-build-ctxual-tsk-with curr-tsk obj))))

(cl-defmethod occ-clock-unassociated-p ((obj occ-ctx))
  (or
   (occ-clock-marker-unnamed-clock-p)
   ;; TODO: BUG: Here provide option to user in case of non-unnamed tsk to
   ;; increase time prop or other prop or continue to other clock. or
   ;; force checkout for clock.
   (not (occ-associable-p (occ-ctxual-current-tsk obj)))))

(cl-defmethod occ-edit-until-associable ((obj occ-ctxual-tsk))
  (let ((retval nil))
    (occ-try-until 3 (or (not (eq t retval))
                         (occ-associable-p obj))
      (setq retval
            (occ-props-edit-combined obj '(timebeing add 10))))
    retval))

(cl-defmethod occ-edit-clock-if-unassociated ((obj occ-ctx))
  (let*  ((curr-tsk        (occ-current-tsk))
          (ctxual-curr-tsk (occ-build-ctxual-tsk-with curr-tsk obj)))
    (if (and
         (not
          (occ-clock-marker-unnamed-clock-p))
         ctxual-curr-tsk
         (not (occ-associable-p ctxual-curr-tsk)))
        (occ-edit-until-associable ctxual-curr-tsk)
      t)))
;; (occ-edit-properties (occ-current-ctxual-tsk) '(timebeing add 10))


(cl-defmethod occ-clock-in-if-not ((obj occ-ctx)
                                   &key
                                   filters
                                   builder
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (unless builder (error "Builder can not be nil"))
  (let ((filters            (or filters (occ-match-filters)))
        (builder            (or builder #'occ-build-ctxual-tsk-with))
        (return-transform   t) ;as return value is going to be used.)
        (action             (or action  (occ-get-helm-actions-tree obj '(t actions general))))
        (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "occ-clock-in-if-not((obj occ-ctx)): begin")
    (if (occ-edit-clock-if-unassociated obj) ;; (occ-clock-unassociated-p obj) ;; (occ-edit-clock-if-unassociated obj)
        (prog1                ;current clock is not matching
            t
          (occ-debug :debug
                     "occ-clock-in-if-not: Now really going to clock with this-command=%s"
                     this-command)
          ;; TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm
          (occ-debug :debug
             "TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm")
          (let ((retval (occ-clock-in obj
                                      :filters             filters
                                      :builder             builder
                                      :return-transform    return-transform
                                      :action              action
                                      :action-transformer  action-transformer
                                      :auto-select-if-only auto-select-if-only
                                      :timeout             timeout)))
            (occ-debug :debug "occ-clock-in-if-not: operate %s retval %s"
                              (occ-return-in-labels-p retval
                                                      occ-return-quit-label
                                                      occ-return-timeout-label)
                              (occ-return-get-value retval))
            (if (occ-return-in-labels-p retval
                                        occ-return-quit-label
                                        occ-return-timeout-label)
                (unless (occ-return-get-value retval)
                  ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not obj) and (occ-clock-in obj)
                  ;; begin occ-clock-in-curr-ctx-if-not
                  ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
                  ;; occ-clock-in-if-not: Now really going to clock.
                  ;; in occ-clock-in occ-ctx 1
                  ;; user input 111 retval t
                  ;; trying to create unnamed tsk.
                  ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
                  ;; occ-clock-in-if-not: Now really clock done.
                  ;; not able to find associated, or intentionally not selecting a clock
                  (if (occ-clock-marker-unnamed-clock-p)
                      (occ-debug :debug "occ-clock-in-if-not: already clock-in into unnamed task ")
                    (if (occ-config-clock-in)
                        (progn
                          (occ-debug :debug "trying to create unnamed tsk.")
                          (occ-message "trying to create unnamed tsk.")
                          (occ-maybe-create-clockedin-unnamed-ctxual-tsk obj))
                      (occ-message "occ-clock-in(obj occ-ctx): clock-in not allowed."))))
              (occ-debug :debug "occ-clock-in-if-not: Can not operate on %s"
                                (occ-format (occ-return-get-value retval)))))
          (occ-debug :debug "occ-clock-in-if-not: Now really clock done."))
      (prog1
          nil
        (occ-debug :debug
                   "occ-clock-in-if-not: Current tsk already associate to %s"
                   (occ-format obj 'captilize))))))
;; occ-clock-in-if-not


(cl-defmethod occ-consider-for-clockin-in-p ()
  (>
   (float-time (time-since *occ-last-buff-sel-time*))
   *occ-tsk-current-ctx-time-interval*))

(cl-defmethod occ-try-to-clock-in-p ((curr occ-ctx)
                                     (prev occ-ctx))
  (not              ;BUG: Reconsider whether it is catching case after some delay.
   (equal curr prev)))

(cl-defmethod occ-try-to-clock-in-p ((curr occ-ctx)
                                     (prev null))
  t)

(defvar occ-clock-in-ctx-auto-select-if-only t "occ-clock-in-ctx-auto-select-if-only")

(cl-defmethod occ-clock-in-if-chg ((obj occ-ctx)
                                   &key
                                   filters
                                   builder
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (let* ((filters            (or filters (occ-match-filters)))
         (builder            (or builder #'occ-build-ctxual-tsk-with))
         (action             (or action  (occ-get-helm-actions-tree obj '(t actions general))))
         (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
         (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "occ-clock-in-if-chg((obj occ-ctx)): begin")
    (if (occ-consider-for-clockin-in-p)
        (progn
          (setq *occ-tsk-current-ctx* obj)

          (if (occ-try-to-clock-in-p obj *occ-tsk-previous-ctx*)
              (when (occ-clock-in-if-not obj
                                         :filters             filters
                                         :builder             builder
                                         :action              action
                                         :action-transformer  action-transformer
                                         :auto-select-if-only auto-select-if-only
                                         :timeout             timeout)
                (occ-debug :debug "occ-clock-in-if-chg((obj occ-ctx)): calling occ-clock-in-if-not")
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*))
            (prog1
                nil
              ;; BUG *occ-tsk-previous-ctx* *occ-tsk-current-ctx* not getting
              ;; updated with simple buffer switch as idle tiem occur. IS IT CORRECT OR BUG
              ;; TODO: here describe reason for not trying properly, need to print where necessary.
              (occ-describe-try-to-clock-in-p *occ-tsk-current-ctx*
                                              *occ-tsk-previous-ctx*))))
      (occ-debug :nodisplay "occ-clock-in-if-chg: not enough time passed."))))
;; occ-clock-in-if-chg


(defvar *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar *occ-buff-sel-timer*                nil)
(defvar *occ-tsk-current-ctx-time-interval* 7)
(defvar *occ-tsk-previous-ctx*              nil)
(defvar *occ-tsk-current-ctx*               nil)


(defvar occ-ignore-buffer-names '(" *helm" "*Help*") "occ-ignore-buffer-names")
(defvar occ-ignore-buffer-regexps '(" *helm" "*Help*") "occ-ignore-buffer-names")

(defun occ-add-ignore-buffer-names ()
  (interactive)
  (let ((buffname (buffer-name (current-buffer))))
    (push buffname occ-ignore-buffer-names)))

(cl-defmethod occ-describe-try-to-clock-in-p ((curr occ-ctx)
                                              (prev occ-ctx))
  (let ((buff (occ-ctx-buffer curr)))
    (let ((msg (cond
                 ((not (occ-chgable-p))
                  (format "clock is not changeable now."))
                 ((not buff)
                  (format "context buffer is null"))
                 ((not (buffer-live-p buff))
                  (format "context buffer is not live now."))
                 ((minibufferp buff)
                  (format "context buffer is minibuffer."))
                 ((occ-ignore-p buff)
                  (format "context buffer is ignored buffer."))
                 ((equal prev curr)
                  (format "context is not changed."))
                 (t (format "Unknown reason.")))))
      (let ((full-msg (format "occ-clock-in-if-chg: ctx %s not suitable to associate as %s"
                              (occ-format curr 'capitalize)
                              msg)))
         ;; (occ-debug :nodisplay full-msg)
        (occ-message full-msg)))))


;;;###autoload
(defun occ-clock-in-curr-ctx (&optional force)
  (interactive "P")
  (let ((ctx (occ-make-ctx-at-point)))
    (let ((filters             (occ-match-filters))
          (builder             #'occ-build-ctxual-tsk-with)
          (action              (occ-get-helm-actions-tree ctx '(t actions general)))
          (action-transformer  (occ-get-helm-actions-tree-genertator ctx '(t actions general edit)))
          (auto-select-if-only nil) ; occ-clock-in-ctx-auto-select-if-only)
          (timeout             occ-idle-timeout))
      (occ-clock-in-if-not ctx
                           :filters             filters
                           :builder             builder
                           :action              action
                           :action-transformer  action-transformer
                           :auto-select-if-only auto-select-if-only
                           :timeout             timeout))))

;;;###autoload
(defun occ-clock-in-curr-ctx-if-not (&optional force)
  (interactive "P")
  ;; TODO: Add code to which check if only focus present than only trigger
  ;; else postpone it by calling run-with-idle-plus-timer
  (occ-debug :debug "begin occ-clock-in-curr-ctx-if-not")
  (lotus-with-other-frame-event-debug "occ-clock-in-curr-ctx-if-not" :cancel
    (occ-debug :debug "%s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug" (time-stamp-string))
    (if force
        (occ-clock-in-curr-ctx force)
      (let ((ctx (occ-make-ctx-at-point)))
        (let ((filters             (occ-match-filters))
              (builder             #'occ-build-ctxual-tsk-with)
              (action              (occ-get-helm-actions-tree ctx '(t actions general edit)))
              (action-transformer  (occ-get-helm-actions-tree-genertator ctx '(t actions general edit)))
              (auto-select-if-only occ-clock-in-ctx-auto-select-if-only)
              (timeout             occ-idle-timeout))
          (occ-clock-in-if-chg ctx
                               :filters             filters
                               :builder             builder
                               :action              action
                               :action-transformer  action-transformer
                               :auto-select-if-only auto-select-if-only
                               :timeout             timeout)))))
  (occ-debug :nodisplay "%s: end occ-clock-in-curr-ctx-if-not" (time-stamp-string)))


;;; Timers
;; TODO: Add method/function descriptions

(defun occ-run-curr-ctx-timer ()
  (occ-debug :debug "occ-run-curr-ctx-chg-timer: begin")
  (occ-clock-in-curr-ctx nil))

(defun occ-run-curr-ctx-chg-timer ()
  (occ-debug :debug "occ-run-curr-ctx-chg-timer: begin")
  (occ-clock-in-curr-ctx-if-not nil))


;; TODO: find some better name
;;;###autoload
(defun occ-clock-in-curr-ctx-if-not-timer-function (event)
  (occ-debug :debug "occ-clock-in-curr-ctx-if-not-timer-function: begin")
  ;;BUG: could be the cause of high MEM usage
  (unwind-protect
      (if (occ-config-value-quiet)
          (occ-message "Occ is quiet for some time.")
        (lotus-with-no-recursive-edit-if
            (occ-message "occ-clock-in-curr-ctx-if-not-timer-function: (recursion-depth) [%d] > 0" (recursion-depth))
          (lotus-with-no-active-minibuffer-if
              (occ-debug :debug "occ-clock-in-curr-ctx-if-not-timer-function: minibuffer active")
            (occ-cancel-timer)
            (if (eq 'buffer-switch event)
                (occ-run-curr-ctx-chg-timer)
              (occ-run-curr-ctx-timer)))))
    ;; to bypass QUIT
    (occ-try-clock-schedule-next-timeout nil)))


(defun occ-cancel-timer ()
  (when *occ-buff-sel-timer*
    (cancel-timer *occ-buff-sel-timer*)
    (setq *occ-buff-sel-timer* nil)))

(cl-defmethod occ-try-clock-in-next-timeout ()
  "Get next timeout to try clock-in"
  (occ-debug :debug "occ-try-clock-in-next-timeout: begin")
  (let* ((ctx             (occ-make-ctx-at-point))
         (ctxual-curr-tsk (occ-ctxual-current-tsk ctx)))
    (cond
     ((null ctxual-curr-tsk)          3)
     ((occ-unnamed-p ctxual-curr-tsk) (+ *occ-tsk-current-ctx-time-interval* 10))
     (t                               30))))

(cl-defmethod occ-try-clock-schedule-next-timeout (event)
  "Get next timeout to try clock-in"
  (occ-debug :debug "occ-try-clock-schedule-next-timeout: begin")
  (occ-cancel-timer)
  (setq *occ-buff-sel-timer*
        ;; distrubing while editing.
        ;; run-with-timer
        (run-with-idle-plus-timer
         (occ-try-clock-in-next-timeout)
         nil
         'occ-clock-in-curr-ctx-if-not-timer-function event)))


;;;###autoload
(defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
  (occ-debug :debug "occ-switch-buffer-run-curr-ctx-timer-function: begin")
  (setq *occ-last-buff-sel-time* (current-time))
  (occ-try-clock-schedule-next-timeout 'buffer-switch))


(defvar occ-add-inquery        0)
(defvar occ-add-org-file-timer nil)

(cl-defmethod occ-add-org-buffer ((buff buffer))
  (if (eql buff (current-buffer))
    (when (< occ-add-inquery 3)
      (let ((file (buffer-file-name buff)))
        (unless (cl-member file (occ-files) :key #'file-truename)
          (when (and file
                     (file-exists-p file)
                     (eq major-mode 'org-mode))
            (make-local-variable 'occ-add-inquery)
            (if (y-or-n-p-with-timeout (format "Do you want to add %s to occ: "
                                               (file-name-nondirectory file))
                                       3
                                       nil)
                (progn
                  (setq occ-add-inquery 3)
                  (occ-add-to-spec file))
              (incf occ-add-inquery)))))))
  (occ-add-org-file-timer buff))

(defun occ-add-org-file-timer (&optional buffer)
  (occ-debug :nodisplay
             "occ-add-org-file-timer: started for buff %s, occ-add-inquery %s, occ-add-org-file-timer %s"
             buffer
             occ-add-inquery
             occ-add-org-file-timer)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (make-local-variable 'occ-add-org-file-timer)
        (when occ-add-org-file-timer
          (cancel-timer occ-add-org-file-timer)
          (setq occ-add-org-file-timer nil))
        (when (< occ-add-inquery 3)
          (setq occ-add-org-file-timer
                (run-with-idle-plus-timer (* (1+ occ-add-inquery) 10)
                                          nil
                                          #'occ-add-org-buffer buffer)))))))

;;; occ-obj-method.el ends here
