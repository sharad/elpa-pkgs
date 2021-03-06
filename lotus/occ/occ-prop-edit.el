;;; occ-prop-edit.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-prop-edit)


(require 'time-stamp)


(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'lotus-utils)
(eval-when-compile
  (require 'lotus-utils))
(require 'lotus-idle-utils)
(eval-when-compile
  (require 'lotus-idle-utils))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))


(eval-when-compile
  (require 'occ-macros))
(require 'occ-macros)
(require 'occ-obj-method)
(require 'occ-obj-utils)
(require 'occ-prop)
;; (require 'occ-property-rank-methods)


(cl-defgeneric occ-select-propetry (obj
                                    &optional prompt)
  "occ-select-propetry")

(cl-defmethod occ-select-propetry ((obj occ-obj-ctx-tsk)
                                   &optional prompt)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-debug :debug "occ-select-propetry: %s" (occ-format tsk 'capitalize))
    (let ((prompt (or prompt "proptery: "))
          (fixed-keys '(edit done))
          (keys       (occ-properties-to-edit obj)))
      (if keys
          (let ((maxkeylen (apply
                            #'max
                            (mapcar #'(lambda (sym) ;https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
                                        (length (symbol-name sym)))
                                    (append keys fixed-keys))))
                (key-vals  (occ-get-properties tsk keys)))
            (occ-debug :debug "occ-select-propetry: for %s with keys =%s got key-vals = %s"
                              (occ-format tsk 'capitalize)
                              keys
                              key-vals)
            (if key-vals
                (let* ((key-val-collection
                        (mapcar
                         #'(lambda (key-val)
                             (cons
                              (if (cdr key-val)
                                  (format "%s: %s" (car key-val) (cdr key-val))
                                (symbol-name (car key-val)))
                              (car key-val)))
                         key-vals))
                       (key-val-collection (append
                                            key-val-collection
                                            (mapcar #'(lambda (fk) (cons (symbol-name fk) fk))
                                                    fixed-keys))))
                  (if key-val-collection
                      (let ((sel
                             (assoc
                              (occ-completing-read prompt
                                                   key-val-collection
                                                   nil
                                                   t)
                              key-val-collection)))
                        (occ-debug :debug "selected option %s" sel)
                        (cdr sel))
                    (error "Not Keys Vals Collection %s for %s" key-val-collection (occ-format tsk 'capitalize))))
              (error "Not Keys Vals for %s" (occ-format tsk 'capitalize))))
        (occ-debug :debug "Not Keys for %s" (occ-format tsk 'capitalize))))))


(defun org-get-flag-proprty-drawer (&optional force)
  (let ((range (org-get-property-block (point) force)))
    (when range
      org-cycle-subtree-status)))

(defun org-flag-proprty-drawer (flag
                                &optional force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  ;; (recenter-top-bottom 2)
  ;; (unless flag                  ;; creating issue in cleanupfn error as display buffer and current buffer is not same.
  ;;   (recenter-top-bottom 2))
  (let ((prop-range (org-get-property-block (point) force)))
    ;; first show heading
    (when (eq org-cycle-subtree-status 'folded)
      (unless flag
        ;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00573.html
        (progn
          (when (or
                 (org-invisible-p)
                 (org-invisible-p2))
            (org-show-context 'org-goto)))
        (progn                                        ; changed from org-show-tsk to org-show-entry
          (org-show-entry)
          (occ-debug :debug
                     "did %s entry `%s'" (if flag "close" "open") heading)
          (org-unlogged-message "CHILDREN")
          (setq org-cycle-subtree-status 'children))))
    ;; show expand property if flag is nil, else hide
    (let* ((prop-range (org-get-property-block (point) force))
           (prop-loc   (when (consp prop-range) (1- (car prop-range)))))
      (when prop-range
        (occ-debug :debug "pos %d before jumping to %s drawer, will jump to pos %d"
                   (point)
                   (if flag "close" "open")
                   prop-loc)
        (goto-char prop-loc)
        (occ-debug :debug "reached to %s drawer" (if flag "close" "open"))
        (if (org-at-drawer-p)
            ;; show drawer
            (let ((drawer (org-element-at-point)))
              (when (memq (org-element-type drawer)
                          '(node-property drawer property-drawer))
                (occ-debug :debug
                           "trying to %s drawer %s current pos %d"
                           (if flag "close" "open")
                           drawer
                           (point))
                (org-flag-drawer flag drawer)
                ;; Make sure to skip drawer entirely or we might flag
                ;; it another time when matching its ending line with
                ;; `org-drawer-regexp'.
                (when nil       ;;BUG ?? what
                  (goto-char (org-element-property :end drawer)))))
          (occ-debug :debug "not at drawer to %s current pos is %s"
                     (if flag "close" "open")
                     (point)))
        (goto-char prop-loc)
        (occ-debug :debug "reached to %s drawer1 current pos %d"
                   (if flag "close" "open")
                   (point))
        prop-range))))


(defun org-get-flag-proprty-drawer-at-marker (marker
                                              &optional force)
  (let ((buff (marker-buffer marker))
        (loc  (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (org-get-flag-proprty-drawer force))))))

(defun org-flag-proprty-drawer-at-marker (marker
                                          flag
                                          &optional force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker))
        (heading (org-get-heading 'notags)))
    (when (and buff loc)
      (with-current-buffer buff
        (let ((currloc (point)))
          (goto-char loc)
          (occ-debug :debug "%s: org-flag-proprty-drawer-at-marker: called to %s drawer of heading `%s' in file %s loc %d"
                     (time-stamp-string)
                     (if flag "close" "open")
                     heading
                     (buffer-file-name buff)
                     loc)
          (org-flag-proprty-drawer flag force))))))


(cl-defmethod occ-open-prop-block ((obj marker))
  ;; Find better name
  (let ((mrk              obj)
        (buffer-read-only nil))
    ;; (occ-debug :debug "timer started for win %s" win)
    (let ((buff (marker-buffer   mrk))
          (pos  (marker-position mrk)))
      ;; show proptery drawer
      (if buff
          (progn
            (switch-to-buffer buff)
            (goto-char pos)
            (set-marker mrk (point))
            (recenter-top-bottom 2)
            (let* ((prop-range (org-flag-proprty-drawer-at-marker mrk nil))
                   (prop-loc   (when (consp prop-range) (1- (car prop-range)))))
              (show-all)
              (if (numberp prop-loc)
                  (goto-char prop-loc)
                (if nil
                    (error "occ-open-prop-block: no prop-loc % for buff %s marker %s"
                           prop-loc buff mrk)
                  t))))
        (error "occ-open-prop-block: no buff %s found for object %s"
               (occ-format obj 'capitalize))))))

(cl-defmethod occ-open-prop-block ((obj null))
  (occ-open-prop-block (point-marker)))


(cl-defmethod occ-props-edit ((obj occ-obj-ctx-tsk))
  (occ-debug :debug "occ-props-edit: begin %s"
             (occ-format obj 'capitalize))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((prop nil))
      (while (not
              (member
               (setq prop (occ-select-propetry obj))
               '(edit done)))
        ;; TODO: handle (occ-select-propetry obj ctx) return NIL
        (let ((retval
               (occ-editprop obj prop)))
          (when retval
            ;; (occ-tsk-update-tsks t)
            (occ-debug :debug "occ-props-edit-with: done with retval %s" retval)
            retval))))))


;; do both fast and interactive editing.
;; (occ-props-edit obj)
(cl-defmethod occ-props-edit-combined ((obj occ-obj-ctx-tsk))
  (let* ((sources (list
                   (helm-build-sync-source "fast edit"
                     :candidates (occ-gen-edits-if-required obj nil nil :param-only t)
                     :action (list (cons "Edit" #'funcall)))
                   (helm-build-sync-source "edit"
                     :candidates (list
                                  (cons "Edit" #'(lambda () (occ-props-edit obj))))
                     :action (list (cons "Edit" #'funcall)))
                   (helm-build-sync-source "other"
                     :candidates '(("Continue" . t)
                                   ("Checkout" . checkout)))))
         (retval
          (helm-timed occ-idle-timeout nil
            (helm :sources sources))))
    (if (eq retval t)
        t)))


(cl-defmethod occ-props-edit-in-cloned-buffer ((obj occ-obj-ctx-tsk))
  (occ-debug :debug "occ-props-edit-in-cloned-buffer: begin")
  (let ((mrk (occ-obj-marker obj)))
    (org-with-cloned-marker mrk "<proptree>"
      (let ((cloned-mrk (point-marker)))
        (org-with-narrow-to-marker mrk
          (if (occ-open-prop-block cloned-mrk)
              (occ-props-edit obj)
            (error "occ-props-edit-in-cloned-buffer: can not edit props for %s"
                   (occ-format obj 'capitalize))))))))


(defun occ-props-edit-handle-response (prop timeout timer cleanup local-cleanup win)
  (cond
   ((eql 'done prop)
    (funcall cleanup win local-cleanup)
    (when timer (cancel-timer timer)))
   ((eql 'edit prop)
    ;; (funcall cleanup win local-cleanup)
    (occ-debug :debug "occ-obj-prop-edit: debug editing")
    (when timer (cancel-timer timer))
    (when (and win (windowp win) (window-valid-p win))
      (select-window win 'norecord)))
   (t
    (funcall cleanup win local-cleanup)
    (when timer (cancel-timer timer)))))

(cl-defmethod occ-props-window-edit ((obj occ-obj-ctx-tsk)
                                     &key
                                     return-transform
                                     timeout)
  (let* ((timeout (or timeout occ-idle-timeout)))
    (let* ((local-cleanup
              #'(lambda ()
                  (occ-debug :warning "occ-props-window-edit(obj occ-obj-ctx-tsk): local-cleanup called")
                  (occ-debug :debug "occ-props-window-edit(obj occ-obj-ctx-tsk): local-cleanup called")
                  (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                    (abort-recursive-edit)))))
        (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
            timeout timer cleanup local-cleanup win
            (condition-case-control err
              (let ((prop (occ-props-edit-in-cloned-buffer obj)))
                (occ-props-edit-handle-response prop timeout timer cleanup local-cleanup win)
                (occ-debug :debug "occ-props-window-edit(obj occ-obj-ctx-tsk) noquit: label %s value %s"
                                  occ-return-true-label obj)
                (if return-transform ;Here caller know if return value is going to be used.
                    (occ-make-return occ-return-true-label obj)
                  obj))
              ((quit)
               (progn
                 (occ-debug :warning "occ-props-window-edit(obj occ-obj-ctx-tsk): canceling timer")
                 (occ-debug :debug "occ-props-window-edit(obj occ-obj-ctx-tsk): canceling timer")
                 (funcall cleanup win local-cleanup)
                 (if timer (cancel-timer timer))
                 (signal (car err) (cdr err))
                 (occ-debug :debug "occ-props-window-edit(obj occ-obj-ctx-tsk) quit: label %s value %s"
                                   occ-return-quit-label nil)
                 (when return-transform ;Here caller know if return value is going to be used.
                   (occ-make-return occ-return-quit-label nil)))))))))

(cl-defmethod occ-props-window-edit ((obj occ-ctx)
                                     &key
                                     filters
                                     builder
                                     return-transform ;Here caller know if return value is going to be used.
                                     action
                                     action-transformer
                                     timeout)
  (let* ((filters            (or filters nil))
         (builder            (or builder #'occ-build-ctsk-with))
         (action             (or action (occ-get-helm-actions-tree obj '(t actions general))))
         (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
         (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "occ-select-obj-prop-edit((obj occ-ctx)): begin")
    (let ((buff (occ-ctx-buffer obj)))
      (if (and
           (buffer-live-p buff)
           (not (occ-helm-buffer-p buff)))
        (let ((retval-ctx-tsk (occ-select obj
                                          :filters            filters
                                          :builder            builder
                                          :return-transform   return-transform ;Here caller know if return value is going to be used.
                                          :action             action
                                          :action-transformer action-transformer
                                          :timeout            timeout)))
          ;; (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): action-transformer: %s action %s"
          ;;                   action-transformer action)
          (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                            retval-ctx-tsk
                            (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                            (occ-return-get-label retval-ctx-tsk))
          ;; BUG: will do run recursively as another method with (obj null) is define below.
          (when (and
                 (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label)
                 (occ-return-get-value retval-ctx-tsk))
              (occ-props-window-edit (occ-return-get-value retval-ctx-tsk)
                                     :return-transform   return-transform
                                     :timeout timeout)
              (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): No selection"))
          (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): returning original: %s, retval: %s with label %s operate: %s"
                            retval-ctx-tsk
                            (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                            (occ-return-get-label retval-ctx-tsk)
                            (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label))
          retval-ctx-tsk)
        (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                     (buffer-live-p buff)
                     (not (occ-helm-buffer-p buff)))
        (occ-debug :debug "occ-props-window-edit((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                          (buffer-live-p buff)
                          (not (occ-helm-buffer-p buff)))
        (when return-transform ;Here caller know if return value is going to be used.
          (occ-make-return occ-return-false-label nil))))))

(cl-defmethod occ-props-window-edit ((obj null)
                                     &key
                                     filters
                                     builder
                                     return-transform
                                     action
                                     action-transformer
                                     timeout)
  (occ-debug :debug "occ-select-obj-prop-edit((obj null)):")
  (let ((filters            (or filters nil))
        (builder            (or builder #'occ-build-ctsk-with))
        (action             (or action (occ-get-helm-actions-tree obj '(t actions general))))
        (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
        (timeout            (or timeout occ-idle-timeout)))
      (occ-props-window-edit (occ-make-ctx-at-point)
                             :filters            filters
                             :builder            builder
                             :return-transform   return-transform
                             :action             action
                             :action-transformer action-transformer
                             :timeout            timeout)))


(cl-defmethod occ-safe-props-window-edit ((obj occ-ctx)
                                          &key
                                          filters
                                          builder
                                          return-transform
                                          action
                                          action-transformer
                                          timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx-at-point) occ-idle-timeout))
  (occ-debug :debug "occ-safe-props-window-edit((obj occ-ctx)): begin")
  (let ((filters            (or filters nil))
        (builder            (or builder #'occ-build-ctsk-with))
        (action             (or action (occ-get-helm-actions-tree obj '(t actions general))))
        (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "begin occ-safe-props-window-edit")
    (occ-debug-return "occ-safe-props-window-edit((obj occ-ctx)) no-active"
      (lotus-with-no-active-minibuffer-if
          (progn
            (occ-debug :debug "occ-safe-props-window-edit: [minibuffer-body] lotus-with-no-active-minibuffer-if")
            (occ-debug :debug "occ-safe-props-window-edit: minibuffer already active quitting")
            (occ-debug :debug nil))
        ;;; TODO: extend lotus-with-other-frame-event-debug it to include elscreen change also.
        (occ-debug-return "occ-safe-props-window-edit((obj occ-ctx)) frame-event-debug"
          (lotus-with-other-frame-event-debug "occ-safe-props-window-edit" :cancel
            (occ-debug :debug "occ-safe-props-window-edit: lotus-with-other-frame-event-debug")
            (prog1
                (let ((buff (occ-ctx-buffer obj)))
                  (if (eq (current-buffer) buff)
                      (occ-debug-return "occ-safe-props-window-edit((obj occ-ctx)) direct"
                        (occ-props-window-edit obj
                                               :filters            filters
                                               :builder            builder
                                               :return-transform   return-transform
                                               :action             action
                                               :action-transformer action-transformer
                                               :timeout            timeout))
                   (occ-debug :debug "context is not for current buffer.")))
              (occ-debug :debug "finished occ-safe-props-window-edit"))))))))

(cl-defmethod occ-safe-props-window-edit ((obj marker)
                                          &key
                                          filters
                                          builder
                                          action
                                          action-transformer
                                          timeout)
  (occ-debug :debug "occ-safe-props-window-edit((obj marker)): begin")
  (let ((selected (occ-safe-props-window-edit (occ-make-ctx marker)
                                              :filters            filters
                                              :builder            builder
                                              :return-transform   return-transform
                                              :action             action
                                              :action-transformer action-transformer
                                              :timeout            timeout)))
    (occ-debug :debug "occ-safe-props-window-edit((obj marker)): returning %s" selected)
    selected))

(cl-defmethod occ-safe-ignore-quit-props-window-edit ((obj occ-ctx)
                                                      &key
                                                      filters
                                                      builder
                                                      return-transform
                                                      action
                                                      action-transformer
                                                      timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug :debug "occ-safe-ignore-quit-props-window-edit((obj occ-ctx)): begin")
  (let ((filters            (or filters nil))
        (builder            (or builder #'occ-build-ctsk-with))
        (action             (or action (occ-get-helm-actions-tree obj '(t actions general))))
        (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "called occ-delayed-select-obj-prop-edit-when-idle")
    (occ-debug :debug "%s: begin: occ-delayed-select-obj-prop-edit-when-idle" (time-stamp-string))
    ;; timed-newwin of occ-delayed-select-obj-prop-edit pass quit
    ;; signal to caller mean here, so need to be handled, else this function can
    ;; not return any value to its caller, which result into no next-action in
    ;; caller function.

    ;; (condition-case-control nil
    ;;   (progn
    ;;     ;; TODO: Add code to which check if only focus present than only trigger
    ;;     ;; else postpone it by calling run-with-idle-plus-timer
    ;;     (occ-debug
    ;;            :debug
    ;;            "occ-delayed-select-obj-prop-edit-when-idle: calling occ-delayed-select-obj-prop-edit with this-command=%s" this-command)
    ;;     (occ-safe-props-window-edit obj
    ;;                                 :collector          collector
    ;;                                 :action             action
    ;;                                 :action-transformer action-transformer
    ;;                                 :timeout            timeout))

    ;;   ;; (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit-when-idle" :cancel
    ;;   ;;   (occ-debug :debug "occ-delayed-select-obj-prop-edit-when-idle: lotus-with-other-frame-event-debug")
    ;;   ;;   (occ-delayed-select-obj-prop-edit ctx timeout))
    ;;   ((quit)))
    (occ-debug :debug
           "occ-delayed-select-obj-prop-edit-when-idle: calling occ-delayed-select-obj-prop-edit with this-command=%s" this-command)
    (prog1
      ;; TODO: Add code to which check if only focus present than only trigger
      ;; else postpone it by calling run-with-idle-plus-timer
      (occ-safe-props-window-edit obj
                                  :filters            filters
                                  :builder            builder
                                  :return-transform   return-transform
                                  :action             action
                                  :action-transformer action-transformer
                                  :timeout            timeout)
      ;; (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit-when-idle" :cancel
      ;;   (occ-debug :debug "occ-delayed-select-obj-prop-edit-when-idle: lotus-with-other-frame-event-debug")
      ;;   (occ-delayed-select-obj-prop-edit ctx timeout))
      (occ-debug :debug
                 "%s: end: occ-delayed-select-obj-prop-edit-when-idle"
                 (time-stamp-string)))))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  occ-idle-timeout nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-delayed-select-obj-prop-edit args)) (list ctx timeout))

;;; occ-prop-edit.el ends here
