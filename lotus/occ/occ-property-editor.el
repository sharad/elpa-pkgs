;;; occ-property-editor.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-property-editor)


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


(cl-defgeneric occ-select-propetry (obj
                                    &optional prompt)
  "occ-select-propetry")

(cl-defmethod occ-select-propetry ((obj occ-obj-ctx-tsk)
                                   &optional prompt)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (occ-debug :debug "occ-select-propetry: %s" (occ-Format tsk))
    (let ((prompt     (or prompt
                          (format "%s proptery: "
                                  (occ-Format tsk))))
          (fixed-keys '(edit
                        done))
          (keys       (occ-properties-to-edit obj)))
      (if keys
          (let ((maxkeylen (apply #'max
                                  (mapcar #'(lambda (sym) ;https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
                                              (length (symbol-name sym)))
                                          (append keys fixed-keys))))
                (key-vals  (occ-get-properties tsk keys)))
            (occ-debug :debug "occ-select-propetry: for %s with keys =%s got key-vals = %s"
                              (occ-Format tsk)
                              keys
                              key-vals)
            (if key-vals
                (let* ((key-val-collection (mapcar #'(lambda (key-val)
                                                       (cons
                                                        (if (cdr key-val)
                                                            (format "%s: %s" (car key-val) (cdr key-val))
                                                          (symbol-name (car key-val)))
                                                        (car key-val)))
                                                   key-vals))
                       (key-val-collection (append key-val-collection
                                                   (mapcar #'(lambda (fk) (cons (symbol-name fk) fk))
                                                           fixed-keys))))
                  (cl-assert key-val-collection)
                  (if key-val-collection
                      (let* ((key-sel (occ-completing-read prompt
                                                           key-val-collection
                                                           nil
                                                           t))
                             (sel (assoc key-sel
                                         key-val-collection)))
                        (occ-debug :debug "selected option %s" sel)
                        (cdr sel))
                    (occ-error "Not Keys Vals Collection %s for %s" key-val-collection (occ-Format tsk))))
              (occ-error "Not Keys Vals for %s" (occ-Format tsk))))
        (occ-debug :debug "Not Keys for %s" (occ-Format tsk))))))


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
  (let ((prop-range (org-get-property-block (point)
                                            force)))
    ;; first show heading
    (when (eq org-cycle-subtree-status 'folded)
      (unless flag
        ;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00573.html
        (progn
          (when (or (org-invisible-p)
                    (org-invisible-p2))
            (org-show-context 'org-goto)))
        (progn                                        ; changed from org-show-tsk to org-show-entry
          (org-show-entry)
          (occ-debug :debug "did %s entry `%s'"
                     (if flag "close" "open")
                     heading)
          (org-unlogged-message "CHILDREN")
          (setq org-cycle-subtree-status 'children))))
    ;; show expand property if flag is nil, else hide
    (let* ((prop-range (org-get-property-block (point)
                                               force))
           (prop-loc   (when (consp prop-range)
                         (1- (car prop-range)))))
      (when prop-range
        (occ-debug :debug "pos %d before jumping to %s drawer, will jump to pos %d"
                   (point)
                   (if flag "close" "open")
                   prop-loc)
        (goto-char prop-loc)
        (occ-debug :debug "reached to %s drawer"
                   (if flag "close" "open"))
        (if (org-at-drawer-p)
            ;; show drawer
            (let ((drawer (org-element-at-point)))
              (when (memq (org-element-type drawer)
                          '(node-property drawer property-drawer))
                (occ-debug :debug "trying to %s drawer %s current pos %d"
                           (if flag "close" "open")
                           drawer
                           (point))
                (org-flag-drawer flag
                                 drawer)
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
    (when (and buff
               loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (org-get-flag-proprty-drawer force))))))

(defun org-flag-proprty-drawer-at-marker (marker
                                          flag
                                          &optional force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  (let ((buff    (marker-buffer marker))
        (loc     (marker-position marker))
        (heading (org-get-heading 'notags)))
    (when (and buff
               loc)
      (with-current-buffer buff
        (let ((currloc (point)))
          (goto-char loc)
          (occ-debug :debug "%s: org-flag-proprty-drawer-at-marker: called to %s drawer of heading `%s' in file %s loc %d"
                     (time-stamp-string)
                     (if flag "close" "open")
                     heading
                     (buffer-file-name buff)
                     loc)
          (org-flag-proprty-drawer flag
                                   force))))))


(cl-defmethod occ-open-property-block ((obj marker))
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
                    (occ-error "occ-open-property-block: no prop-loc % for buff %s marker %s"
                           prop-loc buff mrk)
                  t))))
        (occ-error "occ-open-property-block: no buff %s found for object %s"
               (occ-Format obj))))))

(cl-defmethod occ-open-property-block ((obj null))
  (occ-open-property-block (point-marker)))


(cl-defmethod occ-properties-editor ((obj occ-obj-ctx-tsk))
  (occ-debug :debug "occ-properties-editor: begin %s"
             (occ-Format obj))
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((prop nil))
      (while (and (not (member (setq prop (occ-select-propetry obj))
                               '(edit done)))
                  prop)
        ;; TODO: handle (occ-select-propetry obj ctx) return NIL
        ;; No prop or NIL value can happen in CTX like *scratch*
        ;; (cl-assert prop)
        (let ((retval (occ-op-prop-edit obj
                                        prop)))
          (when retval
            ;; (occ-tsk-update-tsks t)
            (occ-debug :debug "occ-properties-editor-with: done with retval %s" retval)
            retval))))))


(cl-defmethod occ-properties-editor-in-cloned-buffer ((obj occ-obj-ctx-tsk))
  (occ-debug :debug "occ-properties-editor-in-cloned-buffer: begin")
  (let ((mrk (occ-obj-marker obj)))
    (org-with-cloned-marker mrk "<proptree>"
      (let ((cloned-mrk (point-marker)))
        (org-with-narrow-to-marker mrk
          (if (occ-open-property-block cloned-mrk)
              (occ-properties-editor obj)
            (occ-error "occ-properties-editor-in-cloned-buffer: can not edit props for %s"
                       (occ-Format obj))))))))


(defun occ-properties-editor-handle-response (prop
                                              timeout
                                              timer
                                              cleanup
                                              local-cleanup
                                              win)
  (cond ((eql 'done prop)
         (funcall cleanup
                  win
                  local-cleanup)
         (when timer
           (cancel-timer timer)))
        ((eql 'edit prop)
         ;; (funcall cleanup win local-cleanup)
         (occ-debug :debug "occ-obj-prop-edit: debug editing")
         (when timer
           (cancel-timer timer))
         (when (and win
                    (windowp win)
                    (window-valid-p win))
           (select-window win 'norecord)))
        (t
         (funcall cleanup
                  win
                  local-cleanup)
         (when timer
           (cancel-timer timer)))))

(cl-defmethod occ-properties-window-editor ((obj occ-obj-ctx-tsk)
                                            &key
                                            return-transform
                                            timeout)
  (let* ((timeout (or timeout
                      occ-idle-timeout)))
    (let* ((local-cleanup #'(lambda ()
                              (occ-debug :warning "occ-properties-window-editor(obj occ-obj-ctx-tsk): local-cleanup called")
                              (occ-debug :debug "occ-properties-window-editor(obj occ-obj-ctx-tsk): local-cleanup called")
                              (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                                (abort-recursive-edit)))))
        (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
            timeout timer cleanup local-cleanup win
            (condition-case-control err
              (let ((prop (occ-properties-editor-in-cloned-buffer obj)))
                (occ-properties-editor-handle-response prop
                                                       timeout
                                                       timer
                                                       cleanup
                                                       local-cleanup
                                                       win)
                (occ-debug :debug "occ-properties-window-editor(obj occ-obj-ctx-tsk) noquit: label %s value %s"
                           occ-return-true-label obj)
                (if return-transform ;Here caller know if return value is going to be used.
                    (occ-make-return occ-return-true-label
                                     obj)
                  obj))
              ((quit)
               (progn
                 (occ-debug :warning "occ-properties-window-editor(obj occ-obj-ctx-tsk): canceling timer")
                 (occ-debug :debug "occ-properties-window-editor(obj occ-obj-ctx-tsk): canceling timer")
                 (funcall cleanup
                          win
                          local-cleanup)
                 (if timer
                     (cancel-timer timer))
                 (signal (car err)
                         (cdr err))
                 (occ-debug :debug "occ-properties-window-editor(obj occ-obj-ctx-tsk) quit: label %s value %s"
                            occ-return-quit-label
                            nil)
                 (when return-transform ;Here caller know if return value is going to be used.
                   (occ-make-return occ-return-quit-label
                                    nil)))))))))

(cl-defmethod occ-properties-window-editor ((obj occ-ctx)
                                            &key
                                            filters
                                            builder
                                            ap-normal
                                            ap-transf
                                            return-transform ;Here caller know if return value is going to be used.
                                            timeout)
  (let* ((filters   (or filters nil))
         (builder   (or builder #'occ-build-ctsk-with))
         (ap-normal '(t actions general))
         (ap-transf '(t actions general edit))
         (timeout   (or timeout occ-idle-timeout)))
    (occ-debug :debug "occ-select-obj-prop-edit((obj occ-ctx)): begin")
    (let ((buff (occ-ctx-buffer obj)))
      (if (and (buffer-live-p buff)
               (not (occ-helm-buffer-p buff)))
        (let ((retval-ctx-tsk (occ-select obj
                                          :filters          filters
                                          :builder          builder
                                          :ap-normal        ap-normal
                                          :ap-transf        ap-transf
                                          :return-transform return-transform ;Here caller know if return value is going to be used.
                                          :timeout          timeout)))
          ;; (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): ap-transf: %s action %s"
          ;;                   ap-transf action)
          (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                     retval-ctx-tsk
                     (occ-format (occ-obj-obj retval-ctx-tsk) 'capitalize)
                     (occ-return-get-label retval-ctx-tsk))
          ;; BUG: will do run recursively as another method with (obj null) is define below.
          (when (and (occ-return-in-labels-p retval-ctx-tsk
                                             occ-return-select-label)
                     (occ-obj-obj retval-ctx-tsk))
            (occ-properties-window-editor (occ-obj-obj retval-ctx-tsk)
                                          :return-transform return-transform
                                          :timeout          timeout
              (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): No selection")))
          (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): returning original: %s, retval: %s with label %s operate: %s"
                            retval-ctx-tsk
                            (occ-Format (occ-obj-obj retval-ctx-tsk))
                            (occ-return-get-label retval-ctx-tsk)
                            (occ-return-in-labels-p retval-ctx-tsk
                                                    occ-return-select-label))
          retval-ctx-tsk)
        (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                   (buffer-live-p buff)
                   (not (occ-helm-buffer-p buff)))
        (occ-debug :debug "occ-properties-window-editor((obj occ-ctx)): not running  as context buff is deleted or not live 1 %s, 2 %s"
                   (buffer-live-p buff)
                   (not (occ-helm-buffer-p buff)))
        (when return-transform ;Here caller know if return value is going to be used.
          (occ-make-return occ-return-false-label
                           nil))))))

(cl-defmethod occ-properties-window-editor ((obj null)
                                            &key
                                            filters
                                            builder
                                            ap-normal
                                            ap-transf
                                            return-transform
                                            timeout)
  (occ-debug :debug "occ-select-obj-prop-edit((obj null)):")
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-build-ctsk-with))
        (ap-normal '(t actions general))
        (ap-transf '(t actions general edit))
        (timeout   (or timeout occ-idle-timeout)))
    (occ-properties-window-editor (occ-make-ctx-at-point)
                                  :filters          filters
                                  :builder          builder
                                  :ap-normal        ap-normal
                                  :ap-transf        ap-transf
                                  :return-transform return-transform
                                  :timeout          timeout)))


(cl-defmethod occ-safe-properties-window-editor ((obj occ-ctx)
                                                 &key
                                                 filters
                                                 builder
                                                 ap-normal
                                                 ap-transf
                                                 return-transform
                                                 timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx-at-point) occ-idle-timeout))
  (occ-debug :debug "occ-safe-properties-window-editor((obj occ-ctx)): begin")
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-build-ctsk-with))
        (ap-normal '(t actions general))
        (ap-transf '(t actions general edit))
        (timeout   (or timeout occ-idle-timeout)))
    (occ-debug :debug "begin occ-safe-properties-window-editor")
    (occ-debug-return "occ-safe-properties-window-editor((obj occ-ctx)) no-active"
      (lotus-with-no-active-minibuffer-if
          (progn
            (occ-debug :debug "occ-safe-properties-window-editor: [minibuffer-body] lotus-with-no-active-minibuffer-if")
            (occ-debug :debug "occ-safe-properties-window-editor: minibuffer already active quitting")
            (occ-debug :debug nil))
        ;;; TODO: extend lotus-with-other-frame-event-debug it to include elscreen change also.
        (occ-debug-return "occ-safe-properties-window-editor((obj occ-ctx)) frame-event-debug"
          (lotus-with-other-frame-event-debug "occ-safe-properties-window-editor" :cancel
            (occ-debug :debug "occ-safe-properties-window-editor: lotus-with-other-frame-event-debug")
            (prog1
                (let ((buff (occ-ctx-buffer obj)))
                  (if (eq (current-buffer)
                          buff)
                      (occ-debug-return "occ-safe-properties-window-editor((obj occ-ctx)) direct"
                        (occ-properties-window-editor obj
                                                      :filters          filters
                                                      :builder          builder
                                                      :ap-normal        ap-normal
                                                      :ap-transf        ap-transf
                                                      :return-transform return-transform
                                                      :timeout          timeout))
                   (occ-debug :debug "context is not for current buffer.")))
              (occ-debug :debug "finished occ-safe-properties-window-editor"))))))))

(cl-defmethod occ-safe-properties-window-editor ((obj marker)
                                                 &key
                                                 filters
                                                 builder
                                                 ap-normal
                                                 ap-transf
                                                 timeout)
  (occ-debug :debug "occ-safe-properties-window-editor((obj marker)): begin")
  (let ((selected (occ-safe-properties-window-editor (occ-make-ctx marker)
                                                     :filters          filters
                                                     :builder          builder
                                                     :return-transform return-transform
                                                     :ap-normal        ap-normal
                                                     :ap-transf        ap-transf
                                                     :timeout          timeout)))
    (occ-debug :debug "occ-safe-properties-window-editor((obj marker)): returning %s" selected)
    selected))

(cl-defmethod occ-safe-ignore-quit-properties-window-editor ((obj occ-ctx)
                                                             &key
                                                             filters
                                                             builder
                                                             return-transform
                                                             ap-normal
                                                             ap-transf
                                                             timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug :debug "occ-safe-ignore-quit-properties-window-editor((obj occ-ctx)): begin")
  (let ((filters   (or filters nil))
        (builder   (or builder #'occ-build-ctsk-with))
        (ap-normal '(t actions general))
        (ap-transf '(t actions general edit))
        (timeout   (or timeout occ-idle-timeout)))
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
    ;;     (occ-safe-properties-window-editor obj
    ;;                                 :collector          collector
    ;;                                 :ap-normal          ap-normal
    ;;                                 :ap-transf          ap-transf
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
        (occ-safe-properties-window-editor obj
                                           :filters          filters
                                           :builder          builder
                                           :ap-normal        ap-normal
                                           :ap-transf        ap-transf
                                           :return-transform return-transform
                                           :timeout          timeout)
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

;;; occ-property-editor.el ends here
