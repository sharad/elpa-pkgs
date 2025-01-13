;;; sessions-unified-core-fsession.el --- Maintain session per frame for named frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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



(provide 'sessions-unified-core-fsession)


;; (eval-when-compile
;;   (require 'elscreen))
(require 'emacs-panel)
(require 'desktop)
(require 'session)
;; (require 'elscreen)
;; (require 'utils-custom)


;; (require 'session-unified)
;; (require 'desktop-unified)
;; (require 'sessions-unified)


(require 'sessions-unified-core-common)
(require 'sessions-unified-elscreen)
(require 'sessions-unified-tab-bar)


(defvar session-unified-debug nil)


(defvar *sessions-unified-frames-session* nil "Stores all elscreen sessions here.")
(defvar *sessions-unified-frames-session-old* nil "Stores all discarded elscreen sessions here.")
(defvar *sessions-unified-frame-session-restore-lock* nil
  "*sessions-unified-frame-session-restore-lock* if it is true than only frame session will get restored.")
;; (defun server-create-frame-before-adrun ()
;;   "remove-scratch-buffer"
;;   (setq *sessions-unified-frame-session-restore-lock* t))

(defvar session-unified-utils-select-frame-fn #'select-frame-set-input-focus
  "session-unified-utils-select-frame-fn")
;; (setq session-unified-utils-select-frame-fn #'select-frame)
(defvar *desktop-vc-read-inprogress* nil "desktop-vc-read-inpgrogress")

(defvar *session-unified-frame-session-restore-display-function* #'display-about-screen
  "function to display screen with frame-session-restore, e.g.
display-about-screen, spacemacs-buffer/goto-buffer")


(eval-after-load "desktop"
  '(progn
     (add-to-list
      'desktop-globals-to-save
      '*sessions-unified-frames-session*)
     (add-to-list
      'desktop-globals-to-save
      '*sessions-unified-frames-session-old*)))

(eval-after-load "session"
  '(progn
     (add-to-list
      'session-globals-include
      '(*sessions-unified-frames-session* 100))
     (add-to-list
      'session-globals-include
      '(*sessions-unified-frames-session-old* 100))))


(defun completing-read-timeout-if-default-input (seconds
                                                 prompt
                                                 collection
                                                 &optional
                                                 predicate
                                                 require-match
                                                 initial-input
                                                 hist
                                                 def
                                                 inherit-input-method)
  (if initial-input
      (with-timeout (seconds
                     (progn
                       (when (active-minibuffer-window)
                         (abort-recursive-edit))
                       initial-input))
        (completing-read prompt
                         collection
                         predicate
                         require-match
                         initial-input
                         hist
                         def
                         inherit-input-method))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist
                     def
                     inherit-input-method)))

(defun completing-read-timeout (seconds
                                prompt
                                collection
                                &optional
                                predicate
                                require-match
                                initial-input
                                hist
                                def
                                inherit-input-method)
  (with-timeout (seconds
                 (progn
                   (when (active-minibuffer-window)
                     (abort-recursive-edit))
                   initial-input))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist
                     def
                     inherit-input-method)))



(defun fmsession-migration ()
  (interactive)
  (dolist (session (directory-files "~/.emacs.d/.cache/session-unified/session/frames/" nil "[a-zA-Z]+"))
    (cl-pushnew
     (cons session
           (lotus-read-sexp
            (concat "~/.emacs.d/.cache/session-unified/session/frames/" session "/elscreen")))
     *sessions-unified-frames-session*)))


(defun fmsession-delete-session (session)
  (interactive
   (list (fmsession-read-location)))
  ;; save somewhere as backup
  (if (and session
           (y-or-n-p (format "Can I delete screen \"%s\" session: " session)))
      (progn
        (push (cl-find session
                       *sessions-unified-frames-session*
                       :key 'car
                       :test 'string-equal)
              *sessions-unified-frames-session-old*)
        (setq *sessions-unified-frames-session*
              (cl-remove session *sessions-unified-frames-session*
                         :key 'car
                         :test 'string-equal)))
    (session-unfiy-notify "Not deleting screen \"%s\" session: " session)))


(defun fmsession-modify-element (fun)
  (mapcar fun
          (copy-tree *sessions-unified-frames-session*)))

(defun fmsession-modify-name (fun)
  (mapcar #'(lambda (x)
              (setcar x (funcall fun (car x)))
              x)
          (copy-tree *sessions-unified-frames-session*)))

(defun sessions-unified-core-fsession-store-to-file (file)
  (interactive "Ffile: ")
  (with-temp-file file
    (insert
     (prin1-to-string *sessions-unified-frames-session*))))

(defun sessions-unified-core-fsession-restore-from-file (file)
  (interactive "ffile: ")
  (setq *sessions-unified-frames-session*
        (append *sessions-unified-frames-session*
                (lotus-read-sexp file))))


(defun fmsession-get-locations ()
  (remove nil
          (mapcar #'(lambda (f) (frame-parameter f 'frame-spec-id))
                  (frame-list))))


(defun fmsession-read-location-internal (&optional initial-input)
  (condition-case nil
      (ido-completing-read "Session: "
                           (cl-remove-if-not #'(lambda (dir)
                                                 (not
                                                  (member dir (fmsession-get-locations))))
                                             (mapcar 'car
                                                     *sessions-unified-frames-session*))
                           nil
                           nil
                           initial-input)
    ('quit nil)))

(defun fmsession-read-location-internal (&optional initial-input)
  (condition-case nil
      (completing-read-timeout 7
                               "Session: "
                               (cl-remove-if-not #'(lambda (dir)
                                                     (let ((specs (fmsession-get-locations)))
                                                       (not (member dir specs))))
                                                 (mapcar #''car
                                                         *sessions-unified-frames-session*))
                               nil
                               nil
                               initial-input)
    ('quit nil)))

(defun fmsession-read-location (&optional initial-input)
  ;; keeps on reading name.
  (let ((locations (fmsession-get-locations))
        (used t)
        selection)
    (while used
      (setq used (member (setq selection
                               (fmsession-read-location-internal initial-input))
                         locations)))
    selection))


;;;###autoload
(defvar *sessions-unified-core-fsession-registerd-apps* nil
  "list of app accept FRAME")

;;;###autoload
(defun sessions-unified-core-fsession-store (session-name &optional frame)
  "Store the elscreen tab configuration."
  (interactive (list (fmsession-read-location)))
  ;; (elscreen-session-store session-name frame)
  (unless (assoc session-name *sessions-unified-frames-session*)
    (push (list session-name)
          *sessions-unified-frames-session*))
  (dolist (app-name *sessions-unified-core-fsession-registerd-apps*)
    (let ((frame-data (sessions-unified--get-frame-data app-name (or frame
                                                                     (selected-frame)))))
      (let ((app-fsession-alist (assoc session-name
                                       *sessions-unified-frames-session*)))
        (if (assoc app-name app-fsession-alist)
            (setcdr (assoc app-name
                           app-fsession-alist)
                    frame-data)
          (push (cons app-name frame-data)
                app-fsession-alist))))))
;;;###autoload
(defun sessions-unified-core-fsession-restore (session-name &optional frame)
  "Restore the elscreen tab configuration."
  (interactive
   (list (fmsession-read-location)))
  (if session-name
      (when (assoc session-name *sessions-unified-frames-session*)
        (dolist (app-name *sessions-unified-core-fsession-registerd-apps*)
          (session-unfiy-notify "start")
          (let ((app-fsession-alist (assoc session-name
                                           *sessions-unified-frames-session*)))
            (let ((frame-data (cdr (assoc app-name
                                          app-fsession-alist))))
              (when session-unified-debug
                (session-unfiy-notify "Nstart: session-session %s" app-name))
              (if frame-data
                  (sessions-unified--set-frame-data app-name frame-data (or frame
                                                                            (selected-frame)))
                (session-unfiy-notify "Error: frame-data %s" frame-data))))))
    (session-unfiy-notify "Error: session-name is %s" session-name)))



(defun sessions-unified-core-fsession-get-wm-desktop-name ()
  let ((xwin-enabled    (protable-display-graphic-p))
       (wm-hints        (if xwin-enabled (ignore-errors (emacs-panel-wm-hints))))
       (wm-desktop-name (if wm-hints
                            (nth (nth 1 (assoc 'current-desktop wm-hints))
                                 (cdr  (assoc 'wm-desktop-names wm-hints))))))
  (if xwin-enabled
      (unless wm-hints
        (session-unfiy-notify "Some error in wm-hints")))
  wm-desktop-name)

(defun frame-session-set-this-location (frame &optional try-guessing)
  "Possible value of TRY_GUESS is T or 'ONLY"
  ;; ask, guess-ask, guess-notask
  ;; nil ask
  ;; guess
  ;; not-ask
  (interactive
   (list (selected-frame)))

  (if frame
      (funcall session-unified-utils-select-frame-fn frame)
    (error "frame is nil"))

  (session-unfiy-notify "in frame-session-set-this-location")

  (let* ((xwin-enabled    (protable-display-graphic-p))
         (wm-desktop-name (sessions-unified-core-fsession-get-wm-desktop-name))
         (location        (if (and try-guessing
                                   wm-desktop-name
                                   (member wm-desktop-name
                                           (mapcar #'car
                                                   *sessions-unified-frames-session*)))
                              (progn
                                (session-unfiy-notify "NO need to call interactive (fmsession-read-location wm-desktop-name[%s])"
                                                      wm-desktop-name)
                                wm-desktop-name)
                            (progn
                              (if (eq try-guessing 'only)
                                  (session-unfiy-notify "could not guess will return nil as try-guessing = %s set."
                                                        try-guessing)
                                (session-unfiy-notify "NEED to call interactive (fmsession-read-location wm-desktop-name[%s])"
                                                      wm-desktop-name))
                              ;; BUG: causing first emacsclient frame to be jammed which require pkill -USR2 emacs
                              (unless (eq try-guessing 'only)
                                (fmsession-read-location wm-desktop-name))))))
    (session-unfiy-notify "%s" location)
    (when location
      (set-frame-parameter frame 'frame-spec-id location))
    location))


(defun frame-session-restore (frame &optional try-guessing)
  (when t
    (session-unfiy-notify "in frame-session-restore")
    (if (and *sessions-unified-frame-session-restore-lock*
             (null *desktop-vc-read-inprogress*))
        (progn
          (session-unfiy-notify "pass in frame-session-restore")
          (if frame
              (funcall session-unified-utils-select-frame-fn frame)
            (error "frame is nil"))

          (sessions-unified-core-fsession-restore
           (frame-session-set-this-location frame try-guessing))

          ;; (if (fboundp 'elscreen-get-conf-list)
          ;;     (sessions-unified-core-fsession-restore
          ;;      (frame-session-set-this-location frame try-guessing))
          ;;   (when nil
          ;;     (with-eval-after-load "elscreen"
          ;;       ;; see if gets run again and again.
          ;;       (progn
          ;;         (sessions-unified-core-fsession-restore
          ;;          (frame-session-set-this-location (or frame (selected-frame)) try-guessing))))))

          (when (and *session-unified-frame-session-restore-display-function*
                     (functionp '*session-unified-frame-session-restore-display-function*))
            (funcall *session-unified-frame-session-restore-display-function*))
          frame)
      (progn
        (session-unfiy-notify "not restoring screen session.")
        (if *desktop-vc-read-inprogress*
            (session-unfiy-notify "as desktop restore is in progress *desktop-vc-read-inprogress* %s"
                                  *desktop-vc-read-inprogress*))
        (if (null *sessions-unified-frame-session-restore-lock*)
            (session-unfiy-notify "as another frame session restore in progress *sessions-unified-frame-session-restore-lock* %s"
                                  *sessions-unified-frame-session-restore-lock*))))))

(defun frame-session-restore-force (frame)
  (let ((location (frame-parameter (selected-frame) 'frame-spec-id)))
    (if location
        (session-unfiy-notify "already location %s set" location))
    (frame-session-restore frame t)))

(defun frame-session-restore-uninteractive (frame)
  (let ((location (frame-parameter (selected-frame) 'frame-spec-id)))
    (if location
        (session-unfiy-notify "already location %s set" location))
    (frame-session-restore frame 'only)))

(defun frame-session-apply (frame)
  "Apply existing frame session to FRAME."
  (interactive
   (list (selected-frame)))
  (progn
    (funcall session-unified-utils-select-frame-fn frame)
    (sessions-unified-core-fsession-restore (fmsession-read-location) frame)))

(defun frame-session-save (frame)
  (session-unfiy-notify "in frame-session-save:")
  (let ((location (frame-parameter frame 'frame-spec-id)))
    (when location
      (session-unfiy-notify "saved the session for %s" location)
      (sessions-unified-core-fsession-store location
                                            frame))))

;;;###autoload
(defun save-all-frames-session ()
  (dolist (f (frame-list))
    (frame-session-save f)))

;;;###autoload
(defun frame-session-restore-hook-func ()
  "Add to hook"
  ;; (add-hook 'after-make-frame-functions 'frame-session-set-this-location t)
  (session-unfiy-notify "adding frame-session-restore-hook-func hooks")
  (when t
    (add-hook 'after-make-frame-functions
              #'frame-session-restore-uninteractive)
    (add-hook 'after-make-frame-functions ; frame-session-restore-force need user input so putting at end of hook
              #'frame-session-restore-force t)
    (add-hook 'delete-frame-functions
              #'frame-session-save)))
;;;###autoload
(defun frame-session-restore-unhook-func ()
  "Add to hook"
  ;; (add-hook 'after-make-frame-functions 'frame-session-set-this-location t)
  (remove-hook 'after-make-frame-functions
               #'frame-session-restore-uninteractive)
  (remove-hook 'after-make-frame-functions
               #'frame-session-restore-force)
  (remove-hook 'delete-frame-functions
               #'frame-session-save))

(defun frame-session-check-hook-func ()
  (if (called-interactively-p 'interactive)
      (progn
        (lotus-show-hook-member 'frame-session-restore-force 'after-make-frame-functions)
        (lotus-show-hook-member 'frame-session-save 'delete-frame-functions))
    (progn
      (member #'frame-session-restore-force after-make-frame-functions)
      (member #'frame-session-save delete-frame-functions))))


;;;###autoload
(defun frame-session-restore-add-hooks ()
  (interactive)
  (frame-session-restore-hook-func))

;;;###autoload
(defun frame-session-restore-remove-hooks ()
  (interactive)
  (frame-session-restore-unhook-func))

;;;###autoload
(defun frame-session-name ()
  (interactive)
  (message "Session: %s"
           (frame-parameter (selected-frame) 'frame-spec-id)))


(cl-defmethod sessions-unified--session-store ((app (eql :fsession)))
  (save-all-frames-session))
(cl-defmethod sessions-unified--session-restore ((app (eql :fsession)))

  (when (y-or-n-p-with-timeout (format "Do you want to set session of frame? ")
                               10 t)
    (let ((*sessions-unified-frame-session-restore-lock* t))
      (frame-session-restore (selected-frame) 'only))))
(cl-defmethod sessions-unified--session-enable ((app (eql :fsession)))
  (frame-session-restore-hook-func)
  (cl-call-next-method))
(cl-defmethod sessions-unified--session-disable ((app (eql :fsession)))
  (frame-session-restore-unhook-func)
  (cl-call-next-method))
(cl-defmethod sessions-unified--session-check ((app (eql :fsession)))
  (frame-session-check-hook-func))



(when session-unified-debug
  (frame-parameter (selected-frame) 'frame-spec-id)
  after-make-frame-functions
  delete-frame-functions
  *lotus-after-init-hook*)

;;; sessions-unified-core-fsession.el ends here
