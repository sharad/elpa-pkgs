;;; sessions-unified-frame.el --- Maintain session per frame for named frame  -*- lexical-binding: t; -*-

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



(provide 'sessions-unified-frame)


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
   (list
    (fmsession-read-location)))
  ;; save somewhere as backup
  (if (and
       session
       (y-or-n-p (format "Can I delete screen \"%s\" session: " session)))
      (progn
        (push
         (cl-find session
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
              (setcar x (funcall fun (cl-first x)))
              x)
          (copy-tree *sessions-unified-frames-session*)))

(defun fmsession-store-to-file (file)
  (interactive "Ffile: ")
  (with-temp-file file
    (insert
     (prin1-to-string *sessions-unified-frames-session*))))

(defun fmsession-restore-from-file (file)
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
                                                 (mapcar 'car *sessions-unified-frames-session*))
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

(defun fmsession-store (session-name &optional nframe)
  "Store the elscreen tab configuration."
  (interactive (list (fmsession-read-location)))
  (elscreen-session-store session-name nframe))

(defun fmsession-restore (session-name &optional nframe)
  "Restore the elscreen tab configuration."
  (interactive
   (list (fmsession-read-location)))
  (session-unfiy-notify "start")
  (if (and (fboundp 'elscreen-get-conf-list)
           (elscreen-get-conf-list 'screen-history))
      (elscreen-session-restore session-name nframe)
    (session-unfiy-notify "Error: not restoring screen session as screen-history config not found.")))


(defun server-create-frame-around-adrun ()
  "remove-scratch-buffer"
  (if *elscreen-session-restore-data*
      (let* ((buffer-file (get-buffer (cl-rest (assoc 'cb *elscreen-session-restore-data*))))
             (file-path  (if (consp buffer-file)
                             (cl-rest buffer-file)))
             (buff (or (if file-path
                           (find-buffer-visiting file-path))
                       (if (consp buffer-file)
                           (cl-first buffer-file)
                         buffer-file))))
        (when buff
          (elscreen-kill)
          (elscreen-find-and-goto-by-buffer buff nil nil)
          (setq *elscreen-session-restore-data* nil)
          (elscreen-notify-screen-modification 'force-immediately))))
  t)

(defadvice server-create-window-system-frame
    (around remove-scratch-buffer activate)
  "remove-scratch-buffer"
  (let ((*sessions-unified-frame-session-restore-lock* t))
    (prog1
        ad-do-it
      (server-create-frame-around-adrun))))

(defadvice server-create-tty-frame
    (around remove-scratch-buffer activate)
  "remove-scratch-buffer"
  (let ((*sessions-unified-frame-session-restore-lock* t))
    (prog1
        ad-do-it
      (server-create-frame-around-adrun))))


(defun frame-session-set-this-location (nframe &optional try-guessing)
  "Possible value of TRY_GUESS is T or 'ONLY"
  ;; ask, guess-ask, guess-notask
  ;; nil ask
  ;; guess
  ;; not-ask
  (interactive
   (list (selected-frame)))

  (if nframe
      (funcall session-unified-utils-select-frame-fn nframe)
    (error "nframe is nil"))

  (session-unfiy-notify "in frame-session-set-this-location")

  (let* ((xwin-enabled (protable-display-graphic-p))
         (wm-hints
          (if xwin-enabled
              (ignore-errors (emacs-panel-wm-hints))))
         (desktop-name (if wm-hints
                           (nth (nth 1 (assoc 'current-desktop wm-hints))
                                (cl-rest  (assoc 'desktop-names wm-hints)))))
         (location (if (and try-guessing
                            desktop-name
                            (member desktop-name
                                    (mapcar #'car
                                            *sessions-unified-frames-session*)))
                       (progn
                         (session-unfiy-notify "NO need to call interactive (fmsession-read-location desktop-name[%s])"
                                               desktop-name)
                         desktop-name)
                     (progn
                       (if (eq try-guessing 'only)
                           (session-unfiy-notify "could not guess will return nil as try-guessing = %s set."
                                                 try-guessing)
                         (session-unfiy-notify "NEED to call interactive (fmsession-read-location desktop-name[%s])"
                                               desktop-name))
                       ;; BUG: causing first emacsclient frame to be jammed which require pkill -USR2 emacs
                       (unless (eq try-guessing 'only)
                         (fmsession-read-location desktop-name))))))
    (if xwin-enabled
        (unless wm-hints
          (session-unfiy-notify "Some error in wm-hints")))
    (session-unfiy-notify "%s" location)
    (when location
      (set-frame-parameter nframe 'frame-spec-id location))
    location))


(defun frame-session-restore (nframe &optional try-guessing)
  (when t
    (session-unfiy-notify "in frame-session-restore")
    (if (and *sessions-unified-frame-session-restore-lock*
             (null *desktop-vc-read-inprogress*))
        (progn
          (session-unfiy-notify "pass in frame-session-restore")
          (if nframe
              (funcall session-unified-utils-select-frame-fn nframe)
            (error "nframe is nil"))
          (if (fboundp 'elscreen-get-conf-list)
              (fmsession-restore
               (frame-session-set-this-location nframe try-guessing))
            (when nil
              (with-eval-after-load "elscreen"
                ;; see if gets run again and again.
                (progn
                  (fmsession-restore
                   (frame-session-set-this-location (or nframe (selected-frame)) try-guessing))))))
          ;; nframe)

          (when (and *session-unified-frame-session-restore-display-function*
                     (functionp '*session-unified-frame-session-restore-display-function*))
            (funcall *session-unified-frame-session-restore-display-function*))
          nframe)
      (progn
        (session-unfiy-notify "not restoring screen session.")
        (if *desktop-vc-read-inprogress*
            (session-unfiy-notify "as desktop restore is in progress *desktop-vc-read-inprogress* %s"
                                  *desktop-vc-read-inprogress*))
        (if (null *sessions-unified-frame-session-restore-lock*)
            (session-unfiy-notify "as another frame session restore in progress *sessions-unified-frame-session-restore-lock* %s"
                                  *sessions-unified-frame-session-restore-lock*))))))

(defun frame-session-restore-force (nframe)
  (let ((location (frame-parameter (selected-frame) 'frame-spec-id)))
    (if location
        (session-unfiy-notify "already location %s set" location))
    (frame-session-restore nframe t)))

(defun frame-session-restore-uninteractive (nframe)
  (let ((location (frame-parameter (selected-frame) 'frame-spec-id)))
    (if location
        (session-unfiy-notify "already location %s set" location))
    (frame-session-restore nframe 'only)))

(defun frame-session-apply (nframe)
  "Apply existing frame session to NFRAME."
  (interactive
   (list (selected-frame)))
  (progn
    (funcall session-unified-utils-select-frame-fn nframe)
    (fmsession-restore (fmsession-read-location) nframe)))

(defun frame-session-save (nframe)
  (session-unfiy-notify "in frame-session-save:")
  (let ((location (frame-parameter nframe 'frame-spec-id)))
    (when location
      (session-unfiy-notify "saved the session for %s" location)
      (fmsession-store location nframe))))

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



(defun elscreen-session-store (elscreen-session &optional nframe)
  (interactive
   (list (fmsession-read-location)))
  (let ((session-list (elscreen-session-session-list-get (or nframe
                                                             (selected-frame)))))
    (if (assoc elscreen-session *sessions-unified-frames-session*)
        (setcdr (assoc elscreen-session
                       *sessions-unified-frames-session*)
                session-list)
      (push (cons elscreen-session
                  session-list)
            *sessions-unified-frames-session*))))

(defun elscreen-session-restore (elscreen-session &optional nframe)
  (interactive
   (list (fmsession-read-location)))
  (session-unfiy-notify "start")
  (if elscreen-session
      (let ((elscreen-session-list
             (cl-rest (assoc elscreen-session
                             *sessions-unified-frames-session*))))
        (when session-unified-debug
          (session-unfiy-notify "Nstart: session-session %s" elscreen-session))
        (if elscreen-session-list
            (elscreen-session-session-list-set elscreen-session-list
                                               (or nframe
                                                   (selected-frame)))
          (session-unfiy-notify "Error: elscreen-session-list %s" elscreen-session-list)))
    (session-unfiy-notify "Error: elscreen-session is %s" elscreen-session)))



(when session-unified-debug
  (frame-parameter (selected-frame) 'frame-spec-id)
  after-make-frame-functions
  delete-frame-functions
  *lotus-after-init-hook*)

;;; sessions-unified-frame.el ends here
