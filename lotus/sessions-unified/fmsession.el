;;; fmsession.el --- Frame session management        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
;; Keywords: convenience, frames, internal, tools

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

;; fmsession = frame + elscreen
;; centaur-tabs < awasome tabs
;; tab-line-mode

;;; Code:

(provide 'fmsession)


;; (eval-when-compile
;;   (require 'elscreen))
(require 'emacs-panel)
(require 'desktop)
(require 'session)
;; (require 'elscreen)
(require 'utils-custom)


(require 'session-unified)
(require 'desktop-unified)
(require 'sessions-unified)


(defvar session-unified-debug nil)


(defvar session-unified-utils-select-frame-fn #'select-frame-set-input-focus "session-unified-utils-select-frame-fn")
;; (setq session-unified-utils-select-frame-fn #'select-frame)


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


;; Not required
(defun sessions-unified-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

;; Not required
(defun sessions-unified-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (sessions-unified-put-alist key value (symbol-value symbol))))

;; (lotus-elscreen-get-screen-to-name-alist)
(with-eval-after-load "elscreen"
  (defun lotus-elscreen-get-screen-to-name-alist ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((lexical-binding nil)
            (screen-list (sort (elscreen-get-screen-list) '<))
            screen-name
            screen-to-name-alist
            nickname-type-map)
       (elscreen-save-screen-excursion
        (mapcar
         #'(lambda (screen)
             ;; If nickname exists, use it.
             (setq screen-name (elscreen-get-screen-nickname screen))
             ;; Nickname does not exist, so examine major-mode and buffer-name.
             (when (null screen-name)
               (elscreen-goto-internal screen)

               (setq nickname-type-map
                     (mapcar
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (or (elscreen-get-alist-to-nickname elscreen-mode-to-nickname-alist-internal
                                                              'string-match
                                                              (symbol-name major-mode))
                              (elscreen-get-alist-to-nickname elscreen-buffer-to-nickname-alist-internal
                                                              'string-match
                                                              (buffer-name))
                              (cons 'buffer-name (cons (buffer-name)
                                                       (buffer-file-name))))))
                      (window-list)))

               (let (nickname-list)
                 (while (> (length nickname-type-map) 0)
                   (let ((type (cl-first (cl-first nickname-type-map)))
                         (buff-file (cl-rest (cl-first nickname-type-map))))
                     (when buff-file
                       (setq nickname-list (cons buff-file nickname-list)))
                     (setq nickname-type-map
                           (if (eq type 'nickname)
                               (delete (cl-first nickname-type-map) nickname-type-map)
                             (cl-rest nickname-type-map)))))
                 ;; (setq screen-name
                 ;;       (mapconcat 'identity (reverse nickname-list) ":"))
                 (setq screen-name (reverse nickname-list))))

             ;; (sessions-unified-set-alist 'screen-to-name-alist screen screen-name)
             (push (cons screen screen-name) screen-to-name-alist))
         screen-list))

       ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
       (reverse screen-to-name-alist))))


  (defun lotus-elscreen-get-screen-to-name-alist ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((lexical-binding nil)
            (screen-list (sort (elscreen-get-screen-list) '<))
            screen-name
            screen-to-name-alist
            nickname-type-map)
       (elscreen-save-screen-excursion
        (mapcar
         #'(lambda (screen)
             ;; If nickname exists, use it.
             (setq screen-name (elscreen-get-screen-nickname screen))
             ;; Nickname does not exist, so examine major-mode and buffer-name.
             (when (null screen-name)
               (elscreen-goto-internal screen)

               (setq nickname-type-map
                     (mapcar
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (or (elscreen-get-alist-to-nickname
                               elscreen-mode-to-nickname-alist-internal
                               'string-match (symbol-name major-mode))
                              (elscreen-get-alist-to-nickname
                               elscreen-buffer-to-nickname-alist-internal
                               'string-match (buffer-name))
                              (cons 'buffer-name (cons (buffer-name) (buffer-file-name))))))
                      (window-list)))

               (let (nickname-list)
                 (while (> (length nickname-type-map) 0)
                   (let ((type (cl-first (cl-first nickname-type-map)))
                         (buff-file (cl-rest (cl-first nickname-type-map))))
                     (when buff-file
                       (setq nickname-list (cons buff-file nickname-list)))
                     (setq nickname-type-map
                           (if (eq type 'nickname)
                               (delete (cl-first nickname-type-map) nickname-type-map)
                             (cl-rest nickname-type-map)))))
                 ;; (setq screen-name
                 ;;       (mapconcat 'identity (reverse nickname-list) ":"))
                 (setq screen-name (reverse nickname-list))))

             ;; (sessions-unified-set-alist 'screen-to-name-alist screen screen-name)
             (push (cons screen screen-name) screen-to-name-alist))
         screen-list))

       ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
       (reverse screen-to-name-alist)))))


(defun lotus-elscreen-get-screen-to-name-alist ()
  ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
  (elscreen-notify-screen-modification-suppress
   (elscreen-set-window-configuration (elscreen-get-current-screen)
                                      (elscreen-current-window-configuration))
   (let* ((lexical-binding nil)
          (screen-list (sort (elscreen-get-screen-list) '<))
          screen-name
          screen-to-name-alist
          nickname-type-map)
     (elscreen-save-screen-excursion
      (mapcar
       #'(lambda (screen)
           ;; If nickname exists, use it.
           (setq screen-name (elscreen-get-screen-nickname screen))
           ;; Nickname does not exist, so examine major-mode and buffer-name.
           (when (null screen-name)
             (elscreen-goto-internal screen)

             (setq nickname-type-map
                   (mapcar
                    (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (or (elscreen-get-alist-to-nickname
                             elscreen-mode-to-nickname-alist-internal
                             'string-match (symbol-name major-mode))
                            (elscreen-get-alist-to-nickname
                             elscreen-buffer-to-nickname-alist-internal
                             'string-match (buffer-name))
                            (cons 'buffer-name (cons (buffer-name) (buffer-file-name))))))
                    (window-list)))

             (let (nickname-list)
               (while (> (length nickname-type-map) 0)
                 (let ((type (cl-first (cl-first nickname-type-map)))
                       (buff-file (cl-rest (cl-first nickname-type-map))))
                   (when buff-file
                     (setq nickname-list (cons buff-file nickname-list)))
                   (setq nickname-type-map
                         (if (eq type 'nickname)
                             (delete (cl-first nickname-type-map) nickname-type-map)
                           (cl-rest nickname-type-map)))))
               ;; (setq screen-name
               ;;       (mapconcat 'identity (reverse nickname-list) ":"))
               (setq screen-name (reverse nickname-list))))

           ;; (sessions-unified-set-alist 'screen-to-name-alist screen screen-name)
           (push (cons screen screen-name) screen-to-name-alist))
       screen-list))

     ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
     (reverse screen-to-name-alist))))

(defun lotus-elscreen-get-desktop-buffer-args-list ()
  ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
  (elscreen-notify-screen-modification-suppress
   (elscreen-set-window-configuration (elscreen-get-current-screen)
                                      (elscreen-current-window-configuration))
   (let* ((screen-list (sort (elscreen-get-screen-list) '<))
          screen-name)
     (let ((desktop-buffers (elscreen-save-screen-excursion
                             (mapcan #'(lambda (screen)
                                         ;; If nickname exists, use it.
                                         (setq screen-name (elscreen-get-screen-nickname screen))
                                         ;; Nickname does not exist, so examine major-mode and buffer-name.
                                         (when (null screen-name)
                                           (elscreen-goto-internal screen)
                                           (mapcar #'(lambda (window)
                                                       (window-buffer window))
                                                   (window-list))))
                                     screen-list))))
       ;; (session-unfiy-notify "desktop-buffers: %s" desktop-buffers)
       (when desktop-buffers
         (remove nil
                 (mapcar #'desktop-make-create-buffer-list
                         desktop-buffers)))))))

;; with-eval-after-load "elscreen"

;; (defvar elscreen-session-restore-create-scratch-buffer nil "elscreen-session-restore-create-scratch-buffer")

;; (setq desktop-base-file-name "session.desktop")

;;{{ http://stackoverflow.com/a/13711234
;; from: http://stackoverflow.com/questions/847962/what-alternate-session-managers-are-available-for-emacs
;; (desktop-save (fmsession-read-location))
;; (desktop-read (fmsession-read-location))

;; (desktop-make-create-buffer-list (current-buffer))

;; (require 'utils-config)

(defvar *elscreen-session-restore-data* nil "elscreen session restore data like current screen buffer among multiple screens.")

(defun elscreen-session-session-list-get (&optional nframe)
  (with-selected-frame (or nframe (selected-frame))
    (let (session-list)
      (push (cons 'screens (lotus-elscreen-get-screen-to-name-alist)) session-list)
      (push (cons 'current-buffer-file (cons (buffer-name (current-buffer)) (buffer-file-name))) session-list)
      (push (cons 'current-screen (elscreen-get-current-screen)) session-list)
      (push (cons 'desktop-buffers (lotus-elscreen-get-desktop-buffer-args-list)) session-list))))

(defun elscreen-session-session-list-set (session-list &optional nframe)
  ;; TODO BUG minibuffer should not get windows, which is happening now
  (let ((nframe (or nframe (selected-frame))))
    (unless (elscreen-get-frame-confs nframe)
      (elscreen-make-frame-confs nframe)))

  (if session-list                    ;may causing error
      (with-selected-frame (or nframe (selected-frame))

        (if (and elscreen-frame-confs
                 (elscreen-get-frame-confs nframe))
            (let* ((desktop-buffers
                    (cl-rest (assoc 'desktop-buffers session-list)))
                   (screens
                    (or
                     (cl-rest (assoc 'screens session-list))
                     `((,(length session-list) "*scratch*"))))
                   (session-current-screen-buffers
                    (nth 1 (assoc (cl-rest (assoc 'current-screen session-list))
                                  screens)))
                   (session-current-buffer-file
                    (cl-rest (assoc 'current-buffer-file session-list))))
              ;; (when t
              (when session-unified-debug
                (session-unfiy-notify "Bstart: session-current-screen-buffers %s" session-current-screen-buffers)
                (session-unfiy-notify "Astart: screen-to-name-alist %s" session-list)
                (session-unfiy-notify "Cstart: desktop-buffers %s" desktop-buffers))

              ;; ready file for buffer in session-list, using desktop-restore methods
              (if desktop-buffers
                  ;; recreate desktop buffer if not present.
                  (let ((bufs (mapcar
                               #'(lambda (bl) (nth 2 bl))
                               desktop-buffers)))
                    (session-unfiy-notify "Please wait I am busy to restore %d\nbuffers %s"
                                          (length desktop-buffers) bufs)
                    (let ((desktop-buffer-ok-count 0)
                          (desktop-buffer-fail-count 0)
                          desktop-first-buffer)
                      (ignore desktop-buffer-ok-count)
                      (ignore desktop-buffer-fail-count)
                      (ignore desktop-first-buffer)
                      (dolist (desktop-buffer-args desktop-buffers)
                        (let ((bufname (nth 2 desktop-buffer-args))
                              (file-path (nth 1 desktop-buffer-args)))
                          (session-unfiy-notify "restoring %s" bufname)
                          (if (find-buffer-visiting file-path)
                              (session-unfiy-notify "buffer %s already here" bufname)
                            (if (stringp bufname)
                                (if (get-buffer bufname)
                                    (session-unfiy-notify "buffer %s already here" bufname)
                                  (let ()
                                    (session-unfiy-notify "Hello 1")
                                    (session-unfiy-notify "Desktop lazily opening %s" bufname)
                                    (unless (ignore-errors
                                              (save-window-excursion
                                                (apply 'desktop-create-buffer desktop-buffer-args)))
                                      (session-unfiy-notify "Desktop lazily opening Failed."))
                                    (session-unfiy-notify "Hello 2")
                                    (session-unfiy-notify "restored %s" bufname)))
                              (session-unfiy-notify "bufname: %s is not string" bufname))))))
                    (session-unfiy-notify "Restored %d\nbuffers %s"
                                          (length desktop-buffers) bufs))
                (session-unfiy-notify "No desktop-buffers"))

              ;; setup elscreens with buffers
              (while screens
                (session-unfiy-notify "while screen: %s" screens)
                ;; (setq screen (cl-first (cl-first screens)))
                ;; (setq buff-files (cl-rest  (cl-first screens)))
                (let* ((screen         (cl-first (cl-first screens)))
                       (buff-files     (cl-rest  (cl-first screens)))
                       (not-first-buff nil))

                  (while buff-files
                    (if (and elscreen-frame-confs
                             (elscreen-get-frame-confs nframe))
                        (progn

                          (unless (eq screen 0)
                            (elscreen-create))

                          (let* ((buff-file  (cl-first buff-files))
                                 (file-path  (if (consp buff-file)
                                                 (cl-rest buff-file)))
                                 (buff (ignore-errors
                                         (get-buffer
                                          (or (if file-path
                                                  (find-buffer-visiting file-path))
                                              (if (consp buff-file)
                                                  (cl-first buff-file)
                                                buff-file)))))
                                 (minibuff-name " *Minibuf"))
                            (session-unfiy-notify "  while buff: %s file-path: %s" buff file-path)
                            (when (and buff
                                       (bufferp buff)
                                       (not
                                        (string-equal (substring (buffer-name buff) 0 (min (length (buffer-name buff)) (length minibuff-name)))
                                                      minibuff-name))) ;check once for if buff is here or not.
                              ;; newly added here to avoid " *Minibuffer*"
                              (if not-first-buff
                                  (switch-to-buffer-other-window buff)
                                (switch-to-buffer buff)
                                (setq not-first-buff t)))
                            (session-unfiy-notify "test4")))
                      (error "3 Screen is not active for frame %s" nframe))

                    (setq buff-files (cl-rest buff-files))

                    (session-unfiy-notify "progn buff-files: %s" buff-files)
                    (when session-unified-debug (session-unfiy-notify "else"))))

                (setq screens (cl-rest screens))
                (session-unfiy-notify "while screen: %s" screens)
                (session-unfiy-notify "test5")) ;; (while screens

              ;; (when elscreen-session-restore-create-scratch-buffer
              ;;   (elscreen-find-and-goto-by-buffer (get-buffer-create "*scratch*") t t))

              (if (and elscreen-frame-confs
                       (elscreen-get-frame-confs nframe))
                  (progn
                    (when nil (elscreen-create))                 ;trap
                    ;; set current screen, window, and buffer.
                    (let* ((file-path  (if (consp session-current-buffer-file)
                                           (cl-rest session-current-buffer-file)))
                           (buff
                            (ignore-errors
                              (get-buffer
                               (or (if file-path
                                       (find-buffer-visiting file-path))
                                   (if (consp session-current-buffer-file)
                                       (cl-first session-current-buffer-file)
                                     session-current-buffer-file))))))
                      (when (and buff
                                 (bufferp buff))
                        (elscreen-find-and-goto-by-buffer buff nil nil)
                        (setq *elscreen-session-restore-data* session-current-buffer-file))))
                (error "2 Screen is not active for frame %s" nframe)))
          (error "1 Screen is not active for frame %s" nframe))

        ;; (let* ((desktop-buffers
        (when session-unified-debug
          (session-unfiy-notify "elscreen-notify-screen-modification"))
        (elscreen-notify-screen-modification 'force-immediately)
        (session-unfiy-notify "elscreen-session-session-list-set: DONE."))

    (session-unfiy-notify "elscreen-session-session-list-set: Error: Session do not exists.")))

(defvar *frames-elscreen-session* nil "Stores all elscreen sessions here.")
(defvar *frames-elscreen-session-old* nil "Stores all discarded elscreen sessions here.")

(eval-after-load "desktop"
  '(progn
     (add-to-list
      'desktop-globals-to-save
      '*frames-elscreen-session*)
     (add-to-list
      'desktop-globals-to-save
      '*frames-elscreen-session-old*)))

(eval-after-load "session"
  '(progn
     (add-to-list
      'session-globals-include
      '(*frames-elscreen-session* 100))
     (add-to-list
      'session-globals-include
      '(*frames-elscreen-session-old* 100))))

(defun fmsession-migration ()
  (interactive)
  (dolist (session (directory-files "~/.emacs.d/.cache/session-unified/session/frames/" nil "[a-zA-Z]+"))
    (cl-pushnew
     (cons session
           (lotus-read-sexp
            (concat "~/.emacs.d/.cache/session-unified/session/frames/" session "/elscreen")))
     *frames-elscreen-session*)))


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
                  *frames-elscreen-session*
                  :key 'car
                  :test 'string-equal)
         *frames-elscreen-session-old*)
        (setq *frames-elscreen-session*
              (cl-remove session *frames-elscreen-session*
                         :key 'car
                         :test 'string-equal)))
    (session-unfiy-notify "Not deleting screen \"%s\" session: " session)))


(defun fmsession-modify-element (fun)
  (mapcar fun
          (copy-tree *frames-elscreen-session*)))

(defun fmsession-modify-name (fun)
  (mapcar #'(lambda (x)
              (setcar x (funcall fun (cl-first x)))
              x)
          (copy-tree *frames-elscreen-session*)))

(defun fmsession-store-to-file (file)
  (interactive "Ffile: ")
  (with-temp-file file
    (insert
     (prin1-to-string *frames-elscreen-session*))))

(defun fmsession-restore-from-file (file)
  (interactive "ffile: ")
  (setq *frames-elscreen-session*
        (append *frames-elscreen-session*
                (lotus-read-sexp file))))


(defun fmsession-get-locations ()
  (remove nil
          (mapcar #'(lambda (f) (frame-parameter f 'frame-spec-id))
                  (frame-list))))


(defun elscreen-session-store (elscreen-session &optional nframe)
  (interactive
   (list (fmsession-read-location)))
  (let ((session-list (elscreen-session-session-list-get (or nframe
                                                             (selected-frame)))))
    (if (assoc elscreen-session *frames-elscreen-session*)
        (setcdr (assoc elscreen-session
                       *frames-elscreen-session*)
                session-list)
      (push (cons elscreen-session
                  session-list)
            *frames-elscreen-session*))))

(defun elscreen-session-restore (elscreen-session &optional nframe)
  (interactive
   (list (fmsession-read-location)))
  (session-unfiy-notify "start")
  (if elscreen-session
      (let ((elscreen-session-list
             (cl-rest (assoc elscreen-session
                             *frames-elscreen-session*))))
        (when session-unified-debug
          (session-unfiy-notify "Nstart: session-session %s" elscreen-session))
        (if elscreen-session-list
            (elscreen-session-session-list-set elscreen-session-list
                                               (or nframe
                                                   (selected-frame)))
          (session-unfiy-notify "Error: elscreen-session-list %s" elscreen-session-list)))
    (session-unfiy-notify "Error: elscreen-session is %s" elscreen-session)))

(defun fmsession-read-location-internal (&optional initial-input)
  (condition-case nil
      (ido-completing-read "Session: "
                           (cl-remove-if-not #'(lambda (dir)
                                                 (not
                                                  (member dir (fmsession-get-locations))))
                                             (mapcar 'car *frames-elscreen-session*))
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
                                                 (mapcar 'car *frames-elscreen-session*))
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

;; (elscreen-restore)
;;}}

;;{{

(defvar *frame-session-restore* nil "*frame-session-restore* if it is true than only frame session will get restored.")
;; (defun server-create-frame-before-adrun ()
;;   "remove-scratch-buffer"
;;   (setq *frame-session-restore* t))

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
  (let ((*frame-session-restore* t))
    (prog1
        ad-do-it
      (server-create-frame-around-adrun))))

(defadvice server-create-tty-frame
    (around remove-scratch-buffer activate)
  "remove-scratch-buffer"
  (let ((*frame-session-restore* t))
    (prog1
        ad-do-it
      (server-create-frame-around-adrun))))
;;}}


;;{{
;; (progn


  ;; "per frame session"
  ;; (require 'emacs-panel)

(defvar *desktop-vc-read-inprogress* nil "desktop-vc-read-inpgrogress")

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
                                    (mapcar #'car *frames-elscreen-session*)))
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

(defvar *frame-session-restore-screen-display-function* #'display-about-screen
  "function to display screen with frame-session-restore, e.g.
display-about-screen, spacemacs-buffer/goto-buffer")

(defun frame-session-restore (nframe &optional try-guessing)
  (when t
    (session-unfiy-notify "in frame-session-restore")
    (if (and *frame-session-restore*
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

          (when (and *frame-session-restore-screen-display-function*
                     (functionp '*frame-session-restore-screen-display-function*))
            (funcall *frame-session-restore-screen-display-function*))
          nframe)
      (progn
        (session-unfiy-notify "not restoring screen session.")
        (if *desktop-vc-read-inprogress*
            (session-unfiy-notify "as desktop restore is in progress *desktop-vc-read-inprogress* %s"
                                  *desktop-vc-read-inprogress*))
        (if (null *frame-session-restore*)
            (session-unfiy-notify "as another frame session restore in progress *frame-session-restore* %s"
                                  *frame-session-restore*))))))

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

(when session-unified-debug
  (frame-parameter (selected-frame) 'frame-spec-id)
  after-make-frame-functions
  delete-frame-functions
  *lotus-after-init-hook*)

  ;;}}


;;; fmsession.el ends here
