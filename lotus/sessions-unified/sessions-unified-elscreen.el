;;; sessions-unified-elscreen.el --- sessions for elscreen  -*- lexical-binding: t; -*-

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

(provide 'sessions-unified-elscreen)


(require 'sessions-unified-core-fsession)


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
     (let ((elscrn-fn #'(lambda (screen)
                          ;; If nickname exists, use it.
                          (setq screen-name (elscreen-get-screen-nickname screen))
                          ;; Nickname does not exist, so examine major-mode and buffer-name.
                          (when (null screen-name)
                            (elscreen-goto-internal screen)

                            (setq nickname-type-map
                                  (mapcar #'(lambda (window)
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
                                (let ((type      (car (car nickname-type-map)))
                                      (buff-file (cdr (car nickname-type-map))))
                                  (when buff-file
                                    (setq nickname-list (cons buff-file
                                                              nickname-list)))
                                  (setq nickname-type-map
                                        (if (eq type 'nickname)
                                            (delete (car nickname-type-map) nickname-type-map)
                                          (cdr nickname-type-map)))))
                              ;; (setq screen-name
                              ;;       (mapconcat 'identity (reverse nickname-list) ":"))
                              (setq screen-name (reverse nickname-list))))
                          ;; (sessions-unified-set-alist 'screen-to-name-alist screen screen-name)
                          (push (cons screen screen-name)
                                screen-to-name-alist))))
       (elscreen-save-screen-excursion
        (mapcar elscrn-fn
                screen-list)))
     ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
     (reverse screen-to-name-alist))))
;;
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
       ;; (session-unify-notify "desktop-buffers: %s" desktop-buffers)
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


(defun server-create-frame-around-adrun ()
  "remove-scratch-buffer"
  (if *elscreen-session-restore-data*
      (let* ((buffer-file (get-buffer (cdr (assoc 'cb *elscreen-session-restore-data*))))
             (file-path  (if (consp buffer-file)
                             (cdr buffer-file)))
             (buff (or (if file-path
                           (find-buffer-visiting file-path))
                       (if (consp buffer-file)
                           (car buffer-file)
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


;; (defun elscreen-session-session-list-get (&optional nframe)
(cl-defmethod sessions-unified--get-frame-data ((app (eql :elsession)) frame)
  (with-selected-frame (or nframe (selected-frame))
    (let (fsession-data)
      (push (cons 'screens (lotus-elscreen-get-screen-to-name-alist)) fsession-data)
      (push (cons 'current-buffer-file (cons (buffer-name (current-buffer)) (buffer-file-name))) fsession-data)
      (push (cons 'current-screen (elscreen-get-current-screen)) fsession-data)
      (push (cons 'desktop-buffers (lotus-elscreen-get-desktop-buffer-args-list)) fsession-data))))

;; (defun elscreen-session-session-list-set (fsession-data &optional nframe)
(cl-defmethod sessions-unified--set-frame-data ((app (eql :elsession)) frame  data)
  (let ((fsession-data data))
    (if (and (fboundp 'elscreen-get-conf-list)
             (elscreen-get-conf-list 'screen-history))
        (progn
          ;; TODO BUG minibuffer should not get windows, which is happening now
          (let ((nframe (or nframe (selected-frame))))
            (unless (elscreen-get-frame-confs nframe)
              (elscreen-make-frame-confs nframe)))

          (if fsession-data                    ;may causing error
              (with-selected-frame (or nframe (selected-frame))

                (if (and elscreen-frame-confs
                         (elscreen-get-frame-confs nframe))
                    (let* ((desktop-buffers (cdr (assoc 'desktop-buffers
                                                        fsession-data)))
                           (screens (or (cdr (assoc 'screens
                                                    fsession-data))
                                        `((,(length fsession-data) "*scratch*"))))
                           (session-current-screen-buffers (nth 1 (assoc (cdr (assoc 'current-screen
                                                                                     fsession-data))
                                                                         screens)))
                           (session-current-buffer-file (cdr (assoc 'current-buffer-file
                                                                    fsession-data))))
                      ;; (when t
                      (when session-unified-debug
                        (session-unify-notify "Bstart: session-current-screen-buffers %s" session-current-screen-buffers)
                        (session-unify-notify "Astart: screen-to-name-alist %s" fsession-data)
                        (session-unify-notify "Cstart: desktop-buffers %s" desktop-buffers))

                      ;; ready file for buffer in fsession-data, using desktop-restore methods
                      (if desktop-buffers
                          ;; recreate desktop buffer if not present.
                          (let ((bufs (mapcar #'(lambda (bl) (nth 2 bl))
                                              desktop-buffers)))
                            (session-unify-notify "Please wait I am busy to restore %d\nbuffers %s"
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
                                  (session-unify-notify "restoring %s" bufname)
                                  (if (find-buffer-visiting file-path)
                                      (session-unify-notify "buffer %s already here" bufname)
                                    (if (stringp bufname)
                                        (if (get-buffer bufname)
                                            (session-unify-notify "buffer %s already here" bufname)
                                          (let ()
                                            (session-unify-notify "Hello 1")
                                            (session-unify-notify "Desktop lazily opening %s" bufname)
                                            (unless (ignore-errors
                                                      (save-window-excursion
                                                        (apply 'desktop-create-buffer desktop-buffer-args)))
                                              (session-unify-notify "Desktop lazily opening Failed."))
                                            (session-unify-notify "Hello 2")
                                            (session-unify-notify "restored %s" bufname)))
                                      (session-unify-notify "bufname: %s is not string" bufname))))))
                            (session-unify-notify "Restored %d\nbuffers %s"
                                                  (length desktop-buffers) bufs))
                        (session-unify-notify "No desktop-buffers"))

                      ;; setup elscreens with buffers
                      (while screens
                        (session-unify-notify "while screen: %s" screens)
                        ;; (setq screen (car (car screens)))
                        ;; (setq buff-files (cdr  (car screens)))
                        (let* ((screen         (car (car screens)))
                               (buff-files     (cdr  (car screens)))
                               (not-first-buff nil))

                          (while buff-files
                            (if (and elscreen-frame-confs
                                     (elscreen-get-frame-confs nframe))
                                (progn

                                  (unless (eq screen 0)
                                    (elscreen-create))

                                  (let* ((buff-file  (car buff-files))
                                         (file-path  (if (consp buff-file)
                                                         (cdr buff-file)))
                                         (buff (ignore-errors
                                                 (get-buffer
                                                  (or (if file-path
                                                          (find-buffer-visiting file-path))
                                                      (if (consp buff-file)
                                                          (car buff-file)
                                                        buff-file)))))
                                         (minibuff-name " *Minibuf"))
                                    (session-unify-notify "  while buff: %s file-path: %s" buff file-path)
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
                                    (session-unify-notify "test4")))
                              (error "3 Screen is not active for frame %s" nframe))

                            (setq buff-files (cdr buff-files))

                            (session-unify-notify "progn buff-files: %s" buff-files)
                            (when session-unified-debug (session-unify-notify "else"))))

                        (setq screens (cdr screens))
                        (session-unify-notify "while screen: %s" screens)
                        (session-unify-notify "test5")) ;; (while screens

                      ;; (when elscreen-session-restore-create-scratch-buffer
                      ;;   (elscreen-find-and-goto-by-buffer (get-buffer-create "*scratch*") t t))

                      (if (and elscreen-frame-confs
                               (elscreen-get-frame-confs nframe))
                          (progn
                            (when nil (elscreen-create))                 ;trap
                            ;; set current screen, window, and buffer.
                            (let* ((file-path  (if (consp session-current-buffer-file)
                                                   (cdr session-current-buffer-file)))
                                   (buff
                                    (ignore-errors
                                      (get-buffer
                                       (or (if file-path
                                               (find-buffer-visiting file-path))
                                           (if (consp session-current-buffer-file)
                                               (car session-current-buffer-file)
                                             session-current-buffer-file))))))
                              (when (and buff
                                         (bufferp buff))
                                (elscreen-find-and-goto-by-buffer buff nil nil)
                                (setq *elscreen-session-restore-data* session-current-buffer-file))))
                        (error "2 Screen is not active for frame %s" nframe)))
                  (error "1 Screen is not active for frame %s" nframe)

                  ;; (let* ((desktop-buffers
                  (when session-unified-debug
                    (session-unify-notify "elscreen-notify-screen-modification"))
                  (elscreen-notify-screen-modification 'force-immediately)
                  (session-unify-notify "elscreen-session-session-list-set: DONE.")))

            (session-unify-notify "elscreen-session-session-list-set: Error: Session do not exists.")))
      (prog1
          nil
        (session-unify-notify "Error: not restoring screen session as screen-history config not found.")))))

;;; sessions-unified-elscreen.el ends here
