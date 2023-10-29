;;; occ-util-common.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-util-common)


(require 'org-clock)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'org-clock-utils-lotus)


(require 'occ-config)
(eval-when-compile
  (require 'occ-macros))
(require 'occ-obj-accessor)
(require 'occ)
(eval-when-compile
  (require 'occ-debug-method))
(require 'occ-debug-method)
(require 'occ-obj-common)


(defvar occ-verbose 0)


;; Resume clock (Unnamed task 933) y
;; Cannot restart clock because task does not contain unfinished clock
(defvar occ-org-clock-persist nil "Control org-clock-persist at time of occ clock-in")
(defvar occ-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of occ clock-in")



(defvar occ-list-select-ap-normal-keys '(t actions general edit checkout))
(defvar occ-list-select-ap-transf-keys '(t actions general edit checkout))


(defun occ-obj-list-select-ap-normal-keys ()
  occ-list-select-ap-normal-keys)
(defun occ-obj-list-select-ap-transf-keys ()
  occ-list-select-ap-transf-keys)


;; DEPENDENCY remove it.
(defun dirname-of-file (file &optional final-slash)
  ;; (ido-no-final-slash
  (if final-slash
      (expand-file-name (file-name-directory file))
    (directory-file-name (expand-file-name (file-name-directory file)))))


;; ;;;###autoload
;; (defun occ-error (&rest args)
;;   (apply #'error args)
;;   (apply #'occ-debug :debug args))


(defun downcase-sym (sym)
  (let ((symname (downcase (symbol-name sym))))
    (or (intern-soft symname)
        (intern symname))))
(defun upcase-sym (sym)
  (let ((symname (upcase (symbol-name sym))))
    (or (intern-soft symname)
        (intern symname))))
(defun sym2key (sym)
  (if (keywordp sym)
      sym
    (or (intern-soft (concat ":" (symbol-name sym)))
        (intern (concat ":" (symbol-name sym))))))
(defun key2sym (sym)
  (if (keywordp sym)
      (or (intern-soft (substring (symbol-name sym) 1))
          (intern (substring (symbol-name sym) 1)))
    sym))


(defun occ-valid-marker (marker)
  (when (and marker
             (marker-buffer marker))
    marker))


(defun occ-chgable-p ()
  "Stay with a clock at least 2 mins."
  (if org-clock-start-time
      (let ((clock-duration
             (if (and (stringp org-clock-start-time)
                      (string= "" org-clock-start-time))
                 0
               (float-time (time-since org-clock-start-time)))))
        (or (< clock-duration 60)
            (> clock-duration 120)))
    t))

;;;###autoload
(defun occ-straight-org-clock-clock-in (clock
                                        &optional
                                        resume
                                        start-time)
  ;; lotus-org-with-safe-modification
  (let ((org-log-note-clock-out nil))
    (progn
     (lotus-org-clock-load-only)
     (prog1
         (let ((org-clock-persist               nil)  ;; occ-org-clock-persist
               (org-clock-auto-clock-resolution nil)) ;; occ-org-clock-auto-clock-resolution)
           ;; BUG: clocking in to other unnamed clock than specified
           (org-clock-clock-in clock
                               resume
                               start-time)
           t)
       (setq org-clock-loaded t)))))


(defun occ-completing-read (prompt
                            collection
                            &optional
                            predicate
                            require-match
                            initial-input
                            hist
                            def
                            inherit-input-method)
  (let ((helm-always-two-windows nil))
    (occ-debug-uncond "occ-completing-read: prompt %s collection %s"
                      prompt collection)
    (funcall occ-completing-read-function prompt
             collection
             predicate
             require-match
             initial-input
             hist
             def
             inherit-input-method)))


(defun occ-util-select-from-sym-list (prompt symlist)
  (let ((symstr (completing-read prompt
                                 symlist)))
    (intern symstr)))


(defun occ-util-combine (&rest elem-lists)
  (if (cdr elem-lists)
      (mapcan #'(lambda (ef)
                  (mapcar #'(lambda (e)
                              (cons e ef))
                          (car elem-lists)))
              (apply #'occ-util-combine (cdr elem-lists)))
    (mapcar #'(lambda (e)
                (list e))
            (car elem-lists))))


(defun occ-insert-node-before-element (node element list)
  ;; https://groups.google.com/forum/#!topic/comp.lang.lisp/83g9zkq_CQY
  (let ((pos (cl-position element list)))
    (if pos
        (if (= pos 0)
            (cons node list) ;There's no way to be destructive in this case, so just cons.
          (let ((tail (nthcdr (1- pos) list)))
            (when (null tail)
              (occ-error "There is no position ~D in ~S." pos list))
            (cl-pushnew node (cl-rest tail))
            list))
      (occ-error "not able to find element: %s" (occ-obj-format element)))))
(defun occ-insert-node-after-element (node element list)
  ;; https://groups.google.com/forum/#!topic/comp.lang.lisp/83g9zkq_CQY
  (let ((pos (cl-position element list)))
    (if pos
        (if (= pos 0)
            (cons node list) ;There's no way to be destructive in this case, so just cons.
          (let ((tail (nthcdr pos list)))
            (when (null tail) (occ-error "There is no position ~D in ~S." pos list))
            (push node (cl-rest tail))
            list))
      (occ-error "not able to find element: %s" (occ-obj-format element)))))


(defun occ-helm-buffer-p (buffer)
  (string-match "^*helm"
                (buffer-name buffer)))


;;;###autoload
(defun occ-after-save-hook-fun ()
  (let ((file (buffer-file-name)))
    (when (and file
               (eq major-mode 'org-mode))
      (if (cl-member file
                     (occ-obj-files)
                     :test #'(lambda (f1 f2)
                               (string= (file-truename f1)
                                        (file-truename f2))))
          ;; TODO workaround do complete nil, later change it to optimized.
          ;; TODO update existing occ-collection.tree or occ-collection.list
          ;; (occ-reset-deafult-tsk-collection)
          (occ-reset-deafult-collection-object)
        (occ-debug :debug "file %s not resetting deafult-tsk-collection" file)))))


(defun occ-warn-on-buffer-kill ()
  (let ((curr-buff (current-buffer)))
    (if (memq curr-buff
              (mapcar #'find-buffer-visiting
                      (occ-obj-files)))
        (if (called-interactively-p 'interactive)
            (y-or-n-p (format "%s is being used in occ, should kill it." (current-buffer))))
      t)))

(defun occ-do-setup-buffer ()
  ;; BUG: do necessary steps for occ-buffers
  (unless (eq major-mode 'org-mode)
    (org-mode))
  (add-hook 'kill-buffer-query-functions
            'occ-warn-on-buffer-kill t t))


(defun occ-find-file-noselect (file
                               &optional
                               nowarn
                               rawfile
                               wildcards)
  (let ((org-clock-persist nil))
    ;; To avoid (org-clock-load) via org-mode-hook
    (find-file-noselect file
                        nowarn
                        rawfile
                        wildcards)))


(defun occ-obj-add-face-properties (text &rest properties)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Properties.html
  ;; Create a copy of the text string
  (let ((modified-str (copy-sequence text)))
    ;; Iterate through the text string and modify the background color at each position
    (dotimes (pos (length text))
      (let ((face-at-pos (get-text-property pos 'face text)))
        (when (= ?w
                 (char-syntax (aref text pos)))
          (add-text-properties pos (1+ pos)
                               (list 'face (cons face-at-pos
                                                 properties))
                               modified-str))))
    modified-str))


;;;###autoload
(defun occ-run-with-deafult-tsk-collection (fn)
  (if (and (occ-collector-get (occ-collector-default-key))
           (occ-default-collection))
      (when t
        (funcall fn))
    (add-hook '*occ-collection-change-hook*
              fn)))
(defalias 'occ-run-with-global-tsk-collection #'occ-run-with-deafult-tsk-collection)


;;;###autoload
(defun occ-clouser-call-obj-on-cand (obj)
  "Closed OBJ will be called on CANDIDATE arg"
  #'(lambda (candidate)
      (occ-debug "occ-clouser-call-obj-on-cand: Running fun %s on %s" obj (occ-obj-format candidate))
      (if (functionp obj)
          (funcall obj candidate)
        obj)))

;;;###autoload
(defun occ-clouser-call-cand-on-obj (obj)
  "CANDIDATE-FUN arguemnt will be called on closed OBJ"
  #'(lambda (candidate-fun)
      (occ-debug "occ-clouser-call-cand-on-obj: Running fun %s on %s" candidate-fun (occ-obj-format obj))
      (funcall candidate-fun obj)))


;;;###autoload
(defun occ-lambda-call-obj (obj)
  "Closed OBJ will be called, ignoring CANDIDATE arg"
  #'(lambda (candidate)
      (ignore candidate)
      (if (functionp obj)
          (funcall obj)
        obj)))

;;;###autoload
(defun occ-lambda-call-cand ()
  "CANDIDATE-FUN arguemnt will be called"
  #'(lambda (candidate-fun)
      (funcall candidate-fun)))


(defun occ-back-to-heading ()
  (condition-case e
      (unless (org-before-first-heading-p)
        ;; it is a file
        (org-back-to-heading t))
    ((error) (occ-error e))))

(replace-regexp-in-string "\\([A-Z]+\\)"
                          "\" (setq \\1 (skeleton-read \"\\1\")) \""
                          "--PROPERTY--")

(defun occ-line-to-skeleton (line)
  `( > ,(trim-string line)  \n))
(defvar occ-skeleton-file nil)
(defun occ-buffer-content-to-skeleton (&optional force)
  (unless (and (not force)
               occ-skeleton-file)
    (setq occ-skeleton-file
          (read-file-name "file:")))
  (let ((lines (with-current-buffer (find-file-noselect occ-skeleton-file)
                 ;; skip header
                 (goto-char (point-min))
                 (re-search-forward "^[^;]")
                 (beginning-of-line)
                 (split-string (buffer-substring-no-properties (point) (point-max)) "\n"))))
    (apply #'append
           (mapcar #'occ-line-to-skeleton
                   lines))))
(defun occ-make-skeleton (&optional force)
  (interactive "P")
  (eval `(define-skeleton occ-skeleton
           "Test"
           ,@(occ-buffer-content-to-skeleton force))))
(defun occ-run-skeleton (&optional str arg force)
  (interactive "P\nP")
  (atomic-change-group
    (skeleton-proxy-new (occ-buffer-content-to-skeleton force)
                        str
                        arg)))


(cl-defun occ-build-org-store-log-note-function (&key
                                                 success-fun
                                                 fail-fun
                                                 run-before)
  #'(lambda ()
      (let ((org-note-abort-before org-note-abort))
        (if run-before
            (unwind-protect
                (if org-note-abort-before
                    (and fail-fun
                         (funcall fail-fun))
                  (and success-fun
                       (funcall success-fun)))
              (funcall #'org-store-log-note))
          (unwind-protect
              (funcall #'org-store-log-note)
            (if org-note-abort-before
                (and fail-fun
                     (funcall fail-fun))
              (and success-fun
                   (funcall success-fun))))))))
(defvar occ-store-log-note-local-function nil)
(make-variable-buffer-local 'occ-store-log-note-local-function)

(defun occ-store-log-note-invoke-local-fun ()
  (funcall occ-store-log-note-local-function))

(cl-defun occ-add-log-note-buffer (target-buffer
                                   &key
                                   buff
                                   chgcount
                                   success
                                   fail
                                   run-before)
  "Prepare buffer for taking a note, to add this note later."
  (switch-to-buffer target-buffer 'norecord)
  (erase-buffer)
  (let ((store-log-note-function (occ-build-org-store-log-note-function :success-fun success
                                                                        :fail-fun fail
                                                                        :run-before run-before)))
    (if (memq org-log-note-how '(time state))
        (let (current-prefix-arg)
          ;; (org-store-log-note)
          (funcall store-log-note-function))
      (let ((org-inhibit-startup t))
        (org-mode))
      (let ((note-for (cond
                       ((eq org-log-note-purpose 'clock-out) "stopped clock")
                       ((eq org-log-note-purpose 'done)  "closed todo item")
                       ((eq org-log-note-purpose 'state)
                        (format "state change from \"%s\" to \"%s\""
                                (or org-log-note-previous-state "")
                                (or org-log-note-state "")))
                       ((eq org-log-note-purpose 'reschedule)
                        "rescheduling")
                       ((eq org-log-note-purpose 'delschedule)
                        "no longer scheduled")
                       ((eq org-log-note-purpose 'redeadline)
                        "changing deadline")
                       ((eq org-log-note-purpose 'deldeadline)
                        "removing deadline")
                       ((eq org-log-note-purpose 'refile)
                        "refiling")
                       ((eq org-log-note-purpose 'note)
                        "this entry")
                       (t (error "This should not happen")))))
        (insert (format (string-join
                         '("# Insert note for %s."
                           "# and %d changes in  buffer %s"
                           "# Finish with C-c C-c, or cancel with C-c C-k.\n\n")
                         "\n")
                        note-for
                        chgcount
                        (buffer-name buff))))
      (when org-log-note-extra (insert org-log-note-extra))
      ;; (setq-local org-finish-function 'org-store-log-note)
      (setq-local org-store-log-note-local-function store-log-note-function)
      (setq-local org-finish-function #'org-store-log-note-invoke-local-fun)
      (run-hooks 'org-log-buffer-setup-hook))))

(cl-defun occ-add-log-note-with-timed-new-win (win-timeout
                                               &key
                                               npurpose
                                               buff
                                               chgcount
                                               success
                                               fail
                                               run-before)
  "Pop up a window for taking a note, and add this note later."
  ;; (remove-hook 'post-command-hook 'org-add-log-note-background)
  ;; (setq org-log-note-window-configuration (current-window-configuration))
  ;; (delete-other-windows)

  ;; (move-marker org-log-note-return-to (point))
  (lotus-with-no-active-minibuffer-if
      (progn                            ;could schedule in little further.
        (lwarn 'org-onchange :debug "org-add-log-note-with-timed-new-win: [minibuff body] lotus-with-no-active-minibuffer-if")
        (lwarn 'org-onchange :debug "add-log-note-background: minibuffer already active quitting")
        (message "add-log-note-background: minibuffer already active quitting")
        (message nil))
    (lwarn 'org-onchange :debug "org-add-log-note-with-timed-new-win: [body] lotus-with-no-active-minibuffer-if")
    (let ((win-timeout (or win-timeout 7))
          (cleanupfn-local nil))
      (setq org-log-note-window-configuration (current-window-configuration))
      (lotus-with-timed-new-win
          win-timeout timer cleanupfn-newwin cleanupfn-local win
          (condition-case nil
              (let ((target-buffer (get-buffer-create "*Org Note*")))
                (org-add-log-note-buffer target-buffer
                                         :buff buff
                                         :chgcount chgcount
                                         :success success
                                         :fail fail
                                         :run-before run-before))
            ((quit)
             (progn
               (funcall cleanupfn-newwin win cleanupfn-local)
               (if timer (cancel-timer timer))
               (signal (cl-first err) (cl-rest err)))))))))



(defun occ-add-entity ()
  (interactive)
  (get-buffer-create "*Org Note*"))


;;; occ-util-common.el ends here

