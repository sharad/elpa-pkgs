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


;; (replace-regexp-in-string "\\([A-Z]+\\)"
;;                           "\" (setq \\1 (skeleton-read \"\\1\")) \""
;;                           "--PROPERTY--")

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


(defun occ-entity-store-note (org-marker
                              return-to-marker
                              win-config)
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (prog1 (buffer-string)
               (kill-buffer)))
        (note (cdr (assq org-log-note-purpose org-log-note-headings)))
        lines)
    (while (string-match "\\`# .*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (when (string-match "\\s-+\\'" txt)
      (setq txt (replace-match "" t t txt)))
    (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))
    (when (org-string-nw-p note)
      (setq note
            (org-replace-escapes
             note
             (list (cons "%u" (user-login-name)
                         (cons "%U" user-full-name)
                         (cons "%t" (format-time-string
                                     (org-time-stamp-format 'long 'inactive)
                                     org-log-note-effective-time))
                         (cons "%T" (format-time-string
                                     (org-time-stamp-format 'long nil)
                                     org-log-note-effective-time))
                         (cons "%d" (format-time-string
                                     (org-time-stamp-format nil 'inactive)
                                     org-log-note-effective-time))
                         (cons "%D" (format-time-string
                                     (org-time-stamp-format nil nil)
                                     org-log-note-effective-time))
                         (cons "%s" (cond
                                     ((not org-log-note-state) "")
                                     ((string-match-p org-ts-regexp
                                                      org-log-note-state)
                                      (format "\"[%s]\""
                                              (substring org-log-note-state 1 -1)))
                                     (t (format "\"%s\"" org-log-note-state))))
                         (cons "%S"
                               (cond
                                ((not org-log-note-previous-state) "")
                                ((string-match-p org-ts-regexp
                                                 org-log-note-previous-state)
                                 (format "\"[%s]\""
                                         (substring
                                          org-log-note-previous-state 1 -1)))
                                (t (format "\"%s\""
                                           org-log-note-previous-state))))))))
      (when lines (setq note (concat note " \\\\")))
      (push note lines))
    (when (and lines (not org-note-abort))
      (with-current-buffer (marker-buffer org-marker)
        (org-fold-core-ignore-modifications
          (org-with-wide-buffer
           ;; Find location for the new note.
           (goto-char org-marker)
           (when nil
             (set-marker org-marker nil))
           ;; Note associated to a clock is to be located right after
           ;; the clock.  Do not move point.
           (unless (eq org-log-note-purpose 'clock-out)
             (goto-char (org-log-beginning t)))
           ;; Make sure point is at the beginning of an empty line.
           (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert-and-inherit "\n"))
                  ((looking-at "[ \t]*\\S-") (save-excursion (insert-and-inherit "\n")))))
           ;; In an existing list, add a new item at the top level.
           ;; Otherwise, indent line like a regular one.
           (let ((itemp (org-in-item-p)))
             (if itemp
                 (indent-line-to
                  (let ((struct (save-excursion
                                  (goto-char itemp)
                                  (org-list-struct))))
                    (org-list-get-ind (org-list-get-top-point struct) struct)))
               (org-indent-line)))
           (insert-and-inherit (org-list-bullet-string "-") (pop lines))
           (let ((ind (org-list-item-body-column (line-beginning-position))))
             (dolist (line lines)
               (insert-and-inherit "\n")
               (unless (string-empty-p line)
                 (indent-line-to ind)
                 (insert-and-inherit line)))
             (insert-and-inherit "\n")
             (org-lotus-modification-post-action))
           (message "Note stored")
           (org-back-to-heading t))))))
  ;; Don't add undo information when called from `org-agenda-todo'.
  (set-window-configuration win-config)
  (with-current-buffer (marker-buffer return-to-marker)
    (goto-char return-to-marker))
  (move-marker return-to-marker nil)
  ;; (when org-log-post-message (message "%s" org-log-post-message))
  t)


(defvar occ-entity-type nil)
(make-variable-buffer-local 'occ-entity-type)
(defun occ-entity-star ()
  (interactive)
  (if (= 0
         (current-column))
      (let ((template (occ-obj-capture+-helm-select-template)))
        (if template
            (progn
              (setq-local occ-entity-type 'entry)
              (insert (org-capture-plus-fill-template template)))
          (self-insert-command 1 ?\*)))
    (self-insert-command 1 ?\*)))

(defun occ-entity-finalize (org-marker
                            return-to-marker
                            win-config)
  "Finish taking a log note, and insert it to where it belongs."
  (cond ((eq occ-entity-type 'entry)
         (occ-error "Implement it."))
        (t (occ-entity-store-note org-marker
                                  return-to-marker
                                  win-config))))

(defun occ-entity-kill (org-marker
                        win-config)
  (if win-config
      (progn
        (set-window-configuration win-config)
        (setq win-config nil))
    (occ-error "win-config is nil"))
  (kill-buffer (get-buffer "*Org Entity*")))

(defun occ-entity-refile ()
  (interactive))

(defun occ-entity-replace-template ()
  (interactive))


(defvar occ-entity-cmd-local-plist nil)
(make-variable-buffer-local 'occ-entity-cmd-local-plist)
(defun occ-entity-cmd (cmd)
  (let ((cmd-fn (plist-get occ-entity-cmd-local-plist cmd)))
    (if cmd-fn
        (funcall cmd-fn))))
(defun occ-entity-cmd-finalize ()
  (interactive)
  (occ-entity-cmd :finalize))
(defun occ-entity-cmd-kill ()
  (interactive)
  (occ-entity-cmd :kill))
(defun occ-entity-cmd-refile ()
  (interactive)
  (occ-entity-cmd :refile))
(defun occ-entity-cmd-replace-template ()
  (interactive)
  (occ-entity-cmd :replace-template))
(defvar occ-entity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "*" #'occ-entity-star)
    (define-key map "\C-c\C-c" #'occ-entity-cmd-finalize)
    (define-key map "\C-c\C-k" #'occ-entity-cmd-kill)
    (define-key map "\C-c\C-w" #'occ-entity-cmd-refile)
    (define-key map "\C-c\C-r" #'occ-entity-cmd-replace-template)
    map)
  "Keymap for `occ-entity-mode', a minor mode.
      Use this map to set additional keybindings for when Org mode is used
      for a capture buffer.")

(define-minor-mode occ-entity-mode
  "Minor mode for special key bindings in a capture buffer.

      Turning on this mode runs the normal hook `occ-entity-mode-hook'."
  nil " Cap" occ-entity-mode-map
  (setq-local occ-entity-mode t)
  (setq-local header-line-format
              (substitute-command-keys
               "\\<occ-entity-mode-map>Capture buffer.  Finish \
      `\\[occ-entity-cmd-finalize]', refile `\\[occ-entity-cmd-refile]', \
      abort `\\[occ-entity-cmd-kill]', recapture `\\[occ-entity-cmd-replace-template]'.")))

(cl-defun occ-build-functions (&key
                               org-marker
                               return-to-marker
                               win-config
                               success-fun
                               fail-fun
                               run-before)
  (let ((win-config (or win-config
                        (current-window-configuration))))
   (let ((finalize #'(lambda ()
                       (occ-entity-finalize org-marker
                                            return-to-marker
                                            win-config)))
         (kill     #'(lambda ()
                       (occ-entity-kill org-marker
                                        win-config))))
     (list :finalize finalize
           :kill     kill))))

(defvar occ-entity-buffer-setup-hook nil)

(cl-defun occ-add-entity-buffer (target-buffer
                                 &key
                                 org-marker
                                 return-to-marker
                                 win-config
                                 chgcount
                                 success
                                 fail
                                 run-before)
  "Prepare buffer for taking a note, to add this note later."
  (switch-to-buffer target-buffer 'norecord)
  (erase-buffer)
  ;; (setq occ-store-entity-local-org-marker org-marker)
  ;; (with-current-buffer target-buffer
  ;;   (setq occ-store-entity-local-org-marker org-marker))
  (let ((functions (occ-build-functions :org-marker       org-marker
                                        :return-to-marker return-to-marker
                                        :win-config       win-config
                                        :success-fun      success
                                        :fail-fun         fail
                                        :run-before       run-before)))
    (if nil ;; (memq org-entity-how '(time state))
        (let (current-prefix-arg)
          ;; (occ-entity-finalize-internal)
          (funcall (plist-get functions :finalize)))
      (let ((org-inhibit-startup t))
        (org-mode))
      (occ-entity-mode t)
      (goto-char (point-max))
      (insert "HHHH")
      (setq-local occ-entity-cmd-local-plist functions)
      (run-hooks 'occ-entity-buffer-setup-hook))))

(cl-defun occ-do-entity-add (marker
                             &key
                             win-config)
  (occ-assert marker)
  (occ-assert (marker-buffer marker))
  (let ((win-timeout     7)
        (cleanupfn-local nil))
    (lotus-with-timed-new-win win-timeout timer cleanupfn-newwin cleanupfn-local win
      (condition-case nil
          (let ((target-buffer (get-buffer-create "*Org Entity*")))
            (occ-add-entity-buffer target-buffer
                                   :org-marker       marker
                                   :return-to-marker (point-marker)
                                   :win-config       win-config
                                   :chgcount         nil
                                   :success          nil
                                   :fail             nil
                                   :run-before       nil))
        ((quit)
         (progn
           (funcall cleanupfn-newwin win cleanupfn-local)
           (if timer (cancel-timer timer))
           (signal (cl-first err)
                   (cl-rest err))))))))

(cl-defmethod occ-do-add-entity ((obj marker)
                                 &key
                                 win-config)
  (occ-do-entity-add obj
                     :win-config win-config))

(cl-defmethod occ-do-add-entity ((obj null))
  (let* ((win-config (current-window-configuration))
         (ctx-tsk    (occ-obj-list-select (occ-obj-make-ctx-at-point)
                                          (occ-collections-all)
                                          :filters (occ-list-filters)
                                          :ap-normal '(t actions select)
                                          :obtrusive t)))
    (when ctx-tsk
      (occ-do-add-entity (copy-marker (occ-obj-marker ctx-tsk))
                         :win-config win-config))))

(defun occ-add-entity ()
  (interactive)
  (occ-do-add-entity nil))

;;; occ-util-common.el ends here

