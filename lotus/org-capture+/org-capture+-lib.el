;;; org-capture+-lib.el --- org capture plus         -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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

;;; Code:

;; Required libraries

;; [[file:org-capture+-lib.org::*Required libraries][Required libraries:1]]
(require 'crm)
(require 'org)
(require 'org-capture)
(require 'dired)
(require 'org-misc-utils-lotus)
(require 'org-crypt)
;; Required libraries:1 ends here

;; Debug code

;; [[file:org-capture+-lib.org::*Debug code][Debug code:1]]
(defvar org-capture+-debug nil "org-capture+-debug")

(defun org-capture+-debug (level &rest args)
  (when org-capture+-debug
    (when (cl-first args)
      (apply #'format args)
      (when (member level
                    '(:emergency :error :warning :debug))
        ;; (apply #'lwarn 'occ level args)
        (apply #'lwarn 'org-capture+ level args))
      (unless (eq level :nodisplay)
        (apply #'message args)))))
;; Debug code:1 ends here

;; Mode definition


;; [[file:org-capture+-lib.org::*Mode definition][Mode definition:1]]
(defvar org-capture-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-capture-plus-finalize)
    (define-key map "\C-c\C-k" #'org-capture-kill)
    (define-key map "\C-c\C-w" #'org-capture-refile)
    (define-key map "\C-c\C-r" #'org-capture-plus-replace-template)
    map)
  "Keymap for `org-capture-plus-mode', a minor mode.
      Use this map to set additional keybindings for when Org mode is used
      for a capture buffer.")

(defvar org-capture-plus-mode-hook nil
  "Hook for the `org-capture-plus-mode' minor mode.")

(define-minor-mode org-capture-plus-mode
  "Minor mode for special key bindings in a capture buffer.

      Turning on this mode runs the normal hook `org-capture-plus-mode-hook'."
  nil " Cap" org-capture-plus-mode-map
  (setq-local org-capture-mode t)
  (setq-local header-line-format
   (substitute-command-keys
    "\\<org-capture-plus-mode-map>Capture buffer.  Finish \
      `\\[org-capture-plus-finalize]', refile `\\[org-capture-refile]', \
      abort `\\[org-capture-kill]', recapture `\\[org-capture-plus-replace-template]'.")))
;; Mode definition:1 ends here

;; org-capture-plus-finalize


;; [[file:org-capture+-lib.org::*org-capture-plus-finalize][org-capture-plus-finalize:1]]
(defun org-capture-plus-finalize (&optional stay-with-capture)
  (interactive "P")
  (let ((before-finalize (org-capture-get :cap+before-finalize))
        (after-finalize  (org-capture-get :cap+after-finalize))
        (buff            (org-base-buffer (current-buffer)))
        (pos             (point-min))
        (marker          (make-marker)))
    (set-marker marker pos buff)
    ;; (buffer-base-buffer (current-buffer))
    (if (or (null before-finalize)
            (save-restriction (save-excursion (funcall before-finalize marker))))
        (when (org-capture-finalize stay-with-capture)
          (unless (null after-finalize)
            (save-restriction (save-excursion (funcall after-finalize marker)))))
      (lwarn 'org-capture+ :error
             "org-capture-plus-finalize: before-finalize %s returned NULL, so not finalizing."
             before-finalize))))
;; org-capture-plus-finalize:1 ends here

;; Providing log note function for capture


;; [[file:org-capture+-lib.org::*Providing log note function for capture][Providing log note function for capture:1]]
;; check org-store-log-note
  ;; check org-add-log-note
  ;; check org-add-log-setup
  ;;
  ;; effective-time

(defun org-capture-plus-place-log-note ()
  "Place the template plainly.
  If the target locator points at an Org node, place the template into
  the text of the entry, before the first child.  If not, place the
  template at the beginning or end of the file.
  Of course, if exact position has been required, just put it there."
  ;; (debug)
  (let* ((txt                 (org-capture-get :template))
         (beg                 nil)
         (end                 nil)
         (note-purpose        (or (org-capture-get :note-purpose)
                                  'note))
         (effective-time      (org-current-effective-time))
         (note-state          (org-capture-get :note-state))
         (note-previous-state (org-capture-get :note-previous-state))
         (note-how            (org-capture-get :note-how))
         (note-extra          (org-capture-get :note-extra)))
    ;; (cond
    ;;   ((org-capture-get :exact-position)
    ;;    (goto-char (org-capture-get :exact-position)))
    ;;   ((and (org-capture-get :target-entry-p)
    ;;         (bolp)
    ;;         (looking-at org-outline-regexp))
    ;;    ;; we should place the text into this entry
    ;;    (if (org-capture-get :prepend)
    ;;        ;; Skip meta data and drawers
    ;;        (org-end-of-meta-data t)
    ;;        ;; go to ent of the entry text, before the next headline
    ;;        (outline-next-heading)))
    ;;   (t
    ;;    ;; beginning or end of file
    ;;    (goto-char (if (org-capture-get :prepend) (point-min) (point-max)))))
    (ignore note-how)
    (ignore note-extra)
    (if (and (org-capture-get :target-entry-p)
             (bolp)
             (looking-at org-outline-regexp))
        (let ((note (cl-rest (assq note-purpose org-log-note-headings)))
              lines)
          (progn
            (while (string-match "\\`# .*\n[ \t\n]*" txt)
              (setq txt (replace-match "" t t txt)))
            (when (string-match "\\s-+\\'" txt)
              (setq txt (replace-match "" t t txt)))
            (setq lines (org-split-string txt "\n"))
            (when (org-string-nw-p note)
              (setq note
                    (org-replace-escapes
                     note
                     (list (cons "%u" (user-login-name))
                           (cons "%U" user-full-name)
                           (cons "%t" (format-time-string
                                       (org-time-stamp-format 'long 'inactive)
                                       effective-time))
                           (cons "%T" (format-time-string
                                       (org-time-stamp-format 'long nil)
                                       effective-time))
                           (cons "%d" (format-time-string
                                       (org-time-stamp-format nil 'inactive)
                                       effective-time))
                           (cons "%D" (format-time-string
                                       (org-time-stamp-format nil nil)
                                       effective-time))
                           (cons "%s" (cond
                                       ((not note-state) "")
                                       ((string-match-p org-ts-regexp note-state)
                                        (format "\"[%s]\""
                                                (substring note-state 1 -1)))
                                       (t (format "\"%s\"" note-state))))
                           (cons "%S"
                                 (cond
                                  ((not note-previous-state) "")
                                  ((string-match-p org-ts-regexp
                                                   note-previous-state)
                                   (format "\"[%s]\""
                                           (substring
                                            note-previous-state 1 -1)))
                                  (t (format "\"%s\""
                                             note-previous-state)))))))
              (when lines (setq note (concat note " \\\\")))
              (push note lines)))

          (when lines ;; (and lines (not (or current-prefix-arg org-note-abort)))
            (progn ;; with-current-buffer (marker-buffer note-marker)
              (progn ;; org-with-wide-buffer
               ;; Find location for the new note.
               ;; (goto-char note-marker)
               ;; (set-marker note-marker nil)

               ;; Note associated to a clock is to be located right after
               ;; the clock.  Do not move point.
               (unless (eq note-purpose 'clock-out)
                 (goto-char (org-log-beginning t)))
               ;; Make sure point is at the beginning of an empty line.
               (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
                     ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
               ;; In an existing list, add a new item at the top level.
               ;; Otherwise, indent line like a regular one.
               (let ((itemp (org-in-item-p)))
                 (if itemp
                     (indent-line-to
                      (let ((struct (save-excursion
                                      (goto-char itemp) (org-list-struct))))
                        (org-list-get-ind (org-list-get-top-point struct) struct)))
                   (org-indent-line)))

               ;; (or (bolp) (newline))
               ;; (org-capture-empty-lines-before)
               (setq beg (point))
               (insert (org-list-bullet-string "-") (pop lines))
               (let ((ind (org-list-item-body-column (line-beginning-position))))
                 (dolist (line lines)
                   (insert "\n")
                   (indent-line-to ind)
                   (insert line)))
               ;; (message "Note stored")
               ;; (org-capture-empty-lines-after)
               (org-capture-position-for-last-stored beg)
               (setq end (point))
               (let ((end end)) ;; (1- end)
                 (org-capture-mark-kill-region beg end)
                 (org-capture-narrow beg end))
               (if (or (re-search-backward "%\\?" beg t)
                       (re-search-forward "%\\?" end t))
                   (replace-match ""))
               (when nil
                 (org-back-to-heading t)
                 (org-cycle-hide-drawers 'children))
               ;; Fix `buffer-undo-list' when `org-store-log-note' is called
               ;; from within `org-add-log-note' because `buffer-undo-list'
               ;; is then modified outside of `org-with-remote-undo'.
               (when (eq this-command 'org-agenda-todo)
                 (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))))))
      (error "marker %s buffer is nil" 'marker))))

(defalias 'org-capture-plus-place-entry      #'org-capture-place-entry)
(defalias 'org-capture-plus-place-table-line #'org-capture-place-table-line)
(defalias 'org-capture-plus-place-plain-text #'org-capture-place-plain-text)
(defalias 'org-capture-plus-place-item       #'org-capture-place-item)
(defalias 'org-capture-place-log-note        #'org-capture-plus-place-log-note)
;; Providing log note function for capture:1 ends here

;; Overriding org-capture-place-template function


;; [[file:org-capture+-lib.org::*Overriding org-capture-place-template function][Overriding org-capture-place-template function:1]]
(defun org-capture-plus-place-template (capture-buffer
                                        &optional
                                        inhibit-wconf-store
                                        nodisplay)
  "Insert the template at the target location, and display the buffer.
     When `inhibit-wconf-store', don't store the window configuration, as it
     may have been stored before."
  (let ((capture-buffer (or capture-buffer
                            (org-capture-get-indirect-buffer (org-capture-get :buffer)
                                                             "CAPTURE"))))
    (unless inhibit-wconf-store
      (org-capture-put :return-to-wconf (current-window-configuration)))
    (if capture-buffer
        (progn
          (unless nodisplay
            (delete-other-windows)
            (org-switch-to-buffer-other-window capture-buffer))
          (with-current-buffer capture-buffer
            (widen)
            (outline-show-all)
            (goto-char (org-capture-get :pos))
            (org-fold-show-all)
            (setq-local outline-level 'org-outline-level)
            (pcase (org-capture-get :type)
              ((or `nil `entry) (org-capture-plus-place-entry))
              (`table-line      (org-capture-plus-place-table-line))
              (`plain           (org-capture-plus-place-plain-text))
              (`item            (org-capture-plus-place-item))
              (`checkitem       (org-capture-plus-place-item))
              (`log-note        (org-capture-plus-place-log-note)))
            (org-capture-plus-mode 1)
            (setq-local org-capture-current-plist org-capture-plist)))
      (error "Not capture-buffer"))
    capture-buffer))

(defalias 'org-capture-place-template #'org-capture-plus-place-template)
;; Overriding org-capture-place-template function:1 ends here


(defun org-capture-plus-replace-template ()
  (org-capture-kill)
  (org-capture-plus-set-target-location)

  (condition-case error
      (org-capture-put :template
                       (org-capture-fill-template))
    ((error quit)
     (when (get-buffer "*Capture*")
       (kill-buffer "*Capture*"))
     (error "Capture abort: %s" error)))

  (setq org-capture-clock-keep (org-capture-get :clock-keep))
  (if (and (not (org-capture-get :target))
           (eq 'immdediate
               (cl-first (org-capture-get :target)))) ;; (equal goto 0)
      ;;insert at point
      (org-capture-insert-template-here)
    (let ((capture-buffer (org-capture-get-indirect-buffer (org-capture-get :buffer)
                                                           "CAPTURE")))
      (condition-case error
          (let ((nodisplay           (org-capture-get :nodisplay))
                (inhibit-wconf-store (eq 'function
                                         (cl-first (org-capture-get :target)))))
            (org-capture-plus-place-template capture-buffer
                                             inhibit-wconf-store
                                             nodisplay))
        ((error quit)
         (if (and (buffer-base-buffer capture-buffer)
                  (string-prefix-p "CAPTURE-" (buffer-name)))
             (kill-buffer (current-buffer)))
         (set-window-configuration (org-capture-get :return-to-wconf))
         (error "Capture template `%s': %s"
                (org-capture-get :key)
                (nth 1 error))))
      (when (and (derived-mode-p 'org-mode)
                 (org-capture-get :clock-in))
        (condition-case nil
            (progn
              (when (org-clock-is-active)
                (org-capture-put :interrupted-clock
                                 (copy-marker org-clock-marker)))
              (org-clock-in)
              (setq-local org-capture-clock-was-started t))
          (error "Could not start the clock in this capture buffer")))
      (when (org-capture-get :immediate-finish)
        (with-current-buffer capture-buffer
          (org-capture-plus-finalize))))))





;; set target improved

;; [[file:org-capture+-lib.org::*set target improved][set target improved:1]]
(defun org-capture-plus-set-target-location (&optional target nodisplay)
  "Find TARGET buffer and position.
      Store them in the capture property list."
  (ignore nodisplay)
  (let ((target-entry-p t))
    (save-excursion
      (pcase (or target (org-capture-get :target))
        (`(file ,path)
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (setq target-entry-p nil))
        (`(id ,id)
         (pcase (org-id-find id)
           (`(,path . ,position)
            (set-buffer (org-capture-target-buffer path))
            (widen)
            (org-capture-put-target-region-and-position)
            (goto-char position))
           (_ (error "Cannot find target ID \"%s\"" id))))
        (`(file+headline ,path ,headline)
         (set-buffer (org-capture-target-buffer path))
         ;; Org expects the target file to be in Org mode, otherwise
         ;; it throws an error.  However, the default notes files
         ;; should work out of the box.  In this case, we switch it to
         ;; Org mode.
         (unless (derived-mode-p 'org-mode)
           (org-display-warning
            (format "Capture requirement: switching buffer %S to Org mode"
                    (current-buffer)))
           (org-mode))
         (org-capture-put-target-region-and-position)
         (widen)
         (goto-char (point-min))
         (if (re-search-forward (format org-complex-heading-regexp-format
                                        (regexp-quote headline))
                                nil t)
             (beginning-of-line)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* " headline "\n")
           (beginning-of-line 0)))
        (`(file+olp ,path . ,outline-path)
         (let ((m (org-find-olp (cons (org-capture-expand-file path)
                                      outline-path))))
           (set-buffer (marker-buffer m))
           (org-capture-put-target-region-and-position)
           (widen)
           (goto-char m)
           (set-marker m nil)))
        (`(file+regexp ,path ,regexp)
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (goto-char (point-min))
         (if (not (re-search-forward regexp nil t))
             (error "No match for target regexp in file %s" path)
           (goto-char (if (org-capture-get :prepend)
                          (match-beginning 0)
                        (match-end 0)))
           (org-capture-put :exact-position (point))
           (setq target-entry-p
                 (and (derived-mode-p 'org-mode) (org-at-heading-p)))))
        (`(file+olp+datetree ,path . ,outline-path)
         (let ((m (if outline-path
                      (org-find-olp (cons (org-capture-expand-file path)
                                          outline-path))
                    (set-buffer (org-capture-target-buffer path))
                    (point-marker))))
           (set-buffer (marker-buffer m))
           (org-capture-put-target-region-and-position)
           (widen)
           (goto-char m)
           (set-marker m nil)
           (require 'org-datetree)
           (org-capture-put-target-region-and-position)
           (widen)
           ;; Make a date/week tree entry, with the current date (or
           ;; yesterday, if we are extending dates for a couple of hours)
           (let ((date (calendar-gregorian-from-absolute
                        (cond (org-overriding-default-time
                               ;; Use the overriding default time.
                               (time-to-days org-overriding-default-time))
                              ((or (org-capture-get :time-prompt)
                                   (equal current-prefix-arg 1))
                               ;; Prompt for date.
                               (let ((prompt-time
                                      (org-read-date nil t nil "Date for tree entry:" (current-time))))
                                 (org-capture-put
                                  :default-time
                                  (cond ((and (or (not (boundp 'org-time-was-given))
                                                  (not org-time-was-given))
                                              (not (= (time-to-days prompt-time) (org-today))))
                                         ;; Use 00:00 when no time is given for another
                                         ;; date than today?
                                         (apply #'encode-time
                                                (append '(0 0 0)
                                                        (cl-cdddr (decode-time prompt-time)))))
                                        ((string-match "\\([^ ]+\\)--?[^ ]+[ ]+\\(.*\\)"
                                                       org-read-date-final-answer)
                                         ;; Replace any time range by its start.
                                         (apply #'encode-time
                                                (org-read-date-analyze
                                                 (replace-match "\\1 \\2" nil nil
                                                                org-read-date-final-answer)
                                                 prompt-time (decode-time prompt-time))))
                                        (t prompt-time)))
                                 (time-to-days prompt-time)))
                              (t
                               ;; Current date, possibly corrected for late night
                               ;; workers.
                               (org-today))))))
             (funcall (if (eq (org-capture-get :tree-type) 'week) #'org-datetree-find-iso-week-create #'org-datetree-find-date-create)
                      date
                      ;; the following is the keep-restriction argument for
                      ;; org-datetree-find-date-create
                      (if outline-path 'subtree-at-point)))))
        (`(file+function ,path ,function)
         (set-buffer (org-capture-target-buffer path))
         (org-capture-put-target-region-and-position)
         (widen)
         (funcall function)
         (org-capture-put :exact-position (point))
         (setq target-entry-p
               (and
                (derived-mode-p 'org-mode)
                (org-at-heading-p))))
        (`(function ,fun)
         (funcall fun)
         (org-capture-put :exact-position (point))
         (setq target-entry-p
               (and (derived-mode-p 'org-mode) (org-at-heading-p))))
        (`(clock)
         (if (and (markerp       org-clock-hd-marker)
                  (marker-buffer org-clock-hd-marker))
             (progn
               (set-buffer (marker-buffer org-clock-hd-marker))
               (org-capture-put-target-region-and-position)
               (widen)
               (goto-char org-clock-hd-marker))
           (error "No running clock that could be used as capture target")))
        (`(marker ,hd-marker)
         (let ((hd-marker
                (cond
                 ((markerp hd-marker) hd-marker)
                 ((symbolp hd-marker) (symbol-value hd-marker))
                 (t (error "value %s is not marker" hd-marker)))))
           (message "hd-marker %s" hd-marker)
           (if (and (markerp hd-marker)
                    (marker-buffer hd-marker))
               (progn
                 (set-buffer (marker-buffer hd-marker))
                 (org-capture-put-target-region-and-position)
                 (widen)
                 (goto-char hd-marker)
                 (progn
                   (org-capture-put :exact-position (point))
                   (setq target-entry-p
                         (and (derived-mode-p 'org-mode)
                              (org-at-heading-p)))))
             (error "No running clock that could be used as capture target"))))
        (target (error "Invalid capture target specification: %S" target)))

      (org-capture-put :buffer         (current-buffer)
                       :pos            (point)
                       :target-entry-p target-entry-p
                       :decrypted      (and (featurep 'org-crypt)
                                            (org-at-encrypted-entry-p)
                                            (save-excursion
                                              (org-decrypt-entry)
                                              (and (org-back-to-heading t)
                                                   (point))))))))
;; set target improved:1 ends here

;; capture plus fill template

;; [[file:org-capture+-lib.org::*capture plus fill template][capture plus fill template:1]]
(defun org-capture-plus-fill-template (&optional
                                       template
                                       initial
                                       annotation
                                       nodisplay)
  "Fill a TEMPLATE and return the filled template as a string.
  The template may still contain \"%?\" for cursor positioning.
  INITIAL content and/or ANNOTATION may be specified, but will be overridden
  by their respective `org-store-link-plist' properties if present."
  (ignore nodisplay)
  (let* ((template (or template (org-capture-get :template)))
         (buffer (org-capture-get :buffer))
         (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
         (time (let* ((c (or (org-capture-get :default-time) (current-time)))
                      (d (decode-time c)))
                 (if (< (nth 2 d) org-extend-today-until)
                     (encode-time 0 59 23 (1- (nth 3 d)) (nth 4 d) (nth 5 d))
                   c)))
         (v-t (format-time-string (org-time-stamp-format nil) time))
         (v-T (format-time-string (org-time-stamp-format t) time))
         (v-u (format-time-string (org-time-stamp-format nil t) time))
         (v-U (format-time-string (org-time-stamp-format t t) time))
         (v-c (and kill-ring (current-kill 0)))
         (v-x (or (org-get-x-clipboard 'PRIMARY)
                  (org-get-x-clipboard 'CLIPBOARD)
                  (org-get-x-clipboard 'SECONDARY)
                  ""))                                          ;ensure it is a string
         ;; `initial' and `annotation' might have been passed.  But if
         ;; the property list has them, we prefer those values.
         (v-i (or (plist-get org-store-link-plist :initial)
                  (and (stringp initial) (org-no-properties initial))
                  (org-capture-get :initial)
                  ""))
         (v-a
          (let ((a (or (plist-get org-store-link-plist :annotation)
                       annotation
                       (org-capture-get :annotation)
                       "")))
            ;; Is the link empty?  Then we do not want it...
            (if (equal a "[[]]") "" a)))
         (l-re "\\[\\[\\(.*?\\)\\]\\(\\[.*?\\]\\)?\\]")
         (v-A (if (and v-a (string-match l-re v-a))
                  (replace-match "[[\\1][%^{Link description}]]" nil nil v-a)
                v-a))
         (v-l (if (and v-a (string-match l-re v-a))
                  (replace-match "[[\\1]]" nil nil v-a)
                v-a))
         (v-L (if (and v-a (string-match l-re v-a))
                  (replace-match "\\1" nil nil v-a)
                v-a))
         (v-n user-full-name)
         (v-k (if (marker-buffer org-clock-marker)
                  (org-no-properties org-clock-heading)
                ""))
         (v-K (if (marker-buffer org-clock-marker)
                  (org-link-make-string
                   (format "%s::*%s"
                           (buffer-file-name (marker-buffer org-clock-marker))
                           v-k)
                   v-k)
                ""))
         (v-f (or (org-capture-get :original-file-nondirectory) ""))
         (v-F (or (org-capture-get :original-file) ""))
         (org-capture--clipboards
          (delq nil
                (list v-i
                      (org-get-x-clipboard 'PRIMARY)
                      (org-get-x-clipboard 'CLIPBOARD)
                      (org-get-x-clipboard 'SECONDARY)
                      v-c))))
    (setq org-store-link-plist (plist-put org-store-link-plist :annotation v-a))
    (setq org-store-link-plist (plist-put org-store-link-plist :initial v-i))
    (unless template
      (setq template "")
      (message "no template") (ding)
      (sit-for 1))
    (save-window-excursion
      (org-switch-to-buffer-other-window (get-buffer-create "*Capture*"))
      (erase-buffer)
      (setq buffer-file-name nil)
      (setq mark-active nil)
      (insert template)
      (goto-char (point-min))
      ;; %[] insert contents of a file.
      (save-excursion
        (while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
          (let ((filename (expand-file-name (match-string 1)))
                (beg (copy-marker (match-beginning 0)))
                (end (copy-marker (match-end 0))))
            (unless (org-capture-escaped-%)
              (delete-region beg end)
              (set-marker beg nil)
              (set-marker end nil)
              (condition-case error
                  (insert-file-contents filename)
                (error
                 (insert (format "%%![couldn not insert %s: %s]"
                                 filename
                                 error))))))))
      ;; Mark %() embedded elisp for later evaluation.
      (org-capture-expand-embedded-elisp 'mark)
      ;; Expand non-interactive templates.
      (let ((regexp "%\\(:[-A-Za-z]+\\|<\\([^>\n]+\\)>\\|[aAcfFikKlLntTuUx]\\)"))
        (save-excursion
          (while (re-search-forward regexp nil t)
            ;; `org-capture-escaped-%' may modify buffer and cripple
            ;; match-data.  Use markers instead.  Ditto for other
            ;; templates.
            (let ((pos (copy-marker (match-beginning 0)))
                  (end (copy-marker (match-end 0)))
                  (value (match-string 1))
                  (time-string (match-string 2)))
              (unless (org-capture-escaped-%)
                (delete-region pos end)
                (set-marker pos nil)
                (set-marker end nil)
                (let* ((inside-sexp? (org-capture-inside-embedded-elisp-p))
                       (replacement
                        (pcase (string-to-char value)
                          (?< (format-time-string time-string time))
                          (?:
                           (or (plist-get org-store-link-plist (intern value))
                               ""))
                          (?i
                           (if inside-sexp? v-i
                             ;; Outside embedded Lisp, repeat leading
                             ;; characters before initial place holder
                             ;; every line.
                             (let ((lead (concat "\n"
                                                 (org-current-line-string t))))
                               (replace-regexp-in-string "\n" lead v-i nil t))))
                          (?a v-a)
                          (?A v-A)
                          (?c v-c)
                          (?f v-f)
                          (?F v-F)
                          (?k v-k)
                          (?K v-K)
                          (?l v-l)
                          (?L v-L)
                          (?n v-n)
                          (?t v-t)
                          (?T v-T)
                          (?u v-u)
                          (?U v-U)
                          (?x v-x))))
                  (insert
                   (if inside-sexp?
                       ;; Escape sensitive characters.
                       (replace-regexp-in-string "[\\\"]" "\\\\\\&" replacement)
                     replacement))))))))
      ;; Expand %() embedded Elisp.  Limit to Sexp originally marked.
      (org-capture-expand-embedded-elisp)
      ;; Expand interactive templates.  This is the last step so that
      ;; template is mostly expanded when prompting happens.  Turn on
      ;; Org mode and set local variables.  This is to support
      ;; completion in interactive prompts.
      (let ((org-inhibit-startup t)) (org-mode))
      (org-clone-local-variables buffer "\\`org-")
      (let (strings)                                          ; Stores interactive answers.
        (save-excursion
          (let ((regexp "%\\^\\(?:{\\([^}]*\\)}\\)?\\([CgGLptTuU]\\)?"))
            (while (re-search-forward regexp nil t)
              (let* ((items (and (match-end 1)
                                 (save-match-data
                                   (split-string (match-string-no-properties 1)
                                                 "|"))))
                     (key (match-string 2))
                     (beg (copy-marker (match-beginning 0)))
                     (end (copy-marker (match-end 0)))
                     (prompt (nth 0 items))
                     (default (nth 1 items))
                     (completions (nthcdr 2 items)))
                (unless (org-capture-escaped-%)
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil)
                  (pcase key
                    ((or "G" "g")
                     (let* ((org-last-tags-completion-table
                             (org-global-tags-completion-table
                              (cond ((equal key "G") (org-agenda-files))
                                    (file (list file))
                                    (t nil))))
                            (org-add-colon-after-tag-completion t)
                            (ins (mapconcat #'identity
                                            (let ((crm-separator "[ \t]*:[ \t]*"))
                                              (ignore crm-separator)
                                              (completing-read-multiple (if prompt (concat prompt ": ") "Tags: ")
                                                                        org-last-tags-completion-table nil nil nil
                                                                        'org-tags-history))
                                  ":")))
                       (when (org-string-nw-p ins)
                         (unless (eq (char-before) ?:) (insert ":"))
                         (insert ins)
                         (unless (eq (char-after) ?:) (insert ":"))
                         (when (org-at-heading-p) (org-align-tags)))))
                    ((or "C" "L")
                     (let ((insert-fun (if (equal key "C") #'insert
                                         (lambda (s) (org-insert-link 0 s)))))
                       (pcase org-capture--clipboards
                         (`nil nil)
                         (`(,value) (funcall insert-fun value))
                         (`(,first-value . ,_)
                          (funcall insert-fun
                                   (read-string "Clipboard/kill value: "
                                                first-value
                                                'org-capture--clipboards
                                                first-value)))
                         (_ (error "Invalid `org-capture--clipboards' value: %S"
                                   org-capture--clipboards)))))
                    ("p"
                     ;; We remove keyword properties inherited from
                     ;; target buffer so `org-read-property-value' has
                     ;; a chance to find allowed values in sub-trees
                     ;; from the target buffer.
                     (setq-local org-keyword-properties nil)
                     (let* ((origin (set-marker (make-marker)
                                                (org-capture-get :pos)
                                                (org-capture-get :buffer)))
                            ;; Find location from where to get allowed
                            ;; values.  If `:target-entry-p' is
                            ;; non-nil, the current headline in the
                            ;; target buffer is going to be a parent
                            ;; headline, so location is fine.
                            ;; Otherwise, find the parent headline in
                            ;; the target buffer.
                            (pom (if (org-capture-get :target-entry-p) origin
                                   (let ((level (progn
                                                  (while (org-up-heading-safe))
                                                  (org-current-level))))
                                     (org-with-point-at origin
                                       (let ((l (if (org-at-heading-p)
                                                    (org-current-level)
                                                  most-positive-fixnum)))
                                         (while (and l (>= l level))
                                           (setq l (org-up-heading-safe)))
                                         (if l (point-marker)
                                           (point-min-marker)))))))
                            (value
                             (org-read-property-value prompt pom default)))
                       (org-set-property prompt value)))
                    ((or "t" "T" "u" "U")
                     ;; These are the date/time related ones.
                     (let* ((upcase? (equal (upcase key) key))
                            (org-end-time-was-given nil)
                            (time (org-read-date upcase? t nil prompt)))
                       (org-insert-time-stamp
                        time (or org-time-was-given upcase?)
                        (member key '("u" "U"))
                        nil nil (list org-end-time-was-given))))
                    (`nil
                     ;; Load history list for current prompt.
                     (setq org-capture--prompt-history
                           (gethash prompt org-capture--prompt-history-table))
                     (push (org-completing-read
                            (concat (or prompt "Enter string")
                                    (and default (format " [%s]" default))
                                    ": ")
                            completions
                            nil nil nil 'org-capture--prompt-history default)
                           strings)
                     (insert (car strings))
                     ;; Save updated history list for current prompt.
                     (puthash prompt org-capture--prompt-history
                              org-capture--prompt-history-table))
                    (_
                     (error "Unknown template placeholder: \"%%^%s\""
                            key))))))))
        ;; Replace %n escapes with nth %^{...} string.
        (setq strings (nreverse strings))
        (save-excursion
          (while (re-search-forward "%\\\\\\([1-9][0-9]*\\)" nil t)
            (unless (org-capture-escaped-%)
              (replace-match
               (nth (1- (string-to-number (match-string 1))) strings)
               nil t)))))
      ;; Make sure there are no empty lines before the text, and that
      ;; it ends with a newline character or it is empty.
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (line-beginning-position))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (if (bobp) (delete-region (point) (line-end-position))
        (end-of-line)
        (delete-region (point) (point-max))
        (insert "\n"))
      ;; Return the expanded template and kill the capture buffer.
      (untabify (point-min) (point-max))
      (set-buffer-modified-p nil)
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
        (kill-buffer (current-buffer))))))
;; capture plus fill template:1 ends here

;; new capture

;; [[file:org-capture+-lib.org::*new capture][new capture:1]]
(defun org-capture-plus-get-template (template)
  (cond ((stringp template) template)
        ((fboundp template) (funcall template))
        ((symbolp template) (symbol-value template))
        (t                  template)))

  ;;;###autoload
(defun org-capture-run (type target template &rest plist)
  "Capture something.
    \\<org-capture-plus-mode-map>
    This will let you select a template from `org-capture-templates', and
    then file the newly captured information.  The text is immediately
    inserted at the target location, and an indirect buffer is shown where
    you can edit it.  Pressing `\\[org-capture-plus-finalize]' brings you back to the \
    previous
    state of Emacs, so that you can continue your work.

    When called interactively with a `\\[universal-argument]' prefix argument \
    GOTO, don't
    capture anything, just go to the file/headline where the selected
    template stores its notes.

    With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to \
    the last note stored.

    When called with a `C-0' (zero) prefix, insert a template at point.

    When called with a `C-1' (one) prefix, force prompting for a date when
    a datetree entry is made.

    ELisp programs can set KEYS to a string associated with a template
    in `org-capture-templates'.  In this case, interactive selection
    will be bypassed.

    If `org-capture-use-agenda-date' is non-nil, capturing from the
    agenda will use the date at point as the default date.  Then, a
    `C-1' prefix will tell the capture process to use the HH:MM time
    of the day at point (if any) or the current HH:MM time."
  ;; (interactive "P")

  (when (and org-capture-use-agenda-date
             (eq major-mode 'org-agenda-mode))
    (setq org-overriding-default-time
          (org-get-cursor-date t))) ;; (equal goto 1)


  (let* ((orig-buf   (current-buffer))
         (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                              org-capture-link-is-already-stored)
                         (plist-get org-store-link-plist :annotation)
                       (ignore-errors (org-store-link nil))))
         ;; (template (or org-capture-entry (org-capture-select-template keys)))
         (template   (or org-capture-entry
                         (org-capture-plus-get-template template)))
         (initial    nil))
    (setq initial (or org-capture-initial
                      (and (org-region-active-p)
                           (buffer-substring (point) (mark)))))
    (when (stringp initial)
      (remove-text-properties 0
                              (length initial)
                              '(read-only t)
                              initial))
    (when (stringp annotation)
      (remove-text-properties 0
                              (length annotation)
                              '(read-only t)
                              annotation))

    ;; (org-capture-set-plist template)

    (setq org-capture-plist plist)
    (org-capture-put ;; :key (cl-first entry)
                     ;; :description (nth 1 entry)
                     :target target)

    (let ((txt template)
          (type (or type
                    'entry)))
      (when (or (not txt)
                (and (stringp txt)
                     (not (string-match "\\S-" txt))))
        ;; The template may be empty or omitted for special types.
        ;; Here we insert the default templates for such cases.
        (cond
         ((eq type 'item)            (setq txt "- %?"))
         ((eq type 'checkitem)       (setq txt "- [ ] %?"))
         ((eq type 'table-line)      (setq txt "| %? |"))
         ((member type '(nil entry)) (setq txt "* %?\n  %a"))))
      (org-capture-put :template txt
                       :type     type))

    (org-capture-get-template)

    (org-capture-put :original-buffer            orig-buf
                     :original-file              (or (buffer-file-name orig-buf)
                                                     (and (featurep 'dired)
                                                          (cl-first (rassq orig-buf
                                                                      dired-buffers))))
                     :original-file-nondirectory (and (buffer-file-name orig-buf)
                                                      (file-name-nondirectory
                                                       (buffer-file-name orig-buf)))
                     :annotation                 annotation
                     :initial                    initial
                     :return-to-wconf            (current-window-configuration)
                     :default-time               (or org-overriding-default-time
                                                     (org-current-time)))

    (org-capture-plus-set-target-location)

    (condition-case error
        (org-capture-put :template
                         (org-capture-fill-template))
      ((error quit)
       (when (get-buffer "*Capture*")
         (kill-buffer "*Capture*"))
       (error "Capture abort: %s" error)))

    (setq org-capture-clock-keep (org-capture-get :clock-keep))
    (if (and (not (org-capture-get :target))
             (eq 'immdediate
                 (cl-first (org-capture-get :target)))) ;; (equal goto 0)
        ;;insert at point
        (org-capture-insert-template-here)
      (let ((capture-buffer (org-capture-get-indirect-buffer (org-capture-get :buffer)
                                                             "CAPTURE")))
        (condition-case error
            (let ((nodisplay           (org-capture-get :nodisplay))
                  (inhibit-wconf-store (eq 'function
                                           (cl-first (org-capture-get :target)))))
              (org-capture-plus-place-template capture-buffer
                                               inhibit-wconf-store
                                               nodisplay))
          ((error quit)
           (if (and (buffer-base-buffer capture-buffer)
                    (string-prefix-p "CAPTURE-" (buffer-name)))
               (kill-buffer (current-buffer)))
           (set-window-configuration (org-capture-get :return-to-wconf))
           (error "Capture template `%s': %s"
                  (org-capture-get :key)
                  (nth 1 error))))
        (when (and (derived-mode-p 'org-mode)
                   (org-capture-get :clock-in))
          (condition-case nil
              (progn
                (when (org-clock-is-active)
                  (org-capture-put :interrupted-clock
                                   (copy-marker org-clock-marker)))
                (org-clock-in)
                (setq-local org-capture-clock-was-started t))
            (error "Could not start the clock in this capture buffer")))
        (when (org-capture-get :immediate-finish)
          (with-current-buffer capture-buffer
            (org-capture-plus-finalize)))))))

        ;; (defalias 'org-capture-run 'org-capture-plus)
;; new capture:1 ends here

;; Application



;; [[file:org-capture+-lib.org::*Application][Application:1]]
(defun org-goto-refile (&optional refile-targets)
  "Refile goto."
  ;; mark paragraph if no region is set
  (let* ((org-refile-targets (or refile-targets
                                 org-refile-targets))
         (target             (save-excursion (safe-org-refile-get-location)))
         (file               (nth 1 target))
         (pos                (nth 3 target)))
    (when (set-buffer (find-file-noselect file)) ;; (switch-to-buffer (find-file-noselect file) 'norecord)
      (goto-char pos))))

(defun org-create-new-task ()
  (interactive)
  (org-capture-run
   'entry
   '(function org-goto-refile)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))


(when nil

  (org-capture-run
   'log-note
   '(clock)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1)

  (org-capture-run
   'log-note
   '(clock)
   "Test\n"
   :unnarrowed nil
   :empty-lines 1)

  ;; https://orgmode.org/manual/Template-elements.html#Template-elements
  ;; template expansion properties
  (org-capture-run
   'log-note
   '(marker org-clock-marker)
   "Hello"
   :unnarrowed nil
   :empty-lines 1)

  (org-capture-run
   'log-note
   '(marker testmrkr)
   "Test Hello 1"
   ;; :immediate-finish t
   :empty-lines 1)

  (org-capture-run
   'entry
   '(marker testmrkr)
   "* Hello"
   ;; :immediate-finish t
   :empty-lines 1)

  (org-capture-run
   'entry
   '(clock)
   "* Hello"
   ;; :immediate-finish t
   :empty-lines 1)


  (org-capture-run
   'entry
   '(function org-goto-refile)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1)





  (org-capture-run
   'log-note
   '(marker testmrkr)
   "Test Hello 1"
   ;; :immediate-finish t
   :empty-lines 1))




(when nil
  (let (helm-sources)
    ;; (when (marker-buffer org-clock-default-task)
    ;;   (push
    ;;    (helm-build-sync-source "Default Task"
    ;;     :candidates (list (lotus-org-marker-selection-line org-clock-default-task))
    ;;     :action (list ;; (cons "Select" 'identity)
    ;;              (cons "Clock in and track" #'identity)))
    ;;    helm-sources))

    ;; (when (marker-buffer org-clock-interrupted-task)
    ;;   (push
    ;;    (helm-build-sync-source "The task interrupted by starting the last one"
    ;;      :candidates (list (lotus-org-marker-selection-line org-clock-interrupted-task))
    ;;      :action (list ;; (cons "Select" 'identity)
    ;;               (cons "Clock in and track" #'identity)))
    ;;    helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Task"
         :candidates (list (lotus-org-marker-selection-line org-clock-marker))
         :action (list ;; (cons "Select" 'identity)
                  (cons "Clock in and track" #'identity)))
       helm-sources))

    ;; (when org-clock-history
    ;;   (push
    ;;    (helm-build-sync-source "Recent Tasks"
    ;;      :candidates (mapcar 'sacha-org-context-clock-dyntaskpl-selection-line dyntaskpls)
    ;;      :action (list ;; (cons "Select" 'identity)
    ;;               (cons "Clock in and track" #'(lambda (dyntaskpl) (plist-get dyntaskpl ))))
    ;;    helm-sources)))

    (helm helm-sources)))
;; Application:1 ends here

;; Provide this file

;; [[file:org-capture+-lib.org::*Provide this file][Provide this file:1]]
(provide 'org-capture+-lib)
;;; org-capture+-lib.el ends here
;; Provide this file:1 ends here
