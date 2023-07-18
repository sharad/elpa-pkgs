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
(require 'occ-debug-method)
(require 'occ-obj-common)


(defvar occ-verbose 0)


;; Resume clock (Unnamed task 933) y
;; Cannot restart clock because task does not contain unfinished clock
(defvar occ-org-clock-persist nil "Control org-clock-persist at time of occ clock-in")
(defvar occ-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of occ clock-in")



(defvar occ-list-select-ap-normal-keys '(t actions general edit checkout))
(defvar occ-list-select-ap-transf-keys '(t actions general edit checkout))


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
            (if (null tail) (occ-error "There is no position ~D in ~S." pos list))
            (push node (cl-rest tail))
            list))
      (occ-error "not able to find element: %s" (occ-obj-format element)))))


(defun occ-helm-buffer-p (buffer)
  (string-match "^*helm" (buffer-name buffer)))


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
              (mapcar #'find-buffer-visiting (occ-obj-files)))
        (if (called-interactively-p 'interactive)
            (y-or-n-p (format "%s is being used in occ, should kill it." (current-buffer))))
      t)))

(defun occ-setup-buffer ()
  ;; BUG: do necessary steps for occ-buffers
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
      (message "occ-clouser-call-obj-on-cand: Running fun %s on %s" obj (occ-obj-format candidate))
      (if (functionp obj)
          (funcall obj candidate)
        obj)))

;;;###autoload
(defun occ-clouser-call-cand-on-obj (obj)
  "CANDIDATE-FUN arguemnt will be called on closed OBJ"
  #'(lambda (candidate-fun)
      (message "occ-clouser-call-cand-on-obj: Running fun %s on %s" candidate-fun (occ-obj-format obj))
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

;;; occ-util-common.el ends here

