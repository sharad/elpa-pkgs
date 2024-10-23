;;; org-rl-utils.el --- org rl utils                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <>
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

(provide 'org-rl-utils)


(require 'time-stamp)
(require 'lotus-idle-utils)
(require 'org-clock-utils-lotus)
(require 'org-misc-utils-lotus)
(require 'org-clock)
(eval-when-compile
  (require 'lotus-misc-utils)
  (require 'org-misc-utils-lotus))


(require 'org-rl-intf)


(defvar org-rl-capture+-helm-templates-alist org-capture+-helm-templates-alist)

(defun org-default-rl-clock-p (clock-marker)
  (ignore clock-marker)
  t)
(defun org-default-rl-clock-clock-in (clock-marker &optional resume start-time)
  (org-rl-straight-org-clock-clock-in clock-marker resume start-time))
(defun org-default-rl-clock-out (&optional switch-to-state fail-quietly at-time)
  (org-clock-out switch-to-state fail-quietly at-time))
(defun org-default-rl-clock-clock-out (clock-marker &optional fail-quietly at-time)
  (org-clock-clock-out clock-marker fail-quietly at-time))
(defun org-default-rl-select-other-clock (clock-marker &optional target)
  (interactive)
  (ignore clock-marker)
  (org-rl-debug nil "org-rl-select-other-clock: target[%s]" target)
  (org-with-refile
      file loc (or target org-refile-targets) "Refile other org heading"
    (let ((marker (make-marker)))
      (set-marker marker loc)
      marker)))
(defun org-default-rl-capture+-helm-templates-alist (clock-marker)
  (ignore clock-marker)
  org-rl-capture+-helm-templates-alist)

(org-rl-intf-register 'default
                      (list :org-rl-clock-p                       #'org-default-rl-clock-p
                            :org-rl-clock-clock-in                #'org-default-rl-clock-clock-in
                            :org-rl-clock-out                     #'org-default-rl-clock-out
                            :org-rl-select-other-clock            #'org-default-rl-select-other-clock
                            :org-rl-capture+-helm-templates-alist #'org-default-rl-capture+-helm-templates-alist))


(defvar org-rl-org-clock-persist nil "Control org-clock-persist at time of org-resolve clock-in")
(defvar org-rl-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of org-resolev clock-in")

;; lotus-with-file-pos-new-win: selecting buf report.org<hostapdng> [2 times]
;; org--align-node-property: Match data clobbered by buffer modification hooks
;; TODO: FIX: org--align-node-property: Match data clobbered by buffer modification hooks
;; BUG: TODO: need to use (occ-clock-in occ-ctxtual-tsk)
(defun org-rl-straight-org-clock-clock-in (clock-marker &optional resume start-time)
  (progn
    (org-rl-debug nil "org-rl-straight-org-clock-clock-in: begin")
    (lotus-org-clock-load-only)
    (let ((org-clock-persist               org-rl-org-clock-persist)
          (org-clock-auto-clock-resolution org-rl-org-clock-auto-clock-resolution))
      (org-with-narrow-to-marker clock-marker
        (lotus-org-with-safe-modification
          (org-entry-put nil "Effort" "10")))

      (org-rl-intf-clock-clock-in clock-marker resume start-time))))



(defun org-rl-org-clock-clock-in (clock-marker &optional resume start-time)
  (org-rl-straight-org-clock-clock-in clock-marker resume start-time))

(defun org-rl-org-clock-out (&optional switch-to-state fail-quietly at-time)
  (org-rl-intf-clock-out switch-to-state fail-quietly at-time))

(defun org-rl-org-clock-clock-out (clock-marker &optional fail-quietly at-time)
  (org-rl-intf-clock-clock-out clock-marker fail-quietly at-time))

(defun org-rl-org-capture+-helm-templates-alist (clock-marker)
  (org-rl-intf-capture+-helm-templates-alist clock-marker))

;;;###autoload
(defun org-rl-org-select-other-clock (clock-marker &optional target)
  (interactive)
  (org-rl-intf-select-other-clock clock-marker target))


(defvar org-rl-debug nil "Debug org advanced resolve clock")

(defun org-rl-debug (level &rest args)
  (let* ((ilevel (or level :debug))
         (ts     (time-stamp-string))
         (fmt    (format "%s: %s" ts (cl-first args)))
         (args   (append (list fmt) (cl-rest args))))
    (when org-rl-debug
      (apply #'lwarn 'org-rl-clock ilevel args)
      (when level
        (message "%s"
                 (concat (format "org-rl-clock %s: " ilevel)
                         (apply #'format args)))))))


(defun time-aware-completing-read (interval prompt-fn options-fn &optional default-fn)
  (with-select-frame-set-input-disable-raise
    (with-timeout (interval
                   (time-aware-completing-read interval prompt-fn options-fn default-fn))
      (let ((prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn))
            (options (if (functionp options-fn) (funcall options-fn) options-fn))
            (default (if (functionp default-fn) (funcall default-fn) default-fn)))
        (ignore default)
        (completing-read prompt options)))))

(defun time-aware-read-number (interval prompt-fn default-fn)
  (with-select-frame-set-input-disable-raise
    (with-timeout (interval
                   (time-aware-read-number interval prompt-fn default-fn))
      (let ((prompt (if (functionp prompt-fn) (funcall prompt-fn) prompt-fn))
            (default (if (functionp default-fn) (funcall default-fn) default-fn)))
        (read-number prompt default)))))


(defun time-p (time)
  (or (eq 'now time)
      (and (consp time)
           (nth 1 time))))

(defun time-eq (time1 time2)
  (< (abs (time-to-seconds (time-subtract time1 time2))) 60))


(defun time-get-time (time)
  (when time
    (if (time-p time)
        (if (eq time 'now)
            (current-time)
          time)
      (error "Wring time %s passed." time))))


(defun org-get-heading-from-marker (mrk)
  (org-rl-debug :warning "org-get-heading-from-marker: marker = %s, (markerp mrk) = %s, (marker-buffer mrk) = %s"
                mrk
                (markerp mrk)
                (if (markerp mrk) (marker-buffer mrk) mrk))
  (let ((heading
         (if (and
              (markerp mrk)
              (marker-buffer mrk))
             (lotus-with-marker mrk
               (org-get-heading t))
           "imaginary")))
    (org-rl-debug :warning "org-rl-clock-heading: heading = %s" heading)
    heading))

(defun org-get-heading-from-clock (clock)
  (let ((mrk (cl-first clock)))
    (org-get-heading-from-marker mrk)))

;;; org-rl-utils.el ends here
