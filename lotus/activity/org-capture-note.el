;;; org-capture-note.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: s <>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'org-capture-note)


(require 'org-capture+)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)

;; TODO: see it https://orgmode.org/manual/Template-expansion.html#Template-expansion

(drive-extended@ @org-capture-dest (@dest-class)
  "Capture Dest class"

  (setf @:type nil
        @:target nil
        @:template nil
        @:capture-plist nil)

  (def@ @@ :dispatch (marker)
    (@:init)
    (setf @:marker marker))

  (def@ @@ :valid-markerp ()
    (cond
     ((markerp @:marker) @:marker)
     ((functionp @:marker)
      (let ((m  (funcall @:marker))
            (if (markerp m) m))))
     ((symbolp @:marker)
      (let ((m (symbol-value @:marker))
            (if (markerp m) m))))
     (t nil)))

  (def@ @@ :get-marker ()
    (cond
     ((markerp @:marker) @:marker)
     ((functionp @:marker)
      (let ((m (funcall @:marker)))
        (if (markerp m)
            m
          (error "f no marker %s" @:marker))))
     ((symbolp @:marker)
      (let ((m (symbol-value @:marker)))
        (if (markerp m)
            m
          (error "s no marker %s" @:marker))))
     (t
      (error "can not find marker %s" @:marker))))

  (def@ @@ :receive (type target template &rest capture-plist)
    ;; TODO
    ;; add necessary code for interactive note.
    (@:message "Test %s %s %s %s" type target template capture-plist)
    (apply #'org-capture-run
           (or type     @:type)
           (or target   @:target)
           (or template @:template)
           (if capture-plist
               (if (evenp (length capture-plist))
                   (append capture-plist @:capture-plist)
                 (error "Wrong capture capture-plist: %s" capture-plist))
             @:capture-plist))))

(drive-extended@ @org-capture-immediate-dest (@org-capture-dest)
  (setf @:capture-plist (append (list :immediate-finish t :no-save t
                                      @:capture-plist)))
  (setf @:doc "Non-Interactive capture"))

(drive-extended@ @org-capture-edit-dest (@org-capture-dest)
  (setf @:capture-plist (append (list :immediate-finish t :no-save t)
                                @:capture-plist))
  (setf @:doc "Interactive capture"))

(drive-extended@ @org-capture-edit-entry-dest (@org-capture-edit-dest)
  (setf (@ @@ :type) 'entry)
  (def@ @@ :receive (target template &rest capture-plist)
    ;; (apply (@  :receive))
    (@:message "Calling Sup receive")
    (apply @^:receive @@ @:type target template capture-plist)
    (@:message "Called Sup receive")
    (@:message "%s %s" target template)))

;; (@! @org-capture-edit-entry-dest :receive '(clock) "* Hello")


(defobjgen@ @note-class :gen-org-capture-edit-entry-dest-note ()
  "Generator for org note message"
  (push @org-capture-edit-entry-dest
        @:dests))


(setf @org-capture-edit-entry-dest-note
      (@! @note-class :gen-org-capture-edit-entry-dest-note "org-capture-edit-entry-dest-note"))

;; (@! @org-capture-edit-entry-dest-note :send '(clock) "* Hello")


(when nil ;;https://orgmode.org/manual/Template-expansion.html#Template-expansion

  ;;TODO: Will be required later.

  (org-capture-run 'entry
                   '(marker org-clock-marker)
                   "* Hello %^{PROMPT}"
                   ;; :immediate-finish t
                   :empty-lines 1)

  (org-capture-run 'entry
                   '(clock)
                   "* Hello %^{PROMPT}"
                   ;; :immediate-finish t
                   :empty-lines 1
                   :immediate-finish t)

  (org-capture-run 'entry
                   '(clock)
                   "* Hello Test"
                   ;; :immediate-finish t
                   :empty-lines 1
                   :immediate-finish t
                   :no-save t
                   :nodisplay t)

  (org-capture-run 'entry
                   '(marker org-clock-marker)
                   "* Hello %^{PROMPT}"
                   :immediate-finish t
                   :empty-lines 1))

;;; org-capture-note.el ends here
