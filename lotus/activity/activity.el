;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'activity)


(require 'cl-seq)
(require '@)


(eval-when-compile
  (require 'activity-macro))
(require 'activity-base)


(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")

;; ;;;###autoload
;; (defvar activity-subdirs
;;   (mapcar
;;    #'(lambda (dir)
;;        (expand-file-name dir (file-name-directory load-file-name)))
;;    '("node-dest" "activities")))

;; ;;;###autoload
;; (dolist (dir (mapcar
;;               #'(lambda (dir)
;;                   (expand-file-name dir (file-name-directory load-file-name)))
;;               '("node-dest" "activities")))
;;   (@:message "adding %s to load path" dir)
;;   (add-to-list 'load-path dir))

;; ;;;###autoload
;; (eval-when-compile
;;   '(dolist (dir (mapcar
;;                  #'(lambda (dir)
;;                      (expand-file-name dir (file-name-directory load-file-name)))
;;                  '("node-dest" "activities")))
;;      (@:message "adding %s to load path" dir)
;;      (add-to-list 'load-path dir)))

;; ;;;###autoload
;; (defun activity-add-subdirs-load-path ()
;;   (dolist (dir activity-subdirs)
;;     (add-to-list 'load-path dir)))



(drive-extended@ @activity-interface (@activity-base) "activity"
  (def@ @@ :key ()
    (@:error "Implement :key interface"))
  (def@ @@ :activate ()
    (@:error "Implement :activate interface"))
  (def@ @@ :deactivate ()
    (@:error "Implement :deactivate interface")))

(drive-extended@ @activity (@activity-base) "activity"
  "Activity class"
  (def@ @@ :init ()
    (@^:init)
    (@:message "@activity-class :init")
    (setf @:started-acts   nil
          @:activities     nil))

  (def@ @@ :reset ()
    (@:deactivate-all)
    (setf @:started-acts   nil
          @:activities   nil))

  (def@ @@ :activate (act)
    (when (@! act :activate)
      (remove act
              @:activities)
      (push act
            @:started-acts)))

  (def@ @@ :deactivate (act)
    (when (@! act :deactivate)
      (remove act
              @:started-acts)
      (push act
            @:activities)))

  (def@ @@ :activate-all ()
    (dolist (act @:activities)
      (@:activate act)))

  (def@ @@ :deactivate-all ()
    (dolist (act @:activities)
      (@:deactivate act)))

  (def@ @@ :add (activity)
    (let ((key (@! activity :key)))
      (if (member key
                  (@mapcar @:key @:activities))
          (@:error "activity with %s already present." key)
        (push activity
              @:activities))))

  (def@ @@ :find (key)
    (cl-find-if #'(lambda (e) (qual (@! e :key) key))
                @:activities))

  (def@ @@ :inspect ()
    (@:message
     "active: [%s], insinuate: [%s], uninsinuate: [%s]"
     @:activites
     @:started-acts))

  (@:init))


(defun activity-inspect ()
  (interactive)
  (@! @activity :inspect))

;;;###autoload
(defun activity-reset ()
  (interactive)
  (@! @activity :reset))

;;;###autoload
(defun activity-activate (key)
  (interactive
   (list (completing-read "activity: "
                          (@mapcar @:key (@ @activity :activities)))))
  ;; (activity-add-subdirs-load-path)
  (let ((act (@! @activity :find key)))
    (when act
      (@! @activity :activate))))

;;;###autoload
(defun activity-deactivate (key)
  (interactive
   (list (completing-read "activity: "
                          (@mapcar @:key
                                   (@ @activity :started-acts)))))
  (let ((act (@! @activity :find key)))
    (when act
      (@! @activity :deactivate))))

;;;###autoload
(defun activity-activate-all ()
  (interactive)
  ;; (activity-add-subdirs-load-path)
  (@! @activity :activate-all))

;;;###autoload
(defun activity-deactivate-all ()
  (interactive)
  (@! @activity :deactivate-all))

;;;###autoload
(defun activity-register (activity)
  (interactive)
  (@! @activity :add activity))


(when nil
  (activity-inspect)
  )



;; (require 'buff-trans)
;; (require 'mail-event)
;; (require 'org-clock-trans)



(defun activity-bind-hooks ()
  "Watch for activity in buffers."
  ;; (add-hook 'after-save-hook 'activity-save nil t)
  ;; (add-hook 'auto-save-hook 'activity-save nil t)
  ;; (add-hook 'first-change-hook 'activity-ping nil t)
  )

(defun activity-unbind-hooks ()
  "Stop watching for activity in buffers."
  ;; (remove-hook 'after-save-hook 'activity-save t)
  ;; (remove-hook 'auto-save-hook 'activity-save t)
  ;; (remove-hook 'first-change-hook 'activity-ping t)
  )

(defun activity-turn-on (defer)
  "Turn on Activity."
  (ignore defer)
  (activity-bind-hooks))

(defun activity-turn-off ()
  "Turn off Activity."
  (activity-unbind-hooks))


;;;###autoload
(define-minor-mode activity-mode
  "Toggle Activity (Activity mode)."
  :lighter    " act"
  :init-value nil
  :global     nil
  :group      'activity
  (cond
    (noninteractive (setq activity-mode nil))
    (activity-mode (activity-turn-on t))
    (t (activity-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-activity-mode activity-mode
  (lambda () (activity-mode 1)))

;;; activity.el ends here
