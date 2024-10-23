;;
;; buffer-utils.el
;; Login : <s@taj>
;; Started on  Tue Jul 27 00:19:15 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


;; Follow mode.
;; Dead useful for wider-than-tall-hires monitors.  Will let you do a
;; C-x 3, then use both windows as a single tall window, just broken in
;; half and displayed side by side.  Neat-o.
;; get it from: http://www.andersl.com/emacs/follow.html

(provide 'lotus-buffer-utils)


;; (require 'cl)

(eval-when-compile
  '(require 'cl))

;; (require 'ibuffer)
;; (require 'uniquify)
;; (require 'planner)
;; believe it need not be here
;; (eval-after-load "ido"
;;   '(lotus-disable-startup-interrupting-feature))
(require 'buffer-move nil t)




(dolist (ext '(".rem"))
  (add-to-list 'completion-ignored-extensions ext))


(progn ;; deh-require-maybe follow-mode
  ;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
  (autoload 'follow-mode "follow.el") ;; "follow.el" nil t)
  (autoload 'follow-delete-other-windows-and-split "follow.el") ;; "follow.el" nil t)

  ;;
  ;; Some global keys so that I can activate Follow Mode fast.
  ;;
  (add-hook 'follow-mode-hook 'my-follow-mode-hook))


(defmacro set-assoc (key val alist)
  `(progn
     (when (null (assoc ,key ,alist))
       (setq ,alist (acons ,key nil ,alist)))
     (setcdr (assoc ,key ,alist) ,val)))


;; https://github.com/purcell/ibuffer-vc/blob/master/ibuffer-vc.el
;; (xrequire 'ibuffer-vc)



;; ;; from https://github.com/purcell/emacs.d/blob/master/init-uniquify.el
;; ;; from http://jasonmbaker.com/7-tools-for-working-with-python-in-emacs-and
;; (setq uniquify-buffer-name-style 'reverse
;;       uniquify-separator "/"
;;       uniquify-after-kill-buffer-p t ; rename after killing uniquified
;;                                         ; don't muck with special buffers (or Gnus mail buffers
;;       uniquify-ignore-buffers-re "^\\*")


(unless (fboundp 'next-buffer)
  (defun next-buffer ()
    "Switch to the next buffer in cyclic order."
    (interactive)
    (let ((buffer (current-buffer)))
      (switch-to-buffer (other-buffer buffer))
      (bury-buffer buffer))))
(unless (fboundp 'prev-buffer)
  (defun prev-buffer ()
    "Switch to the previous buffer in cyclic order."
    (interactive)
    (let ((list (nreverse (buffer-list)))
          found)
      (while (and (not found) list)
        (let ((buffer (cl-first list)))
          (if (and (not (get-buffer-window buffer))
                   (not (string-match "\\` " (buffer-name buffer))))
              (setq found buffer)))
        (setq list (cl-rest list)))
      (switch-to-buffer found))))


;; http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00978.html
;;;###autoload
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))
;; then M-x sticky-buffer-mode.
;; believe it need not be here
;; (lotus-disable-startup-interrupting-feature)
;;
;; (setq debug-on-error t)


;;;###autoload
(defun ido-schedule-kill-buffer ()
  "Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive)
  (ido-buffer-internal 'kill 'schedule-kill-buffer "Kill buffer: " (buffer-name (current-buffer)) nil 'ignore))

(defun schedule-kill-buffer ())
;; schedule killing buffer, if not accessed again in fixed time.

;;; buffer-utils.el ends here
