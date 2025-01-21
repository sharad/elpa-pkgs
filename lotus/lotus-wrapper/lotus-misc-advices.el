;;; lotus-misc-advices.el --- misc advices           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sharad

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'lotus-misc-advices)


(require 'org)

;;;###autoload
(defun alternate--erc-identd-start (&optional port)
  "Start an identd server listening to port 8113.
Port 113 (auth) will need to be redirected to port 8113 on your
machine -- using iptables, or a program like redir which can be
run from inetd.  The idea is to provide a simple identd server
when you need one, without having to install one globally on your
system."
  (interactive (list (read-string "Serve identd requests on port: " "8113")))
  (unless port (setq port erc-identd-port))
  (when (stringp port)
    (setq port (string-to-number port)))
  (when erc-identd-process
    (delete-process erc-identd-process))
  (setq erc-identd-process
        (make-network-process :name "identd"
                              :buffer nil
                              :host 'local :service port
                              :server t :noquery t :nowait nil
                              :filter 'erc-identd-filter))
  (set-process-query-on-exit-flag erc-identd-process nil))
;;;###autoload
(defalias 'override--erc-identd-start #'alternate--erc-identd-start)


;;;###autoload
(defun fixed--pm--run-other-hooks (allow syms hook &rest args)
  (when (and allow polymode-mode pm/polymode)
    (save-excursion
      (dolist (sym syms)
        (dolist (buf (eieio-oref pm/polymode '-buffers))
          (when (buffer-live-p buf)
            (unless (eq buf (current-buffer))
              (with-current-buffer buf
                (when (memq sym (symbol-value hook))
                  (if args
                      (apply sym args)
                    (funcall sym)))))))))))
;;;###autoload
(defalias 'override--pm--run-other-hooks #'fixed--pm--run-other-hooks)




(defun magit-toplevel-fn-with (&optional directory))









;;;###autoload
(defun fixed--semantic-mode (oldfun &rest r)
  (cl-flet ((buffer-list ()
              (remove-if-not #'buffer-live-p (buffer-list))))
    (apply oldfun r)))
;;;###autoload
(defalias 'around--semantic-mode #'fixed--semantic-mode)


;; clean-buffer-list from midnight -- postpone till recursive-edit is free


(defun around--magit-toplevel-around-advice-fn-with (oldfn &rest r)
  (or (apply oldfn r)
      (unless (car r)
        (let ((filename (buffer-file-name)))
          (when filename
            (locate-dominating-dir-by-file filename ".git"))))))

;;; lotus-misc-advices.el ends here
