;;; debug-info.el --- debug informations             -*- lexical-binding: t; -*-

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

(provide 'debug-info)


;;(require 'notify)
(require 'backtrace-debug)

;; https://www.emacswiki.org/emacs/EmacsMemoryDebugging
;; https://www.reddit.com/r/emacs/comments/aol1oz/what_are_some_good_strategies_to_reduce_emacs/
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2004-09/msg00342.html
;; http://davin.50webs.com/research/2010/diagnose.el.html

(when nil
  cons-cells-consed
  floats-consed
  vector-cells-consed
  symbols-consed
  string-chars-consed
  misc-objects-consed
  intervals-consed
  strings-consed


  (assoc 'pmem (process-attributes (emacs-pid)))

  (/ (* (cl-rest (assoc 'vsize (process-attributes (emacs-pid)))) 100.000)
     (+ (cl-first (memory-info)) (nth 3 (memory-info))))

  (/ (* (cl-rest (assoc 'vsize (process-attributes (emacs-pid)))) 100.000)
     (cl-first (memory-info))))


(defvar process-monitor-memory-usage-period-seconds 60)


(defvar process-monitor-mem-usage-alist nil)
(defvar process-monitor-mem-usage-timer-alist nil)

(defun process-percentage-mem-usage (pid)
  (let ((attrib (process-attributes pid)))
    (when attrib
      (/ (* (cl-rest (assoc 'rss attrib)) 100.000)
         (cl-first (memory-info))))))

(defun process-pid-list ()
  (let ((process-comm-pid-alist
         (mapcar #'(lambda (pid)
                     (let ((attribs (process-attributes pid)))
                       (cons (cl-rest (assoc 'comm attribs)) pid)))
                 (list-system-processes))))
    (cl-rest (assoc (completing-read "Process PID: " process-comm-pid-alist)
                 process-comm-pid-alist))))

;;;###autoload
(defun process-check-mem-usage-diff (pid)
  (interactive
   (list (process-pid-list)))
  (let ((attribs (process-attributes pid)))
    (if attribs
        (let ((old-mem  (or (cl-rest (assoc pid process-monitor-mem-usage-alist)) 0))
              (new-mem  (or (process-percentage-mem-usage pid) 0))
              (pgm-name (or (cl-rest (assoc 'comm attribs)) "anonymous")))
          (unless (= old-mem new-mem)
            (let ((msg (format "pid %d memory usage %s by (%f - %f) = %f"
                               pid
                               (if (< old-mem new-mem) "increased" "decreaded")
                               new-mem old-mem
                               (- new-mem old-mem))))
              (if (assoc pid process-monitor-mem-usage-alist)
                  (setf (cl-rest (assoc pid process-monitor-mem-usage-alist)) new-mem)
                (push (cons pid new-mem) process-monitor-mem-usage-alist))
              (notify (format "memory usage of %s[%d]" pgm-name pid)
                      msg))))
      (process-monitor-memory-usage-stop pid))))

(defun process-attrib-diff (alist)
  ;; TODO: Create a diff function and use it in process-check-mem-usage-diff function and notify-memory-usage macro.
  "Diff function."
  ())

;;;###autoload
(defun process-monitor-memory-usage-stop (pid)
  (interactive
   (list (process-pid-list)))
  (when (assoc pid process-monitor-mem-usage-timer-alist)
    (cancel-timer
     (cl-rest (assoc pid process-monitor-mem-usage-timer-alist)))
    (setq process-monitor-mem-usage-timer-alist
          (remove (assoc pid process-monitor-mem-usage-timer-alist) process-monitor-mem-usage-timer-alist))
    (setq process-monitor-mem-usage-alist
          (remove (assoc pid process-monitor-mem-usage-alist) process-monitor-mem-usage-alist))))

;;;###autoload
(defun process-monitor-memory-usage (pid)
  (interactive
   (list (process-pid-list)))
  (if (process-attributes pid)
      (unless (assoc pid process-monitor-mem-usage-timer-alist)
        (let ((timer (run-with-timer process-monitor-memory-usage-period-seconds
                                     process-monitor-memory-usage-period-seconds
                                     #'process-check-mem-usage-diff pid)))
          (push (cons pid timer) process-monitor-mem-usage-timer-alist)))
    (process-monitor-memory-usage-stop pid)))

(defmacro notify-memory-usage (tag fun &rest body)
  `(let ((pid   (emacs-pid))
         (alist nil))
    (let ((prev-percentage-mem (process-percentage-mem-usage pid)))
      (prog1
          ,@body
        (let ((next-percentage-mem (process-percentage-mem-usage pid)))
          (unless (= prev-percentage-mem next-percentage-mem)
            (push (cons 'percentage-mem (- prev-percentage-mem next-percentage-mem))))
          (dolist (pair alist)
            (apply fun
                   "%s %s by %f\n"
                    tag
                    (if (> (cl-rest pair) 0) "increased" "decreased")
                    (cl-rest pair))))))))
(put 'notify-memory-usage 'lisp-indent-function 2)

(defmacro notify-memory-usage-message (tag &rest body)
  `(notify-memory-usage ,tag #'message ,@body))
(put 'notify-memory-usage-message 'lisp-indent-function 1)

;; (defmacro __test_mac (fun)
;;   `(message "%s" ,fun))
;; (__test_mac 'x)

;; (defmacro install-debug-on-macro (fun)
;;   `(defun ,fun (&rest args)
;;      (message "running %s" ',fun)
;;      (debug)))

(defvar debug-replaced-functions nil)

(defun debug-replace-on (fun)
  "Installed debug function, can later be removed by uninstall-debug-on"
  (interactive "afunction name: ")
  (message "installing %s" fun)

  (let ((def (if (fboundp fun)
                 (symbol-function fun))))
    (push (cons fun def) debug-replaced-functions)
    (eval `(defun ,fun (&rest args)
             (message "running %s" ',fun)
             (debug)))))
(defun debug-uninstall-on (fun)
  (interactive
   (list (let* ((funs (mapcar #'car debug-replaced-functions))
                (funname (completing-read "function name: "
                                          funs
                                          #'(lambda (s) (memq s funs))
                                          t)))
           (intern funname nil))))
  (let* ((fundef (assoc fun debug-replaced-functions))
         (fun (car fundef))
         (def (cdr fundef)))
    (setq debug-replaced-functions
          (delete* fun debug-replaced-functions :key #'car))
    (fmakunbound fun)
    (if def
        (progn
          (message "setting %s" def)
          (fset fun def))
      (message "def is null"))))

;; (defun __test__ (x) x)
;; (__test__ 1)
;; (symbol-function '__test__)

;; (cdr (assoc '__test__ debug-replaced-functions))
;; (assoc y'__test__ debug-replaced-functions)


(defvar debug-instrumented-functions nil)

(defun debug-instrument-on (fun)
  "Installed debug function, can later be removed by uninstall-debug-on"
  (interactive "afunction name: ")
  (message "installing %s" fun)

  (let ((def (if (fboundp fun)
                 (symbol-function fun))))
    ;; add-advice
    ))
(defun debug-uninstall-on (fun)
  (interactive
   (list (let* ((funs (mapcar #'car debug-instrumented-functions))
                (funname (completing-read "function name: "
                                          funs
                                          #'(lambda (s) (memq s funs))
                                          t)))
           (intern funname nil))))
  (let* ((fundef (assoc fun debug-instrumented-functions))
         (fun (car fundef))
         (def (cdr fundef)))
    (setq debug-instrumented-functions
          (delete* fun debug-instrumented-functions :key #'car))
    ;; remove-advice
    ))

;;;###autoload
(defun add-to-debug-ignore-errors(errstr)
  (interactive "serror string: ")
  (unless (member errstr debug-ignored-errors)
    (push errstr debug-ignored-errors)))
(progn
  (add-to-debug-ignore-errors "Nothing to complete")
  (add-to-debug-ignore-errors "Unmatched Text during Lexical Analysis")
  (add-to-debug-ignore-errors "‘global’ non-zero exit: global: GTAGS not found.")
  (add-to-debug-ignore-errors "‘global’ non-zero exit: global: only name char is allowed with -c option."))

(when nil
  (defvar custom-record-command-error-list nil
    "custom-record-command-error-list")

  (defun custom-record-command-error (data context signal)
    (push (list data context signal)
          custom-record-command-error-list)
    (nbutlast custom-record-command-error-list
              (- (length custom-record-command-error-list) 8)))

  (add-function :before command-error-function
                #'custom-record-command-error)

  (remove-function command-error-function
                   #'custom-record-command-error))

;;; debug-info.el ends here
