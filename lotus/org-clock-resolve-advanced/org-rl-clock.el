;;; org-rl-clock.el --- Org resolve clock            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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

;;

;;; Code:

(provide 'org-rl-clock)


(require 'time-stamp)
(require 'org-capture+-helm)
(require 'org-capture+-lib)
(require 'org-capture)
(eval-when-compile
    (require 'org-capture+-macros))

(require 'org-rl-obj-cps)
(require 'org-rl-obj)


(defun org-rl-get-resume-clocks (resume-clocks clock-alist)
  (if (cl-notany #'(lambda (name-clock)
                     (when (cl-rest name-clock)
                       (org-rl-clock-current-real (cl-rest name-clock))))                       ;;no current running clock should be present.
                 clock-alist)
      (cl-remove-if #'(lambda (name-clock)
                        (or (not (consp name-clock))
                            (null (cl-rest name-clock))
                            (org-rl-clock-null (cl-rest name-clock))))
                    (append clock-alist
                            (unless (consp resume-clocks) nil)))))


(cl-defmethod org-rl-clock-opt-jump-to ((clock org-rl-clock)
                                        &optional
                                        resume-clocks)
  (ignore resume-clocks)
  (org-rl-clock-clock-jump-to clock))

(cl-defmethod org-rl-clock-opt-cancel-prev ((prev org-rl-clock)
                                            (next org-rl-clock)
                                            &optional
                                            resume-clocks)
  ;; (setf (org-rl-clock-cancel prev) t)
  (org-rl-clock-clock-cancel prev)
  (org-rl-debug nil "cancelled prev, Can not find previous clock presently [todo]")
  (setf (org-rl-clock-start prev) (org-rl-clock-start prev))
  (setf (org-rl-clock-marker prev) nil)
  ;; (org-rl-clocks-action nil nil prev next)
  ;; TODO: add off to restart now (org-rl-clock-restart-now)
  (list (list prev next)
        (org-rl-get-resume-clocks resume-clocks
                                  (list (cons :prev prev)
                                        (cons :next next)))))

(cl-defmethod org-rl-clock-opt-cancel-next ((prev org-rl-clock)
                                            (next org-rl-clock)
                                            &optional
                                            resume-clocks)
  ;; what to do with prev.
  ;; cancel next clock
  ;; add next clock time
  ;; (setf (org-rl-clock-cancel next) t)
  (org-rl-clock-clock-cancel next)
  ;;should 'now be used here? todo
  (setf (org-rl-clock-start next) (org-rl-clock-stop prev))
  (setf (org-rl-clock-stop  next) (org-rl-clock-stop prev))
  (setf (org-rl-clock-marker  next) nil)
  ;; (org-rl-clocks-action nil nil prev next)
  ;; TODO: add off to restart now (org-rl-clock-restart-now)
  (list (list prev next)
        (org-rl-get-resume-clocks resume-clocks
                                  (list (cons :prev prev)
                                        (cons :next next)))))

(cl-defmethod org-rl-clock-opt-include-in-prev ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen-mins
                                                &optional
                                                resume
                                                fail-quietly
                                                resume-clocks)

  (org-rl-debug nil "begin %s" 'org-rl-clock-opt-include-in-prev)

  (let ((maxtimelen-secs (org-rl-get-time-gap-secs prev next)))
    (if (= (org-rl-compare-time-gap-mins prev next timelen-mins) 0)
        (progn
          (org-rl-debug nil "compare = %s" 'org-rl-clock-opt-include-in-prev)
          (setf (org-rl-clock-stop-time prev) (time-add
                                               (org-rl-clock-start-time prev) maxtimelen-secs))
          (org-rl-clock-clock-out prev fail-quietly))
      (if (> (org-rl-compare-time-gap-mins prev next timelen-mins) 0)
          (if (> timelen-mins 0)
              (setf prev (org-rl-clock-expand-time prev (abs timelen-mins) resume))
            (progn
              (setf prev (org-rl-clock-clock-out prev fail-quietly))     ;if already not necessary
              (org-rl-clock-clock-out next fail-quietly)     ;if necessary
              (setf next
                    (org-rl-make-clock (org-rl-clock-marker prev)
                                       (time-subtract (org-rl-clock-start-time next)
                                                      (abs timelen-mins))
                                       (org-rl-clock-stop-time next)))
              (org-rl-clock-clock-in-out next resume fail-quietly)))
        (error "timelen-mins %d is greater than time difference %d[=%d] between clocks"
               timelen-mins
               maxtimelen-secs
               (float-time (org-rl-get-time-gap prev
                                                next))))))
  ;; (org-rl-clocks-action resume fail-quietly prev next)
  (org-rl-debug nil "finish %s" 'org-rl-clock-opt-include-in-prev)
  ;; TODO: add off to restart now (org-rl-clock-restart-now)
  (list (list prev next)
        (org-rl-get-resume-clocks resume-clocks
                                  (list (cons :prev prev)
                                        (cons :next next)))))

(cl-defmethod org-rl-clock-opt-include-in-next ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                timelen-mins
                                                &optional
                                                resume
                                                fail-quietly
                                                resume-clocks)
  (org-rl-debug nil "begin %s" 'org-rl-clock-opt-include-in-next)
  (let ((maxtimelen-secs (org-rl-get-time-gap-secs prev
                                                   next)))
    (if (= 0 (org-rl-compare-time-gap-mins prev
                                           next
                                           timelen-mins))
        (progn
          (org-rl-debug nil "compare = %s" 'org-rl-clock-opt-include-in-next)
          (setf (org-rl-clock-stop-time next) (time-add (org-rl-clock-start-time next)
                                                        maxtimelen-secs))
          (org-rl-clock-clock-out next
                                  fail-quietly))
      (if (> (org-rl-compare-time-gap-mins prev
                                           next
                                           timelen-mins)
             0)
          (if (< timelen-mins 0)
              (setf next (org-rl-clock-expand-time next
                                                   timelen-mins
                                                   resume))
            (progn
              (org-rl-clock-clock-out prev fail-quietly)     ;if necessary
              (setf next (org-rl-clock-clock-out next fail-quietly))     ;if necessary
              (setf prev (org-rl-make-clock (org-rl-clock-marker next)
                                            (org-rl-clock-stop-time prev)
                                            (time-add (org-rl-clock-stop-time prev)
                                                      (abs timelen-mins))))
              (setf prev (org-rl-clock-clock-in-out prev resume fail-quietly))))
        (error "timelen-mins %d is greater than time difference %d[=%d] between clocks"
               timelen-mins
               maxtimelen-secs
               (float-time (org-rl-get-time-gap prev
                                                next))))))
  ;; (org-rl-clocks-action resume fail-quietly prev next)
  (org-rl-debug nil "finish %s" 'org-rl-clock-opt-include-in-next)
  ;; TODO: add off to restart now (org-rl-clock-restart-now)
  (list (list prev next)
        (org-rl-get-resume-clocks resume-clocks
                                  (list (cons :prev prev)
                                        (cons :next next)))))

(cl-defmethod org-rl-clock-opt-include-in-other ((prev org-rl-clock)
                                                 (next org-rl-clock)
                                                 other-marker
                                                 timelen-mins
                                                 &optional
                                                 resume
                                                 fail-quietly
                                                 resume-clocks)
  (org-rl-debug nil "begin %s" 'org-rl-clock-opt-include-in-other)
  (org-rl-debug nil "begin %s: prev=%s next=%s other-marker-%s timelen-mins=%d"
                'org-rl-clock-opt-include-in-other
                prev
                next
                other-marker
                timelen-mins)

  (let ((maxtimelen-secs   (org-rl-get-time-gap-secs prev
                                                     next))
        (other-marker      (or other-marker
                               (org-rl-org-select-other-clock (org-rl-marker (cl-some #'org-rl-clock-real-p
                                                                                      (list prev
                                                                                            next))))))
        (resume-alist      nil))
    (ignore maxtimelen-secs)
    (progn
      (setf prev (org-rl-clock-clock-out prev fail-quietly))     ;if necessary
      (setf next (org-rl-clock-clock-out next fail-quietly))     ;if necessary
      (push (cons :prev prev) resume-alist)
      (push (cons :next next) resume-alist))
    (if (> timelen-mins 0)
        (setq prev (org-rl-clock-clock-in-out (org-rl-make-clock other-marker
                                                                 (org-rl-clock-stop-time prev)
                                                                 (time-add (org-rl-clock-stop-time prev)
                                                                           timelen-mins))
                                              resume
                                              fail-quietly))
      (setq next (org-rl-clock-clock-in-out (org-rl-make-clock other-marker
                                                               (time-subtract (org-rl-clock-start-time next)
                                                                              (abs timelen-mins))
                                                               (org-rl-clock-stop-time next))
                                            resume
                                            fail-quietly)))
    (progn
      (push (cons :other (if (> timelen-mins 0) prev next))
            resume-alist)
            ;; (org-rl-clocks-action nil nil prev next)
      (org-rl-debug nil "finish %s" 'org-rl-clock-opt-include-in-other)
      ;; TODO: add off to restart now (org-rl-clock-restart-now)
      (list (list prev
                  next)
            (org-rl-get-resume-clocks resume-clocks
                                      resume-alist)))))

(cl-defmethod org-rl-clock-opt-include-in-new ((prev org-rl-clock)
                                               (next org-rl-clock)
                                               template
                                               timelen-mins
                                               &optional
                                               resume
                                               fail-quietly
                                               resume-clocks)
  (org-rl-debug nil "begin %s" 'org-rl-clock-opt-include-in-new)
  (org-rl-debug nil "begin %s: prev=%s next=%s timelen-mins=%d"
                'org-rl-clock-opt-include-in-new
                prev
                next
                timelen-mins)
  (org-rl-debug nil "begin %s: template=%s"
                'org-rl-clock-opt-include-in-new
                template)

  (let ((mrk (org-rl-org-select-other-clock
              (org-rl-marker (cl-some #'org-rl-clock-real-p
                                      (list prev next))))))
    (org-rl-debug nil "begin %s: org-rl-select-other-clock: %s" 'org-rl-clock-opt-include-in-new mrk)
    (org-rl-debug nil "begin %s: template: %s" 'org-rl-clock-opt-include-in-new template)
    (after-org-capture-run mrk 'entry `(marker ,mrk) template '(:empty-lines 1)
      (progn
        (org-rl-debug nil "org-rl-clock-opt-include-in-new: org-capture-last-stored-marker = %s" org-capture-last-stored-marker)
        (org-rl-debug nil "org-rl-clock-opt-include-in-new: (org-capture-get :position-for-last-stored 'local) = %s" (org-capture-get :position-for-last-stored 'local))
        ;; (org-rl-clock-opt-include-in-other prev next mrk timelen-mins resume fail-quietly resume-clocks)
        (let ((maxtimelen-secs (org-rl-get-time-gap-secs prev next)))
          (org-rl-clock-cps-process-option timelen-mins
                                           (cons 'include-in-other mrk)
                                           prev
                                           next
                                           maxtimelen-secs
                                           resume
                                           fail-quietly
                                           resume-clocks)))))
  ;; (org-rl-clocks-action nil nil prev next)
  (org-rl-debug nil "finish %s" 'org-rl-clock-opt-include-in-new)
  ;; TODO: add off to restart now (org-rl-clock-restart-now)
  ;; next processing will be handled in after-org-capture-run cps
  nil)


(cl-defmethod org-rl-org-clock-get-clocks ((prev org-rl-clock)
                                           (next org-rl-clock)
                                           opt
                                           timelen-mins
                                           maxtimelen-mins
                                           &optional
                                           resume
                                           fail-quietly
                                           resume-clocks)
  (cond ((eq opt 'jump-prev)
         ;; finish here
         (org-rl-clock-opt-jump-to prev resume-clocks)
         nil)

        ((eq opt 'jump-next)
         ;; finish here
         (org-rl-clock-opt-jump-to next resume-clocks)
         nil)

        ((eq opt 'cancel-prev)
         (org-rl-clock-opt-cancel-prev prev next resume-clocks))
        ;; set org-clock-leftover-time here

        ((eq opt 'cancel-next)
         (org-rl-clock-opt-cancel-next prev next resume-clocks))

        ((eq opt 'include-in-prev)
         ;; include timelen-mins in prev
         ;; update timelength-mins
         (org-rl-clock-opt-include-in-prev prev next timelen-mins resume fail-quietly resume-clocks))
        ;; set org-clock-leftover-time here
        ((eq opt 'include-in-next)
         (org-rl-clock-opt-include-in-next  prev next timelen-mins resume fail-quietly resume-clocks))

        ((eq opt 'include-in-other) ;; subtract timelen-mins from timelength-mins
         (org-rl-clock-opt-include-in-other prev next nil timelen-mins resume fail-quietly resume-clocks))

        ((eq opt 'include-in-new)
         (org-rl-clock-opt-include-in-new prev next nil timelen-mins resume fail-quietly resume-clocks))

        ((consp opt)
         (org-rl-debug nil "org-rl-clock-time-process-option: in consp")
         (let ((caropt (cl-first opt)))
           (cond
            ((eq caropt 'include-in-other) ;; subtract timelen-mins from timelength-mins
             (let ((other-marker   (cl-rest opt)))
               (org-rl-clock-opt-include-in-other prev next other-marker timelen-mins resume fail-quietly resume-clocks)))

            ((eq caropt 'include-in-new)
             (let ((template (cl-rest opt)))
               (org-rl-clock-opt-include-in-new prev next template timelen-mins resume fail-quietly resume-clocks)))
            (t (error "Wrong option %s" opt)))))

        ((eq opt 'done)
         nil)

        ((eq opt 'restart)
         (list (list prev next)
               resume-clocks))

        (t (error "Wrong option %s" opt))))

(cl-defmethod org-rl-clock-time-process-option ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                opt
                                                timelen-mins
                                                maxtimelen-mins
                                                &optional
                                                resume
                                                fail-quietly
                                                resume-clocks)
  (ignore maxtimelen-mins)
  (org-rl-debug nil "org-rl-clock-time-process-option: begin")
  (org-rl-debug :warning "started org-rl-clock-time-process-option: selected opt=%s" opt)
  (let* ((clocks (org-rl-org-clock-get-clocks prev
                                              next
                                              opt
                                              timelen-mins
                                              maxtimelen-mins
                                              resume
                                              fail-quietly
                                              resume-clocks)))
    (org-rl-debug nil "org-rl-clock-time-process-option: finished")
    clocks))


(defun org-rl-clock-resume-clock (resume-clocks)
  (org-rl-debug :warning "resume clocks %s" resume-clocks)
  (if org-clock-marker
      (org-rl-debug :warning "already clocking-in not resuming clocks %s" resume-clocks)
    (let* ((resume-clocks (cl-remove-if-not #'consp resume-clocks))
           (resume-clocks (append resume-clocks '((:done nil))))
           (resume-clocks (mapcar #'(lambda (el)
                                      (cons (format "%s %s"
                                                    (cl-first el)
                                                    (if (cl-rest el) (org-rl-format (cl-rest el))))
                                            el))
                                    resume-clocks))
           (sel       (completing-read "resume clock: " resume-clocks))
           (sel-clock (assoc sel resume-clocks)))
      (when sel-clock
        (org-rl-clock-clock-in sel-clock)))))


;; NOTE: Remember here the concept of Positive and Negative and Full time.
;; Read time which could be positive or negative or full
;; TODO: option for restart also required for with active clock.
(cl-defmethod org-rl-clock-simple-resolve-time ((prev org-rl-clock)
                                                (next org-rl-clock)
                                                &optional
                                                resume
                                                fail-quietly
                                                resume-clocks)
  "Read time which could be positive or negative or full"
  ;; BUG how to handle current time == 'now
  ;; BUG how to handle when prev == next
  "Resolve clock time"
  ;; last-input-event
  ;; last-event-frameo
  ;; TODO: send some tag or signal when other frame selection
  ;; set pre-command-hook to know if other frame is getting focus
  ;; than save data for this function and abort this function invocation here
  ;; again run this function in that frame.

  (org-rl-clock-assert prev)
  (org-rl-clock-assert next)

  (org-rl-debug nil "org-rl-clock-simple-resolve-time: begin")
  (lotus-with-override-minibuffer-if
      (progn
        (org-rl-debug nil "org-rl-clock-simple-resolve-time: [minibuffer-body] lotus-with-override-minibuffer-if active minibuffer found aborting it."))
    (org-rl-debug nil "org-rl-clock-simple-resolve-time: [body] lotus-with-override-minibuffer-if")
    (let ((debug-prompt t)
          (maxtimelen-secs (org-rl-get-time-gap-secs prev next)))
      ;;;
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 10 5] with maxtimelen-secs 5
      ;; Warning (org-rl-clock): You have selected opt subtract and timelen-mins 9
      ;; Warning (org-rl-clock): going to run prev[STARTED Unnamed task 565 51 0] next[imaginary 5 5] with maxtimelen-secs 0
      (org-rl-debug nil
                    "org-rl-clock-simple-resolve-time: going to run %s with maxtimelen-secs %d"
                    (org-rl-clock-time-adv-debug-prompt prev next) maxtimelen-secs)
      ;; (assert (> maxtimelen-secs 0))
      (when (> maxtimelen-secs 0)
        (let* ((maxtimelen-mins-fn #'(lambda () (org-rl-get-time-gap-mins prev next)))
               (options (org-rl-clock-build-options prev next maxtimelen-secs resume fail-quietly resume-clocks))
               (option
                (org-rl-clock-read-option
                 org-rl-read-interval-secs
                 #'(lambda ()
                     (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                       (if debug-prompt
                           (format "%s Select option [%d]: " (org-rl-clock-time-debug-prompt prev next) maxtimelen-mins)
                         (format "Select option [%d]: " maxtimelen-mins))))
                 options
                 maxtimelen-mins-fn))
               (opt (cl-first option))
               (timelen-mins
                (org-rl-clock-read-timelen-mins
                 org-rl-read-interval-secs
                 #'(lambda ()
                     (let ((maxtimelen-mins (funcall maxtimelen-mins-fn)))
                       (if debug-prompt
                           (format "%s [%s] how many minutes? [%d] " (org-rl-clock-time-debug-prompt prev next) opt maxtimelen-mins)
                         (format "[%s] how many minutes? [%d] " opt maxtimelen-mins))))
                 opt
                 maxtimelen-mins-fn)))
          ;; (barely-started-p (< (- (float-time last-valid)
          ;;                         (float-time (cl-rest clock))) 45))
          ;; (start-over-p (and subtractp barely-started-p))
          ;; cancel prev and add to time


          (org-rl-debug nil "You have selected opt %s and timelen-mins %d" opt timelen-mins)
          (let ((maxtimelen-mins (org-rl-get-time-gap-mins prev next))) ;get maxtimelen-mins time again
            (if (<=
                 (abs timelen-mins)
                 maxtimelen-mins)
                (let* ((clocks
                        (org-rl-clock-time-process-option prev next
                                                          opt timelen-mins
                                                          maxtimelen-mins
                                                          resume
                                                          fail-quietly
                                                          resume-clocks))
                       (resolve-clocks (nth 0 clocks))
                       (resume-clocks (nth 1 clocks))
                       (prev (nth 0 resolve-clocks))
                       (next (nth 1 resolve-clocks)))
                  (org-rl-debug nil "(org-rl-clock-null prev[%s]) %s" (org-rl-format prev) (org-rl-clock-null prev))
                  (org-rl-debug nil "(org-rl-clock-null next[%s]) %s" (org-rl-format next) (org-rl-clock-null next))
                  (if (and
                       resolve-clocks
                       (not
                        (and
                         (org-rl-clock-null prev)
                         (org-rl-clock-null next)))
                       (> (org-rl-get-time-gap-mins prev next) 0))
                      (org-rl-clock-simple-resolve-time prev next resume fail-quietly resume-clocks)
                    (if resume
                        (org-rl-clock-resume-clock resume-clocks))
                    (if resolve-clocks
                       (org-rl-debug :warning "Done prev[%s] next[%s] gap[%d]"
                                     (org-rl-format prev)
                                     (org-rl-format next)
                                     (if (and
                                          (org-rl-clock-null prev)
                                          (org-rl-clock-null next))
                                         (org-rl-get-time-gap-mins prev next)
                                       -1)))))
                (org-rl-debug :warning "Done no clock to resolve")

              (org-rl-debug nil "Error given time %d can not be greater than %d" timelen-mins maxtimelen-mins)))))))
  (org-rl-debug nil "org-rl-clock-simple-resolve-time: finished"))


;; BUG: TODO: need to use (occ-clock-in occ-ctxtual-tsk)
(defun org-rl-clock-clock-in-as-it-is (marker)
  (interactive
   (list (point-marker)))
  (let ((start (org-clock-get-nth-half-clock-time marker)))
    (if start
        (org-rl-straight-org-clock-clock-in (list marker start) nil start)
      (error "start is null."))))

;;; org-rl-clock.el ends here
