;;; occ-select.el --- selection                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

;; Main machinery
;; TODO: Document it well

;;; Code:

(provide 'occ-select)


(require 'org-misc-utils-lotus)


(require 'occ-list-filter)
(require 'occ-helm)


(defvar occ-list-select-ap-normal-keys '(t ap-normals general))
(defvar occ-list-select-ap-transf-keys '(t actions general edit))

;; TODO:
;; (helm-resume-select-buffer)
;; helm-buffers
;; helm-buffer

(defvar occ-helm-select-buffer-name "*helm occ select")
(defun occ-helm-select-buffer ()
  occ-helm-select-buffer-name)

(cl-defmethod occ-list-select-internal ((obj occ-ctx)
                                        &key
                                        filters
                                        builder
                                        ap-normal
                                        ap-transf
                                        auto-select-if-only
                                        timeout
                                        obtrusive
                                        prompt)
  "Main Machinery, TODO: Document it, NOTE: ACTION-TRANSFORMER is
superseding ACTION, As in helm ACTION-TRANSFORMER are superseding
ACTION "
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))


  ;; (lotus-with-no-active-minibuffer-if <- TODO: This should be there only for first level command, not in internal function
  ;;                                              as it may be creating problem of occ-capture


  ;; NOTE: ACTION-TRANSFORMER is superseding ACTION

  (progn ;; lotus-with-no-active-minibuffer-if
    (occ-debug :debug "Running occ-list-select-internal")
    (occ-debug :debug "occ-list-select-internal: [minibuffer-body] lotus-with-no-active-minibuffer-if")
    (occ-debug :debug "occ-list-select-internal: minibuffer already active quitting")
    (occ-debug :debug nil)
    (prog1
        (let ((ap-normal (occ-build-ap-normal ap-normal occ-list-select-ap-normal-keys))
              (ap-transf (occ-build-ap-transf ap-transf occ-list-select-ap-transf-keys))
              (timeout   (or timeout occ-idle-timeout)))

          (let* ((candidates-unfiltered (occ-list obj
                                                  :builder   builder
                                                  :obtrusive obtrusive))
                 (unfiltered-count      (length candidates-unfiltered))
                 (candidates-filtered   (occ-filter obj
                                                    filters
                                                    candidates-unfiltered)))
            (occ-helm-act obj
                          candidate-filtered
                          :unfiltered-count unfiltered-count
                          :filters filters
                          :builder builder
                          :ap-normal ap-normal
                          :ap-transf ap-transf
                          :auto-select-if-only auto-select-if-only
                          :timeout timeout
                          :prompt prompt)))
      (occ-debug :debug "Running occ-list-select-internal"))))

(cl-defmethod occ-list-select ((obj occ-ctx)
                               &key
                               filters
                               builder
                               ap-normal
                               ap-transf
                               return-transform
                               auto-select-if-only
                               timeout
                               obtrusive
                               prompt)
  "TODO: Document it, Note: RETURN-TRANSFORM palying its game here."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (let ((ap-normal (occ-build-ap-normal ap-normal occ-list-select-ap-normal-keys)) ;NOTE: Adding newly
        (ap-transf (occ-build-ap-transf ap-transf occ-list-select-ap-transf-keys))
        (timeout   (or timeout occ-idle-timeout)))
    (helm-timed timeout (occ-helm-select-buffer)
      (occ-debug :debug "running occ-list-select")
      (let ((ap-normal (if return-transform (occ-return-tranform ap-normal) ap-normal)) ;as return value is going to be used.
            (ap-transf (if return-transform (occ-return-tranform ap-transf) ap-transf)))
        (occ-message "occ-list-select: ap-normal: %s" ap-normal)
        (let ((selected (occ-list-select-internal obj
                                                  :filters             filters
                                                  :builder             builder
                                                  :ap-normal           ap-normal
                                                  :ap-transf           ap-transf
                                                  :auto-select-if-only auto-select-if-only
                                                  :timeout             timeout
                                                  :obtrusive           obtrusive
                                                  :prompt              prompt)))
          (occ-debug :debug "occ-list-select: selected = %s" selected)
          (if return-transform
              (or selected ;as return value is going to be used.
                  (occ-make-return occ-return-quit-label selected))
            selected))))))


;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (occ-error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (occ-error "Window #<window 12> too small for splitting")

(cl-defmethod occ-select ((obj occ-ctx)
                          &key
                          filters
                          builder
                          ap-normal
                          ap-transf
                          return-transform
                          auto-select-if-only
                          timeout
                          obtrusive
                          prompt)
  "return interactively selected TSK or NIL,   TODO: Document it."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (unless builder (occ-error "Builder can not be nil"))
  (occ-debug :debug "occ-select((obj occ-ctx)): begin")
  (let ((ap-normal (occ-build-ap-normal ap-normal occ-list-select-ap-normal-keys)) ;NOTE: Adding newly
        (ap-transf (occ-build-ap-transf ap-transf occ-list-select-ap-transf-keys))
        (timeout            (or timeout occ-idle-timeout)))
    (let* ((unfiltered-count (occ-length)))
      (if (> unfiltered-count 0)
          (let ((retval (occ-list-select obj
                                         :filters             filters
                                         :builder             builder
                                         :return-transform    return-transform
                                         :ap-normal           ap-normal
                                         :ap-transf           ap-transf
                                         :auto-select-if-only auto-select-if-only
                                         :timeout             timeout
                                         :obtrusive           obtrusive
                                         :prompt              prompt)))
            (occ-debug :debug "occ-select((obj occ-ctx)): occ-list-select returned %s"
                       (occ-format retval 'capitalize))
            retval)
        (prog1
            (when return-transform
              (occ-make-return occ-return-nocndidate-label nil))
          (occ-message "occ-select((obj occ-ctx)): no candidates available from %d."
                       unfiltered-count))))))

(cl-defmethod occ-select ((obj null)
                          &key
                          filters
                          builder
                          return-transform
                          ap-normal
                          ap-transf
                          auto-select-if-only
                          timeout
                          obtrusive
                          prompt)
  "TODO: Document it."
  ;; NOTE: AP-TRANSF is superseding AP-NORMAL
  (occ-debug :debug "occ-select((obj null)): begin")
  (let ((retval (occ-select (occ-make-ctx-at-point)
                            :filters             filters
                            :builder             builder
                            :return-transform    return-transform
                            :ap-normal           ap-normal
                            :ap-transf           ap-transf
                            :auto-select-if-only auto-select-if-only
                            :timeout             timeout
                            :obtrusive           obtrusive
                            :prompt              prompt)))
    (occ-debug :debug "occ-select((obj null)): occ-select((obj occ-ctx)) returned %s"
                      (occ-format retval 'capitalize))
    retval))


(occ-testing
 (let ((obj                    (occ-make-ctx-at-point))
       (occ-list-select-keys-1 '(t actions general))
       (occ-list-select-keys-2 '(t actions select)))
   (occ-select obj
               :filters            (occ-list-filters)
               :builder            #'occ-build-ctsk-with
               :action             (occ-get-helm-actions obj
                                                         occ-list-select-keys-1)
               :action-transformer #'(lambda (action candidate)
                                       (occ-get-helm-actions obj
                                                             occ-list-select-keys-2))
               :return-transform   nil
               :timeout            occ-idle-timeout
               :obtrusive         t)))

;;; occ-select.el ends here
