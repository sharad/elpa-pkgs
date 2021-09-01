;;; occ-util-method.el --- Occ util method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s


;; Author: s <sh4r4d@gmail.com>
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

(provide 'occ-util-method)




(defvar occ-list-select-keys occ-list-select-action-keys)
;; (setq occ-list-select-keys '(t actions normal))
;; (setq occ-list-select-keys occ-list-select-ap-transf-keys)

(cl-defmethod occ-match-select ((obj occ-obj-ctx)
                                &key
                                obtrusive
                                prompt)
  "Will open helm selection for tsk, here return-transform must
be NIL, using (occ-match-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters            (occ-match-filters))
        (builder            #'occ-build-ctxual-tsk-with)
        (ap-normal             (occ-build-ap-normal occ-list-select-keys))
        (ap-transf (occ-build-ap-transf occ-list-select-keys))
        (return-transform   nil)
        (timeout            occ-idle-timeout))
    (occ-select obj
                :filters            filters
                :builder            builder
                :ap-normal          ap-normal
                :ap-transf          ap-transf
                :return-transform   return-transform
                :timeout            timeout
                :obtrusive          obtrusive
                :prompt             prompt)))

(cl-defmethod occ-list-select ((obj occ-obj-ctx)
                               &key
                               ap-normal ;; TODO: -- newly added
                               obtrusive
                               prompt)
  "Will open helm selection for tsk, here return-transform must
be NIL, using (occ-list-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (ap-normal          (occ-build-ap-normal occ-list-select-keys))
        (ap-transf          (occ-build-ap-transf occ-list-select-keys))
        (return-transform   nil)
        (timeout            occ-idle-timeout))
      (occ-message "occ-list-select: action: %s" action)
      (occ-select obj                   ;; TODO: passing action has no affect it show its own debug it ?
                  :filters            filters
                  :builder            builder
                  :ap-normal          ap-normal
                  :ap-transf          ap-transf
                  :return-transform   return-transform
                  :timeout            timeout
                  :obtrusive          obtrusive
                  :prompt             prompt)))

(occ-testing
 (occ-list-debug-select (occ-make-ctx-at-point)
                        :action (occ-get-helm-actions nil '(t actions select))
                        :obtrusive nil))

(occ-testing
 (let ((obj (occ-make-ctx-at-point)))
   (occ-select obj
             :filters            (occ-list-filters)
             :builder            #'occ-build-ctsk-with
             :ap-normal          (occ-build-ap-normal occ-list-select-keys)
             :ap-transf          (occ-build-ap-transf occ-list-select-keys)
             :return-transform   nil
             :timeout            occ-idle-timeout
             :obtrusive         t)))

(cl-defmethod occ-list-debug-select ((obj occ-obj-ctx)
                                     &key
                                     action
                                     obtrusive
                                     prompt)
  "Will open helm selection for tsk, which then again run helm
selection for actions to run on selected tsk. It is mainly meant
for testing given action on selected tsk."

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (ap-normal          (occ-build-ap-normal ap-normal occ-list-select-keys))
        (ap-transf          (occ-build-ap-transf ap-transf occ-list-select-keys))
        (return-transform   t)
        (timeout            occ-idle-timeout))
      (occ-message "occ-list-debug-select: action: %s" action)
      (let ((retval-ctx-tsk (occ-select obj
                                        :filters            filters
                                        :builder            builder
                                        :return-transform   return-transform
                                        :ap-normal          ap-normal
                                        :ap-transf          ap-transf
                                        :timeout            timeout
                                        :obtrusive          obtrusive
                                        :prompt             prompt)))
        (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                          retval-ctx-tsk
                          (occ-format (occ-return-get-value retval-ctx-tsk)
                                      'capitalize)
                          (occ-return-get-label retval-ctx-tsk))
        (if (and (occ-return-in-labels-p retval-ctx-tsk
                                         occ-return-select-label)
                 (occ-return-get-value retval-ctx-tsk))
            (let ((ctsk     (occ-return-get-value retval-ctx-tsk))
                  (launcher (cdr (assoc (completing-read "Action: " action) action))))
              (funcall launcher ctsk))
          (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))

(occ-testing
 ;; (occ-get-helm-actions nil '(t actions select)) -> nil
 (occ-list-select (occ-make-ctx-at-point)
                  :action (occ-get-helm-actions nil '(t actions select))
                  :obtrusive nil))

(cl-defmethod occ-list-launch ((obj occ-obj-ctx)
                               &key
                               obtrusive
                               prompt)
  "TODO?: Will open helm selection for tsk, here return-transform
must be NIL, using (occ-list-filters) for FILTERS"

  ;; NOTE: AP-TRANSF is superseding AP-NORMAL

  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (return-transform   t)
        (ap-normal          (occ-build-ap-normal occ-list-select-keys))
        (ap-transf          (occ-build-ap-transf occ-list-select-keys))
        (timeout            occ-idle-timeout))
    (occ-message "occ-list-launch: action: %s" action)
    (let ((retval-ctx-tsk (occ-select obj
                                      :filters            filters
                                      :builder            builder
                                      :return-transform   return-transform
                                      :ap-normal          ap-normal
                                      :ap-transf          ap-transf
                                      :timeout            timeout
                                      :obtrusive          obtrusive
                                      :prompt             prompt)))
       (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                         retval-ctx-tsk
                         (occ-format (occ-return-get-value retval-ctx-tsk)
                                     'capitalize)
                         (occ-return-get-label retval-ctx-tsk))
       (if (and (occ-return-in-labels-p retval-ctx-tsk
                                        occ-return-select-label)
                (occ-return-get-value retval-ctx-tsk))
           (let* ((action      (occ-get-helm-actions obj
                                                     occ-list-select-keys))
                  (ctx-tsk     (occ-return-get-value retval-ctx-tsk))
                  (launcher    (cdr (assoc (completing-read "Action: " action)
                                           action))))
             (funcall launcher ctx-tsk))
         (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))


(cl-defmethod occ-list-select-interactive ())
(cl-defmethod occ-list-select-get-obj ())

;;; occ-util-method.el ends here
