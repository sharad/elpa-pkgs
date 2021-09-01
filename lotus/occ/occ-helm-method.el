;;; occ-helm-method.el --- occ helm method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

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

;;

;;; Code:

(provide 'occ-helm-method)


;; (defvar occ-helm-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map helm-map)
;;     ;; (define-key map (kbd "RET")           'helm-ff-RET)
;;     (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
;;     (define-key map (kbd "S-RET")         'occ-helm-run-child-clock-in)
;;     (helm-define-key-with-subkeys map
;;       '((kbd "DEL") ?\d 'helm-ff-delete-char-backward
;;         (C-backspace . helm-ff-run-toggle-auto-update)
;;         ([C-c DEL] . helm-ff-run-toggle-auto-update
;;          nil 'helm-ff-delete-char-backward--exit-fn)))
;;     (when helm-ff-lynx-style-map
;;       (define-key map (kbd "<left>")      'helm-find-files-up-one-level)
;;       (define-key map (kbd "<right>")     'helm-execute-persistent-action))
;;     (delq nil map))
;;   "Keymap for `helm-find-files'.")

(defvar occ-helm-doc-header " (\\<helm-find-files-map>\\[helm-find-files-up-one-level]: Go up one level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defun occ-helm-run-child-clock-in ()
  "Run mail attach files command action from `helm-source-find-files'."
  ;; (interactive)                         ;TODO: to move to occ-commands.el
  (with-helm-alive-p
    (helm-exit-and-execute-action 'occ-child-clock-in)))
(put 'occ-helm-run-child-clock-in 'helm-only t)
;; add occ-child-clock-in in action


;;
;; https://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

;; function 'sacha/helm-org-create-tsk

;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source (prompt action)
  (helm-build-dummy-source prompt
    :action (helm-make-actions prompt
                               action)))

(cl-defmethod occ-helm-build-obj-source ((obj occ-obj-ctx) &optional actions)
  (occ-helm-build-candidates   :source
                               (occ-list obj)
                               actions))


(defun occ-helm-build-candidate-source-prompt (prompt
                                               candidates
                                               unfiltered-count)
  (let ((override       (and prompt
                             (consp prompt)
                             (eq :overrride (first prompt))))
        (prompt         (when (consp prompt)
                          (if (consp (cdr prompt))
                              (cadr prompt)
                            (cdr prompt))))
        (filtered-count (length candidates)))
    (if (and override
             prompt)
        prompt
      (format "Select matching %s(%d/%d)%s"
              (symbol-name (cl-inst-classname (car candidates)))
              unfiltered-count
              filtered-count
              (format " %s" prompt)))))

(cl-defmethod occ-helm-build-candidates-source ((obj        occ-ctx)
                                                (candidates list)
                                                &key
                                                unfiltered-count
                                                filters
                                                builder
                                                ap-normal
                                                ap-transf
                                                auto-select-if-only
                                                timeout
                                                prompt)
  (let ((filtered-count (length candidates))
        (called-never   t))
     (let ((gen-candidates #'(lambda ()
                               (let ((candidates-visible (if called-never
                                                             (progn
                                                               (setq called-never nil)
                                                               candidates)
                                                           (let* ((candidates-unfiltered (occ-list obj :builder builder))
                                                                  (candidates-filtered   (occ-filter obj filters candidates-unfiltered)))
                                                             (setq filtered-count
                                                                   (length candidates-filtered))
                                                             candidates-filtered))))
                                 (mapcar #'occ-candidate candidates-visible)))))
                                         
       (when (> unfiltered-count 0)
         (let ((gen-candidate-lambda   #'(lambda () (funcall gen-candidates)))
               (source-name            (occ-helm-build-candidate-source-prompt prompt
                                                                               candidates
                                                                               unfiltered-count)))
           (occ-message "occ-helm-build-candidates-source: ap-normal: %s" ap-normal)
           (let* ((ap-normal (occ-build-ap-normal ap-normal))
                  (ap-transf (occ-build-ap-transf ap-transf ap-normal)))
             (let ((helm-actions (occ-obj-ap-helm-item ap-normal obj))
                   (helm-transfm (occ-obj-ap-helm-item ap-transf obj)))
              (helm-build-sync-source source-name
                                  :candidates gen-candidate-lambda
                                  ;; :header-name
                                  :ap-normal helm-actions
                                  :ap-transf helm-transfm
                                  :filtered-candidate-transformer nil
                                  :history   'org-refile-history))))))))

(cl-defmethod occ-helm-build-candidates-sources ((obj        occ-ctx)
                                                 (candidates list)
                                                 &key
                                                 unfiltered-count
                                                 filters
                                                 builder
                                                 ap-normal
                                                 ap-transf
                                                 auto-select-if-only
                                                 timeout
                                                 prompt)
  (occ-message "occ-helm-build-candidates-sources: action: %s" action)
  (list (occ-helm-build-candidates-source obj
                                          candidates
                                          :unfiltered-count unfiltered-count
                                          :filters          filters
                                          :builder          builder
                                          :ap-normal        ap-normal
                                          :ap-transf        ap-transf
                                          :prompt           prompt)
        (occ-helm-dummy-source "Create (fast as child)"             #'occ-fast-procreate-child)
        (occ-helm-dummy-source "Create Anonymous (fast as unnamed)" #'occ-fast-procreate-anonymous-child)
        (occ-helm-dummy-source "Create by Template (use template)"  #'occ-procreate-child-clock-in)))



(cl-defmethod occ-get-first-helm-actions-for-obj ((obj occ-obj)
                                                  (apn occ-ap-normal)
                                                  (apt null))
  (let ((act (first (occ-obj-ap-helm-get-actions obj
                                                 apn
                                                 apt))))
    act))

(cl-defmethod occ-get-first-helm-actions-for-obj ((obj occ-obj)
                                                  (apn occ-ap-normal)
                                                  (apt occ-ap-transf))
  (let ((act (first (occ-obj-ap-helm-get-actions obj
                                                 apn
                                                 apt))))
    act))

;; (cl-defmethod occ-helm-act-on-single ((obj                 occ-ctx)
;;                                       (candidates-filtered list)
;;                                       &key
;;                                       unfiltered-count
;;                                       filters
;;                                       builder
;;                                       ap-normal
;;                                       ap-transf
;;                                       auto-select-if-only
;;                                       timeout
;;                                       prompt)
;;   ;; OBJ ignored
;;   "OBJ ignored"

;;   (let* ((candidate   (car candidates-filtered)))

;;     (let* ((helm-actions                 (occ-obj-ap-helm-item ap-normal candidate))
;;            (helm-transfm                 (occ-obj-ap-helm-item ap-transf candiate))
;;            (helm-first-action-via-normal (first helm-actions)))

;;       (let* ((helm-actions-via-transf      (funall helm-transfm helm-actions candidate))
;;              (helm-first-action-via-transf (first helm-actions-via-transf)))

;;         (let ((helm-action-to-call (or (cdr-safe helm-first-action-via-transf) helm-first-action-via-transf
;;                                        (cdr-safe helm-first-action-via-normal) helm-first-action-via-normal)))

;;           (funcall helm-action-to-call candidate))))))

(cl-defmethod occ-helm-act-on-single ((obj                 occ-ctx)
                                      (candidates-filtered list)
                                      &key
                                      unfiltered-count
                                      filters
                                      builder
                                      ap-normal
                                      ap-transf
                                      auto-select-if-only
                                      timeout
                                      prompt)
  ;; OBJ ignored
  "OBJ ignored"
  (let* ((ap-normal (occ-build-ap-normal ap-normal))
         (ap-transf (occ-build-ap-transf ap-transf ap-normal)))
    (let* ((candidate   (car candidates-filtered))
           (helm-action (occ-get-first-helm-actions-for-obj obj
                                                            ap-normal
                                                            ap-transf)))
      (funcall helm-action candidate))))

(cl-defmethod occ-helm-act-on-multiple ((obj        occ-ctx)
                                        (candidates-filtered list)
                                        &key
                                        unfiltered-count
                                        filters
                                        builder
                                        ap-normal
                                        ap-transf
                                        auto-select-if-only
                                        timeout
                                        prompt)
    (let ((in-occ-helm t))
         (progn
           (run-with-timer 0.08 nil #'(lambda ()
                                        (if in-occ-helm
                                            (helm-refresh)
                                          (occ-debug :debug "Running occ-list-select-internal helm is gone"))))
           ;; :keymap occ-helm-map
           (occ-message "occ-list-select-internal: helm-action: %s" helm-action)
           (let ((candidates-sources (occ-helm-build-candidates-sources obj
                                                                        candidates-filtered
                                                                        :unfiltered-count unfiltered-count
                                                                        :filters          filters
                                                                        :builder          builder
                                                                        :ap-normal        ap-normal
                                                                        :ap-transf        ap-transf
                                                                        :prompt           prompt)))
             (prog1
                 (helm :sources candidates-sources
                       :buffer  (occ-helm-select-buffer)
                       :resume  'noresume)
               (setq in-occ-helm nil))))))

(cl-defmethod occ-helm-act ((obj                 occ-ctx)
                            (candidates-filtered list)
                            &key
                            unfiltered-count
                            filters
                            builder
                            ap-normal
                            ap-transf
                            return-transform
                            auto-select-if-only
                            timeout
                            prompt)
  (when candidates-filtered
    (let* ((ap-normal (if return-transform (occ-return-tranform ap-normal) ap-normal)) ;as return value is going to be used.
           (ap-transf (if return-transform (occ-return-tranform ap-transf) ap-transf)))
      (let ((fun (if (and auto-select-if-only
                          (= 1 (length candidates-filtered)))
                     #'occ-helm-act-on-single
                   #'occ-helm-act-on-multiple)))
        (funcall fun obj
                 candidate-filtered
                 :unfiltered-count unfiltered-count
                 :filters   filters
                 :builder   builder
                 :ap-normal ap-normal
                 :ap-transf ap-transf
                 :auto-select-if-only auto-select-if-only
                 :timeout timeout
                 :prompt prompt)))))


(defun occ-helm-select-XYZ (obj
                            selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
  (let (helm-sources)
    (push (occ-helm-build-obj-source obj (list (cons "Clock in and track" selector)))
          helm-sources)

    (when (and (org-clocking-p)
               (marker-buffer org-clock-marker))
      (push (helm-build-sync-source "Current Clocking Tsk"
              :candidates (list (occ-candidate (occ-build-obj-with (occ-current-tsk)
                                                                   obj)))
              :action (list (cons "Clock in and track"
                                  selector)))
            helm-sources))
    (funcall action (helm helm-sources))))


;; (cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
;;   "occ-sacha-helm-action")

;; (cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
;;   ;; (occ-message "sacha marker %s" (car dyntskpls))
;;   ;; (setq sacha/helm-org-refile-locations tbl)
;;   (progn
;;     (helm
;;      (list
;;       (helm-build-sync-source "Select matching tsks"
;;         :candidates (mapcar 'occ-candidate ctxask)
;;         :action (list ;; (cons "Select" 'identity)
;;                  (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
;;         :history 'org-refile-history)))))
;; ;; (helm-build-dummy-source "Create tsk"
;; ;;   :action (helm-make-actions
;; ;;            "Create tsk"
;; ;;            'sacha/helm-org-create-tsk))

;;; occ-helm-method.el ends here
