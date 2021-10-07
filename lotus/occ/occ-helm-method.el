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


(eval-when-compile
  (require 'helm-source))


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


;; (fmakunbound 'occ-helm-null-candidate)

(cl-defmethod occ-helm-null-candidate ((obj occ-ctx))
  nil)

;;
;; https://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

;; function 'sacha/helm-org-create-tsk

;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source (prompt
                              action)
  (helm-build-dummy-source prompt
    :action (helm-make-actions prompt
                               action)))

(cl-defmethod occ-obj-helm-build-obj-source ((obj occ-obj-ctx)
                                             (collections list)
                                             &optional
                                             actions)
  (occ-helm-build-candidates :source (occ-obj-list-with obj
                                                        collections)
                             actions))


(cl-defmethod occ-obj-helm-fun-action-function-call-source ((prompt     string)
                                                            (candidates list))
  (helm-build-sync-source prompt
    :candidates candidates
    :action     (list (cons "Run action"
                            (occ-lambda-call-cand)))))

(cl-defmethod occ-obj-helm-build-dummy-source ((prompt string)
                                               (fun    compiled-function))
  (occ-helm-dummy-source prompt
                         fun))

(cl-defmethod occ-obj-helm-build-dummy-source ((prompt string)
                                               (fun    symbol))
  (let ((source (occ-helm-dummy-source prompt
                                       fun)))
    (occ-build-select-source source)))


(fmakunbound 'occ-helm-build-collection-source-prompt)

(cl-defmethod occ-helm-build-collection-source-prompt ((obj        occ-ctx)
                                                       (collection occ-obj-collection)
                                                       obj-class-name
                                                       unfiltered-count
                                                       filtered-count
                                                       &key
                                                       prompt)
  (let ((override        (and prompt
                              (consp prompt)
                              (eq :overrride
                                  (first prompt))))
        (prompt          (when (consp prompt)
                           (if (consp (rest prompt))
                               (nth 1 prompt)
                             (rest prompt))))
        (collection-name (occ-obj-collection-name collection))
        (collection-desc (occ-obj-collection-desc collection)))
    (if (and override
             prompt)
        prompt
      (format "Select matching %s(%d/%d) [%s]%s"
              obj-class-name
              unfiltered-count
              filtered-count
              (or collection-desc
                  collection-name)
              (format " %s" (or prompt ""))))))

(cl-defmethod occ-obj-helm-build-collection-source ((obj        occ-ctx)
                                                    (collection occ-obj-collection)
                                                    &key
                                                    filters
                                                    builder
                                                    ap-normal
                                                    ap-transf
                                                    auto-select-if-only
                                                    timeout
                                                    prompt)
  ;; (cl-assert candidates)
  (let* ((timeout               (or timeout occ-idle-timeout))
         (candidates-unfiltered (occ-obj-list-with obj collection :builder builder))
         (unfiltered-count      (length candidates-unfiltered))
         (candidates-filtered   (occ-obj-filter obj
                                                filters
                                                candidates-unfiltered))
         (filtered-count        (length candidates-filtered))
         candidates-new-unfiltered
         candidates-new-filtered
         (filtered-new-count 0)
         (called-never          t))

    (if (and auto-select-if-only
             (<= filtered-count 1))
        (occ-build-select-candidate (first candidates-filtered))
      (progn
        (occ-message "occ-obj-helm-build-collection-source: (length candidates-unfiltered) = %d, called-never = %s"
                     (length candidates-unfiltered)
                     called-never)
        (let ((gen-candidates #'(lambda ()
                                  (occ-message "occ-obj-helm-build-collection-source|lambda: (length candidates-unfiltered) = %d, called-never = %s, filters = %s"
                                               (length candidates-unfiltered)
                                               called-never
                                               filters)
                                  (let ((candidates-visible (if called-never
                                                                (progn
                                                                  (setq called-never nil)
                                                                  candidates-unfiltered)
                                                              (let* ((candidates-new-unfiltered (occ-obj-list-with obj collection
                                                                                                                   :builder builder))
                                                                     (candidates-new-filtered (occ-obj-filter obj
                                                                                                              filters
                                                                                                              candidates-new-unfiltered)))
                                                                (setq filtered-new-count (length candidates-new-filtered))
                                                                candidates-new-filtered))))
                                    (cl-assert candidates-visible)
                                    (mapcar #'occ-obj-candidate
                                            candidates-visible)))))

          (when (> filtered-count 0) ;; (> unfiltered-count 0)
            (let ((gen-candidate-lambda #'(lambda () (funcall gen-candidates)))
                  (source-name          (occ-helm-build-collection-source-prompt obj
                                                                                 collection
                                                                                 (symbol-name (occ-cl-inst-classname (first candidates-unfiltered)))
                                                                                 unfiltered-count
                                                                                 filtered-count
                                                                                 :prompt prompt)))
              (occ-debug "occ-obj-helm-build-collection-source: ap-normal: %s" ap-normal)
              (occ-debug "occ-obj-helm-build-collection-source: ap-transf: %s" ap-transf)
              (let ((helm-actions (occ-obj-ap-helm-item ap-normal obj))
                    (helm-transfm (occ-obj-ap-helm-item ap-transf obj)))
                (occ-debug "occ-obj-helm-build-collection-source: helm-actions: %s" helm-actions)
                (occ-debug "occ-obj-helm-build-collection-source: helm-transfm: %s" helm-transfm)
                (progn
                  (occ-debug "occ-obj-helm-build-collection-source: helm-actions:")
                  (dolist (a helm-actions)
                    (occ-debug " occ-obj-helm-build-collection-source: helm-action: %s" a))
                  (occ-debug "occ-obj-helm-build-collection-source: helm-transfm: %s" helm-transfm))
                (let ((source (helm-build-sync-source source-name
                                :candidates                     gen-candidate-lambda
                                ;; :header-name
                                :action                         helm-actions
                                :action-transformer             helm-transfm
                                :filtered-candidate-transformer nil
                                :history                        'org-refile-history)))
                  (occ-build-select-source source))))))))))


(defun occ-helm-build-extra-actions-ctx-buffer-source ()
  (unless (member (buffer-name (current-buffer))
                  occ-ignore-buffer-names)
    (let ((source (occ-obj-helm-fun-action-function-call-source "Other Actions"
                                                                (list (cons (format "Add current buffer %s to ignore list" (current-buffer))
                                                                            #'(lambda ()
                                                                                (let ((buff (buffer-name (current-buffer))))
                                                                                  (pushnew buff occ-ignore-buffer-names)
                                                                                  (occ-helm-null-candidate obj))))))))
      (occ-build-select-source source))))
    
(defun occ-helm-build-dummy-sources ()
  (list (occ-obj-helm-build-dummy-source "Create (fast as child)"             #'occ-do-fast-procreate-child)
        (occ-obj-helm-build-dummy-source "Create Anonymous (fast as unnamed)" #'occ-do-fast-procreate-anonymous-child)
        (occ-obj-helm-build-dummy-source "Create by Template (use template)"  #'occ-do-fast-procreate-child)))


(cl-defmethod occ-obj-helm-build-collections-sources ((obj         occ-ctx)
                                                      (collections list)
                                                      &key
                                                      filters
                                                      builder
                                                      ap-normal
                                                      ap-transf
                                                      auto-select-if-only
                                                      timeout
                                                      prompt)
  ;; (occ-debug "occ-obj-helm-build-collections-sources: ap-normal: %s" ap-normal)
  (mapcar (lambda (collection)
            (occ-obj-helm-build-collection-source obj
                                                  collection
                                                  :filters          filters
                                                  :builder          builder
                                                  :ap-normal        ap-normal
                                                  :ap-transf        ap-transf
                                                  :auto-select-if-only auto-select-if-only
                                                  :timeout          timeout
                                                  :prompt           prompt))
          collections))

(cl-defmethod occ-obj-helm-build-sources ((obj         occ-ctx)
                                          (collections list)
                                          &key
                                          filters
                                          builder
                                          ap-normal
                                          ap-transf
                                          auto-select-if-only
                                          timeout
                                          prompt)
  ;; (occ-debug "occ-obj-helm-build-collections-sources: ap-normal: %s" ap-normal)
  (let ((collection-sources (occ-obj-helm-build-collections-sources obj
                                                                    collections
                                                                    :filters          filters
                                                                    :builder          builder
                                                                    :ap-normal        ap-normal
                                                                    :ap-transf        ap-transf
                                                                    :auto-select-if-only auto-select-if-only
                                                                    :prompt           prompt)))
    (append collection-sources
            (occ-helm-build-dummy-sources)
            (list (occ-helm-build-extra-actions-ctx-buffer-source)))))



(cl-defmethod occ-obj-get-first-helm-actions-for-obj ((obj occ-obj)
                                                      (apn occ-ap-normal)
                                                      (apt null))
  (let ((act (first (occ-obj-ap-helm-get-actions obj
                                                 apn
                                                 apt))))
    (rest act)))

(cl-defmethod occ-obj-get-first-helm-actions-for-obj ((obj occ-obj)
                                                      (apn occ-ap-normal)
                                                      (apt occ-ap-transf))
  (let ((act (first (occ-obj-ap-helm-get-actions obj
                                                 apn
                                                 apt))))
    (rest act)))


(cl-defmethod occ-obj-helm-act-on-candidate ((obj    occ-ctx)
                                             (source occ-select-candidate)
                                             &key
                                             filters
                                             builder
                                             ap-normal
                                             ap-transf
                                             auto-select-if-only
                                             timeout
                                             prompt)
  ;; OBJ ignored
  "OBJ ignored"
  (let* ((helm-action (occ-obj-get-first-helm-actions-for-obj obj
                                                              ap-normal
                                                              ap-transf)))
    (occ-message "occ-obj-helm-act-on-single: (cons p helm-action) %s, (functionp helm-action) %s"
                 (consp helm-action)
                 (functionp helm-action))
    (occ-message "occ-obj-helm-act-on-single: (cons p (nth 1 helm-action)) %s, (functionp (nth 1 helm-action)) %s"
                 (consp (rest helm-action))
                 (functionp (rest helm-action)))
    (funcall helm-action (occ-sel-obj source))))

(cl-defmethod occ-obj-helm-act-on-multiple ((obj         occ-ctx)
                                            (collections list)
                                            &key
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
                                          (occ-debug "Running occ-list-select-internal helm is gone"))))
           ;; :keymap occ-helm-map
           ;; (occ-debug "occ-obj-helm-act-on-multiple: ap-normal: %s" ap-normal)
           (let ((cand-sources (occ-obj-helm-build-sources obj
                                                           collections
                                                           :filters          filters
                                                           :builder          builder
                                                           :ap-normal        ap-normal
                                                           :ap-transf        ap-transf
                                                           :auto-select-if-only auto-select-if-only
                                                           :prompt           prompt)))
             ;; (occ-message "Hello")
             (if (occ-select-candidate-p (first cand-sources))
                 (occ-obj-helm-act-on-candidate obj
                                                (first cand-sources)
                                                :filters          filters
                                                :builder          builder
                                                :ap-normal        ap-normal
                                                :ap-transf        ap-transf
                                                :auto-select-if-only auto-select-if-only
                                                :prompt           prompt)
               (when (occ-sel-obj (first cand-sources))
                 (prog1
                     (helm :sources (mapcar #'occ-sel-obj cand-sources)
                           :buffer  (occ-helm-select-buffer)
                           :resume  'noresume)
                   ;; (occ-message "Hi")
                   (setq in-occ-helm nil))))))))

(cl-defmethod occ-obj-helm-act ((obj         occ-ctx)
                                (collections list)
                                &key
                                filters
                                builder
                                ap-normal
                                ap-transf
                                return-transform
                                auto-select-if-only
                                timeout
                                prompt)
  (when collections
    (occ-debug "occ-obj-helm-act1: ap-normal: %s" ap-normal)
    (occ-debug "occ-obj-helm-act1: ap-transf: %s" ap-transf)
    (let* ((ap-normal (occ-obj-build-ap-normal ap-normal))
           (ap-transf (occ-obj-build-ap-transf ap-transf
                                               (occ-obj-ap-base ap-normal))))
      (occ-debug "occ-obj-helm-act2: ap-normal: %s" ap-normal)
      (occ-debug "occ-obj-helm-act2: ap-transf: %s" ap-transf)
      (let* ((ap-normal (if return-transform (occ-obj-return-tranform ap-normal obj) ap-normal)) ;as return value is going to be used.
             (ap-transf (if return-transform (occ-obj-return-tranform ap-transf obj) ap-transf)))
        (occ-obj-helm-act-on-multiple obj
                                      collections
                                      :filters             filters
                                      :builder             builder
                                      :ap-normal           ap-normal
                                      :ap-transf           ap-transf
                                      :auto-select-if-only auto-select-if-only
                                      :timeout             timeout
                                      :prompt              prompt)))))


(defun occ-helm-select-XYZ (obj
                            selector
                            action)
  ;; here
  ;; (occ-debug "sacha marker %s" (first ctxasks))
  (let (helm-sources)
    (push (occ-obj-helm-build-obj-source obj (list (cons "Clock in and track" selector)))
          helm-sources)
    (when (and (org-clocking-p)
               (marker-buffer org-clock-marker))
      (push (helm-build-sync-source "Current Clocking Tsk"
              :candidates (list (occ-obj-candidate (occ-obj-build-obj-with (occ-current-tsk)
                                                                           obj)))
              :action     (list (cons "Clock in and track" selector)))
            helm-sources))
    (funcall action (helm helm-sources))))


;; (cl-defgeneric occ-obj-sacha-helm-action (ctxask clockin-fn)
;;   "occ-obj-sacha-helm-action")

;; (cl-defmethod occ-obj-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
;;   ;; (occ-debug "sacha marker %s" (first dyntskpls))
;;   ;; (setq sacha/helm-org-refile-locations tbl)
;;   (progn
;;     (helm
;;      (list
;;       (helm-build-sync-source "Select matching tsks"
;;         :candidates (mapcar 'occ-obj-candidate ctxask)
;;         :action (list ;; (cons "Select" 'identity)
;;                  (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
;;         :history 'org-refile-history)))))
;; ;; (helm-build-dummy-source "Create tsk"
;; ;;   :action (helm-make-actions
;; ;;            "Create tsk"
;; ;;            'sacha/helm-org-create-tsk))

;;; occ-helm-method.el ends here
