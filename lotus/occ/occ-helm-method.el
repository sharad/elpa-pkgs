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
(require 'occ-assert)


(defvar occ-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "RET")           'helm-ff-RET)
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "S-RET")         'occ-helm-run-child-clock-in)
    ;; (helm-define-key-with-subkeys map
    ;;   '((kbd "DEL") ?\d 'helm-ff-delete-char-backward
    ;;     (C-backspace . helm-ff-run-toggle-auto-update)
    ;;     ([C-c DEL] . helm-ff-run-toggle-auto-update)
    ;;     nil 'helm-ff-delete-char-backward--exit-fn))
    (when t ;; helm-ff-lynx-style-map
      (define-key map (kbd "<left>")      'helm-find-files-up-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action))
    (delq nil map))
  "Keymap for `helm-find-files'.")

(defvar occ-helm-doc-header " (\\<helm-find-files-map>\\[helm-find-files-up-one-level]: Go up one level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defun occ-helm-run-child-clock-in ()
  "Run mail attach files command action from `helm-source-find-files'."
  ;; (interactive)                         ;TODO: to move to occ-commands.el
  (with-helm-alive-p
    (helm-exit-and-execute-action 'occ-child-clock-in)))
(put 'occ-helm-run-child-clock-in 'helm-only t)
;; add occ-child-clock-in in action


(defvar occ-helm-before-init-hook nil)
(defvar occ-helm-after-init-hook nil)
(defun occ-helm-cleanup ())
;; checkout (defclass helm-source-ffiles (helm-source-sync) - in helm-files.el
;; C-h C-f helm-source-ffiles
;; C-h C-f helm-source
(defclass occ-helm-source-sync (helm-source-sync)
  ((header-name
    :initform (lambda (name)
                (concat name (substitute-command-keys
                              occ-helm-doc-header))))
   (init
    :initform (lambda ()
                (setq helm-ff-auto-update-flag
                      helm-ff-auto-update-initial-value)
                (setq helm-ff--auto-update-state
                      helm-ff-auto-update-flag)
                (helm-set-local-variable 'bookmark-make-record-function
                                         #'helm-ff-make-bookmark-record)
                (require 'helm-external)))
   (candidates :initform 'helm-find-files-get-candidates)
   (update :initform (lambda ()))
                       
   (match-on-real :initform nil)
   (filtered-candidate-transformer
    :initform '(helm-ff-fct
                helm-ff-maybe-show-thumbnails
                ;; These next two have to be called after
                ;; `helm-ff-fct' as they use only cons cell candidates.
                helm-ff-directories-only
                helm-ff-files-only
                helm-ff-sort-candidates))
   (persistent-action-if :initform 'helm-find-files-persistent-action-if)
   (persistent-help :initform "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
   (help-message :initform 'helm-ff-help-message)
   (mode-line :initform (list "Occ" helm-mode-line-string))
   (volatile :initform t)
   (cleanup :initform 'occ-helm-cleanup)
   (migemo :initform t)
   (nohighlight :initform t)
   (keymap :initform 'occ-helm-map)
   ;; (candidate-number-limit :initform 'helm-ff-candidate-number-limit)
   ;; (action-transformer
   ;;  :initform 'helm-find-files-action-transformer)
   ;; (action :initform 'helm-find-files-actions)
   (before-init-hook :initform 'occ-helm-before-init-hook)
   (after-init-hook :initform 'occ-helm-after-init-hook)
   (group :initform 'occ-helm)))

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
    (occ-build-hsrc-source source)))


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


;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

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
  ;; (occ-assert candidates)
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
        (occ-build-hsrc-candidate (first candidates-filtered))
      (progn
        (occ-debug "occ-obj-helm-build-collection-source: (length candidates-unfiltered) = %d, called-never = %s"
                     (length candidates-unfiltered)
                     called-never)
        (let* ((default-filters filters)
               (filters         filters)
               (gen-candidates #'(lambda ()
                                   (occ-debug "occ-obj-helm-build-collection-source|lambda: (length candidates-unfiltered) = %d, called-never = %s, filters = %s"
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
                                     (occ-assert candidates-visible)
                                     (mapcar #'occ-obj-candidate
                                             candidates-visible))))
               (filter-manage-fn  #'(lambda ()
                                      (interactive)
                                      (with-helm-buffer
                                        (progn ;; code to manager filters
                                          ;; TODO: check https://github.com/emacsmirror/edit-list/blob/master/edit-list.el
                                          ;; TODO: implement list editor
                                          ;; TODO: search emacs elisp interactively modify list
                                          (occ-message "Manage filters here.")
                                          (setf filters default-filters)))
                                      ;; (funcall gen-candidates)
                                      (helm-refresh)))
               (filter-reset-fn  #'(lambda ()
                                    (interactive)
                                    (setf filters default-filters)
                                    ;; (funcall gen-candidates)
                                    (helm-refresh)))
               (filter-inc-fn    #'(lambda ()
                                     (interactive)
                                     ;; (setf level (1+ level))
                                     (setf filters default-filters)
                                     ;; (funcall gen-candidates)
                                     (helm-refresh)))
               (filter-dec-fn    #'(lambda ()
                                     (interactive)
                                     ;; (setf level (1- level))
                                     (setf filters default-filters)
                                     ;; (funcall gen-candidates)
                                     (helm-refresh)))
               (h-map
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map occ-helm-map)
                  (define-key map (kbd "M-<up>")     filter-inc-fn)
                  (define-key map (kbd "M-<down>")   filter-dec-fn)
                  (define-key map (kbd "M-<space>")  filter-reset-fn)
                  (define-key map (kbd "M-<return>") filter-manage-fn)
                  map)))

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
                (occ-debug "occ-obj-helm-build-collection-source: helm-transfm: %s" helm-transfm)
                (progn
                  (occ-debug "occ-obj-helm-build-collection-source: helm-actions:")
                  (dolist (a helm-actions)
                    (occ-debug " occ-obj-helm-build-collection-source: helm-action: %s" a))
                  (occ-debug "occ-obj-helm-build-collection-source: helm-transfm: %s" helm-transfm))

                ;; (let ((source (helm-build-sync-source source-name
                ;;                 :candidates                     gen-candidate-lambda
                ;;                 ;; :header-name
                ;;                 :keymap                         h-map
                ;;                 :action                         helm-actions
                ;;                 :action-transformer             helm-transfm
                ;;                 :filtered-candidate-transformer nil ;; (lambda (candidates source) candidates)
                ;;                 :history                        'org-refile-history)))
                ;;   (occ-build-hsrc-source source))
                ;; * Dynamic Match based templates
                ;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/
                (let ((source (helm-make-source source-name 'occ-helm-source-sync ;; 'helm-source-sync
                                :candidates                     gen-candidate-lambda
                                ;; :header-name
                                :keymap                         h-map
                                :action                         helm-actions
                                :persistent-action              helm-actions
                                :persistent-help                "I don't want this line here"
                                :action-transformer             helm-transfm
                                :filtered-candidate-transformer nil ;; (lambda (candidates source) candidates)
                                :history                        'org-refile-history)))
                  (occ-build-hsrc-source source))))))))))


(defun occ-helm-build-extra-actions-ctx-buffer-source ()
  (unless (member (buffer-name (current-buffer))
                  occ-ignore-buffer-names)
    (let ((source (occ-obj-helm-fun-action-function-call-source "Other Actions"
                                                                (list (cons (format "Add current buffer %s to ignore list" (current-buffer))
                                                                            #'(lambda ()
                                                                                (let ((buff (buffer-name (current-buffer))))
                                                                                  (pushnew buff occ-ignore-buffer-names)
                                                                                  (occ-helm-null-candidate obj))))))))
      (occ-build-hsrc-source source))))

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
                                             (source occ-hsrc-candidate)
                                             &key
                                             filters
                                             builder
                                             ap-normal
                                             ap-transf
                                             auto-select-if-only
                                             timeout
                                             prompt)
  "OBJ ignored"
  (let* ((helm-action (occ-obj-get-first-helm-actions-for-obj obj
                                                              ap-normal
                                                              ap-transf)))
    (occ-debug "occ-obj-helm-act-on-single: (cons p helm-action) %s, (functionp helm-action) %s"
                 (consp helm-action)
                 (functionp helm-action))
    (occ-debug "occ-obj-helm-act-on-single: (cons p (nth 1 helm-action)) %s, (functionp (nth 1 helm-action)) %s"
                 (consp (rest helm-action))
                 (functionp (rest helm-action)))
    (if (occ-obj-obj source)
        (funcall helm-action (occ-obj-obj source))
      (occ-warn "occ-obj-helm-act-on-candidate: wrong source"))))

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
  (let ((cand-sources (occ-obj-helm-build-sources obj
                                                  collections
                                                  :filters          filters
                                                  :builder          builder
                                                  :ap-normal        ap-normal
                                                  :ap-transf        ap-transf
                                                  :auto-select-if-only auto-select-if-only
                                                  :prompt           prompt)))
    
    (if (occ-hsrc-candidate-p (first cand-sources))
        ;; Mean if first cand-sources has only one element then it will pack
        ;; that element using `occ-build-hsrc-source' to be acted by default
        ;; action.
        (occ-obj-helm-act-on-candidate obj
                                       (first cand-sources)
                                       :filters          filters
                                       :builder          builder
                                       :ap-normal        ap-normal
                                       :ap-transf        ap-transf
                                       :auto-select-if-only auto-select-if-only
                                       :prompt           prompt)
      ;; (occ-assert (first cand-sources)) -- can happen when no contxtual match found

      ;; Else all source will be passed to helm to be shown.
      (let* ((in-occ-helm t)
             (timer (run-with-timer 0.08 nil #'(lambda ()
                                                 (if in-occ-helm
                                                     (helm-refresh)
                                                   (occ-debug "Running occ-list-select-internal helm is gone"))))))
        (unwind-protect
            (when (occ-obj-obj (first cand-sources))
              (helm :sources (mapcar #'occ-obj-obj cand-sources)
                    :buffer  (occ-helm-select-buffer)
                    :resume  'noresume))
          (progn
            (setq in-occ-helm nil)
            (cancel-timer timer)))))))
        
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

;;; occ-helm-method.el ends here
