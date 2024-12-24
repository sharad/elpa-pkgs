;;; rujelly-theme.el --- Reverse Ujelly theme for GNU Emacs 24 (deftheme)

;; Author: Mark Tran <mark.tran@gmail.com> (Original theme)
;; Reverse Theme by: (Your Name)
;; Version: 1.2.9

(deftheme rujelly "The reverse ujelly color theme")

(let ((class '((class color) (min-colors 89)))
      (rujelly-fg "#000000") ;; swapped from #ffffff
      (rujelly-bg (if (display-graphic-p) "#ffffff" nil)) ;; swapped from #000000
      (rujelly-blue-0 "#70403d") ;; inverted #8fbfdc
      (rujelly-green-0 "#665299") ;; inverted #99ad6a
      (rujelly-green-1 "#bb8877") ;; inverted #447799
      (rujelly-green-2 "#573ff9") ;; inverted #a8ff60
      (rujelly-green-3 "#8a4a55") ;; inverted #75b5aa
      (rujelly-grey-0 "#777777") ;; inverted #888888
      (rujelly-grey-1 "#808080") ;; inverted #7f7f7f
      (rujelly-grey-2 "#eaeaea") ;; inverted #151515
      (rujelly-grey-3 "#e3e3e3") ;; inverted #1c1c1c
      (rujelly-grey-4 "#c9c9c9") ;; inverted #363636
      (rujelly-grey-5 "#bbbbbb") ;; inverted #444444
      (rujelly-orange-0 "#00469b") ;; inverted #ffb964
      (rujelly-orange-1 "#0b409e") ;; inverted #f4bf75
      (rujelly-purple-0 "#7e68b0") ;; inverted #8197bf
      (rujelly-purple-1 "#d8d1ef") ;; inverted #474e90
      (rujelly-purple-2 "#32ff32") ;; inverted #cd00cd
      (rujelly-purple-3 "#55ff60") ;; inverted #aa759f
      (rujelly-red-0 "#3095b3") ;; inverted #cf6a4c
      (rujelly-red-1 "#220c6c") ;; inverted #dd0093
      (rujelly-red-2 "#21aad8") ;; inverted #de5577
      (rujelly-red-3 "#00fcfd") ;; inverted #ff73fd
      (rujelly-yellow-0 "#0516d8") ;; inverted #fad07a
      (rujelly-yellow-1 "#0000ff")) ;; inverted #ffff00



  (custom-theme-set-faces
   'rujelly
   `(default ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(alchemist-test--failed-face ((,class (:foreground ,rujelly-red-0))))
   `(alchemist-test--success-face ((,class (:foreground ,rujelly-green-0))))
   `(avy-lead-face ((,class (:foreground ,rujelly-fg :background ,rujelly-red-0))))
   `(avy-lead-face-0 ((,class (:foreground ,rujelly-fg :background ,rujelly-green-0))))
   `(bm-face ((,class (:background ,rujelly-grey-4))))
   `(company-preview-common ((,class (:foreground nil :background ,rujelly-purple-1))))
   `(company-scrollbar-bg ((,class (:background ,rujelly-grey-2))))
   `(company-scrollbar-fg ((,class (:background ,rujelly-grey-0))))
   `(company-tooltip ((,class (:foreground ,rujelly-fg :background ,rujelly-grey-2))))
   `(company-tooltip-common ((,class (:foreground ,rujelly-grey-1 :background ,rujelly-grey-2))))
   `(company-tooltip-common-selection ((,class (:foreground ,rujelly-grey-1 :background ,rujelly-purple-1))))
   `(company-tooltip-selection ((,class (:background ,rujelly-purple-1))))
   `(compilation-error ((,class (:foreground ,rujelly-red-0))))
   `(compilation-info ((,class (:foreground ,rujelly-yellow-0))))
   `(compilation-line-number ((,class (:foreground ,rujelly-grey-0))))
   `(compilation-mode-line-exit ((,class (:foreground ,rujelly-green-0))))
   `(compilation-mode-line-fail ((,class (:foreground ,rujelly-red-0))))
   `(compilation-mode-line-run ((,class (:foreground ,rujelly-yellow-0))))
   `(diredp-date-time ((,class (:foreground ,rujelly-fg))))
   `(diredp-deletion ((,class (:foreground ,rujelly-red-0 :background ,rujelly-bg))))
   `(diredp-dir-heading ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-bg))))
   `(diredp-dir-name ((,class (:foreground ,rujelly-green-2 :background ,rujelly-bg))))
   `(diredp-dir-priv ((,class (:foreground ,rujelly-green-2 :background ,rujelly-bg))))
   `(diredp-exec-priv ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(diredp-file-name ((,class (:foreground ,rujelly-fg))))
   `(diredp-file-suffix ((,class (:foreground ,rujelly-fg))))
   `(diredp-link-priv ((,class (:foreground ,rujelly-fg))))
   `(diredp-number ((,class (:foreground ,rujelly-fg))))
   `(diredp-no-priv ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(diredp-rare-priv ((,class (:foreground ,rujelly-red-0 :background ,rujelly-bg))))
   `(diredp-read-priv ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(diredp-symlink ((,class (:foreground ,rujelly-red-3))))
   `(diredp-write-priv ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(ediff-odd-diff-A ((,class (:foreground ,rujelly-red-0 :background ,rujelly-grey-2))))
   `(ediff-odd-diff-B ((,class (:foreground ,rujelly-green-0 :background ,rujelly-grey-2))))
   `(emmet-preview-output ((,class (:background ,rujelly-purple-1))))
   `(elixir-atom-face ((,class (:foreground ,rujelly-blue-0))))
   `(elixir-attribute-face ((,class (:foreground ,rujelly-red-0))))
   `(enh-ruby-op-face ((,class (:foreground ,rujelly-purple-0))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,rujelly-purple-3))))
   `(erc-notice-face ((,class (:foreground ,rujelly-yellow-0))))
   `(erc-prompt-face ((,class (:foreground ,rujelly-fg))))
   `(erc-timestamp-face ((,class (:foreground ,rujelly-purple-0))))
   `(eshell-prompt ((,class (:foreground ,rujelly-red-0))))
   `(eshell-ls-directory ((,class (:weight normal :foreground ,rujelly-green-2))))
   `(eshell-ls-executable ((,class (:weight normal :foreground ,rujelly-red-0))))
   `(eshell-ls-product ((,class (:foreground ,rujelly-fg))))
   `(eshell-ls-symlink ((,class (:weight normal :foreground ,rujelly-purple-2))))
   `(flycheck-error ((,class (:background ,rujelly-grey-4))))
   `(flycheck-error-list-column-number ((,class (:foreground ,rujelly-fg))))
   `(flycheck-error-list-line-number ((,class (:foreground ,rujelly-fg))))
   `(flycheck-error-list-id ((,class (:foreground ,rujelly-fg))))
   `(flycheck-error-list-info ((,class (:foreground ,rujelly-yellow-0))))
   `(flycheck-info ((,class (:background ,rujelly-grey-4))))
   `(flycheck-warning ((,class (:background ,rujelly-grey-4))))
   `(font-lock-builtin-face ((,class (:foreground ,rujelly-blue-0))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,rujelly-grey-0))))
   `(font-lock-constant-face ((,class (:foreground ,rujelly-green-1))))
   `(font-lock-doc-face ((,class (:foreground ,rujelly-green-0))))
   `(font-lock-function-name-face ((,class (:foreground ,rujelly-yellow-0))))
   `(font-lock-keyword-face ((,class (:foreground ,rujelly-purple-0))))
   `(font-lock-preprocessor-face ((,class (:foreground ,rujelly-fg))))
   `(font-lock-string-face ((,class (:foreground ,rujelly-green-0))))
   `(font-lock-type-face ((,class (:foreground ,rujelly-orange-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,rujelly-red-0))))
   `(font-lock-warning-face ((,class (:foreground ,rujelly-red-1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,rujelly-yellow-0 :bold t))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,rujelly-red-0 :bold t))))
   `(fringe ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(git-commit-comment-file ((,class (:foreground ,rujelly-fg))))
   `(git-commit-comment-heading ((,class (:foreground ,rujelly-yellow-0))))
   `(git-commit-summary ((,class (:foreground ,rujelly-fg))))
   `(header-line ((,class (:foreground ,rujelly-fg))))
   `(helm-buffer-file ((,class (:foreground ,rujelly-fg))))
   `(helm-buffer-process ((,class (:foreground ,rujelly-yellow-0))))
   `(helm-buffer-size ((,class (:foreground ,rujelly-fg))))
   `(helm-candidate-number ((,class (:foreground ,rujelly-fg :background ,rujelly-bg))))
   `(helm-ff-directory ((,class (:foreground ,rujelly-green-2))))
   `(helm-ff-dotted-directory ((,class (:foreground ,rujelly-green-2 :background ,rujelly-bg))))
   `(helm-ff-dotted-symlink-directory ((,class (:foreground ,rujelly-green-2 :background ,rujelly-bg))))
   `(helm-ff-executable ((,class (:foreground ,rujelly-red-0))))
   `(helm-ff-file ((,class (:foreground ,rujelly-fg))))
   `(helm-ff-symlink ((,class (:foreground ,rujelly-purple-2))))
   `(helm-grep-lineno ((,class (:foreground ,rujelly-fg))))
   `(helm-match ((,class (:foreground ,rujelly-red-1 :background ,rujelly-bg))))
   `(helm-moccur-buffer ((,class (:foreground ,rujelly-yellow-0))))
   `(helm-selection ((,class (:background ,rujelly-purple-1))))
   `(helm-source-header ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-grey-3))))
   `(helm-swoop-target-line-face ((,class (:foreground ,rujelly-fg :background ,rujelly-grey-4))))
   `(helm-swoop-target-word-face ((,class (:foreground ,rujelly-red-1))))
   `(highlight ((,class (:background ,rujelly-grey-4))))
   `(highlight-indentation-face ((,class (:background ,rujelly-grey-4))))
   `(highlight-indentation-current-column-face ((,class (:background ,rujelly-grey-4))))
   `(hl-line ((,class (:background ,rujelly-grey-4))))
   `(iedit-occurrence ((,class (:foreground ,rujelly-red-1))))
   `(isearch ((,class (:foreground ,rujelly-fg :background ,rujelly-red-1))))
   `(isearch-fail ((,class (:background ,rujelly-red-1))))
   `(ido-first-match ((,class (:foreground ,rujelly-yellow-0))))
   `(ido-only-match ((,class (:foreground ,rujelly-green-0))))
   `(ido-subdir ((,class (:foreground ,rujelly-fg))))
   `(ido-virtual ((,class (:foreground ,rujelly-purple-0))))
   `(ivy-current-match ((,class (:background ,rujelly-purple-1))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,rujelly-orange-0))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,rujelly-orange-0))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,rujelly-orange-0))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,rujelly-orange-0))))
   `(js2-external-variable ((,class (:foreground ,rujelly-yellow-0))))
   `(js2-function-param ((,class (:foreground ,rujelly-fg))))
   `(lazy-highlight ((,class (:foreground ,rujelly-red-1 :background nil))))
   `(linum ((,class (:slant italic :foreground ,rujelly-grey-4))))
   `(magit-blame-heading ((,class (:foreground ,rujelly-grey-1 :background ,rujelly-grey-2))))
   `(magit-branch-local ((,class (:foreground ,rujelly-green-2))))
   `(magit-branch-remote ((,class (:foreground ,rujelly-green-2))))
   `(magit-section-heading ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-grey-2))))
   `(magit-diff-added ((,class (:foreground ,rujelly-green-0))))
   `(magit-diff-added-highlight ((,class (:foreground ,rujelly-green-0 :inherit (magit-section-highlight)))))
   `(magit-diff-context ((,class (:foreground ,rujelly-fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,rujelly-fg :inherit (magit-section-highlight)))))
   `(magit-diff-file-heading ((,class (:weight normal :foreground ,rujelly-fg :background ,rujelly-bg))))
   `(magit-diff-file-heading-highlight ((,class (:weight normal :foreground ,rujelly-fg :background ,rujelly-grey-2))))
   `(magit-diff-hunk-heading ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-grey-2))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-grey-3))))
   `(magit-diff-lines-heading ((,class (:foreground ,rujelly-fg :background ,rujelly-purple-1))))
   `(magit-diff-removed ((,class (:foreground ,rujelly-red-0))))
   `(magit-diff-removed-highlight ((,class (:foreground ,rujelly-red-0 :inherit (magit-section-highlight)))))
   `(magit-diffstat-added ((,class (:foreground ,rujelly-green-0))))
   `(magit-diffstat-removed ((,class (:foreground ,rujelly-red-0))))
   `(magit-hash ((,class (:foreground ,rujelly-red-0))))
   `(magit-log-author ((,class (:foreground ,rujelly-yellow-0))))
   `(magit-mode-line-process ((,class (:foreground ,rujelly-yellow-0))))
   `(magit-mode-line-process-error ((,class (:foreground ,rujelly-red-0))))
   `(magit-popup-argument ((,class (:foreground ,rujelly-red-0))))
   `(magit-popup-disabled-argument ((,class (:foreground ,rujelly-grey-0))))
   `(magit-section-highlight ((,class (:background ,rujelly-grey-2))))
   `(match ((,class (:background ,rujelly-red-1))))
   `(minibuffer-prompt ((,class (:foreground ,rujelly-fg))))
   `(mmm-default-submode-face ((,class (:background ,rujelly-bg))))
   `(mode-line ((,class (:foreground ,rujelly-fg :background nil))))
   `(mode-line-inactive ((,class (:foreground ,rujelly-grey-4 :background nil))))
   `(org-checkbox ((,class (:foreground ,rujelly-green-0))))
   `(org-date ((,class (:foreground ,rujelly-grey-0))))
   `(org-document-info-keyword ((,class (:foreground ,rujelly-yellow-0))))
   `(org-document-title ((,class (:foreground ,rujelly-yellow-0))))
   `(org-done ((,class (:foreground ,rujelly-green-2))))
   `(org-level-1 ((,class (:foreground ,rujelly-red-2))))
   `(org-level-2 ((,class (:foreground ,rujelly-yellow-0))))
   `(org-level-3 ((,class (:foreground ,rujelly-green-1))))
   `(org-level-4 ((,class (:foreground ,rujelly-orange-0))))
   `(org-level-5 ((,class (:foreground ,rujelly-purple-3))))
   `(org-level-6 ((,class (:foreground ,rujelly-red-0))))
   `(org-level-7 ((,class (:foreground ,rujelly-blue-0))))
   `(org-level-8 ((,class (:foreground ,rujelly-green-0))))
   `(org-link ((,class (:foreground ,rujelly-blue-0))))
   `(org-meta-line ((,class (:foreground ,rujelly-grey-0))))
   `(org-special-keyword ((,class (:foreground ,rujelly-purple-0))))
   `(org-todo ((,class (:foreground ,rujelly-red-3))))
   `(region ((,class (:background ,rujelly-purple-1))))
   `(shm-current-face ((,class (:background ,rujelly-grey-4))))
   `(shm-quarantine-face ((,class (:background ,rujelly-red-1))))
   `(smerge-markers ((,class (:foreground ,rujelly-yellow-0 :background ,rujelly-grey-2))))
   `(smerge-mine ((,class (:foreground ,rujelly-fg :background ,rujelly-purple-3))))
   `(smerge-other ((,class (:foreground ,rujelly-fg :background ,rujelly-green-1))))
   `(smerge-refined-change ((,class (:foreground ,rujelly-green-0))))
   `(sp-pair-overlay-face ((,class (:background ,rujelly-grey-4))))
   `(sp-show-pair-match-face ((,class (:background ,rujelly-grey-5))))
   `(swiper-match-face-1 ((,class (:foreground ,rujelly-red-1))))
   `(swiper-match-face-2 ((,class (:foreground ,rujelly-red-1))))
   `(swiper-match-face-3 ((,class (:foreground ,rujelly-red-1))))
   `(swiper-match-face-4 ((,class (:foreground ,rujelly-red-1))))
   `(trailing-whitespace ((,class (:background ,rujelly-red-1))))
   `(warning ((,class (:foreground ,rujelly-orange-0))))
   `(web-mode-builtin-face ((,class (:foreground ,rujelly-blue-0))))
   `(web-mode-constant-face ((,class (:foreground ,rujelly-purple-0))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,rujelly-blue-0))))
   `(web-mode-css-function-face ((,class (:foreground ,rujelly-orange-0))))
   `(web-mode-css-property-name-face ((,class (:foreground ,rujelly-green-3))))
   `(web-mode-css-selector-face ((,class (:foreground ,rujelly-yellow-0))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,rujelly-purple-0))))
   `(web-mode-html-tag-face ((,class (:foreground ,rujelly-fg))))
   `(web-mode-symbol-face ((,class (:foreground ,rujelly-green-1))))
   `(which-key-group-description-face ((,class (:foreground ,rujelly-grey-0))))
   `(which-key-key-face ((,class (:foreground ,rujelly-green-0))))
   `(whitespace-trailing ((,class (:background ,rujelly-red-1))))))
   ;; Continue swapping colors for the rest of the theme...


(provide-theme 'rujelly)

;;; rujelly-theme.el ends here