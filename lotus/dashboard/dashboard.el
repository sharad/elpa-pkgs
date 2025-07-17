;;; dashboard.el --- Dashboard                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(provide 'dashboard)



(require 'recentf)
(require 'projectile)
(require 'warnings)
(require 'all-the-icons)


;; (defun my/dashboard-insert-section (title key items action-fn)
;;   "Insert a section with TITLE and KEY shortcut, listing ITEMS with ACTION-FN."
;;   (insert (propertize (format "\n%s: (%s)\n\n" title key)
;;                       'face '(:weight bold :height 1.2)))
;;   (if items
;;       (cl-loop for item in items
;;                for idx from 1 do
;;                (let ((display (format "%3d   %s\n" idx item)))
;;                  (insert-button display
;;                                 'action (lambda (_)
;;                                           (funcall action-fn item))
;;                                 'follow-link t)))
;;     (insert "  No entries.\n\n")))

;; (defun my/dashboard-insert-centered (str &optional face)
;;   "Insert STR centered horizontally using Spacemacs centering if available.
;; If FACE is provided, apply it."
;;   (if (fboundp 'spacemacs-buffer//insert-centered)
;;       (spacemacs-buffer//insert-centered str face)
;;     ;; fallback
;;     (let* ((buffer-width (window-width))
;;            (str-width (string-width str))
;;            (margin (max 0 (/ (- buffer-width str-width) 2))))
;;       (insert (make-string margin ?\s))
;;       (insert (if face (propertize str 'face face) str))
;;       (insert "\n"))))



(defun my/dashboard-insert-centered (str &optional face icon)
  "Insert STR centered with optional FACE and ICON using Spacemacs centering if available."
  (let ((line (if icon
                  (concat icon " " str)
                str)))
    (if (fboundp 'spacemacs-buffer//insert-centered)
        (spacemacs-buffer//insert-centered line face)
      ;; fallback
      (let* ((buffer-width (window-width))
             (str-width (string-width line))
             (margin (max 0 (/ (- buffer-width str-width) 2))))
        (insert (make-string margin ?\s))
        (insert (if face (propertize line 'face face) line))
        (insert "\n")))))



(when nil


  ;; Example Icons for Sections:

  ;; Warnings:

  (all-the-icons-octicon "alert" :height 1.0 :v-adjust 0)

  ;; Recent Files:

  (all-the-icons-octicon "file-text" :height 1.0 :v-adjust 0)

  ;; Projects:

  (all-the-icons-octicon "repo" :height 1.0 :v-adjust 0)

  ;; TODO:

  (all-the-icons-faicon "tasks" :height 1.0 :v-adjust 0)

  ;; ✅ Example usage in my/dashboard-insert-section:

  (my/dashboard-insert-section
   (concat (all-the-icons-octicon "repo" :height 1.0 :v-adjust 0) " Projects") "p"
   (or (my/dashboard-get-projects) '("No projects found."))
   (lambda (proj) (projectile-switch-project-by-name proj))))

(defun my/dashboard-center-vertically ()
  "Vertically center the dashboard using Spacemacs centering if available."
  (when (fboundp 'spacemacs-buffer//center-lines)
    (spacemacs-buffer//center-lines)))

(defun my/dashboard-insert-section (title key items action-fn)
  "Insert a section with TITLE and KEY, displaying ITEMS with ACTION-FN."
  (my/dashboard-insert-centered (format "%s: (%s)" title key) '(:weight bold :height 1.2))
  (if items
      (cl-loop for item in items
               for idx from 1 do
               (let ((line (format "%3d   %s" idx item)))
                 (insert-button (concat line "\n")
                                'action (lambda (_) (funcall action-fn item))
                                'follow-link t)))
    (my/dashboard-insert-centered "No entries." 'italic))
  (insert "\n"))

(defun my/dashboard-get-recent-files ()
  (seq-take recentf-list 5))

(defun my/dashboard-get-projects ()
  (when (bound-and-true-p projectile-mode)
    (seq-take projectile-known-projects 5)))


(defun my/dashboard-get-todo-items ()
  "Return a list of TODO items using Spacemacs' accumulation if available."
  (when (fboundp 'spacemacs-buffer//get-org-todos)
    (let ((todos (spacemacs-buffer//get-org-todos)))
      (seq-take todos 5))))


(defun my/dashboard-get-todo-items-outside ()
  "Return a list of TODO items from org-agenda-files."
  (require 'org)
  (let (results)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((entry (org-get-heading t t t t)))
             (push (format "%s - %s" file entry) results)))
         "/TODO")))
    (seq-take (nreverse results) 5)))

(defun mydashboard-buttons ()
  ;; (insert (propertize "🚀 My Spacemacs Dashboard\n\n" 'face '(:height 1.5 :weight bold)))
  (my/dashboard-insert-centered "My Spacemacs Dashboard"
                                '(:height 1.5 :weight bold)
                                (all-the-icons-fileicon "emacs" :height 1.2 :v-adjust 0))

  ;; Button: Open Recent Files
  (insert-button "📂 Open Recent Files\n"
                 'action (lambda (_)
                           (call-interactively 'recentf-open-files))
                 'follow-link t)
  ;; Button: Open Bookmarks
  (insert-button "🔖 Open Bookmarks\n"
                 'action (lambda (_)
                           (bookmark-bmenu-list))
                 'follow-link t)
  ;; Button: Open/Create Scratch Buffer
  (insert-button "📝 Open Scratch Buffer\n"
                 'action (lambda (_)
                           (switch-to-buffer "*scratch*"))
                 'follow-link t)
  ;; Button: Run Shell
  (insert-button "💻 Run Shell\n"
                 'action (lambda (_)
                           (shell (generate-new-buffer-name "*shell*")))
                 'follow-link t)
  ;; Button: Run Term
  (insert-button "🖥️ Run Term\n"
                 'action (lambda (_)
                           (ansi-term (getenv "SHELL")))
                 'follow-link t)
  (insert "\n\n")
  (insert (propertize "Press `q` to quit this dashboard." 'face 'italic)))

(defun my/dashboard-get-warnings ()
  "Return a list of recent warnings using Spacemacs' warning accumulation if available."
  (when (fboundp 'configuration-layer//warnings)
    (let ((warnings (configuration-layer//warnings)))
      (seq-take warnings 5))))

(defun my/spacemacs-like-dashboard ()
  "Spacemacs-like dashboard with dynamic warnings, recent files, projects, and TODO."
  (interactive)
  (recentf-mode 1)
  (let ((buf (get-buffer-create "*My Dashboard*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (setq-local cursor-type nil)

      ;; Title
      ;; (insert (propertize "🚀 My Spacemacs Dashboard\n\n" 'face '(:height 1.5 :weight bold)))
      (my/dashboard-insert-centered "My Spacemacs Dashboard"
                                    '(:height 1.5 :weight bold)
                                    (all-the-icons-fileicon "emacs" :height 1.2 :v-adjust 0))


      (mydashboard-buttons)

      ;; Dynamic Warnings
      ;; (my/dashboard-insert-section
      ;;  "Warnings" "w"
      ;;  (my/dashboard-get-warnings)
      ;;  (lambda (_)
      ;;    (switch-to-buffer "*Warnings*")))
      (my/dashboard-insert-section
       (concat (all-the-icons-octicon "alert" :height 1.0 :v-adjust 0) " Warnings") "w"
       (my/dashboard-get-warnings)
       (lambda (_) (switch-to-buffer "*Warnings*")))

      ;; Recent Files
      (my/dashboard-insert-section
       "Recent Files" "r"
       (my/dashboard-get-recent-files)
       (lambda (file)
         (find-file file)))

      ;; Projects
      (my/dashboard-insert-section
       "Projects" "p"
       (or (my/dashboard-get-projects) '("No projects found."))
       (lambda (project)
         (projectile-switch-project-by-name project)))

      ;; TODO
      (my/dashboard-insert-section
       "To-Do" "d"
       (my/dashboard-get-todo-items)
       (lambda (file)
         (find-file file)))

      (insert "\n\n")
      (insert (propertize "Press `q` to quit this dashboard." 'face 'italic))



      ;; Vertically center
      (my/dashboard-center-vertically)

      ;; Keybindings
      (use-local-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "q") #'quit-window)
                       (define-key map (kbd "r") (lambda () (interactive) (call-interactively 'recentf-open-files)))
                       (define-key map (kbd "p") (lambda () (interactive) (projectile-switch-project)))
                       (define-key map (kbd "d") (lambda () (interactive)
                                                   (find-file (car (my/dashboard-get-todo-items)))))
                       (define-key map (kbd "w") (lambda () (interactive)
                                                   (switch-to-buffer "*Warnings*")))
                       map))

      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buf)))

;;; dashboard.el ends here
