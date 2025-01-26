;;; code-snippet.el --- code snippet                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d _at_ Gmail>
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

(provide 'code-snippet)


;; check out
;; (c-mark-function)

;; copy code like below

;; Mgr::Function() {

;;   [...]

;; [1] https://domain/path/file.cpp#L12479

;;             SetTimeout(TIME_SECS);
;;             uint32_t listsz = m_list.size();
;;             list.insert(pair<uint16_t, msga*>
;;                                                      (xyz, test));

;;   [...]

;; [2] https://domain/path/file.cpp#L12726

;;    return Send();

;; }


(defvar yank-highlight-context-elision "[...]"
  "String to indicate elided context when yanking highlighted code.")

(defun yank-highlighted-with-context ()
  "Yank the highlighted code portion with context information elided."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (code (buffer-substring-no-properties start end))
             (context (save-excursion
                        (let ((context-start (progn
                                               (goto-char start)
                                               (or (ignore-errors (backward-block) (point))
                                                   (ignore-errors (backward-paragraph) (point))
                                                   (point-min))))
                              (context-end (progn
                                             (goto-char end)
                                             (or (ignore-errors (forward-block) (point))
                                                 (ignore-errors (forward-paragraph) (point))
                                                 (point-max)))))
                          (buffer-substring-no-properties context-start context-end)))))
        ;; Generate the output with context elided
        (let ((output
               (concat
                (with-temp-buffer
                  (insert context)
                  (goto-char (point-min))
                  (if (re-search-forward (regexp-quote code) nil t)
                      (progn
                        ;; Elide everything before the code snippet
                        (goto-char (match-beginning 0))
                        (insert (concat yank-highlight-context-elision "\n"))
                        ;; Elide everything after the code snippet
                        (goto-char (match-end 0))
                        (insert (concat "\n" yank-highlight-context-elision "\n"))
                        (buffer-string))
                    ;; Fallback to just the highlighted text if context isn't found
                    code)))))
          (kill-new output)
          (message "Yanked code with context elided")))
    (message "No region selected")))


;;; code-snippet.el ends here
