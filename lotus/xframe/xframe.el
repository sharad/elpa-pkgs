;;; xframe.el --- Utility frame using gtk            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  sharad

;; Author: sharad <s@dell5480>
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

(provide 'xframe)


(defun make-xframe (xid &optional width height)
  (let ((width (or width (frame-char-width) 6))
        (height (or height (frame-char-height) 13)))
    (make-frame `((parent-id . ,xid)
                  (undecorated . t)
                  (width . ,width)
                  (height . ,height)
                  (session-unified-no-session . t)
                  (tab-bar-mode . nil)
                  (tab-bar-lines . 0)
                  (tab-bar-lines-keep-state . nil)
                  (centaur-tabs-local-mode . nil)
                  (centaur-tabs-display-line-format . nil)))
    (set centaur-tabs-display-line-format nil)
    (kill-local-variable 'centaur-tabs--local-hlf)))

;;; xframe.el ends here
