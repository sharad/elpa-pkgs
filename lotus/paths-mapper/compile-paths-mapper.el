;;; compile-paths-mapper.el --- Compile path mapper  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'compilation-paths-mapper)

(require 'paths-mapper)
(require 'compile)

(defun compilation-find-missing-file-paths-mapper-params (marker filename directory &rest formats)
  (let* ((path (if (stringp directory)
                  (expand-file-name filename directory)
                 filename))
         (filtered-path (paths-mapper-filter-path path)))
    (if (and filtered-path
             (stringp filtered-path)
             (file-exists-p filtered-path))
        (append
          (list marker
                (paths-mapper-filter-path filename)
                (paths-mapper-filter-path directory))
          formats)
      (progn
        (paths-mapper-read-add-replacement path)
        (if formats
            (compilation-find-missing-file-paths-mapper-params marker filename directory formats)
          (compilation-find-missing-file-paths-mapper-params marker filename directory))))))


(defun compilation-find-file-fix-paths-mapper-path-map (orig-fun &rest args)
  (let* ((filename (nth 1 args))
         (directory (nth 2 args))
         (path (if (stringp directory)
                 (expand-file-name filename directory)
                filename)))
    (if (file-exists-p path)
        (let ((res (apply orig-fun args)))
           (message "display-buffer returned %S" res)
           res)
      (let ((mod-args (apply #'compilation-find-missing-file-paths-mapper-params args)))
        (message "org[%s] mod[%s]" args mod-args)
        (apply orig-fun mod-args)))))

;;;###autoload
(defun compile-paths-mapper-insinuate ()
  (interactive)
  (advice-add 'compilation-find-file :around #'compilation-find-file-fix-paths-mapper-path-map))

;;;###autoload
(defun compile-paths-mapper-uninsinuate ()
  (interactive)
  (advice-remove 'compilation-find-file #'compilation-find-file-fix-paths-mapper-path-map))

;;; compilation-paths-mapper.el ends here
