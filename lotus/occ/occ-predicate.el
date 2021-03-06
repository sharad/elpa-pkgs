;;; occ-predicate.el --- occ predicate               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords:

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

(provide 'occ-predicate)


(cl-defmethod occ-marker= ((obj marker)
                           (mrk marker))
  (if (and
       (occ-valid-marker obj)
       (occ-valid-marker mrk))
   (let ((obj-marker (occ-heading-marker obj))
         (mrk-marker (occ-heading-marker mrk)))
     (if (and
          (occ-valid-marker obj-marker)
          (occ-valid-marker mrk-marker))
      (equal obj-marker
             mrk-marker)))))

(cl-defmethod occ-marker= ((obj occ-obj-tsk)
                           (mrk marker))
  (occ-marker= (occ-obj-marker obj) (occ-obj-marker mrk)))

(cl-defmethod occ-marker= ((obj marker)
                           (tsk occ-obj-tsk))
  (occ-marker= (occ-obj-marker obj) (occ-obj-marker tsk)))

(cl-defmethod occ-marker= ((obj occ-obj-tsk)
                           (mrk null))
  nil)

(cl-defmethod occ-marker= ((obj null)
                           (tsk occ-obj-tsk))
  nil)

(cl-defmethod occ-marker= ((obj occ-obj-tsk)
                           (tsk occ-obj-tsk))
  (occ-marker= (occ-obj-marker obj) (occ-obj-marker tsk)))


(cl-defmethod occ-current-associated-p ((ctx occ-ctx))
  (let ((tsk (occ-current-tsk)))
    (when tsk
     (occ-associable-with-p tsk
                            ctx))))


(cl-defmethod occ-associable-p ((obj null))
  "Test if CTSK is associate"     ;not required.
  nil)

(cl-defmethod occ-associable-p ((obj occ-obj-ctx-tsk))
  "Test if CTSK is associate"     ;not required.
  (> (occ-rank (occ-build-ctxual-tsk obj)) 0))


(cl-defgeneric occ-unammed-p (obj)
  "occ-unnamed-p")

(cl-defmethod occ-unnamed-p ((obj marker))
  (occ-debug :debug "occ-unnamed-p(marker=%s)" obj)
  (occ-clock-marker-unnamed-p obj))

(cl-defmethod occ-unnamed-p ((obj occ-obj-tsk))
  (occ-debug :debug "occ-unnamed-p(occ-tsk=%s)" obj)
  (occ-unnamed-p (occ-obj-marker obj)))


(cl-defmethod occ-current-p ((obj occ-tsk))
  (occ-marker= (occ-current-tsk) obj))

;;; occ-predicate.el ends here
