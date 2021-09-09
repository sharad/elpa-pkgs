;;; occ-helm-config.el --- occ helm configurations   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  sharad

;; Author: sharad
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

(provide 'occ-helm-config)


(require 'occ-helm)


;; TODO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable todo) "TODO"
                                   "* MILESTONE %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk clockable meeting) "MEETING"
                                   "* MEETING %? %^g\n %i\n [%a]\n")

;; NOTE
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable note) "NOTE"
                                   "* <NOTE> %? %^g\n %i\n [%a]\n")
;; INFO
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable info) "INFO"
                                   "* <INFO> %? %^g\n %i\n [%a]\n")
;; EVENT
;;;###autoload
(org-capture+-add-heading-template '(occ tsk unclockable event) "EVENT"
                                   "* <EVENT> %? %^g\n %i\n [%a]\n")


(progn
  (progn
    (progn
      (setq occ-helm-callables nil)
      (occ-build-callable-normal :ignore                   "Ignore"                   #'ignore)
      (occ-build-callable-normal :identity                 "Select"                   #'identity)
      (occ-build-callable-normal :clock-in                 "Clock-in"                 #'occ-clock-in)
      (occ-build-callable-normal :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in)
      (occ-build-callable-normal :try-clock-in             "Try Clock-in"             #'occ-try-clock-in)
      (occ-build-callable-normal :procreate-child          "Procreate Child"          #'occ-procreate-child)
      (occ-build-callable-normal :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in)
      (occ-build-callable-normal :goto                     "Goto"                     #'occ-goto)
      (occ-build-callable-normal :set-to                   "Set To"                   #'occ-set-to)
      (occ-build-callable-normal :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit) ;TODO: implement it.
      (occ-build-callable-normal :proprty-edit-combined    "Proprtes Edit Combined"   #'occ-props-edit-combined) ;TODO: implement it.
      (occ-build-callable-normal :call-with-obj            "Call with object"         #'occ-call-with-obj)
      (occ-build-callable-normal :set-debug-obj            "Set debug obj"            #'occ-set-debug-obj)
      (occ-build-callable-normal :rank                     "Get Rank"                 #'occ-print-rank)
      (occ-build-callable-normal :tsk                      "Get Task"                 #'occ-print-tsk))
    (progn
      (occ-build-callable-generator :fast-edits-gen     "Fast Edits" #'occ-gen-fast-edits)
      (occ-build-callable-generator :edits-gen          "Edit"       #'occ-gen-edits)
      (occ-build-callable-generator :misc-gen           "Misc"       #'occ-gen-misc)
      (occ-build-callable-generator :fast-checkouts-gen "Checkouts"  #'occ-gen-checkouts)))

  (progn
    (setq occ-helm-actions-tree '(t))
    (progn
      (occ-add-helm-actions '(actions select)
                            "Select"
                            :identity)

      (occ-add-helm-actions '(actions general)
                            "Simple"
                            :procreate-child
                            :procreate-child-clock-in
                            :call-with-obj
                            :set-debug-obj
                            :try-clock-in
                            :goto
                            :rank
                            :tsk)

      (occ-add-helm-actions '(actions edit)
                            "Editing"
                            :proprty-window-edit
                            :proprty-edit-combined))
    (progn
      (occ-add-helm-actions '(actions general)
                            "General"
                            :misc-gen)

      (occ-add-helm-actions '(actions edit)
                            "Editing"
                            :fast-edits-gen
                            :edits-gen)

      (occ-add-helm-actions '(actions checkout)
                            "Checkout"
                            :fast-checkouts-gen))))

;;; occ-helm-config.el ends here
