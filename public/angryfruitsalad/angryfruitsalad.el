;;; angryfruitsalad.el --- http://www.emacswiki.org/emacs/AngryFruitSalad

;; Copyright (C) 2012  http://www.emacswiki.org/emacs/AngryFruitSalad

;; Author: http://www.emacswiki.org/emacs/AngryFruitSalad
;; Keywords: lisp

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

;; http://www.emacswiki.org/emacs/AngryFruitSalad

;;; Code:

(provide 'angryfruitsalad)


;;{{

;; AngryFruitSalad

;; This page is about loud, colorful Emacs buffers – whether you like ‘em or not.
;; Contents

;;     A Wash-Out from Alex
;;         Screenshots
;;     A Wash-Out (or In) from Drew
;;     Ridding Emacs of Colors Entirely

;; A Wash-Out from Alex

;; Some people, me (AlexSchroeder) for example, dislike the strong
;; colour Emacs uses for font-lock by default. Some less polite people
;; call this phenomenon “angry fruit salad”.

;; I use the following code in my InitFile to “wash out” the colours
;; of the font-lock Faces. It should bring the colours closer to the
;; colour of the default face. So that they differ from the default
;; foreground colour only slightly.

;; ‘font-lock-warning-face’, however, remains as bright and
;; outstanding as before.

(defun egoge-wash-out-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
		            (face-attribute 'default :foreground)))
	      (col (color-values colour))
	      (list nil))
    (unless degree (setq degree 2))
    (while col
      (push (/ (/ (+ (pop col)
		                 (* degree (pop basec)))
		              (1+ degree))
	             256)
	          list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun egoge-wash-out-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
 			                    :foreground (egoge-wash-out-colour colour degree)))))

(defun egoge-find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
 	      (mapcar (lambda (face)
 		              (and (string-match regexp
 				                             (symbol-name face))
 		                   face))
 		            (face-list))))

(defun egoge-wash-out-fontlock-faces (&optional degree)
  (interactive)
  (mapc (lambda (elt)
 	        (egoge-wash-out-face elt degree))
 	      (delq 'font-lock-warning-face
 	            (egoge-find-faces "^font-lock"))))

(when (> (length (defined-colors))
         16)
  (egoge-wash-out-fontlock-faces 2))

;; Note that you can call ‘egoge-wash-out-fontlock-faces’ with a
;; numeric argument. The higher the argument DEGREE, the more washed
;; out will your font-lock faces appear.  Screenshots

;; A picture of my Emacs without washed colours (left), with
;; ‘egoge-wash-out-fontlock-faces’ called with the argument 1
;; (middle), and with the argument 2 (this is what I use).

;; AngryFruitSaladOne_Screenshot

;; ‘egoge-wash-out-fontlock-faces’ does not make the faces
;; lighter. Instead it decreases the difference of the foreground
;; colour between the font-lock faces and the default face. So it
;; DTRT, if you use an unusual colour, like e. g. Slate Blue.

;; AngryFruitSaladTwo_Screenshot

;; A Wash-Out (or In) from Drew

;; DoReMi provides commands (in library Lisp:doremi-frm.el) that let
;; you wash out the colors used in various faces – ‘doremi-face-fg’,
;; for instance. You just type ‘s’, for “saturation”, at the
;; prompt. Decreasing the saturation washes a color out, making it
;; less vibrant (“angry”). (You can do the same thing with frame
;; backgrounds: ‘doremi-bg’.)

;; If you would like to make such changes generally, to all faces at
;; the same time, you can use command ‘doremi-all-faces-fg’. If need
;; be, you can then use ‘doremi-face-fg’ to go back and tweak a few
;; faces individually.

;; DoReMi commands let you change face colors and other properties
;; incrementally, using the arrow keys or mouse wheel, so you can see
;; what you’re doing as you do it. Go too far? Just go backwards:
;; up/down, until you get just the degree of angriness and fruitiness
;; you like!

;; Feeling particularly angry or fruity today? Or not? It only takes a
;; second to adjust all the Emacs faces and backgrounds to your mood…
;; Have fun!

;; Note:

;;     1. User option ‘doremi-wrap-color-flag’ controls what happens
;;     when a color component (such as saturation) reaches its maximum
;;     or minimum, when you are incrementing or decrementing it. If
;;     non-‘nil’, then the component goes from max to min, and vice
;;     versa. If ‘nil’, then the component’s value is pinned at the
;;     max or min, without wrapping around.  If you are trying to wash
;;     out all face colors at once, then you probably want to set
;;     ‘doremi-wrap-color-flag’ to ‘nil’ before using
;;     ‘doremi-all-faces-fg’. Command ‘doremi-toggle-wrap-color’
;;     toggles this user option.  If ‘doremi-wrap-color-flag’ is
;;     non-‘nil’, then, as you incrementally change colors, whenever
;;     any color gets to one saturation limit (min or max), it cycles
;;     round to the other limit (max or min) again. So, after a color
;;     becomes completely unsaturated (washed-out), the next
;;     incremental change makes it completely saturated. That’s fine
;;     for changing an individual face, but, when you change all faces
;;     simultaneously, some will hit zero saturation before others
;;     (because they were already more washed-out), and then pass to
;;     complete saturation.  2. You might be a bit surprised if you
;;     wash out a color completely, and then try to resaturate
;;     it. Washing it out completely can change its hue, so when you
;;     resaturate, you are resaturating a different hue. This is due
;;     to the algorithm that converts RGB to and from HSV. – DrewAdams

;; Ridding Emacs of Colors Entirely

;; Don’t want colorization? Add the following to your ~/.emacs file.

;; ;; Turn off all colors.
;; ;;   To turn on invoke emacs with --color=auto.

(if (not (assoc 'tty-color-mode
                default-frame-alist))
    (push (cons 'tty-color-mode
                'never)
          default-frame-alist))

;; If you decide you want colors for a particular session run:

;; emacs --color=auto

;; Note that if all you want is to disable syntax hilighting use
;; instead:

;; (global-font-lock-mode 0)

;; It would be nice to be able to toggle colors off and on
;; dynamically, e.g. to use syntax hilighting to get a quick syntax
;; check. If you know how to do this please add to this entry.

;; (I just stick with fundamental-mode nearly all the time, going into
;; a highlighting mode just once in a while. Doing this, it “toggles
;; colors dynamically”, as you ask. TomBaker)

;;}}

;;; angryfruitsalad.el ends here
