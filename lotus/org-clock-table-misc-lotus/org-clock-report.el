;;; org-clock-report.el --- org clock report         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  s

;; Author: s <>
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

(provide 'org-clock-report)


(require 'org-table)
(require 'org-clock)

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(defcustom org-clock-clocktable-plain-report-formatter 'org-plain-report-with-content-note-write
  "Function to turn clocking data into a table.
For more information, see `org-clocktable-write-default'."
  :group 'org-clocktable
  :version "24.1"
  :type 'function)

(defun org-report-content-max-length (text)
  (if text
      (let ((maxlen 0))
        (apply #'max (mapcar #'length (split-string text "\n"))))
      0))
(eval-when-compile
  (defmacro custom-set-max (max value)
    `(let ((val ,value))
       (if (< ,max val)
           (setq ,max val)))))
(defun org-report-get-max-line-length (tbl)
  (let ((max 0)
        (len 0))
    (custom-set-max
     max
     (+ (length "• File: ")
        (length (if (car tbl)
                    (file-relative-name (or (car tbl) (buffer-file-name)) default-directory)
                  default-directory))))
    (dolist (hl (nth 2 tbl))
      (let ((level (cl-first hl))
            (headline (nth 1 hl))
            (content  (nth 5 hl))
            (notes    (nth 6 hl)))
        (custom-set-max
         max
         (+ 1 level (length headline)))
        (custom-set-max
         max
         (+ 1 level (org-report-content-max-length content)))
        (dolist (note notes)
          (custom-set-max
           max
           (+ 1 level (org-report-content-max-length note))))))
    max))

(defun org-plain-report-with-content-note-write (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data-plain-report'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((headline-single-char-str (or (plist-get params :headline-char) "•"))
         (insert-content           (or (plist-get params :insert-content) t))
         (insert-notes             (or (plist-get params :insert-notes) t))
         (hlchars '((1 . "*") (2 . "/")))
         (lwords (assoc (or (plist-get params :lang)
                            (org-bound-and-true-p org-export-default-language)
                            "en")
                        org-clock-clocktable-language-setup))
         (multifile (plist-get params :multifile))
         (block (plist-get params :block))
         (sort (plist-get params :sort))
         (ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (header (plist-get  params :header))
         (narrow (plist-get params :narrow))
         (ws (or (plist-get params :wstart) 1))
         (ms (or (plist-get params :mstart) 1))
         (link (plist-get params :link))
         (maxlevel (or (plist-get params :maxlevel) 3))
         (emph (plist-get params :emphasize))
         (level-p (plist-get params :level))
         (org-time-clocksum-use-effort-durations
          (plist-get params :effort-durations))
         (timestamp (plist-get params :timestamp))
         (properties (plist-get params :properties))
         (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
         (rm-file-column (plist-get params :one-file-with-archives))
         (indent (plist-get params :indent))
         (case-fold-search t)
         range-text total-time tbl level hlc formula pcol
         file-time entries entry headline content notes
         (gap "                                  ")
         (max-report-line-len 0)
         recalc content narrow-cut-p tcol)

    ;; Implement abbreviations
    (when (plist-get params :compact)
      (setq level nil indent t narrow (or narrow '40!) ntcol 1))

    ;; Some consistency test for parameters
    (unless (integerp ntcol)
      (setq params (plist-put params :tcolumns (setq ntcol 100))))

    (when (and narrow (integerp narrow) link)
      ;; We cannot have both integer narrow and link
      (message
       "Using hard narrowing in clocktable to allow for links")
      (setq narrow (intern (format "%d!" narrow))))

    (when narrow
      (cond
        ((integerp narrow))
        ((and (symbolp narrow)
              (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
         (setq narrow-cut-p t
               narrow (string-to-number (substring (symbol-name narrow)
                                                   0 -1))))
        (t
         (error "Invalid value %s of :narrow property in clock table"
                narrow))))

    (when block
      ;; Get the range text for the header
      (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

    ;; Compute the total time
    (setq total-time (apply '+ (mapcar #'(lambda (x) (nth 1 x)) tables)))

    (setq max-report-line-len (apply #'max (mapcar 'org-report-get-max-line-length tables)))

    (with-current-buffer (marker-buffer ipos)
      (message "org-plain-report-with-content-note-write: ipos %s" ipos)
      ;; Now we need to output this tsuff
      (goto-char ipos)

      ;; Insert the text *before* the actual table
      (insert-before-markers
       (or header
           ;; Format the standard header
           (concat
            "#+CAPTION: "
            (nth 9 lwords) " ["
            (substring
             (format-time-string (cl-rest org-time-stamp-formats))
             1 -1)
            "]"
            (if block (concat ", for " range-text ".") "")
            "\n")))

      (when nil
        ;; Insert the narrowing line
        (when (and narrow (integerp narrow) (not narrow-cut-p))
          (insert-before-markers
           "|"                            ; table line starter
           (if multifile "|" "")          ; file column, maybe
           (if level-p   "|" "")          ; level column, maybe
           (if timestamp "|" "")          ; timestamp column, maybe
           (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
           (format "<%d>| |\n" narrow)))  ; headline and time columns

        ;; Insert the table header line
        (insert-before-markers
         "|"                              ; table line starter
         (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
         (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
         (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
         (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
         (concat (nth 4 lwords) "|"
                 (nth 5 lwords) "|\n")))                 ; headline and time columns


      (when nil
        ;; Insert the total time in the table
        (insert-before-markers
         "|-\n"                            ; a hline
         "|"                               ; table line starter
         (if multifile (concat "| " (nth 6 lwords) " ") "")
                                        ; file column, maybe
         (if level-p   "|"      "")        ; level column, maybe
         (if timestamp "|"      "")        ; timestamp column, maybe
         (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
         (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
         (format org-clock-total-time-cell-format
                 (org-minutes-to-clocksum-string (or total-time 0))) ; the time
         "|\n"))                          ; close line


      ;; Now iterate over the tables and insert the data
      ;; but only if any time has been collected
      (when (and total-time (> total-time 0))

        (while (setq tbl (pop tables))
          ;; now tbl is the table resulting from one file.
          (setq file-time (nth 1 tbl))
          (when (or (and file-time (> file-time 0))
                    (not (plist-get params :fileskip0)))
            ;; (insert-before-markers "|-\n")  ; a hline because a new file starts
            ;; First the file time, if we have multiple files
            (when multifile
              ;; Summarize the time collected from this file
              (let ((filename (file-relative-name (or (car tbl) (buffer-file-name)) default-directory)))
                (insert-before-markers
                 (format (concat "File: %s"
                                 (make-string (- max-report-line-len (+ (length "File: ") (length filename))) ?\ )
                                 " %s"
                                 "\n")
                         ;; (file-name-nondirectory (cl-first tbl))
                         filename
                         ;; (if level-p   "| " "") ; level column, maybe
                         ;; (if timestamp "| " "") ; timestamp column, maybe
                         ;; (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
                         (org-minutes-to-clocksum-string (nth 1 tbl)))))) ; the time

            ;; Get the list of node entries and iterate over it
            (setq entries (nth 2 tbl))
            (while (setq entry (pop entries))
              (setq level (cl-first entry)
                    headline (nth 1 entry)
                    content  (nth 5 entry)
                    notes    (nth 6 entry)
                    hlc (if emph (or (cl-rest (assoc level hlchars)) "") ""))
              (when narrow-cut-p
                (if (and (string-match (concat "\\`" org-bracket-link-regexp
                                               "\\'")
                                       headline)
                         (match-end 3))
                    (setq headline
                          (format "[[%s][%s]]"
                                  (match-string 1 headline)
                                  (org-shorten-string (match-string 3 headline)
                                                      narrow)))))
              ;; (setq headline (org-shorten-string headline narrow))

              (insert-before-markers
               ;; "|"                      ; start the table line
               ;; (if multifile "|" "")    ; free space for file name column?
               ;; (if level-p (format "%d|" (cl-first entry)) "")   ; level, maybe
               ;; (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
               ;; (if properties
               ;;     (concat
               ;;      (mapconcat
               ;;       (lambda (p) (or (cl-rest (assoc p (nth 4 entry))) ""))
               ;;       properties "|") "|") "")  ;properties columns, maybe
               ;; (if indent (org-clocktable-indent-string level) "") ; indentation
               (make-string level
                            (if (and headline-single-char-str
                                     (stringp headline-single-char-str)
                                     (> (length headline-single-char-str) 0))
                                (aref headline-single-char-str 0)
                              ?*))
               " "
               hlc headline hlc                                       ; headline
               ;; content "|" notes "|"                            ; new added sharad
               ;; (make-string (min (1- ntcol) (or (- level 1))) ?|)
                                        ; empty fields for higher levels
               (make-string (- max-report-line-len (+ level (length headline))) ?\ )
               hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
               ;; "|\n"                                             ; close line
               "\n"
               (when insert-content
                 (if content  (concat content "\n") ""))
               (when insert-notes
                 (if notes (mapconcat 'identity notes "\n") "")))))))
      ;; When exporting subtrees or regions the region might be
      ;; activated, so let's disable ̀delete-active-region'
      (let ((delete-active-region nil)) (backward-delete-char 1))
      (if (setq formula (plist-get params :formula))
          (cond
           ((eq formula '%)
            ;; compute the column where the % numbers need to go
            (setq pcol (+ 2
                          (length properties)
                          (if multifile 1 0)
                          (if level-p 1 0)
                          (if timestamp 1 0)
                          (min maxlevel (or ntcol 100))))
            ;; compute the column where the total time is
            (setq tcol (+ 2
                          (length properties)
                          (if multifile 1 0)
                          (if level-p 1 0)
                          (if timestamp 1 0)))
            (insert
             (format
              "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
              pcol            ; the column where the % numbers should go
              (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
              tcol            ; column of the total time
              tcol (1- pcol)))  ; range of columns where times can be found

            (setq recalc t))
           ((stringp formula)
            (insert "\n#+TBLFM: " formula)
            (setq recalc t))
           (t (error "Invalid formula in clocktable")))
        ;; Should we rescue an old formula?
        (when (stringp (setq content (plist-get params :content)))
          (when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
            (setq recalc t)
            (insert "\n" (match-string 1 (plist-get params :content)))
            (beginning-of-line 0))))
      (when nil
        ;; Back to beginning, align the table, recalculate if necessary
        (goto-char ipos)
        (skip-chars-forward "^|")
        (org-table-align)
        (when org-hide-emphasis-markers
          ;; we need to align a second time
          (org-table-align))
        (when sort
          (save-excursion
            (org-table-goto-line 3)
            (org-table-goto-column (cl-first sort))
            (org-table-sort-lines nil (cl-rest sort))))
        (when recalc
          (if (eq formula '%)
              (save-excursion
                (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
                (org-table-goto-column pcol nil 'force)
                (insert "%")))
          (org-table-recalculate 'all))
        (when rm-file-column
          ;; The file column is actually not wanted
          (forward-char 1)
          (org-table-delete-column)))
      total-time)))
(defalias 'org-clocktable-plain-report-write-default #'org-plain-report-with-content-note-write)

(defun org-get-clock-note ()
  ;; TODO: improve to take all not just one.
  (when (org-at-clock-log-p)
    (save-excursion
      (previous-line)
      (when (org-at-item-p)
        (let ((ele (org-element-at-point)))
          (let ((begin (org-element-property :contents-begin ele))
                (end (org-element-property :contents-end ele)))
            (buffer-substring begin end)))))))

(defun org-get-clock-note ())

;;;###autoload
(defun org-clock-sum-with-notes (&optional tstart tend headline-filter propname)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
  (org-with-silent-modifications
   (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
                      org-clock-string
                      "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
          (lmax 30)
          (ltimes (make-vector lmax 0))
          (t1 0)
          (level 0)
          ts te dt
          time
          (clock-notes ()))
     (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
     (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
     (if (consp tstart) (setq tstart (org-float-time tstart)))
     (if (consp tend) (setq tend (org-float-time tend)))
     (message "x1")
     (remove-text-properties (point-min) (point-max)
                             `(,(or propname :org-clock-minutes) t
                                :org-clock-force-headline-inclusion t))
     (save-excursion
       (goto-char (point-max))
       (message "x2 current %s" (point-marker))
       (while (re-search-backward re nil t)
         (cond
           ((match-end 2)
            ;; Two time stamps
            (setq ts (match-string 2)
                  te (match-string 3)
                  ts (org-float-time
                      (apply 'encode-time (org-parse-time-string ts)))
                  te (org-float-time
                      (apply 'encode-time (org-parse-time-string te)))
                  ts (if tstart (max ts tstart) ts)
                  te (if tend (min te tend) te)
                  dt (- te ts)
                  t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1))
            (when (> dt 0)
              (message "x3 current %s" (point-marker))
              (push (org-get-clock-note) clock-notes)
              (message "x4 current %s" (point-marker))))
           ((match-end 4)
            ;; A naked time
            (setq t1 (+ t1 (string-to-number (match-string 5))
                        (* 60 (string-to-number (match-string 4))))))
           (t ;; A headline
            ;; Add the currently clocking item time to the total
            (when (and org-clock-report-include-clocking-task
                       (equal (org-clocking-buffer) (current-buffer))
                       (equal (marker-position org-clock-hd-marker) (point))
                       tstart
                       tend
                       (>= (org-float-time org-clock-start-time) tstart)
                       (<= (org-float-time org-clock-start-time) tend))
              (let ((time (floor (- (org-float-time)
                                    (org-float-time org-clock-start-time)) 60)))
                (setq t1 (+ t1 time))))
            (let* ((headline-forced
                    (get-text-property (point)
                                       :org-clock-force-headline-inclusion))
                   (headline-included
                    (or (null headline-filter)
                        (save-excursion
                          (save-match-data (funcall headline-filter))))))
              (setq level (- (match-end 1) (match-beginning 1)))

              (when (>= level lmax)
                (setq ltimes (vconcat ltimes (make-vector lmax 0)) lmax (* 2 lmax)))
              (when (or (> t1 0) (> (aref ltimes level) 0))
                (when (or headline-included headline-forced)
                  (if headline-included
                      (loop for l from 0 to level do
                           (aset ltimes l (+ (aref ltimes l) t1))))
                  (setq time (aref ltimes level))
                  (goto-char (match-beginning 0))
                  (put-text-property (point) (point-at-eol)
                                     (or propname :org-clock-minutes) time)
                  (put-text-property (point) (point-at-eol)
                                     :org-clock-notes
                                     (remove nil clock-notes))
                  (setq clock-notes nil) ;;; newly add as fix
                  (if headline-filter
                      (save-excursion
                        (save-match-data
                          (while
                              (> (funcall outline-level) 1)
                            (outline-up-heading 1 t)
                            (put-text-property
                             (point) (point-at-eol)
                             :org-clock-force-headline-inclusion t))))))
                (setq t1 0)
                (loop for l from level to (1- lmax) do
                     (aset ltimes l 0)))
              ;; empty collected notes, else it will be added into upper headings
              (setq clock-notes nil)))))
       (setq org-clock-file-total-minutes (aref ltimes 0))))))


(defun org-heading-content-only-x ()
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((start
                 (progn
                   (goto-char (org-element-property :contents-begin (org-element-at-point)))
                   (org-end-of-meta-data t)
                   ;; (while (org-at-drawer-p)
                   ;;   (goto-char (org-element-property :end (org-element-at-point))))
                   ;; (if (org-at-heading-p) (backward-char))
                   (point))))
            (unless (org-at-heading-p)
              (progn
                (outline-next-heading)
                ;; (outline-next-visible-heading 1)
                (backward-char)
                (buffer-substring start (point)))))))))

(defun org-clock-get-table-data-plain-report (file params)
  "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME)

LEVEL:     The level of the headline, as an integer.  This will be
           the reduced leve, so 1,2,3,... even if only odd levels
           are being used.
HEADLINE:  The text of the headline.  Depending on PARAMS, this may
           already be formatted like a link.
TIMESTAMP: If PARAMS require it, this will be a time stamp found in the
           entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
           in this sequence.
TIME:      The sum of all time spend in this tree, in minutes.  This time
           will of cause be restricted to the time block and tags match
           specified in PARAMS."
  (let* ((maxlevel (or (plist-get params :maxlevel) 3))
         (timestamp (plist-get params :timestamp))
         (ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (ws (plist-get params :wstart))
         (ms (plist-get params :mstart))
         (block (plist-get params :block))
         (link (plist-get params :link))
         (tags (plist-get params :tags))
         (properties (plist-get params :properties))
         (inherit-property-p (plist-get params :inherit-props))
         todo-only
         (matcher (if tags (cdr (org-make-tags-matcher tags))))
         cc range-text st p time org-clock-notes org-heading-content-only level hdl props tsp tbl)
    (setq org-clock-file-total-minutes nil)
    (when block
      (setq cc (org-clock-special-range block nil t ws ms)
            ts (cl-first cc) te (nth 1 cc) range-text (nth 2 cc)))
    (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
    (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
    (when (and ts (listp ts))
      (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (cl-first ts) (nth 1 ts))))
    (when (and te (listp te))
      (setq te (format "%4d-%02d-%02d" (nth 2 te) (cl-first te) (nth 1 te))))
    ;; Now the times are strings we can parse.
    (if ts (setq ts (org-matcher-time ts)))
    (if te (setq te (org-matcher-time te)))
    (save-excursion
      (org-clock-sum-with-notes ts te
                                (unless (null matcher)
                                  (lambda ()
                                    (let* ((tags-list (org-get-tags-at))
                                           (org-scanner-tags tags-list)
                                           (org-trust-scanner-tags t))
                                      (eval matcher)))))
      (goto-char (point-min))
      (setq st t)

      (while (or (and (bobp) (prog1 st (setq st nil))
                      (get-text-property (point) :org-clock-minutes)
                      (setq p (point-min)))
                 (setq p (next-single-property-change
                          (point) :org-clock-minutes)))
        (goto-char p)
        (when (setq time (get-text-property p :org-clock-minutes))
          (setq org-clock-notes (get-text-property p :org-clock-notes)
                org-heading-content-only (org-heading-content-only-x))
          (save-excursion
            (beginning-of-line 1)
            (when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
                       (setq level (org-reduced-level
                                    (- (match-end 1) (match-beginning 1))))
                       (<= level maxlevel))
              ;; TODO: here check for subtree-file: and call (org-clock-get-table-data-plain-report) and collect tbl from here
              (setq hdl (if (not link)
                            (match-string 2)
                          (org-make-link-string
                           (format "file:%s::%s"
                                   (buffer-file-name)
                                   (save-match-data
                                     (match-string 2)))
                           (org-make-org-heading-search-string
                            (replace-regexp-in-string
                             org-bracket-link-regexp
                             (lambda (m) (or (match-string 3 m)
                                             (match-string 1 m)))
                             (match-string 2)))))
                    tsp (when timestamp
                          (setq props (org-entry-properties (point)))
                          (or (cl-rest (assoc "SCHEDULED" props))
                              (cl-rest (assoc "DEADLINE" props))
                              (cl-rest (assoc "TIMESTAMP" props))
                              (cl-rest (assoc "TIMESTAMP_IA" props))))
                    props (when properties
                            (remove nil
                                    (mapcar
                                     (lambda (p)
                                       (when (org-entry-get (point) p inherit-property-p)
                                         (cons p (org-entry-get (point) p inherit-property-p))))
                                     properties))))
              (when (> time 0) (push (list level hdl tsp time props org-heading-content-only org-clock-notes) tbl))))))

      (setq tbl (nreverse tbl))
      (list file org-clock-file-total-minutes tbl))))


;;;###autoload
(defun org-dblock-write:clocktable-plain-report (params) ;org-dblock-write:clocktable(org-clock.el)
  "Write the standard clocktable."
  (setq params (org-combine-plists org-clock-clocktable-plain-report-default-properties ;; org-clocktable-defaults
                                   ;; (org-dblock-table:clocktable-plain-report params)
                                   params))
  (message "org-dblock-write:clocktable-plain-report: params %s" params)
  (catch 'exit
    (let* ((scope (plist-get params :scope))
           (base-buffer (org-base-buffer (current-buffer)))
           (files (pcase scope
                    (`agenda
                     (org-agenda-files t))
                    (`agenda-with-archives
                     (org-add-archive-files (org-agenda-files t)))
                    (`file-with-archives
                     (let ((base-file (buffer-file-name base-buffer)))
                       (and base-file
                            (org-add-archive-files (list base-file)))))
                    ((or `nil `file `subtree `tree
                         (and (pred symbolp)
                              (guard (string-match "\\`tree\\([0-9]+\\)\\'"
                                                   (symbol-name scope)))))
                     base-buffer)
                    ((pred functionp) (funcall scope))
                    ((and (pred consp)
                         (guard (symbolp (car scope))))
                     (eval scope))
                    ((pred consp) scope)
                    (_ (user-error "Unknown scope: %S" scope))))
           (block (plist-get params :block))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (ws (plist-get params :wstart))
           (ms (plist-get params :mstart))
           (step (plist-get params :step))
           (hide-files (plist-get params :hidefiles))
           (formatter (or (plist-get params :formatter)
                          org-clock-clocktable-plain-report-formatter
                          'org-clocktable-plain-report-write-default))
           cc)
      ;; Check if we need to do steps
      (when block
        ;; Get the range text for the header
        (setq cc (org-clock-special-range block nil t ws ms)
              ts (car cc)
              te (nth 1 cc)))
      (when step
        ;; Write many tables, in steps
        (unless (or block (and ts te))
          (user-error "Clocktable `:step' can only be used with `:block' or `:tstart', `:tend'"))
        (org-clocktable-steps params)
        (throw 'exit nil))

      ;; (org-agenda-prepare-buffers (if (consp files) files (list files)))
      (message "org-dblock-write:clocktable-plain-report: point %s" (plist-get params :point))
      (message "org-dblock-write:clocktable-plain-report: formatter %s" formatter)
      (message "org-dblock-write:clocktable-plain-report: current point %s" (point-marker))

      (let ((origin (or (plist-get params :point)
                        (point-marker)))
            (tables (if (consp files)
                        (mapcar #'(lambda (file)
                                    (with-current-buffer (find-buffer-visiting file)
                                      (save-excursion
                                        (save-restriction
                                          (org-clock-get-table-data-plain-report file params)))))
                                files)
                      ;; Get the right restriction for the scope.
                      (save-restriction
                        (cond
                         ((not scope))       ;use the restriction as it is now
                         ((eq scope 'file) (widen))
                         ((eq scope 'subtree)
                          (message "org-dblock-write:clocktable-plain-report: subtree")
                          (org-narrow-to-subtree))
                         ((eq scope 'tree)
                          (message "org-dblock-write:clocktable-plain-report: tree")
                          (while (org-up-heading-safe))
                          (org-narrow-to-subtree))
                         ((and (symbolp scope)
                               (string-match "\\`tree\\([0-9]+\\)\\'"
                                             (symbol-name scope)))
                          (let ((level (string-to-number
                                        (match-string 1 (symbol-name scope)))))
                            (catch 'exit
                              (while (org-up-heading-safe)
                                (looking-at org-outline-regexp)
                                (when (<= (org-reduced-level (funcall outline-level))
                                          level)
                                  (throw 'exit nil))))
                            (org-narrow-to-subtree))))
                        (list (org-clock-get-table-data-plain-report nil params)))))
            (multifile
             ;; Even though `file-with-archives' can consist of
             ;; multiple files, we consider this is one extended file
             ;; instead.
             (and (not hide-files)
                  (consp files)
                  (not (eq scope 'file-with-archives)))))


        (message "org-dblock-write:clocktable-plain-report: origin %s, point %s" origin (plist-get params :point))
        (message "org-dblock-write:clocktable-plain-report: formatter %s" formatter)

        (funcall formatter
                 origin
                 tables
                 ;; (org-dblock-table:clocktable-plain-report params)
                 (org-combine-plists params
                                     ;; (org-dblock-table:clocktable-plain-report params)
                                     `(:multifile ,multifile)))))))

;; (let ((scope '("a")))
;;   (pcase scope
;;     ((or `nil `file `subtree `tree) 'ABC)
;;     ((and (pred consp)
;;           (guard (symbolp (car scope))))
;;      (list 'x scope))
;;     ((pred consp) scope)))

(setq org-clock-clocktable-plain-report-default-properties
      (list :scope '(directory-files-recursively (expand-file-name "" (org-publish-get-attribute "tasks" "org" :base-directory)) "\\.org$" 7 nil t)
            ;; :block 'lastweek
            :block 'week
            :compact nil
            :stepskip0  t
            :fileskip0 t
            :maxlevel 10
            :indent t
            :level t
            :tcolumns 1
            :formatter 'org-plain-report-with-content-note-write))


(defun org-clocktable-plain-report-insert (&optional propterties)
  ;; NOTE: fun def of org-clock-report
  ;; (org-create-dblock
  ;;  (org-combine-plists
  ;;   (list :scope (if (org-before-first-heading-p) 'file 'subtree))
  ;;   org-clock-clocktable-default-properties
  ;;   '(:name "clocktable")))
  (org-create-dblock
   (org-combine-plists
    (list :scope (if (org-before-first-heading-p) 'file 'subtree))
    ;; (list :scope (directory-files-recursively (expand-file-name "" (org-publish-get-attribute "tasks" "org" :base-directory)) "\\.org$" 7 nil t))
    org-clock-clocktable-plain-report-default-properties
    propterties
    '(:name "clocktable-plain-report"))))

;;;###autoload
(defun org-clock-plain-report (&optional arg) ;org-clock-report(org-clock.el)
  "Update or create a table containing a report about clocked time.

If point is inside an existing clocktable block, update it.
Otherwise, insert a new one.

The new table inherits its properties from the variable
`org-clock-clocktable-default-properties'.  The scope of the
clocktable, when not specified in the previous variable, is
`subtree' when the function is called from within a subtree, and
`file' elsewhere.

When called with a prefix argument, move to the first clock table
in the buffer and update it."
  (interactive "P")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock "clocktable-plain-report")
    (org-show-entry))
  (pcase (org-in-clocktable-p)
    (`nil
     (org-clocktable-plain-report-insert))
    (start (goto-char start)))
  (org-update-dblock))

(defun org-clock-plain-report-block-in-place (&optional properties)
  "Update or create a table containing a report about clocked time.

If point is inside an existing clocktable block, update it.
Otherwise, insert a new one.

The new table inherits its properties from the variable
`org-clock-clocktable-default-properties'.  The scope of the
clocktable, when not specified in the previous variable, is
`subtree' when the function is called from within a subtree, and
`file' elsewhere."
  (org-clock-remove-overlays)
  ;; (when arg
  ;;   (org-find-dblock "clocktable-plain-report")
  ;;   (org-show-entry))
  (pcase (org-in-clocktable-p)
    (`nil
     (org-clocktable-plain-report-insert properties))
    (start (goto-char start)))
  (org-update-dblock))


;;;###autoload
(defun org-clock-plain-report-block-buffer (&rest properties)
  (interactive
   (list nil))
  (let ((buff (get-buffer-create "*org-clock-plain-report-block-buffer*")))
    (with-current-buffer buff
      (org-mode)
      (apply #'org-clock-plain-report-block-in-place properties))
    (switch-to-buffer buff)))

(defun org-clock-plain-report-in-place (properties)
  (message "org-clock-plain-report-in-place: properties = %s" properties)
  (org-dblock-write:clocktable-plain-report (org-combine-plists org-clock-clocktable-plain-report-default-properties
                                                                (list :scope (if (org-before-first-heading-p) 'file 'subtree))
                                                                properties
                                                                '(:name "clocktable-plain-report"))))


(defun org-clock-plain-report-tree (marker &rest properties)
  (interactive)
  (message "org-clock-plain-report-tree: properties = %s" properties)
  (let ((point (with-current-buffer (get-buffer-create "*org-clock-plain-report-block-buffer*")
                 (let ((inhibit-read-only t))
                   ;; (setf (buffer-string) "")
                   (erase-buffer))
                 (point-marker))))
    (message "org-clock-plain-report-tree: before point %s" point)
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (message "org-clock-plain-report-tree: marker %s" marker)
      (org-clock-plain-report-in-place (org-combine-plists (list :point point)
                                                           properties)))
    (switch-to-buffer (marker-buffer point))))


(defvar org-clock-plain-report-block-buffer-idle-timer nil)

(defun org-clock-plain-report-block-buffer-when-idle (secs)
  (interactive "nNumber: ")
  (when org-clock-plain-report-block-buffer-idle-timer
    (cancel-timer org-clock-plain-report-block-buffer-idle-timer)
    (setq org-clock-plain-report-block-buffer-idle-timer nil))
  (let ((secs (if (and secs
                       (> secs 7))
                  secs
                30)))
    (setq org-clock-plain-report-block-buffer-idle-timer
          (run-with-idle-timer secs
                               secs
                               #'org-clock-plain-report-block-buffer))))

;;; org-clock-report.el ends here
