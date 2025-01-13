;;; sessions-unified-core-common.el --- common code  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Music Player Daemon (MPD) user

;; Author: Music Player Daemon (MPD) user <spratap@merunetworks.com>
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

(provide 'sessions-unified-core-common)


(cl-defgeneric sessions-unified--session-store (app))
(cl-defgeneric sessions-unified--session-restore (app))
(cl-defgeneric sessions-unified--session-enable (app))
(cl-defgeneric sessions-unified--session-disable (app))
(cl-defgeneric sessions-unified--session-check (app))



(cl-defgeneric sessions-unified--get-frame-data (app frame)
  "sessions-unified--get-frame-data")
(cl-defgeneric sessions-unified--set-frame-data (app frame data)
  "sessions-unified--set-frame-data")

;;; sessions-unified-core-common.el ends here
