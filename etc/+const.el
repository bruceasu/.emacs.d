;; +const.el --- Define constants.	-*- lexical-binding: t -*-

;; Copyright (C) 1999 - 2024 Suk

;; Author: Suk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:
(defconst custom-template-file
  (expand-file-name "custom-template.el" user-emacs-directory)
  "Custom template file of Suk's Emacs.")

(defconst suk-homepage
  "https://github.com/bruceasu/.emacs.d"
  "The Github page of this Emacs config.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(provide '+const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +const.el ends here
