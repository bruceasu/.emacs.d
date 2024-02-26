;; +custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

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
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require '+const))

(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience)

(defcustom suk-full-name "Suk"
  "Set user full name."
  :type 'string)

(defcustom suk-mail-address "bruceasu@gmail.com"
  "Set user email address."
  :type 'string)

(defcustom suk-proxy "127.0.0.1:1080"
  "Set network proxy."
  :type 'string)

(defcustom suk-cnfonts nil
  "Use cnfonts or not."
  :type 'boolean)

(defcustom user-home-dir (getenv "HOME")
  "User home directory."
  :type 'string)

(if sys/win32p
    (setq user-home-dir (getenv "USERPROFILE"))
)

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :type 'strig)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :type 'strig)


;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(provide '+custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +custom.el ends here
