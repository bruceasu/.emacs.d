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
  (require '+const)
  (require 'package))

(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/bruceasu/.emacs.d"))

(defcustom suk-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Suk logo. nil means official logo."
  :group 'suk
  :type 'string)

(defcustom suk-full-name "Suk"
  "Set user full name."
  :group 'suk
  :type 'string)

(defcustom suk-mail-address "bruceasu@gmail.com"
  "Set user email address."
  :group 'suk
  :type 'string)

(defcustom suk-proxy "127.0.0.1:1080"
  "Set network proxy."
  :group 'suk
  :type 'string)

(defcustom suk-cnfonts nil
  "Use cnfonts or not."
  :group 'suk
  :type 'boolean)

(defcustom user-home-dir (getenv "HOME")
  "User home directory."
  :group 'suk
  :type 'string)

(if sys/win32p
    (setq user-home-dir (getenv "USERPROFILE"))
)

(defcustom suk-icon t
  "Display icons or not."
  :group 'suk
  :type 'boolean)

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom  org-css-file "~/.emacs.d/share/my-org-style-min.css"
  "The org css style file."
  :group 'suk
  :type 'string)

(defcustom windows-bash-path (expand-file-name "C:/Program Files/Git/bin/bash.exe")
  "The windows version of bash."
  :group 'suk
  :type 'string)


(defcustom suk-completion-style 'childframe
  "Completion display style."
  :group 'suk
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom suk-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'suk
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom suk-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'suk
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom suk-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom suk-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+HEADERS"      . ?☰)
    ("#+OPTIONS:"     . ?⚙)
    ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))


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
