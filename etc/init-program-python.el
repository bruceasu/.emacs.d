;; init-program-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

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
;; Python  configurations.
;;

;;; Code:

(use-package 
  python 
  :ensure t 
  :hook (inferior-python-mode . (lambda () 
                                  (process-query-on-exit-flag (get-process "Python")))) 
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil) 
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3") 
             (string= python-shell-interpreter "python")) 
    (setq python-shell-interpreter "python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Live Coding in Python
  (use-package live-py-mode 
    :ensure t)
  (use-package python-black
	:ensure t
	:hook (python-mode . python-black-on-save-mode)))

;; 微软的python语言服务器
  (use-package 
    lsp-python-ms 
    :ensure t 
    :hook (python-mode . (lambda () 
                           (require 'lsp-python-ms) 
                           (lsp))) 
    :custom (lsp-python-ms-executable
             "~/.emacs.d/var/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))


;; 美化lsp-mode
(use-package lsp-ui 
		 :ensure t 
		 :hook (lsp-mode . lsp-ui-mode) 
		 :custom (lsp-ui-doc-delay 1) 
		 :init
		 (setq lsp-ui-doc-enable t)
		 (setq lsp-ui-imenu-colors
			`(,(face-foreground 'font-lock-keyword-face),(face-foreground 'font-lock-string-face),(face-foreground 'font-lock-constant-face),(face-foreground 'font-lock-variable-name-face)))
		 (setq lsp-ui-doc-use-webkit nil)
		 (setq lsp-ui-doc-delay 0.2)
		 (setq lsp-ui-doc-include-signature t)
		 (setq lsp-ui-doc-position 'at-point)
		 (setq lsp-ui-doc-border (face-foreground 'default))
		 ;; Disable eldoc displays in minibuffer
		 (setq lsp-eldoc-enable-hover nil)
		 (setq lsp-ui-sideline-enable t)
		 (setq lsp-ui-sideline-show-hover nil)
		 (setq lsp-ui-sideline-show-diagnostics nil)
		 (setq lsp-ui-sideline-ignore-duplicate t)
		 (setq lsp-ui-imenu-enable t))

(provide 'init-program-python)
;;; init-program-python.el ends here
