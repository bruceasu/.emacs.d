;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

;; 增强了搜索功能
(use-package swiper
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package counsel
  :ensure t
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)
         ("C-x C-r" . 'counsel-recentf)
         ("C-x d" . 'counsel-dired)
         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)

         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)

         ;; Find counsel commands quickly
         ("<f6>" . (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s ."))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance fuzzy matching
  (use-package flx)

  ;; Enhance M-x
  (use-package amx)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
	:ensure t
	:hook ((counsel-mode . counsel-projectile-mode))
	:init (counsel-projectile-mode 1)
	:bind (:map leader-key
				("p" . #'projectile-command-map)))
  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
                ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c v" . counsel-tramp)))
)

;; 美化ivy(swiper和counsel)
(when (display-graphic-p)
  (progn
	;; (use-package all-the-icons-ivy-rich
	;;   :ensure t
	;;   :defer 2
	;;   :init (all-the-icons-ivy-rich-mode 1))
	
	;; (use-package ivy-rich 
	;;   :ensure t
	;;   :defer 2
	;;   :defines all-the-icons-mode-icon-alist
	;;   :functions (all-the-icons-icon-family-for-mode all-the-icons-icon-family-for-file)
	;;   :init (ivy-rich-mode 1) 
	;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
	;;   :preface
	;;   (with-eval-after-load 'all-the-icons
	;; 	(add-to-list 'all-the-icons-mode-icon-alist
	;; 				 '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))

	;;   (defun ivy-rich-switch-buffer-icon (candidate)
	;; 	"Show buffer icons in `ivy-rich'."
	;; 	;; Only on GUI
	;; 	(when (and suk-ivy-icon
	;; 			   (display-graphic-p)
	;; 			   (featurep 'all-the-icons))
	;; 	  (with-current-buffer (get-buffer candidate)
	;; 		(let ((icon (all-the-icons-icon-for-mode major-mode)))
    ;;           (propertize
    ;;            (if (symbolp icon)
    ;;                (all-the-icons-icon-for-mode 'text-mode)
	;; 			 icon)
    ;;            'face `(
    ;;                    :height 1.1
    ;;                    :family ,(all-the-icons-icon-family-for-mode
	;; 							 (if (symbolp icon)
	;; 								 'text-mode
    ;;                                major-mode))
    ;;                    :inherit
    ;;                    ))))))

    ;;   (defun ivy-rich-file-icon (candidate)
	;; 	"Show file icons in `ivy-rich'."
	;; 	;; Only on GUI
	;; 	(when (and suk-ivy-icon
    ;;                (display-graphic-p)
    ;;                (featurep 'all-the-icons))
    ;;       (let ((icon (all-the-icons-icon-for-file candidate)))
	;; 		(propertize
	;; 		 (if (symbolp icon)
	;; 			 (all-the-icons-icon-for-mode 'text-mode)
    ;;            icon)
	;; 		 'face `(
	;; 				 :height 1.1
	;; 				 :family ,(all-the-icons-icon-family-for-file candidate)
	;; 				 :inherit
	;; 				 ))))
	;; 	)
	;;   :config
	;;   (setq ivy-rich-display-transformers-list
	;; 		'(ivy-switch-buffer
	;; 		  (:columns
	;; 		   ((ivy-rich-switch-buffer-icon (:width 2))
	;; 			(ivy-rich-candidate (:width 30))
	;; 			(ivy-rich-switch-buffer-size (:width 7))
	;; 			(ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	;; 			(ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	;; 			(ivy-rich-switch-buffer-project (:width 15 :face success))
	;; 			(ivy-rich-switch-buffer-path
	;; 			   (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
	;; 		        :predicate (lambda (cand) (get-buffer cand)))
	;; 		  ivy-switch-buffer-other-window
	;; 		  (:columns
	;; 		   ((ivy-rich-switch-buffer-icon :width 2)
	;; 			(ivy-rich-candidate (:width 30))
	;; 			(ivy-rich-switch-buffer-size (:width 7))
	;; 			(ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	;; 			(ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	;; 			(ivy-rich-switch-buffer-project (:width 15 :face success))
	;; 			(ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))) :predicate (lambda (cand) (get-buffer cand)))
	;; 		  counsel-find-file
	;; 		  (:columns
	;; 		   ((ivy-read-file-transformer)
	;; 			(ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
	;; 		  counsel-M-x
	;; 		  (:columns
	;; 		   ((counsel-M-x-transformer (:width 40))
	;; 			(ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
	;; 		  counsel-recentf 
	;; 		  (:columns
	;; 		   ((ivy-rich-candidate (:width 0.8))
	;; 			(ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return last modified time of the file
	;; 		  counsel-describe-function 
	;; 		  (:columns
	;; 		   ((counsel-describe-function-transformer (:width 40))
	;; 			(ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the function
	;; 		  counsel-describe-variable 
	;; 		  (:columns
	;; 		   ((counsel-describe-variable-transformer (:width 40))
	;; 			(ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))) ; return docstring of the variable
	;; 		  ))
	;;   :hook (ivy-rich-mode . (lambda () (setq ivy-virtual-abbreviate (or (and ivy-rich-mode 'abbreviate) 'name))))
	;;   )
	
	;; 美化 到处飘，晃眼。
	;; (use-package ivy-posframe-mode
	;;   ;; :disabled
	;;   :ensure t 
	;;   :init (ivy-posframe-mode 1)
	;;   :custom
	;;   (ivy-posframe-parameters
	;;    '((left-fringe . 8) (right-fringe . 8)))
	;;   (ivy-posframe-display-functions-alist
	;;    '((t . ivy-posframe-display-at-frame-center)))
	;;   )
	)
  )


;;;###autoload
;; (defun ivy-telega-chat-highlight (chat)
;;   (let ((unread (funcall (telega--tl-prop :unread_count) chat))
;;         (title (telega-chat-title chat 'with-identity))
;;         (not-muted-p (not (telega-chat-muted-p chat)))
;;         (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

;;     (if (and not-muted-p (> (+ unread mentions) 0))
;;         (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
;;       title)))

;;;###autoload
;; (defun ivy-telega-chat-with ()
;;   "Starts chat with defined peer"
;;   (interactive)

;;   (telega t)
;;   (let ((chats (mapcar
;;                 (lambda (x) (cons (ivy-telega-chat-highlight x) x))
;;                 (telega-filter-chats telega--ordered-chats 'all))))
;;     (ivy-read "chat: " chats
;;               :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
;;               :caller 'ivy-telega-chat-with)))

;; (bind-key "t c" #'ivy-telega-chat-with leader-key)

;; (setq telega-completing-read-function 'ivy-completing-read)

(provide 'init-ivy)
;;; init-ivy.el ends here
