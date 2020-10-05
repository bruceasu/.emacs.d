;; Emacs下的音乐播放器，自带一个函数将~/Music下的所有音乐导入进Bongo再自动播放(bongo-init)
(use-package 
  bongo
  :ensure t
  :hook (after-init . bongo)
  :custom
  (bongo-mode-line-icon-size 10)
  :config
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-global-lastfm-mode nil)
  (defun bongo-init () 
            (interactive) 
            (let ((buffer (current-buffer))) 
              (bongo) 
              (setq bongo-insert-whole-directory-trees "ask") 
              (bongo-insert-file "~/Music") 
              (bongo-insert-enqueue-region (point-min) 
                                           (point-max)) 
              (bongo-play-random) 
              (switch-to-buffer buffer)))
  :bind (:map leader-key
              ("b RET" . 'bongo-dwim) 
              ("b i" . 'bongo-init) 
              ("b x" . 'bongo-kill-region) 
              ("b d" . 'bongo-kill-line) 
              ("b _" . 'bongo-undo) 
              ("b SPC" . 'bongo-pause/resume) 
              ("b TAB" . 'bongo-toggle-collapsed) 
              ("b h" . 'bongo-seek-backward-10) 
              ("b l" . 'bongo-seek-forward-10) 
              ("b a" . 'bongo-insert-enqueue) 
              ("b n" . 'bongo-play-next) 
              ("b p" . 'bongo-play-previous) 
              ("b r" . 'bongo-play-random) 
              ("b s" . 'bongo-sprinkle)))

(provide 'init-music)
