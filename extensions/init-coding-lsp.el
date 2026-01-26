;;; init-coding-lsp.el --- Lightweight Eglot integration -*- lexical-binding: t; -*-
(require 'eglot)
(require 'leader-key)

(defcustom suk/coding-auto-start-eglot t
  "Start Eglot automatically when a supported language server exists."
  :type 'boolean :group 'suk)

(defun suk/coding-find-executable (name)
  "Find executable NAME, including user-level Python scripts on Windows."
  (or (executable-find name)
      (when-let ((appdata (and (eq system-type 'windows-nt) (getenv "APPDATA"))))
        (car (file-expand-wildcards
              (expand-file-name
               (format "Python/Python*/Scripts/%s.exe" name) appdata))))))

(defun suk/coding-language-server ()
  "Return the language server executable for the current buffer."
  (cond
   ((derived-mode-p 'java-mode) (suk/coding-find-executable "jdtls"))
   ((derived-mode-p 'go-mode) (suk/coding-find-executable "gopls"))
   ((derived-mode-p 'python-mode)
    (seq-some #'suk/coding-find-executable
              '("basedpyright-langserver" "pyright-langserver"
                "pylsp" "jedi-language-server")))))

(defun suk/coding-register-eglot-servers ()
  "Register installed language servers using their absolute paths."
  (when-let ((server (suk/coding-find-executable "jdtls")))
    (add-to-list 'eglot-server-programs `((java-mode) ,server)))
  (when-let ((server (suk/coding-find-executable "gopls")))
    (add-to-list 'eglot-server-programs `((go-mode) ,server)))
  (when-let ((server (or (suk/coding-find-executable "basedpyright-langserver")
                         (suk/coding-find-executable "pyright-langserver"))))
    (add-to-list 'eglot-server-programs
                 `((python-mode) ,server "--stdio"))))
(suk/coding-register-eglot-servers)

(defun suk/coding-eglot-ensure ()
  "Start Eglot when this buffer has an installed language server."
  (when (and suk/coding-auto-start-eglot (suk/coding-language-server))
    (eglot-ensure)))

(defun suk/coding-eglot-start ()
  "Start Eglot or explain which language server is missing."
  (interactive)
  (if-let ((server (suk/coding-language-server)))
      (progn (message "Starting Eglot with %s" server) (eglot-ensure))
    (user-error "No language server found; install jdtls, gopls, basedpyright, pyright, pylsp, or jedi-language-server")))

(dolist (hook '(java-mode-hook go-mode-hook python-mode-hook))
  (add-hook hook #'suk/coding-eglot-ensure))

(with-eval-after-load 'leader-key
  (keymap-set leader-key-map "l l" #'suk/coding-eglot-start)
  (keymap-set leader-key-map "l r" #'eglot-rename)
  (keymap-set leader-key-map "l a" #'eglot-code-actions)
  (keymap-set leader-key-map "l f" #'eglot-format-buffer)
  (keymap-set leader-key-map "l d" #'xref-find-definitions)
  (keymap-set leader-key-map "l R" #'xref-find-references)
  (keymap-set leader-key-map "l s" #'consult-eglot-symbols)
  (keymap-set leader-key-map "l e" #'flymake-show-buffer-diagnostics))
(provide 'init-coding-lsp)
