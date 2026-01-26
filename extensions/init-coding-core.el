;;; init-coding-core.el --- Language defaults for coding -*- lexical-binding: t; -*-
(require 'project)
(require 'seq)

(defun suk/coding-project-root ()
  "Return the current project root or `default-directory'."
  (if-let ((project (project-current nil))) (project-root project) default-directory))

(defun suk/coding-project-file (name)
  "Return NAME in the current project when it exists."
  (let ((file (expand-file-name name (suk/coding-project-root))))
    (and (file-exists-p file) file)))

(defun suk/coding-executable-command (&rest names)
  "Return the first executable command found among NAMES."
  (or (seq-some (lambda (name)
                  (when-let ((path (executable-find name)))
                    (shell-quote-argument path))) names)
      (car names)))

(defun suk/coding-maven-command ()
  "Return the project Maven wrapper or the system Maven command."
  (cond ((suk/coding-project-file "mvnw.cmd")
         (shell-quote-argument (suk/coding-project-file "mvnw.cmd")))
        ((suk/coding-project-file "mvnw")
         (shell-quote-argument (suk/coding-project-file "mvnw")))
        (t (suk/coding-executable-command "mvn"))))

(defun suk/coding-project-kind ()
  "Return the current project's supported build kind."
  (cond ((suk/coding-project-file "pom.xml") 'maven)
        ((suk/coding-project-file "go.mod") 'go)
        ((or (suk/coding-project-file "pyproject.toml")
             (suk/coding-project-file "setup.py")
             (suk/coding-project-file "setup.cfg")) 'python)))

(defun suk/coding-build-command ()
  "Return the build command for the current project."
  (pcase (suk/coding-project-kind)
    ('maven (format "%s -DskipTests package" (suk/coding-maven-command)))
    ('go "go build ./...")
    ('python (format "%s -m compileall ." (suk/coding-executable-command "python3" "python")))
    (_ (suk/coding-default-compiler-command))))

(defun suk/coding-test-command ()
  "Return the test command for the current project."
  (pcase (suk/coding-project-kind)
    ('maven (format "%s test" (suk/coding-maven-command)))
    ('go "go test ./...")
    ('python (format "%s -m unittest discover" (suk/coding-executable-command "python3" "python")))
    (_ (suk/coding-default-compiler-command))))

(defun suk/coding-default-compiler-command ()
  "Guess a reasonable compile command for the current buffer."
  (cond
   ((derived-mode-p 'python-mode)
    (if (buffer-file-name)
        (format "%s %s" (suk/coding-executable-command "python3" "python")
                (shell-quote-argument (buffer-file-name)))
      (suk/coding-executable-command "python3" "python")))
   ((derived-mode-p 'go-mode) "go test ./...")
   ((derived-mode-p 'java-mode)
    (if (suk/coding-project-file "pom.xml")
        (format "%s test" (suk/coding-maven-command))
      (if (and (buffer-file-name) (file-name-extension (buffer-file-name)))
          (format "javac %s" (shell-quote-argument
                               (file-name-nondirectory (buffer-file-name))))
        "javac")))
   ((derived-mode-p 'js-mode 'js-ts-mode 'js2-mode 'web-mode)
    (if (buffer-file-name)
        (format "%s %s" (or (executable-find "node") "node")
                (shell-quote-argument (buffer-file-name))) "node"))
   (t "make -k")))

(defun suk/coding-buffer-common-setup ()
  "Common editing defaults for programming buffers."
  (display-line-numbers-mode 1)
  (setq-local show-trailing-whitespace t)
  (setq-local compile-command (suk/coding-test-command))
  (subword-mode 1)
  (flymake-mode 1))

(defun suk/coding-python-setup ()
  "Python defaults."
  (setq-local python-indent-offset 4)
  (setq-local python-shell-interpreter
              (or (executable-find "python3") (executable-find "python")
                  python-shell-interpreter))
  (suk/coding-buffer-common-setup))

(defun suk/coding-go-setup ()
  "Go defaults."
  (setq-local tab-width 4)
  (suk/coding-buffer-common-setup)
  (when (and (executable-find "gofmt") (fboundp 'gofmt-before-save))
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(defun suk/coding-java-setup ()
  "Java defaults."
  (setq-local c-basic-offset 4)
  (suk/coding-buffer-common-setup))

(defun suk/coding-web-setup ()
  "Web defaults."
  (setq-local css-indent-offset 2)
  (setq-local js-indent-level 2)
  (suk/coding-buffer-common-setup))

(add-hook 'prog-mode-hook #'suk/coding-buffer-common-setup)
(add-hook 'python-mode-hook #'suk/coding-python-setup)
(add-hook 'go-mode-hook #'suk/coding-go-setup)
(add-hook 'java-mode-hook #'suk/coding-java-setup)
(add-hook 'web-mode-hook #'suk/coding-web-setup)
(add-hook 'js-mode-hook #'suk/coding-web-setup)
(add-hook 'js2-mode-hook #'suk/coding-web-setup)
(add-hook 'js-ts-mode-hook #'suk/coding-web-setup)
(provide 'init-coding-core)
