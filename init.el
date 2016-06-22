;; Add melpa and marmalade to package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(tool-bar-mode -1)

(require 'flycheck)

(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (file-executable-p eslint)
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook 'flycheck-mode)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("jsx" . "\\.es6\\'")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

'(nuke-trailing-whitespace-always-major-modes
   (quote
    (web-mode jsx-mode js2-mode html-mode objc-mode ada-mode c++-mode c-mode change-log-mode emacs-lisp-mode fortran-mode latex-mode lisp-interaction-mode lisp-mode makefile-mode nroff-mode perl-mode plain-tex-mode prolog-mode scheme-mode sgml-mode tcl-mode slitex-mode sml-mode texinfo-mode python-mode)) t)

;; move between windows with Shift+arrow
(windmove-default-keybindings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(exec-path-from-shell-initialize)

(setq-default indent-tabs-mode nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; magit key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;;Cheat-Sheet (TODO: figure out how to make a command do this for me)
(cheatsheet-add :group "Git"
                :key "C-x g"
                :description "open magit mode")

(cheatsheet-add :group "Window"
                :key "C-x 0"
                :description "delete the selected window")

(cheatsheet-add :group "Window"
                :key "C-x +"
                :description "make all windows the same height")

(cheatsheet-add :group "Window"
                :key "C-x -"
                :description "minimize buffer (according to lines)")
