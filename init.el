;;; package --- Summary:

;;; Commentary:

;; load Emacs 24's package system.  Add MELPA repository.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:
(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;; A reload method to easily reload this file in a live emacs session.
(defun reload () "Reloads Emacs config interactively."
(interactive)
(load "~/.emacs.d/init.el"))

;; setup M-x alternatives based on ergo-emacs recomendations
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;; DASHBOARD STUFF:

;; get rid of splash screen and use dashboard package
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "“Most of you are familiar with the virtues of a programmer. There are three, of course: laziness, impatience, and hubris.” - Larry Wall")
;; Set the banner
(setq dashboard-startup-banner 'official)
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png which displays whatever image you would prefer

;; set dashboard items
(setq dashboard-items '((recents  . 8)
                        (bookmarks . 8)
                        (projects . 8)))



;; WEBDEV STUFF:

;; use web-mode for .jsx and .js files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq js-curly-indent-offset 2)
  (setq js-expr-indent-offset 2)
  (setq js-paren-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq js-square-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq standard-indent 2)
  (setq js-indent-level 2)
  (setq sgml-basic-offset 2)
  (setq sgml-basic-offset 2)
  (setq js2-basic-offset 2))


(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; Prettify Stuff
;; https://github.com/prettier/prettier-emacs

(require 'prettier-js)

;; WARNING: this'll autoformat files on save, be careful!
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.js?\\'" . prettier-js-mode))))

(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))





;; Cheatsheet STUFF:

(require 'cheatsheet)

(global-set-key "\C-cc" 'cheatsheet-show)

(cheatsheet-add-group 'Built-in
                      '(:key "C-x (" :description "start defining a keyboard macro")
                      '(:key "C-x )" :description "stop defining a keyboard macro"))


(cheatsheet-add-group 'Custom
                      '(:key "M-<right>" :description "dumb-jump-go")
                      '(:key "M-<left>" :description "dumb-jump-back")
                      '(:key "C-c g" :description "goto line")
                      '(:key "C-x g" :description "open magit"))


(global-set-key "\C-cg" 'goto-line)

(cheatsheet-add-group 'Cheatsheet
                      '(:key "C-c c" :description "show cheat sheet")
                      '(:key "C-q" :description "close cheat sheet"))

(cheatsheet-add-group 'Projectile
                      '(:key "C-c p f" :description "show all files in project")
                      '(:key "C-c p p" :description "show all projects"))






;; MISC:

;; Use Ctrl-c g to go to a line
(global-set-key "\C-cg" 'goto-line)

;; because tabs are evil and we all know it
(setq-default indent-tabs-mode nil)

;; get rid of that stupid bell sound
(setq ring-bell-function 'ignore)

;; fuck trailing whitespace, amma right? amma right?!?
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; move between windows with Shift+arrow
(windmove-default-keybindings)

;; get rid of the toolbar
(tool-bar-mode -1)

;; get rid of scroll bar
(scroll-bar-mode -1)

;; number lines, currently off but sometimes I feel like having it
(global-linum-mode -1)

;; make yes or no into y or n always
(defalias 'yes-or-no-p 'y-or-n-p)

;; magit key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; get rid of those nasty backup files.
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups"))

;; make company-mode aka autocomplete work in every buffer automatically
(add-hook 'after-init-hook 'global-company-mode)
;; make company case-sensitive
(setq company-dabbrev-downcase nil)

;; get projectile up and running
(projectile-global-mode)
(setq projectile-project-search-path '("~/code/"))

;; enable key bindings for dumb-jump mode
(dumb-jump-mode)
;; add personal key bindings since some of the defaults don't appear to work
(global-set-key (kbd "M-<right>") 'dumb-jump-go)
(global-set-key (kbd "M-<left>") 'dumb-jump-back)
;; source: https://github.com/jacktasia/dumb-jump

;; move lines up and down easily
;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Yasnippet stuff
(add-to-list 'load-path
                "~/.emacs.d/snippets")
   (require 'yasnippet)
   (yas-global-mode 1)(add-to-list 'load-path
                "~/.emacs.d/snippets")
   (require 'yasnippet)
   (yas-global-mode 1)

;; Custom vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (birds-of-paradise-plus)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" default)))
 '(dumb-jump-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(json-reformat:indent-width 2)
 '(magit-diff-use-overlays nil)
 '(nil nil t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (prettier-js solarized-theme rjsx-mode cheatsheet free-keys dumb-jump theme-changer birds-of-paradise-plus-theme elpy add-node-modules-path exec-path-from-shell js2-mode json-mode dashboard page-break-lines company rainbow-mode projectile web-mode yasnippet multi-term markdown-mode magit flycheck)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
