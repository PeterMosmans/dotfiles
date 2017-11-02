;;; init.el --- Emacs initialization file

;; Copyright (c) 2011-2017 Peter Mosmans

;; Author: Peter Mosmans <support AT go-forward.net>
;; Created: 2011
;; Version: (see git tag)
;; Keywords: emacs, dotfile, config

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Customized Emacs initialization file

;;; Code:

;; The following variables are recognized, and can be added to
;; custom-file :
;; my-bibliographies
;; my-font
;; my-capture-file
;; my-dayplanner-file
;; my-org-directory
;; my-theme
;; my-wordnet-dictionary
;; my-wordnet-program
;; start-with-agenda
;;
;; Note that the defaults will be used if the variables aren't defined
(defgroup my-customizations nil
  "Customization parameters used for startup."
  :group 'startup)
(defcustom my-bibliographies nil
  "A file or list of BibTex files used for bibliographies."
  :type 'string
  :group 'my-customizations)
(defcustom my-font "Source Code Pro"
  "Font that will be used (if it is installed)."
  :type 'string
  :group 'my-customizations)
(defcustom my-org-directory "~/org"
  "(Non-standard) org-directory."
  :type 'directory
  :group 'my-customizations)
(defcustom my-capture-file (concat my-org-directory  "/capture-org")
  "Default 'org-mode' capture file."
  :type 'file
  :group 'my-customizations)
(defcustom my-dayplanner-file (concat my-org-directory "/dayplanner.org")
  "Default 'org-mode' dayplanner file."
  :type 'file
  :group 'my-customizations)
(defcustom my-scratch-file "~/scratch.txt"
  "Persistent scratch file which is opened on startup."
  :type 'file
  :group 'my-customizations)
(defcustom my-snippets-dir nil
  "A list of snippet directories that will be loaded by yasnippet."
  :type 'string
  :group 'my-customizations)
(defcustom my-theme 'misterioso
  "Theme that will be applied when starting up."
  :type 'string
  :group 'my-customizations)
(defcustom my-wordnet-dictionary nil
  "Path to wordnet dictionary."
  :type 'string
  :group 'my-customizations)
(defcustom my-wordnet-program nil
  "Location of wordnet executable."
  :type 'string
  :group 'my-customizations)
(defvar my-colors nil
  "Customizable re-usable colors, partially  extracted from the current theme and applied to tabbar and powerline.")

;; Use custom-file to store all customizations
;; (including the aforementioned parameters)
(setq custom-file "~/.emacs.d/variables.el")

(if (file-exists-p custom-file)
    (load custom-file))

;; uncomment for some debugging and verbose (load time) options
;; (setq debug-on-error t)
;; uncomment for package loading times
;; (setq use-package-verbose t)
(package-initialize)

;; Bootstrap `use-package'
(defun my-package-install-refresh-contents (&rest args)
  "Refresh package list before trying to install a new package."
  (message "Refreshing packages list before trying to install a new package.")
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))

(advice-add 'package-install :before 'my-package-install-refresh-contents)

(unless (package-installed-p 'use-package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Load this keybinding first to facilitate editing init.el
(global-set-key (kbd "M-<f11>") (lambda () (interactive) (find-file user-init-file)))
(setq gc-cons-threshold 500000000)     ;; improve startup time
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold 800000)
                       (helm-mode t)
                       (require 'server)
                       (or (server-running-p) ;; start server if not already running
                           (server-start))))

;; add this first, as some packages need to be installed from unstable sources
(use-package package
  :config
  (setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))))

;; define all necessary EXTERNAL alphabetically
;; bind:      keybindings (all keys before :map are bound globally)
;; commands:  load the package (execute config) when these commands are executed
;; config:    execute code *after* a package is loaded
;; defer:     defer loading (implied when using commands, bind or mode)
;; disabled:  (temporarily) disable a package
;; ensure:    make sure the package is installed
;; idle:      delay steps until Emacs is idle (before or after package loading)
;; init:      always execute code *before* a package is loaded
;; load-path: path of the files for local packages
;; mode:      deferred binding
;; pin:       pin to a specific repository

(use-package aggressive-indent
  :defer t
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  )

(use-package bm
  :bind (("C-<f2>" . bm-toggle)
         ("M-<f2>" . bm-next))
  :defer t
  :ensure t
  )

(use-package company
  :config
  (defvar company-mode/enable-yas t "Enable yasnippet for all back ends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)
        company-idle-delay 2
        company-transformers '(company-sort-by-occurrence))
  :defer t
  :ensure t
  :init (global-company-mode)
  )

(use-package company-quickhelp
  :config (setq company-quickhelp-delay 1)
  :defer t
  :ensure t
  :init (company-quickhelp-mode 1)
  )

(use-package company-jedi
  :defer t
  :ensure t
  )

(use-package company-restclient
  :config (add-to-list 'company-backends 'company-restclient)
  :defer t
  :ensure t
  )

(use-package dash
  :defer t
  )

(use-package elpy
  :config
  (setq elpy-rpc-backend "jedi"
        python-shell-completion-native-enable nil)
  (elpy-enable)
  :defer t
  :ensure t
  )

(use-package eyebrowse
  :disabled t
  :init (eyebrowse-mode t)
  )

(use-package fill-column-indicator
  :commands fci-mode
  :config
  (setq fci-rule-color "light slate grey"
        fci-always-use-textual-rule t)
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'markdown-mode-hook 'fci-mode)
  )

(use-package flycheck
  :config
  (setq flycheck-highlighting-mode 'lines) ;; highlight whole line
  (global-flycheck-mode)
  :defer t
  :ensure t
  )

(use-package flymd
  :commands flymd-flyit
  :ensure t
  )

(use-package focus
  :commands focus-mode
  :ensure t
  )

(use-package git-timemachine
  :defer t
  :ensure t
  )

(use-package helm
  :bind (("C-c j" . helm-imenu)        ;; J ump to imenu
         ("C-c y" . helm-show-kill-ring)
         ([M-x] . helm-M-x)
         ([M-f5] . helm-find-files)
         ([M-f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)
         ([capslock] . helm-M-x))
  :config
  (helm-autoresize-mode t)
  (setq helm-buffers-truncate-lines nil)
  :defer t
  :ensure t
  )

(use-package helm-ag
  :commands helm-ag
  :defer t
  :ensure t
  )

(use-package helm-bibtex
  :bind (("C-c b" . helm-bibtex))
  :config
  (if (boundp 'my-bibliographies)
      (setq bibtex-completion-bibliography my-bibliographies))
  (setq bibtex-autokey-year-length 4 ;; use 4 digits for the year
        bibtex-completion-pdf-field "file" ;; Use the file field to locate PDF files
        bibtex-completion-notes-path (concat my-org-directory "/bibtex-notes.org"))
  :defer t
  )

(use-package helm-flyspell
  :bind (("C-;" . helm-flyspell-correct))
  :commands flyspell-mode
  :init
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-;") nil))
  :defer t
  :ensure t
  )

(use-package helm-make
  :bind (("C-c m" . helm-make-projectile))
  )

(use-package helm-org-rifle
  :commands helm-org-rifle
  :ensure t
  )

(use-package helm-projectile
  :bind (([f5] . helm-projectile-find-file)
         ([f10] . helm-projectile-switch-to-buffer)
         ([C-f10] . helm-projectile-switch-project))
  :commands helm-projectile
  :ensure t
  )

(use-package helm-tramp
  :defer t
  :ensure t
  )

(use-package helm-wordnet
  :bind ("C-c w" . helm-wordnet-suggest)
  :config
  (setq helm-wordnet-wordnet-location my-wordnet-dictionary
        helm-wordnet-prog my-wordnet-program)
  :defer t)

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "light slate grey")
  (set-face-background 'highlight-indentation-current-column-face "light slate grey")
  :ensure t
  )

(use-package imenu-list
  :bind ("C-c i" . imenu-list-minor-mode)
  :config (setq imenu-list-position 'left)
  :defer t
  :ensure t
  )

(use-package let-alist
  :defer t
  )

(use-package magit
  :bind (([f1] . magit-status)
         ("C-x g" . magit-status))
  :config
  (setq magit-diff-auto-show nil)
  :defer t
  :ensure t
  )

(use-package markdown-mode
  :defer t
  :ensure t
  )

(use-package mode-icons               ;; show pretty icons on the modeline
  :config (mode-icons-mode)
  (setq mode-icons-desaturate-inactive t  ;; disable automatic coloring
        mode-icons-grayscale-transform t) ;; "re"color black and white images
  :ensure t
  )

(use-package neotree
  :bind (("M-<f8>" . neotree-toggle)
         ("C-c n" . neotree-toggle))
  :commands neotree-toggle
  :config
  (setq  neo-show-hidden-files t
         neo-theme 'ascii              ;; Don't use fancy icons
         neo-window-width 30)
  :ensure t
  )

(use-package nov                       ;; Read epub in Emacs
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :defer t
  )

(use-package ob-async                  ;; Asynchronous execution of org-babel src
  :config
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)
  :defer t
  :disabled t
  :ensure t
  )

(use-package org-ref
  :disabled t
  :ensure t
  )

(use-package org-wc                    ;; Count words in org mode documents
  :bind ("C-c w" . my-org-wc-toggle-overlay)
  :defer t
  :ensure t
  )

(use-package powerline
  :init
  (setq display-time-default-load-average nil ;; hide load average
        display-time-format "%H:%M"
        display-time-24hr-format t
        powerline-default-separator 'arrow)
  (display-time)
  (defface powerline-bold
    '((t :inherit powerline-block1 :bold t))
    "Active powerline block 3 (clock)" :group 'powerline)
  (defface powerline-inactive-bold
    '((t :inherit powerline-inactive-block1))
    "Inactive powerline" :group 'powerline)
  (defface powerline-alert nil
    "Active powerline block 4 (warning)" :group 'powerline)
  (defface powerline-inactive-alert
    `((t :inherit powerline-alert))
    "Inactive powerline" :group 'powerline)
  ;; 'design' own theme - see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
  (setq-default mode-line-format  '
                ("%e"
                 (:eval
                  (let*
                      ((active
                        (powerline-selected-window-active))
                       (mode-line-buffer-id
                        (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                       (mode-line
                        (if active 'mode-line-active 'mode-line-inactive))
                       (face1
                        (if active 'powerline-active1 'powerline-inactive1))
                       (face2
                        (if active 'powerline-active2 'powerline-inactive2))
                       (bold-face
                        (if active 'powerline-bold 'powerline-inactive-bold))
                       (alert-face
                        (if active 'powerline-alert 'powerline-inactive-alert))
                       (separator-left
                        (intern
                         (format "powerline-%s-%s"
                                 (powerline-current-separator)
                                 (car powerline-default-separator-dir))))
                       (separator-right
                        (intern
                         (format "powerline-%s-%s"
                                 (powerline-current-separator)
                                 (cdr powerline-default-separator-dir))))
                       (lhs            ;; left hand side
                        (list
                         (powerline-raw "%3l" face1 'l)   ;; line number
                         (powerline-raw ":" face1)
                         (powerline-raw "%3C" face1 'r)   ;; 1-based column
                         (powerline-raw mode-line-modified face1)
                         (when powerline-display-buffer-size
                           (powerline-buffer-size face1 'l))
                         (when powerline-display-mule-info
                           (powerline-raw mode-line-mule-info face1 'l))
                         (funcall separator-left face1 face2)
                         (when
                             (and
                              (boundp 'which-func-mode)
                              which-func-mode)
                           (powerline-raw which-func-format face2 'l))
                         (when
                             (and
                              (boundp 'erc-track-minor-mode)
                              erc-track-minor-mode)
                           (powerline-raw erc-modified-channels-object face2 'l))
                         (powerline-major-mode face2 'l)
                         (powerline-minor-modes face2 'l)
                         (powerline-process face2)
                         ;; (powerline-narrow face2 'l)
                         (funcall separator-left face2 face1)
                         (powerline-buffer-id face1) ;; buffer name

                         (funcall separator-left face1 face2)
                         (powerline-vc face2 'r)
                         (when
                             (bound-and-true-p nyan-mode)
                           (powerline-raw
                            (list
                             (nyan-create))
                            face2 'l))))
                       (rhs            ;; right hand side
                        (list
                         (funcall separator-right face2 bold-face)
                         (unless window-system
                           (powerline-raw
                            (char-to-string 57505)
                            bold-face 'l))
                         (funcall separator-right face2 bold-face)
                         (when (boundp 'eyebrowse-mode-line-separator)
                           (powerline-raw (eyebrowse-mode-line-indicator) face1))
                         (if (boundp 'org-mode-line-string)
                             (powerline-raw (propertize org-mode-line-string 'face face1) face1)
                           (powerline-raw "NOT CLOCKED IN" alert-face))
                         (when powerline-display-hud
                           (powerline-hud bold-face face1)
                           (powerline-raw (concat " " display-time-string) bold-face 'r)
                           ))))
                    (concat
                     (powerline-render lhs)
                     (powerline-fill face2
                                     (powerline-width rhs))
                     (powerline-render rhs))))))
  :ensure t
  )

(use-package projectile
  :config
  (setq projectile-completion-system 'helm
        projectile-globally-ignored-file-suffixes '(".avi" ".fo" ".jpg" ".mp4"
                                                    ".pdf" ".png" ".pptx" ".svg"
                                                    ".xlsx" ".zip")
        projectile-globally-ignored-directories '("Include" "Lib" "Scripts")
        projectile-indexing-method 'alien  ;; use the fastest indexing method
        projectile-mode-line '(:eval
                               (if
                                   (file-remote-p default-directory)
                                   " Projectile"
                                 (format " [%s]"
                                         (projectile-project-name)))))
  (helm-projectile-on)
  (projectile-mode 1)
  :commands projectile-mode
  :ensure t
  :init (put 'projectile-project-name 'safe-local-variable #'stringp)
  )

(use-package pylint
  :defer t
  :ensure t
  )

(use-package rainbow-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-mode)
  )

(use-package restclient
  :commands restclient-mode
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (when (fboundp 'company-mode)
                                              (company-mode))))
  :ensure t
  )

(use-package restclient-helm
  :defer t
  :ensure t
  )

(use-package tabbar
  :bind (("C-<tab>" . tabbar-forward)
         ("C-S-<tab>" . tabbar-backward)
         ("C-S-<iso-lefttab>" . tabbar-backward)
         ("M-<down>" . tabbar-forward-group)
         ("M-<up>" . tabbar-backward-group)
         ("<f8>" . tabbar-backward)
         ("S-<f8>" . tabbar-backward-group)
         ("<f9>" . tabbar-forward)
         ("S-<f9>" . tabbar-forward-group))
  :ensure t
  :init
  (tabbar-mode t)                      ;; enable the tabbar by default
  )

(use-package web-mode
  :defer t
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook (lambda ()
                             (auto-fill-mode)
                             (when (fboundp 'flyspell-mode)
                               (flyspell-mode))))
  :mode (("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.xml\\'" . web-mode))
  :pin melpa-stable
  )

(use-package which-key
  :defer t
  :ensure t
  :init
  (which-key-mode)
  )

(use-package yafolding
  :bind (("C-|" . yafolding-toggle-element)
         ("C-\\" . yafolding-toggle-all))
  :defer t
  :disabled t
  :ensure t
  :init (add-hook 'prog-mode-hook 'yafolding-mode)
  )

(use-package yaml-mode
  :defer t
  :ensure t
  ;; yaml is a major mode, not based on prog-mode, so manually add modes
  :init
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  )

(use-package yasnippet
  :commands yas-minor-mode
  :config
  (if (boundp 'my-snippet-dirs)
      (dolist (item my-snippet-dirs)   ;; add item per item
        (add-to-list 'yas-snippet-dirs item)))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'bibtex-mode-hook #'yas-minor-mode)
  :ensure t
  )

;; define font for Unicode Private Use Area block
(when (member "Symbol" (font-family-list))
  (set-fontset-font "fontset-default" '(#xf000 . #xffff) (font-spec :name "Symbol")))
(setq
 column-number-indicator-zero-based nil
 compilation-ask-about-save nil
 compile-command "make "
 compilation-read-command nil
 display-time-world-list
 (quote
  (("AST-10AEST" "BNE")
   ("CET-1CET" "AMS")
   )))

;; OS-specific settings
(if (string= system-type "windows-nt")
    (setq w32-enable-caps-lock nil     ;; free the capslock key for useful stuff
          explicit-shell-file-name "c:/programs/msys2/usr/bin/zsh.exe"))

;; generic settings
(setq-default fill-column 80           ;; width of the screen for wrapping
              line-spacing 0
              indent-tabs-mode nil)    ;; always use spaces for indentation
(setq
 auto-save-interval 1000               ;; automatically save after x characters
 bookmark-default-file "~/.emacs.d/bookmarks.emacs"
 column-number-mode t                  ;; show column-number
 comint-prompt-read-only t             ;; read only prompt for shell mode
 completion-ignore-case t              ;; ignore case when completing
 dired-listing-switches "-agoh"
 ediff-window-setup-function 'ediff-setup-windows-plain
 global-font-lock-mode 1               ;; syntax highlighting on by default
 global-hl-line-mode 1                 ;; highlight current line by default
 global-visual-line-mode 1             ;; act on visual lines, enable word wrap
 inhibit-compacting-font-caches t      ;; speed up displaying Unicode glyphs
 inhibit-startup-echo-area-message nil
 inhibit-startup-message t             ;; remove welcome message
 kill-whole-line t                     ;; kill whole line including newline
 line-spacing nil
 make-backup-files nil                 ;; do not create backups
 message-log-max t                     ;; keep and log all messages
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 read-file-name-completion-ignore-case t
 recentf-max-menu-items 15             ;; show maximum x recent menu items
 recentf-max-saved-items 100           ;; save maximum x recent files
 recentf-save-file "~/.emacs.d/recentfiles.emacs"
 rst-preferred-adornments '((35 over-and-under 0) ;; # H1 (once per document)
                            (42 over-and-under 0) ;; * H2
                            (61 simple 0)         ;; = H3
                            (45 simple 0)         ;; - H4
                            (94 simple 0)         ;; ^ H5
                            (34 simple 0))        ;; " H6
 scroll-conservatively 10000
 scroll-margin 1
 scroll-preserve-screen-position 1
 scroll-step 1
 sentence-end-double-space nil         ;; consider single space a sentence break
 show-paren-delay 0
 show-paren-style 'expression          ;; highlight entire bracket expression
 size-indication-mode nil              ;; disable file size mode
 tab-width 4                           ;; default tab width
 tramp-default-method "sshx"           ;; faster than the default scp
 use-package-always-ensure t           ;; always install missing packages
 whitespace-style (quote
                   (face indentation tabs space-before-tab space-after-tab tab-mark trailing)))
(delete-selection-mode 1)              ;; automatically overwrite selected text
(recentf-mode 1)                       ;; enable recently opened files mode
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)          ;; enable y/n answers to yes/no questions
(tool-bar-mode 0)                      ;; disable toolbar
(global-whitespace-mode 1)             ;; globally enable whitespace mode

;; show week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian
                   (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; org mode settings
(set-face-attribute 'org-done nil :strike-through t)
(set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.1)
(set-face-attribute 'org-level-2 nil :inherit 'outline-1 :height 1.1)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo-list)
(global-set-key (kbd "<f12>") 'open-custom-agenda)
(global-set-key (kbd "S-<f12>") 'my-org-clock-in-everywhere)
(global-set-key (kbd "C-<f12>") 'org-clock-out)
(global-set-key (kbd "M-<f12>") 'my-org-clock-show-list)
(if (file-exists-p my-org-directory)  ;; use my-org-directory if it exists
    (setq org-directory my-org-directory)
  (message "Please specify correct my-org-directory: The directory %s does not exist."
           my-org-directory))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (python . t)
   (shell . t)))

(setq org-default-notes-file my-capture-file
      org-agenda-compact-blocks t      ;; skip long block separators
      org-file-apps (quote             ;; add several file handlers
                     ((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.odt\\'" . system)
                      ("\\.pdf\\'" . default)
                      ("\\.x?html?\\'" . default)))
      org-agenda-custom-commands
      '(("c" "category / tag ordened tasks"
         ((tags "+TODO=\"TODO\""
                (
                 (org-agenda-overriding-header "Ordened by category / tag")
                 (org-agenda-prefix-format " %i %b")
                 (org-agenda-sorting-strategy '(category-up tag-up))
                 ))))
        ("o" "Overview of next 14 days, and all tasks"
         ((agenda "" ((org-agenda-entry-types '(:scheduled :deadline))
                      (org-agenda-ndays 14)
                      (org-agenda-remove-tags t)
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-show-all-dates nil)  ;; hide dates with no appointment
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-start-on-weekday nil)  ;; calendar begins today
                      (org-agenda-use-time-grid nil)
                      ))
          (tags-todo "@online"
                     ((org-agenda-overriding-header "")  ;; don't show header
                      (org-agenda-prefix-format "")  ;; don't show prefix
                      (org-agenda-remove-tags t)  ;; remove tags
                      ))
          (tags "+TODO=\"TODO\""
                ((org-agenda-overriding-header "")
                 (org-agenda-prefix-format " %i")
                 (org-agenda-sorting-strategy '(priority-down category-up tag-up))
                 ))
          ))
        ("p" "Progress" agenda ""
         ((org-agenda-entry-types '(:deadline))
          (org-agenda-show-all-dates nil)  ;; hide dates with no appointment
          (org-agenda-start-on-weekday nil)  ;; calendar begins today
          (org-agenda-use-time-grid nil)  ;; don't show timegrid
          (org-agenda-view-columns-initially t) ;; turn on column view
          (org-deadline-warning-days 14)
          )))
      org-agenda-files (list org-directory) ;; all files in the org-directory
      org-agenda-prefix-format '((agenda . " %i%7e ")  ;; org-agenda
                                 (search . "search %-12:c")
                                 (tags . " %6e %t")
                                 (timeline . "timeline % s")
                                 (todo . " %i%6e"))  ;; org-todo-list
      org-agenda-remove-tags nil
      org-agenda-sorting-strategy
      '((agenda priority-down category-up time-up)
        (todo priority-down category-up todo-state-up priority-down)
        (tags priority-down category-up)
        (search priority-down category-up))
      org-agenda-repeating-timestamp-show-all nil
      org-agenda-todo-keyword-format ""
      org-archive-location (concat "archive/%s." (format-time-string "%Y" (current-time)) ".archive::")
      org-capture-templates
      '(("d" "daily objectives"
         entry (file+olp+datetree my-dayplanner-file)
         "* TODO [#A] Daily objectives for %(org-read-date nil nil \"+0d\") [/]\n  DEADLINE: <%(org-read-date nil nil \"+0d\")>\n  - [ ] %?"
         :tree-type week :unnarrowed t)
        ("w" "weekly objectives"
         entry (file+weektree my-dayplanner-file)
         "* TODO [#A] Weekly objectives (week %<%W>) [/]\n  DEADLINE: <%(org-read-date nil nil \"+7d\")>\n  - [ ] %?")
        ("r" "reminder"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  DEADLINE: <%(org-read-date nil nil \"+1d\")>")
        ("t" "TODO"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  %u")
        ("l" "TODO with link to current buffer"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  %i\n   %a")
        ("r" "reference"
         entry (file+headline (concat org-directory "/reference.org") "reference")
         "* %?\n")
        ("s" "someday/maybe"
         entry (file+headline (concat org-directory "/someday-maybe.org") "someday/maybe")
         "* %?\n  %u"))
      org-catch-invisible-edit 'show-and-error
      org-clock-into-drawer t          ;; log clocking data into drawer
      org-clock-out-remove-zero-time-clocks t ;; remove logdata without time
      org-columns-default-format "#+COLUMNS: %60ITEM(Task) %8Effort(estimate){:} %8CLOCKSUM(clocked){:} %8CLOCKSUM_T(today){:}"
      org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
      org-cycle-separator-lines 0      ;; no empty lines needed between subtrees
      org-hide-leading-stars t         ;; only show one star per heading
      org-fontify-done-headline t      ;; change headline face when marked DONE
      org-global-properties
      '(("Effort_ALL" . "0 0:05 0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))
      org-log-into-drawer t            ;; insert notes & time stamps into drawer
      org-refile-targets '((org-agenda-files :level . 2))
      org-use-speed-commands t         ;; enable speed commands
      org-support-shift-select t       ;; keep using shift as selector
      org-src-fontify-natively t       ;; fontify code in blocks
      org-time-clocksum-format         ;; don't show days
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      org-todo-keywords                ;; ! indicates timestamp, @ note & timestamp
      '((sequence "TODO(t)" "REGISTRATION(r)" "DELEGATED(e)" "|" "CANCELLED(c)" "DONE(d)" ))
      )


;; associate certain files with modes
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.prf\\'" . conf-mode))

;;; KEY BINDINGS
(global-set-key (kbd "<scroll>") 'scroll-lock-mode)

;; miscellaneous (for consistency)
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-(") 'check-parens) ;; matching parens
(global-set-key (kbd "C-=") 'expand-region) ;; make selection bigger and bigger
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-M-t") 'my-insert-current-date-time)
(global-set-key (kbd "C-c k") 'calendar)
(global-set-key (kbd "C-c 1") 'my-titlecase-converter)
;; keybindings
;; f1: magit
(global-set-key (kbd "S-<f1>") 'my-cleanup)
(global-set-key (kbd "C-<f1>") 'show-file-name)
(global-set-key (kbd "M-<f1>") 'code-review-region)

;; navigation in buffer (file)
(global-set-key (kbd "<f2>") 'my-switch-to-previous-buffer)
(global-set-key (kbd "C-c p") 'my-switch-to-previous-buffer)
(global-set-key (kbd "S-<f2>") 'my-insert-current-date-time)

;; searching
(global-set-key (kbd "<f3>") 'isearch-repeat-forward)
(global-set-key (kbd "S-<f3>") 'find-grep-dired)
(global-set-key (kbd "C-<f3>") 'diff)
(global-set-key (kbd "M-<f3>") 'mark-whole-buffer)

;; scratchpad, text modes, closing
;; f4: execute macro (kmacro-end-and-call-macro)
(global-set-key (kbd "S-<f4>") (lambda () (interactive) (switch-to-buffer "scratch.txt")))
(global-set-key (kbd "C-<f4>") 'org-mode)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)

;; buffer (file) operations
(global-set-key (kbd "S-<f5>") 'revert-buffer)
(global-set-key (kbd "C-<f5>") 'save-buffer)

;; windows
(global-set-key (kbd "<f6>") 'other-window)
(global-set-key (kbd "S-<f6>") 'split-window-vertically)
(global-set-key (kbd "C-<f6>") 'delete-other-windows)
(global-set-key (kbd "M-<f6>") 'split-window-horizontally)

;; frames
(global-set-key (kbd "<f7>") 'other-frame)
(global-set-key (kbd "S-<f7>") 'make-frame)
(global-set-key (kbd "C-<f7>") 'delete-frame)
(global-set-key (kbd "M-<f7>") 'balance-windows)

(global-set-key (kbd "C-<f8>") 'open-dired)

;; browsing / exploring
(global-set-key (kbd "C-<f9>") 'browse-url-of-buffer)
(global-set-key (kbd "M-<f9>") 'color-theme-select)

;; bookmarks
(global-set-key (kbd "<f11>") 'bookmark-jump)
(global-set-key (kbd "S-<f11>") 'xah-run-current-file)
(global-set-key (kbd "C-<f11>") 'bookmark-set)

;; open dayplanner file
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file my-dayplanner-file)))


;;; SSH / PUTTY HACKS
(if (eq system-uses-terminfo t)         ;; terminal
    (progn                              ;; PuTTY needs to be in SCO mode
      (xterm-mouse-mode 0)              ;; use mouse even in terminal mode
      (menu-bar-mode 0)                 ;; disable menu bar
      ;;      (mouse-wheel-mode t)            ;; putty incompatibility hack
      (define-key key-translation-map [\e] [\M])
      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])
      (define-key input-decode-map "\e[D" [S-left])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[A" [S-up])
      (define-key input-decode-map "\e[B" [S-down])
      (define-key input-decode-map "\e[C" [S-right])
      (define-key input-decode-map "\e[I" [prior])
      (define-key input-decode-map "\e[G" [next])
      (define-key input-decode-map "\e[M" [f1])
      (define-key input-decode-map "\e[Y" [S-f1])
      (define-key input-decode-map "\e[k" [C-f1])
      (define-key input-decode-map "\e\e[M" [M-f1])
      (define-key input-decode-map "\e[N" [f2])
      (define-key input-decode-map "\e[Z" [S-f2])
      (define-key input-decode-map "\e[l" [C-f2])
      (define-key input-decode-map "\e\e[N" [M-f2])
      (define-key input-decode-map "\e[O" [f3])
      (define-key input-decode-map "\e[a" [S-f3])
      (define-key input-decode-map "\e[m" [C-f3])
      (define-key input-decode-map "\e\e[O" [M-f3])
      (define-key input-decode-map "\e[P" [f4])
      (define-key input-decode-map "\e[b" [S-f4])
      (define-key input-decode-map "\e[n" [C-f4])
      (define-key input-decode-map "\e\e[P" [M-f4])
      (define-key input-decode-map "\e[Q" [f5])
      (define-key input-decode-map "\e[c" [S-f5])
      (define-key input-decode-map "\e[o" [C-f5])
      (define-key input-decode-map "\e\e[Q" [M-f5])
      (define-key input-decode-map "\e[R" [f6])
      (define-key input-decode-map "\e[d" [S-f6])
      (define-key input-decode-map "\e[p" [C-f6])
      (define-key input-decode-map "\e\e[R" [M-f6])
      (define-key input-decode-map "\e[S" [f7])
      (define-key input-decode-map "\e[e" [S-f7])
      (define-key input-decode-map "\e[q" [C-f7])
      (define-key input-decode-map "\e\e[S" [M-f7])
      (define-key input-decode-map "\e[T" [f8])
      (define-key input-decode-map "\e[f" [S-f8])
      (define-key input-decode-map "\e[r" [C-f8])
      (define-key input-decode-map "\e\e[T" [M-f8])
      (define-key input-decode-map "\e[U" [f9])
      (define-key input-decode-map "\e[g" [S-f9])
      (define-key input-decode-map "\e[s" [C-f9])
      (define-key input-decode-map "\e\e[U" [M-f9])
      (define-key input-decode-map "\e[V" [f10])
      (define-key input-decode-map "\e[h" [S-f10])
      (define-key input-decode-map "\e[_" [C-f10])
      (define-key input-decode-map "\e\e[V" [M-f10])
      (define-key input-decode-map "\e[W" [f11])
      (define-key input-decode-map "\e[i" [S-f11])
      (define-key input-decode-map "\e[u" [C-f11])
      (define-key input-decode-map "\e\e[W" [M-f11])
      (define-key input-decode-map "\e[X" [f12])
      (define-key input-decode-map "\e[j" [S-f12])
      (define-key input-decode-map "\e[v" [C-f12])
      (define-key input-decode-map "\e\e[X" [M-f12])))

;;; FUNCTIONS
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; customization: overwrite default function in tabbar.el
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*" "*Completions*" "*Warnings*" "*Bookmark Annotation*"))
     "Common"
     )
    ((memq major-mode
           '(dired-mode nav-mode))
     "Browsing"
     )
    ((memq major-mode
           '(org-mode text-mode))
     "Text"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;; http://comments.gmane.org/gmane.emacs.orgmode/81781
(defun my-org-query-clock-out ()
  "Ask the user before clocking out.
This is a useful function for adding to `kill-emacs-query-functions'."
  (if (and (featurep 'org-clock)
           (funcall 'org-clocking-p)
           (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t))                                ;; only fails on keyboard quit or error

(defmacro ediff-char-to-buftype (arg)
  `(cond ((memq ,arg '(?a ?A)) 'A)
         ((memq ,arg '(?b ?B)) 'B)
         ((memq ,arg '(?c ?C)) 'C)
         ((memq ,arg '(?d ?D)) 'D)
         ))


;; http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-chan
(defun ediff-diff-to-diff (arg &optional keys)
  "Copy buffer-X'th difference region to buffer Y \(X,Y are A, B, or C\).
If numerical prefix argument, copy the difference specified in the arg.
Otherwise, copy the difference given by `ediff-current-difference'.
This command assumes it is bound to a 2-character key sequence, `ab', `ba',
`ac', etc., which is used to determine the types of buffers to be used for
copying difference regions.  The first character in the sequence specifies
the source buffer and the second specifies the target.

If the second optional argument, a 2-character string, is given, use it to
determine the source and the target buffers instead of the command keys."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (or keys (setq keys (this-command-keys)))
  (if (eq arg '-) (setq arg -1)) ; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))

  (let* ((key1 (aref keys 0))
         (key2 (aref keys 1))
         (char1 (ediff-event-key key1))
         (char2 (ediff-event-key key2))
         ediff-verbose-p)
    (ediff-copy-diff ediff-current-difference
                     (ediff-char-to-buftype char1)
                     (ediff-char-to-buftype char2))
    ;; recenter with rehighlighting, but no messages
    (ediff-recenter)))

(defun ediff-copy-D-to-C (arg)
  "Copy ARGth difference region from both buffers A and B to C.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-diff-to-diff arg "dc"))

(defun ediff-copy-diff (n from-buf-type to-buf-type
                          &optional batch-invocation reg-to-copy)
  (let* ((to-buf (ediff-get-buffer to-buf-type))
         ;;(from-buf (if (not reg-to-copy) (ediff-get-buffer from-buf-type)))
         (ctrl-buf ediff-control-buffer)
         (saved-p t)
         (three-way ediff-3way-job)
         messg
         ediff-verbose-p
         reg-to-delete reg-to-delete-beg reg-to-delete-end)

    (setq reg-to-delete-beg
          (ediff-get-diff-posn to-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
          (ediff-get-diff-posn to-buf-type 'end n ctrl-buf))

    (if (eq from-buf-type 'D)
        ;; want to copy *both* A and B
        (if reg-to-copy
            (setq from-buf-type nil)
          (setq reg-to-copy (concat (ediff-get-region-contents n 'A ctrl-buf)
                                    (ediff-get-region-contents n 'B ctrl-buf))))
      ;; regular code
      (if reg-to-copy
          (setq from-buf-type nil)
        (setq reg-to-copy (ediff-get-region-contents n from-buf-type ctrl-buf))))

    (setq reg-to-delete (ediff-get-region-contents
                         n to-buf-type ctrl-buf
                         reg-to-delete-beg reg-to-delete-end))

    (if (string= reg-to-delete reg-to-copy)
        (setq saved-p nil) ; don't copy identical buffers
      ;; seems ok to copy
      (if (or batch-invocation (ediff-test-save-region n to-buf-type))
          (condition-case conds
              (progn
                (ediff-with-current-buffer to-buf
                                           ;; to prevent flags from interfering if buffer is writable
                                           (let ((inhibit-read-only (null buffer-read-only)))
                                             (goto-char reg-to-delete-end)
                                             (insert reg-to-copy)
                                             (if (> reg-to-delete-end reg-to-delete-beg)
                                                 (kill-region reg-to-delete-beg reg-to-delete-end))
                                             ))
                (or batch-invocation
                    (setq
                     messg
                     (ediff-save-diff-region n to-buf-type reg-to-delete))))
            (error (message "ediff-copy-diff: %s %s"
                            (car conds)
                            (mapconcat 'prin1-to-string (cdr conds) " "))
                   (beep 1)
                   (sit-for 2) ; let the user see the error msg
                   (setq saved-p nil)
                   )))
      )

    ;; adjust state of difference in case 3-way and diff was copied ok
    (if (and saved-p three-way)
        (ediff-set-state-of-diff-in-all-buffers n ctrl-buf))

    (if batch-invocation
        (ediff-clear-fine-differences n)
      ;; If diff3 job, we should recompute fine diffs so we clear them
      ;; before reinserting flags (and thus before ediff-recenter).
      (if (and saved-p three-way)
          (ediff-clear-fine-differences n))

      (ediff-refresh-mode-lines)

      ;; For diff2 jobs, don't recompute fine diffs, since we know there
      ;; aren't any.  So we clear diffs after ediff-recenter.
      (if (and saved-p (not three-way))
          (ediff-clear-fine-differences n))
      ;; Make sure that the message about saving and how to restore is seen
      ;; by the user
      (message "%s" messg))
    ))

(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 2 (window-width)))
  (org-agenda-align-tags))

(defun set-bfr-to-utf-8-unix ()
  (interactive)
  (set-buffer-file-coding-system
   'utf-8-unix)
  )

(defun my-apply-colors ()
  "Apply theme colors to tabbar and powerline."
  (interactive)
  (when (boundp tabbar-mode)
    ;; These settings override the default settings hardcoded in tabbar mode
    (set-face-attribute 'tabbar-button nil :inherit 'default :box nil)
    ;; default line without tabs
    (set-face-attribute 'tabbar-default nil :background "black")
    (set-face-attribute 'tabbar-selected nil :inherit 'default :height 0.8 :box nil
                        :weight 'bold
                        :foreground `,(face-attribute 'font-lock-keyword-face :foreground))
    (set-face-attribute 'tabbar-unselected nil :height 0.8
                        :box '(:line-width 1 style: released-button))
    ;; mouse-over
    (set-face-attribute 'tabbar-highlight nil :box '(:line-width 3 :style pressed-button)
                        :height 0.8)
    ;; modified tab
    (set-face-attribute 'tabbar-modified nil :box '(:line-width 1 style: released-button)
                        :foreground `,(face-attribute 'font-lock-warning-face :foreground)
                        :background `,(face-attribute 'default :background))
    (set-face-attribute 'tabbar-selected-modified nil
                        :foreground `,(face-attribute 'font-lock-warning-face :foreground))
    (set-face-attribute 'tabbar-separator nil)
    )
  (when (featurep 'powerline)
    (powerline-reset)
    (set-face-attribute 'powerline-active1 nil
                        :background `,(face-attribute 'mode-line :background)
                        :foreground `,(face-attribute 'mode-line :foreground))
    (set-face-attribute 'powerline-active2 nil
                        :background `,(face-attribute 'secondary-selection :background)
                        :foreground `,(face-attribute 'secondary-selection :foreground))
    (set-face-attribute 'powerline-bold nil :weight 'bold)
    (set-face-attribute 'powerline-inactive1 nil
                        :background `,(face-attribute 'mode-line-inactive :background))
    (set-face-attribute 'powerline-inactive-bold nil :weight 'bold)
    (set-face-attribute 'powerline-inactive2 nil
                        :background `,(face-attribute 'mode-line-inactive :background))
    (set-face-attribute 'powerline-inactive-bold nil :weight 'bold)
    (set-face-attribute 'powerline-alert nil :weight 'bold
                        :foreground `,(face-attribute 'font-lock-warning-face :foreground))
    (set-face-attribute 'powerline-inactive-alert nil
                        :background `,(face-attribute 'mode-line-inactive :background))
    ))

(defun my-beautify-json ()
  "Beautify json using Python."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))


(defun my-compile-anywhere ()
  "Search for a Makefile in directories recursively, and compile when found"
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (compile "make"))))

(defun my-extract-colors ()
  "Extract colors from current applied theme."
  (interactive)
  (setq my-colors
        `(:powerline "#eeeeec"           ;; powerline active block 1
                     :background02 "#878787"           ;; powerline active block 2
                     :background03 "#415160"           ;; powerline inactive background
                     :background04 ,(face-attribute 'default :background);; tabbar active
                     :background05 "#000000"           ;; header line
                     :background06 "#415160"           ;; inactive tab / highlight indentation
                     :foreground01 "black"             ;; powerline active block 1
                     :foreground02 "white"             ;; powerline active block 2
                     :foreground03 "white"             ;; powerline inactive
                     :foreground04 ,(face-attribute 'font-lock-warning-face :foreground) ;; powerline alert / modified tab
                     :foreground05 ,(face-attribute 'font-lock-keyword-face :foreground) ;; tabbar active foreground
                     )))

(defun my-insert-current-date-time ()
  "Insert the current date and time into the buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string "%d-%m-%Y %H:%M" (current-time))))

(defun my-reset-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

(defun my-org-clock-in-everywhere ()
  "Clock in from within an org page as well as from within the agenda."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun my-org-clock-show-list ()
  "Show list of recently clocked-time in items."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-clock-in)))

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(defun my-org-wc-toggle-overlay ()
  "Toggle the org-wc overlay."
  (interactive)
  (if (bound-and-true-p org-wc-overlays)
      (org-wc-remove-overlays)
    (org-wc-display nil)))

(defun my-replace-symbols-with-entity-names (start end)
  (interactive "r")
  (let ((count (count-matches "&")))
    (replace-string "&" "&amp;" nil start end)
    (setq end (+ end (* count 4))))
  (dolist (pair web-mode-html-entities)
    (unless (= (cdr pair) 38)
      (let* ((str (char-to-string (cdr pair)))
             (count (count-matches str start end)))
        (setq end (+ end (* count (1+ (length (car pair))))))
        (replace-string str
                        (concat "&" (car pair) ";")
                        nil start end)))))

(defun my-set-default-font (my-font)
  "Set default font to MY-FONT for frames if the font has been installed."
  (if window-system
      (progn
        (if (member my-font (font-family-list))
            (progn
              (set-face-attribute 'default nil :font my-font)
              (set-frame-font my-font nil t))
          (progn
            (message "Font %s is not installed" my-font)
            (if (font-family-list)
                (print (font-family-list)))))
        (if (daemonp)
            (progn
              (message "Removing daemon startup set-default-font hook")
              (remove-hook 'window-configuration-change-hook
                           (lambda ()
                             (my/set-default-font my-font))))))))

(defun my-titlecase-converter ()
  "Convert region to titlecase."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "titlecase" (current-buffer) t)))


(defun compile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))


;; http://emacswiki.org/emacs/SwitchingBuffers
(defun my-switch-to-previous-buffer ()
  "Switch to previously active buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; http://www.blogbyben.com/2015/04/the-joy-of-elisp-powered-code-review.html
(defun code-review-region (beg end)
  "Copy a region of source code, add line numbers."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (line-number (line-number-at-pos))
         (file (buffer-file-name))
         (path (replace-regexp-in-string "^.*branches/" ""
                                         (replace-regexp-in-string
                                          "^.*trunk/" "" file))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match "| " nil nil))
      (goto-char (point-min))
      (insert (format "+---[%s:%s]\n" path line-number))
      (goto-char (point-max))
      (insert "\n+---\n")
      (kill-region (point-min) (point-max)))))

(defun my-cleanup ()
  "Enable handy programming features / defaults"
  (interactive)
  (whitespace-cleanup)
  (fill-paragraph)
  (indent-region (point-min) (point-max)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun open-dired ()
  "Open dired in current directory when available or default directory"
  (interactive)
  (if (string= nil buffer-file-name)
      (dired nil)
    (progn (if (string= ";" path-separator)  ;; on Windows
               (dired (replace-regexp-in-string "/" "\\\\"
                                                (file-name-directory
                                                 (buffer-file-name)) t t))
             (dired (file-name-directory (buffer-file-name)))))))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defun describe-line-endings ()
  (interactive)
  (message (describe-variable buffer-file-coding-system)))

(defun msys-shell ()
  "Run msys bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/programs/msys2/usr/bin/bash"))
    (setq explicit-bash-args '("--login" "-i"))
    (setenv "PATH"
            (concat "/mingw64/bin:/usr/local/bin:/usr/bin:/bin:"
                    (replace-regexp-in-string " " "\\\\ "
                                              (replace-regexp-in-string "\\\\" "/"
                                                                        (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
                                                                                                  (getenv "PATH"))))))
    (call-interactively 'shell)))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is Emacs Lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("cmd" . "cmd /c ")
            ("md" . "\"c:\\program files (x86)\\mozilla firefox\\firefox.exe\" file://")
            ("php" . "php ")
            ("pl" . "perl ")
            ("py" . "python ")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe " "python3 "))
            ("rb" . "ruby ")
            ("js" . "node ")             ; node.js
            ("sh" . "bash ")
            ("ml" . "ocaml ")
            ("vbs" . "cscript ")
            )
          )
         (fName (expand-file-name buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified.  Do you want to save first? ")
        (save-buffer) ) )
    (if (string-equal fSuffix "md") ; special case for markdown file
        (progn
          (shell-command (concat (concat "bash " (file-name-directory fName) "build.sh"))))
      (if (string-equal fSuffix "el") ; special case for emacs lisp
          (load (file-name-sans-extension fName))
        (message "Running...")
        (shell-command (concat progName fName) "*xah-run-current-file output*" )
        (message "No recognized program file suffix for this file.")
        ) ) ))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-D-to-C))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              )))
      (open-custom-agenda)))
  )

(defun my-org-confirm-babel-evaluate (lang body)
  "When LANG is trusted, return false (don't ask for confirmation).
   If true, ask for confirmation to evaluate code."
  (message "Check language %s" lang)
  (not (member lang '("plantuml" "python" "restclient" "sh" "shell"))))

(defun my-align-org-tags ()
  "Align 'org-mode' tags to the right border of the screen."
  (interactive)
  (setq org-tags-column (- 15 (window-width))))

(defun open-custom-agenda ()
  "Open custom agenda view."
  (interactive)
  (org-agenda nil "o")
  )

(defun save-kill-buffer ()
  "Save buffer and kill (close) it."
  (interactive
   ((save-buffer)
    (kill-this-buffer))))


;; http://zck.me/emacs-move-file
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))
;; enable disabled function
(put 'downcase-region 'disabled nil)

;; https://github.com/syl20bnr/spacemacs/issues/8314
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(if (fboundp 'defhydra)
    (progn
      (global-set-key
       (kbd "C-d")
       (defhydra hydra-delete
         (:body-pre (delete-char 1))
         "delete"
         ("k" kill-line "kill (rest of) line")
         ("K" kill-whole-line "kill whole line")
         ("D" kill-word "kill word")
         ("d" delete-char "delete character")
         ("q" nil "quit")))

      (global-set-key
       (kbd "C-n")
       (defhydra hydra-kill-move
         (:body-pre (next-line))
         "move"
         ("n" next-line)
         ("p" previous-line)
         ("f" forward-char)
         ("b" backward-char)
         ("a" beginning-of-line)
         ("e" move-end-of-line)
         ("v" scroll-up-command)
         ("\\" delete-horizontal-space)
         ;; Converting M-v to V here by analogy.
         ("V" scroll-down-command)
         ("k" kill-line "kill (rest of) line")
         ("l" recenter-top-bottom)))

      (global-set-key
       (kbd "C-k")
       (defhydra hydra-kill-move-diff
         (:body-pre (kill-line))
         "move"
         ("n" next-line)
         ("p" previous-line)
         ("f" forward-char)
         ("b" backward-char)
         ("a" beginning-of-line)
         ("e" move-end-of-line)
         ("v" scroll-up-command)
         ("\\" delete-horizontal-space)
         ;; Converting M-v to V here by analogy.
         ("V" scroll-down-command)
         ("k" kill-line "kill (rest of) line")
         ("l" recenter-top-bottom)))))


;; hooks

;; various built in modes
(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "<tab>") 'calendar-forward-month)
            (define-key calendar-mode-map (kbd "S-<tab>") 'calendar-backward-month)
            (copy-face font-lock-keyword-face 'calendar-iso-week-face)
            (setq calendar-week-start-day 1   ;; DD/MM/YYYY
                  calendar-date-style 'european ;; easy navigation
                  )))

(add-hook 'comint-mode-hook
          (lambda ()                   ;; reclaim keybindings for shell mode
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
            (setq comint-process-echoes t))) ;; prevent echoing

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(add-hook 'git-commit-mode-hook
          (lambda ()
            (linum-mode 0)))           ;; as it's derived from text-mode

(add-hook 'markdown-mode-hook 'auto-fill-mode)

(add-hook 'nxml-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (highlight-indentation-mode t)
            (yas-minor-mode 1)
            ))

(add-hook 'prog-mode-hook (lambda ()
                            (flyspell-prog-mode)
                            (linum-mode 1)))

(add-hook 'rst-mode-hook (lambda ()
                           (auto-fill-mode 1)
                           (yas-minor-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (reveal-mode 1)))

(add-hook 'shell-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)))
;; (toggle-truncate-lines 1))) ;; turn off word wrap for shell mode

(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode)
            (linum-mode 1)
            (visual-line-mode 0)       ;; show a symbol for wrapping lines,
            (setq word-wrap 1)))       ;; but still wrap words nicely

;; builtin hooks
(add-hook 'after-init-hook
          (lambda ()
            (if (bound-and-true-p my-theme)
                (load-theme my-theme t))
            ;; make sure that utf8 Unix line endings (LF) are default
            (setq-default default-buffer-file-coding-system 'utf-8-unix)
            (setq-default buffer-file-coding-system 'utf-8-unix)
            (prefer-coding-system 'utf-8-unix)
            (my-set-default-font my-font)
            (if (boundp 'my-scratch-file)
                (progn
                  (find-file my-scratch-file)  ;; only show it if it's the only file
                  (if (get-buffer "*scratch*")
                      (kill-buffer "*scratch*"))))
            (when (boundp 'start-with-agenda)
              (open-custom-agenda))
            (raise-frame)
            ))

(if (fboundp 'my-align-org-tags)
    (add-hook 'window-configuration-change-hook
              (lambda()
                (my-align-org-tags))))

;; workaround to make sure that font is being set when running in daemon mode
(if (daemonp)
    (add-hook 'window-configuration-change-hook
              (lambda ()
                (my-set-default-font my-font))))


(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))


(add-hook 'after-load-theme-hook
          (lambda ()
            (my-extract-colors)
            (my-apply-colors)))
(if (fboundp 'my-org-query-clock-out)
    (add-hook 'kill-emacs-query-functions 'my-org-query-clock-out))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-clock-in-hook 'save-buffer)  ;; save buffer when clocking in...
(add-hook 'org-clock-in-prepare-hook 'my-org-mode-ask-effort)
(add-hook 'org-clock-out-hook (lambda ()
                                (save-buffer)
                                (makunbound 'org-mode-line-string)
                                (force-mode-line-update)))
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0)             ;; as it's derived from text-mode
            (yas-minor-mode 1)
            (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)))

;;; init.el ends here
