;;; init.el --- Emacs initialization file

;; Copyright (c) 2011-2018 Peter Mosmans

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
;; my-start-with-agenda
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
(defcustom my-capture-file (concat my-org-directory  "/capture.org")
  "Default 'org-mode' capture file."
  :type 'file
  :group 'my-customizations)
(defcustom my-dayplanner-file (concat my-org-directory "/dayplanner.org")
  "Default 'org-mode' dayplanner file."
  :type 'file
  :group 'my-customizations)
(defcustom my-org-refile-targets '((org-agenda-files :level . 2))
  "Org-more refile targets."
  :type 'string
  :group 'my-customizations)
(defcustom my-presentation-mode nil
  "True when presentation mode is enabled (remove clock, cursor)."
  :type 'boolean
  :group 'my-customizations)
(defcustom my-snippet-dirs nil
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

;; (setq debug-on-error t)             ;; Uncomment for debugging options
(setq gc-cons-threshold 500000000)     ;; Improve startup time

;; Use custom-file to store all customizations (including the aforementioned parameters)
(setq custom-file "~/.emacs.d/variables.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Check whether package-initialize has already been called
(when (not (fboundp 'package-installed-p))
  (package-initialize))

;; Bootstrap use-package
(when (not (package-installed-p 'use-package))
  (progn
    (setq package-archives `(
                             ("melpa" . "https://melpa.org/packages/")
                             ("melpa-stable" . "https://stable.melpa.org/packages/")
                             )
          )
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package)
    ))

(eval-when-compile
  (require 'use-package))

;; Some packages need to be installed from different sources
(use-package package
  :config
  (setq package-archives `(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))
        use-package-always-defer t     ;; Use lazy loading wherever possible
        use-package-always-ensure t    ;; Enforce automatic installation of all packages
        use-package-always-pin "melpa-stable"
        use-package-verbose t)         ;; Show package loading times
  )

;; define all necessary EXTERNAL alphabetically
;; after:     wrap everything in a with-eval-after-load, so that config will be done last
;; :bind      keybindings (all keys before :map are bound globally) (implies defer)
;;            :bind (("C-<f2>" . bm-toggle)
;; commands:  load the package (execute config) when these commands are executed (implies defer)
;; config:    execute code *after* a package is loaded
;; defer:     defer loading (implied when using bind, mode or interpreter)
;; delight:   hide mode
;; demand:    enforce loading (and enforce bind keys)
;; disabled:  (temporarily) disable a package
;; ensure:    make sure the package is installed
;; hook:      Add functions onto hooks
;;            :hook name-of-mode
;;            :hook ((text-mode . name-of-mode))
;; init:      Execute code *before* a package is loaded (implies defer)
;; load-path: path of the files for local packages
;; mode:      deferred binding
;;            :mode ("\\.py\\'" . python-mode)
;; pin:       pin to a specific repository

(use-package ansible
  :after company
  :hook (
         (ansible-mode . (lambda () (setq company-backends '(company-ansible))))
         (yaml-mode . ansible)
         )
  )

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

(use-package bm                        ;; Bookmark toggle package
  :bind (("C-<f2>" . bm-toggle)
         ("M-<f2>" . bm-next))
  )

(use-package company
  :config
  (defvar company-mode/enable-yas t "Enable yasnippet for all back ends.")
  :delight
  )

(use-package company-jedi             ;; Completion backend for Python (jedi)
  :after company
  )


(use-package company-quickhelp
  :after company
  :config (setq company-quickhelp-delay 0)
  :init (company-quickhelp-mode 1)
  )

(use-package company-restclient
  :after company
  :config (add-to-list 'company-backends 'company-restclient)
  )

(use-package company-statistics        ;; Sort completion candidates based on usage
  :after company
  :config (company-statistics-mode)
  )

(use-package company-tern
  :after company
  :config (add-to-list 'company-backends 'company-tern)
  )

(use-package company-web               ;; Completion backend for web
  :after company
  )

(use-package dash
  :disabled t
  )

(use-package delight                   ;; Hide package (icons) in the modeline
  :pin "gnu"                           ;; Only available on GNU Elpa
  :config                              ;; Automatically hide some modes in the modelie
  (delight '((global-whitespace-mode nil "whitespace")
             (helm-mode nil "helm-mode")))
  )

(use-package dracula-theme)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm
        dump-jump-force-searcher 'ag)
  :disabled t
  :hook prog-mode
  )


(use-package elpy
  :bind (:map elpy-mode-map ("M-." . elpy-goto-definition))
  :config
  (setq python-shell-interpreter "python3")
                                        ;elpy-rpc-backend "jedi"
  ;;       python-shell-completion-native-enable nil)
  (elpy-enable)
  :hook (elpy-mode . flycheck-mode)    ;; Enforce flycheck mode
  :init (with-eval-after-load 'python (elpy-enable))  ;; file is loaded
  )

(use-package emmet-mode
  :hook web-mode
  )

(use-package fill-column-indicator
  :commands fci-mode
  :config
  (setq fci-rule-color "light slate grey"
        fci-always-use-textual-rule t)
  :disabled t
  :init
  :hook (
         markdown-mode
         prog-mode
         rst-mode
         web-mode
         yaml-mode
         )
  )

(use-package flycheck
  :config
  (setq flycheck-highlighting-mode 'symbols ;; highlight whole line
        flycheck-indication-mode 'right-fringe)  ;; Use right fringe for errors
  (global-flycheck-mode)
  )

(use-package flymd
  :commands flymd-flyit
  :pin "melpa"
  )

(use-package focus
  :commands focus-mode
  )

(use-package git-gutter-fringe         ;; Show git status in the fringe
  :bind (
         ("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g k" . git-gutter:revert-hunk)
         ("C-c g s" . git-gutter:stage-hunk)
         )
  :delight
  :init (global-git-gutter-mode)
  )

(use-package git-timemachine
  )

(use-package graphviz-dot-mode         ;; Make it easier to edit (Graphviz) dot files
  )

(use-package groovy-mode               ;; Highlight Groovy (Jenkinsfile)
  )

(use-package helm
  :bind (
         ("C-c j" . helm-imenu)        ;; J ump to imenu
         ("C-c r" . helm-register)     ;; Show registers
         ("M-y" . helm-show-kill-ring)
         ("M-s o" . helm-occur)        ;; Use helm instead of regular occur
         ([M-f10] . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ([M-x] . helm-M-x)
         ([S-f10] . helm-mini)
         ([capslock] . helm-M-x)
         :map helm-map
         ("C-z" . helm-select-action)
         ([tab] . helm-execute-persistent-action)
         )
  :config
  (helm-autoresize-mode t)
  (setq
   helm-buffers-truncate-lines nil     ;; Do not truncate lines
   helm-move-to-line-cycle-in-source t ;; Wrap search results
   )
  :delight                             ;; No need to show that Helm is loaded
  )

(use-package helm-ag
  :after helm
  :commands helm-ag
  )

(use-package helm-bibtex
  :after helm
  :bind (("C-c x" . helm-bibtex))
  :config
  (if (boundp 'my-bibliographies)
      (setq bibtex-completion-bibliography my-bibliographies))
  (setq bibtex-autokey-year-length 4 ;; use 4 digits for the year
        bibtex-completion-pdf-field "file" ;; Use the file field to locate PDF files
        bibtex-completion-notes-path (concat my-org-directory "/bibtex-notes.org"))
  )

(use-package helm-company
  :after helm
  )

(use-package helm-descbinds            ;; Describe keybindings using the Helm interface
  :after helm
  )

(use-package helm-flyspell
  :after (flyspell helm)
  :bind (("C-;" . helm-flyspell-correct))
  :commands flyspell-mode
  :pin "melpa"
  )

(use-package helm-ls-git
  :after helm
  :bind (("C-x C-d" . helm-browse-project))
  :commands helm-ls
  )

(use-package helm-make
  :after projectile
  :bind (("C-c m" . helm-make-projectile))
  )

(use-package helm-org-rifle
  :after helm
  :commands helm-org-rifle
  )

(use-package helm-projectile
  :bind (([f5] . helm-projectile-find-file)
         ([C-f10] . helm-projectile-switch-project))
  )

(use-package helm-tramp
  :after helm
  )

(use-package helm-wordnet
  :after helm
  :bind ("C-c s" . helm-wordnet-suggest)
  :config
  (setq
   helm-wordnet-follow-delay 0
   helm-wordnet-post-arg "-over -synsa"  ;; Show an overview as well as synonyms
   )
  ;; Only try to set the dictionary manually when necessary
  (unless (bound-and-true-p helm-wordnet-wordnet-location)
    (setq helm-wordnet-wordnet-location my-wordnet-dictionary))
  :pin "melpa"
  )

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :config
  (set-face-background 'highlight-indentation-face "light slate grey")
  (set-face-background 'highlight-indentation-current-column-face "light slate grey")
  :hook (prog-mode . highlight-indentation-mode)
  :delight
  )

(use-package hydra
  )

(use-package imenu-list
  :bind ("C-c i" . imenu-list-smart-toggle)
  :config (setq imenu-list-position 'left)
  )

(use-package js2-mode
  :bind ("M-." . nil)
  :hook ((js2-mode . company-mode)
         (js2-mode . tern-mode)
         (js2-mode . js2-imenu-extras-mode)
         (xref-backend-functions . xref-js2-xref-backend))
  :mode ("\\.js\\'" . js2-mode)
  )

(use-package let-alist
  )

(use-package lorem-ipsum               ;; Generate lorem ipsum from within Emacs
  :pin "melpa"
  )

(use-package magit
  :bind (
         ([f1] . magit-status)
         ("C-c g g" . magit-status)
         ("C-c g c" . magit-commit)
         )
  :pin "melpa"                         ;; Needs >2.11
  )

(use-package magithub                  ;; Integrate github into Magit
  :after magit
  :config (magithub-feature-autoinject t)
  :pin "melpa"
  )

(use-package magit-gitflow             ;; Use Magit git flow plugin
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow)
  )

(use-package markdown-mode
  )

(use-package mode-icons                ;; Show pretty icons on the modeline
  :config
  (setq mode-icons-desaturate-active t  ;; Desarurate inactive XPM mode line icons
        mode-icons-grayscale-transform t) ;; "re"color black and white images
        
  (mode-icons-mode t)
  :demand t
  :pin "melpa"
  )

(use-package neotree
  :bind (("M-<f8>" . neotree-toggle)
         ("C-c n" . neotree-toggle))
  :commands neotree-toggle
  :config
  (setq  neo-show-hidden-files t
         neo-theme 'ascii              ;; Don't use fancy icons
         neo-window-width 30)
  )

(use-package nov                       ;; Read epub in Emacs
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

(use-package ob-async                  ;; Asynchronous execution of org-babel source code blocks
  :after org                           ;; when using the :async keyword
  :demand t                            ;; (require 'ob-async)
  :pin "melpa"
  )

(use-package ob-http                   ;; Perform HTTP requests within org-mode
  )

(use-package org-ref
  :after org
  :disabled t
  )

(use-package org-wc                    ;; Count words in org mode documents
  :after org
  :bind ("C-c w" . my-org-wc-toggle-overlay)
  :pin "melpa"
  )

(use-package polymode                 ;; Use modes within modes
  :disabled t
  )

(use-package powerline
  :after mode-icons
  ;;  :config (my-apply-colors)
  :init                                ;; Set parameters before enabling pwerline
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
                       (lhs            ;; The left hand side
                        (list
                         (powerline-raw "%3l" face1 'l)   ;; Line number
                         (powerline-raw ":" face1)
                         (powerline-raw "%3C" face1 'r)   ;; 1-based column
                         (funcall separator-left face1 face2)
                         (when powerline-display-buffer-size
                           (powerline-buffer-size face2 'l)) ;; Buffer size
                         (when powerline-display-mule-info
                           (powerline-raw mode-line-mule-info face2 'l)) ;; Multi lingual environment
                         (powerline-major-mode face2 'l)
                         (powerline-minor-modes face2 'l)
                         (powerline-process face2)
                         (funcall separator-left face2 face1)
                         (powerline-raw mode-line-modified face1)
                         (powerline-buffer-id face1) ;; buffer name
                         (funcall separator-left face1 face2)
                         (powerline-vc face2 'r)  ;; The middle
                         (unless (bound-and-true-p my-presentation-mode)
                           (when (bound-and-true-p which-func-mode)
                             (powerline-raw which-func-format face2 'r)))
                         (when
                             (bound-and-true-p nyan-mode)
                           (powerline-raw
                            (list
                             (nyan-create))
                            face2 'l))))
                       (rhs            ;; Right hand side
                        (list
                         (funcall separator-right face2 bold-face)
                         (unless window-system
                           (powerline-raw
                            (char-to-string 57505)
                            bold-face 'l))
                         (funcall separator-right face2 bold-face)
                         (when (boundp 'eyebrowse-mode-line-separator)
                           (powerline-raw (eyebrowse-mode-line-indicator) face1))
                         (unless (bound-and-true-p my-presentation-mode)
                           (if (boundp 'org-mode-line-string)
                               (powerline-raw (propertize org-mode-line-string 'face face1) face1)
                             (powerline-raw "NOT CLOCKED IN" alert-face))
                           (when powerline-display-hud
                             (powerline-hud bold-face face1)
                             (powerline-raw (concat " " display-time-string) bold-face 'r)
                             )))))
                    (concat
                     (powerline-render lhs)
                     (powerline-fill face2
                                     (powerline-width rhs))
                     (powerline-render rhs))))))
  :pin "melpa"
  :requires mode-icons
  )

(use-package projectile
  :after helm
  :config
  (setq projectile-completion-system 'helm
        projectile-globally-ignored-directories '("Include" "Lib" "Scripts")
        projectile-globally-ignored-file-suffixes '(".avi" ".fo" ".jpg" ".mp4"
                                                    ".pdf" ".png" ".pptx" ".svg"
                                                    ".xlsx" ".zip")
        projectile-indexing-method 'alien  ;; use the fastest indexing method
        projectile-mode-line '(:eval
                               (if (file-remote-p default-directory)
                                   " Projectile"
                                 (format " {%s}"
                                         (projectile-project-name)))))

  (helm-projectile-on)
  (projectile-mode 1)
  ;; :commands projectile-mode
  :init
  (put 'projectile-project-name 'safe-local-variable #'stringp)
  )

(use-package pylint
  :pin "melpa"
  )

(use-package rainbow-mode
  :delight
  :hook (prog-mode . rainbow-mode)
  :pin "gnu"
  )

(use-package restclient
  :after company
  :commands restclient-mode
  :hook (restclient-mode . company-mode)
  )

(use-package restclient-helm
  :after (helm restclient)
  :pin "melpa"
  )

(use-package shackle                   ;; Enforce opening of certain buffers in frames/windows/...
  :config
  (shackle-mode)
  :init
  (setq shackle-rules
        '(
          (compilation-mode :select t :align t :frame t)  ;; Open compilation mode in its own frame
          ))
  )

(use-package tabbar                    ;; Display a tabbar in the header line
  :bind (("C-<tab>" . tabbar-forward)
         ("C-S-<tab>" . tabbar-backward)
         ("C-S-<iso-lefttab>" . tabbar-backward)
         ("M-<down>" . tabbar-forward-group)
         ("M-<up>" . tabbar-backward-group)
         ("<f8>" . tabbar-backward)
         ("S-<f8>" . tabbar-backward-group)
         ("<f9>" . tabbar-forward)
         ("S-<f9>" . tabbar-forward-group))
  :config (tabbar-mode t)              ;; Enable the tabbar by default
  :demand t
  :pin "melpa"
  )

(use-package undo-tree                 ;; Generate a nice visualization of undo
  :bind (
         ("C-x U" . undo-tree-visualize)
         )
  :config (setq
           undo-tree-visualizer-diff t ;; Show diffs by default
           )
  :pin "gnu"
  )


(use-package web-mode
  :config (setq web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2)
                                        ; :hook (auto-fill-mode flyspell-mode)
  :mode (
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.html?\\'"
         "\\.php\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.xml\\'"
         )
  :init (setq web-mode-enable-current-element-highlight t
              web-mode-enable-current-column-highlight t)
  )

(use-package which-key
  :delight
  :init (which-key-mode)
  )

(use-package xref-js2
  :hook (js2-mode . tern-mode)
  )

(use-package yafolding
  :bind (("C-|" . yafolding-toggle-element)
         ("C-\\" . yafolding-toggle-all))
  :disabled t
  :hook (prog-mode . yafolding-mode)
  )

(use-package yaml-mode
  ;; yaml is a major mode, not based on prog-mode, so manually add modes
  :config
  (setq tab-width 2)
  :init
  :hook (yaml-mode . (lambda ()
                       (my-prettier-diff)
                       (add-hook 'after-save-hook 'my-prettier-diff nil t)
                       (highlight-indentation-mode))
                   )
  )

(use-package yasnippet
  :commands yas-minor-mode
  :config
  (if (boundp 'my-snippet-dirs)
      (dolist (item my-snippet-dirs)   ;; Add item per item
        (add-to-list 'yas-snippet-dirs item)))
  (setq yas-indent-line 'fixed)        ;; Use fixed indentation instead of 'per mode'
  (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
         (bibtex-mode . yas-minor-mode)
         (web-mode . (lambda () (yas-activate-extra-mode 'text-mode))))
  )

;; define font for Unicode Private Use Area block
(when (member "Symbol" (font-family-list))
  (set-fontset-font "fontset-default" '(#xf000 . #xffff) (font-spec :name "Symbol")))

;; OS-specific settings
(if (string= system-type "windows-nt")
    (setq w32-enable-caps-lock nil     ;; Free up the capslock key for something useful
          shell-file-name (executable-find "zsh.exe")
          ;; explicit-shell-file-name (executable-find "zsh.exe")
          ))

;; generic settings
(setq-default fill-column 80           ;; width of the screen for wrapping
              line-spacing 0
              indent-tabs-mode nil)    ;; always use spaces for indentation

;; Custom variables used by custom functions
(setq my-alerter-icon (subst-char-in-string ?/ ?\\ (concat data-directory "images/icons/hicolor/128x128/apps/emacs.png"))
      my-toast-app (executable-find "snoreToast.exe"))

;; Emacs variables
(setq
 auth-sources '("~/.authinfo.gpg")     ;; Only use encrypted store for (new) secrets
 auto-save-interval 1000               ;; Automatically save after x characters
 bibtex-autokey-name-length 10         ;; Use a maximum of 10 characters per name
 bibtex-autokey-names 2                ;; Use a maximum of 2 author names
 bibtex-autokey-titleword-length 0     ;; Do not use title words
 bibtex-autokey-titleword-separator "" ;; Do not use title words
 bibtex-autokey-titlewords 0           ;; Do not use title words
 bibtex-autokey-year-length 2          ;; Use 2 characters for a year
 bibtex-entry-format '(numerical-fields opts-or-alts realign required-fields)
 bibtex-maintain-sorted-entries 'entry-class
 bookmark-default-file "~/.emacs.d/bookmarks.emacs"
 calendar-intermonth-text              ;; Show week numbers in the calendar
 '(propertize 
   (format "%2d" (car
                  (calendar-iso-from-absolute
                   (calendar-absolute-from-gregorian
                    (list month day year)))))
   'font-lock-face 'calendar-iso-week-face)
 column-number-indicator-zero-based nil
 column-number-mode t                  ;; Show column-number
 comint-prompt-read-only t             ;; Read only prompt for shell mode
 compilation-ask-about-save nil
 compilation-read-command nil
 compile-command "make "
 completion-ignore-case t              ;; Ignore case when completing
 desktop-path '("~/.emacs.d/desktop/")    ;; Specify directory for desktop files
 dired-listing-switches "-agoh"
 display-time-world-list (quote (("AST-10AEST" "BNE") ("CET-1CEST" "AMS") ("MST+7" "MST")))
 ediff-window-setup-function 'ediff-setup-windows-plain
 frame-resize-pixelwise t              ;; Don't round frame sizes on character sizes
 global-font-lock-mode t               ;; Enable syntax highlighting by default
 global-hl-line-mode t                 ;; Highlight current line by default
 global-visual-line-mode t             ;; act on visual lines, enable word wrap
 inhibit-compacting-font-caches t      ;; speed up displaying Unicode glyphs
 inhibit-startup-echo-area-message nil
 inhibit-startup-message t             ;; Remove welcome message
 ispell-dictionary "en_US"             ;; Default language for spell checker
 ispell-personal-dictionary "~/personal-dictionary"
 ispell-program-name "aspell"          ;; Use aspell as default spell checker
 ispell-silently-savep t               ;; Save to personal dictionary without asking
 js-indent-level 2                     ;; Indentation for JavaScript
 kill-whole-line t                     ;; Kill whole line including newline
 make-backup-files nil                 ;; Do not create backups
 message-log-max t                     ;; Keep and log all messages
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 package-quickstart t
 read-file-name-completion-ignore-case t
 recentf-max-menu-items 15             ;; Show maximum x recent menu items
 recentf-max-saved-items 100           ;; Save maximum x recent files
 recentf-save-file "~/.emacs.d/recentfiles.emacs"
 rst-preferred-adornments '(
                            (35 over-and-under 0) ;; # H1 (once per document)
                            (42 over-and-under 0) ;; * H2
                            (61 simple 0)         ;; = H3
                            (45 simple 0)         ;; - H4
                            (94 simple 0)         ;; ^ H5
                            (34 simple 0)         ;; " H6
                            )
 scroll-conservatively 10000
 scroll-margin 1
 scroll-preserve-screen-position 1
 scroll-step 1
 sentence-end-double-space nil         ;; consider single space a sentence break
 show-paren-delay 0
 show-paren-style 'expression          ;; highlight entire bracket expression
 size-indication-mode nil              ;; disable file size mode
 tab-width 4                           ;; default tab width
 tramp-histfile-override t             ;; Disable histfile for tramp
 tramp-default-method "sshx"
 whitespace-style (quote
                   (face indentation tabs space-before-tab space-after-tab tab-mark trailing))
 )

(customize-set-variable 'tramp-syntax 'simplified)      ;; Use /host:file syntax
(delete-selection-mode 1)              ;; Automatically overwrite selected text
(fset 'yes-or-no-p 'y-or-n-p)          ;; enable y/n answers to yes/no questions
(global-whitespace-mode 1)             ;; Globally enable whitespace mode
(menu-bar-mode 0)                      ;; Disable menu bar by default
(show-paren-mode 1)


;; org mode settings


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo-list)
(global-set-key (kbd "S-<f12>") 'my-org-clock-in-everywhere)
(global-set-key (kbd "C-<f12>") 'org-clock-out)
(global-set-key (kbd "M-<f12>") 'my-org-clock-show-list)
(if (file-exists-p my-org-directory)  ;; use my-org-directory if it exists
    (setq org-directory my-org-directory)
  (message "Please specify correct my-org-directory: The directory %s does not exist."
           my-org-directory))
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (dot . t)
   (http . t)
   (plantuml . t)
   (python . t)
   (shell . t)
   ))

(setq org-agenda-compact-blocks t      ;; skip long block separators
      org-agenda-custom-commands
      '(("t" "All TODO items ordened by category and tag"
         ((tags "+TODO=\"TODO\""
                (
                 (org-agenda-overriding-header "Ordened by category / tag")
                 (org-agenda-prefix-format " %i %b")
                 (org-agenda-sorting-strategy '(category-up tag-up))
                 ))))
        ("p" "projects"
         ((tags "TODO=\"PROJECT\"|CATEGORY=\"projects\""
                (
                 (org-agenda-overriding-header "Projects overview")
                 (org-agenda-prefix-format " %i") ;; l level (spaces)  ;;i icon
                 (org-agenda-sorting-strategy '(category-up))
                 ))))
        ("i" "incubating (someday/maybe)"
         ((tags "CATEGORY=\"incubating\"-TODO=\"DONE\"-TODO=\"CANCELLED\"" ;;+TODO=\"NONACTIONABLE\""
                (
                 (org-agenda-overriding-header "Incubating: someday/maybe ideas")
                 (org-agenda-prefix-format " %i %l")
                 (org-agenda-sorting-strategy '(category-up))
                 ))))
        ("o" "outcomes"
         ((tags "+CATEGORY=\"outcomes\"" ;;+TODO=\"NONACTIONABLE\""
                (
                 (org-agenda-overriding-header "Outcomes")
                 (org-agenda-prefix-format " %i %l")
                 (org-agenda-sorting-strategy '(category-up tag-up))
                 ))))
        ("r" "reference"
         ((tags "CATEGORY=\"reference\""
                (
                 (org-agenda-overriding-header "General and specialized references")
                 (org-agenda-prefix-format " %i %l")
                 (org-agenda-sorting-strategy '(category-up tag-up))
                 ))))
        ("f" "Forthight overview, including all tasks"
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
          (tags "TODO=\"TODO\"+lead"
                ((org-agenda-overriding-header "Leads")
                 (org-agenda-prefix-format "")  ;; don't show prefix
                 (org-agenda-remove-tags t)  ;; remove tags
                 ))
          (tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
                ((org-agenda-overriding-header "Delegated tasks")
                 (org-agenda-prefix-format "")  ;; don't show prefix
                 (org-agenda-remove-tags t)  ;; remove tags
                 ))
          (tags "+TODO=\"TODO\""
                ((org-agenda-overriding-header "TODO")
                 (org-agenda-prefix-format " %i")
                 (org-agenda-sorting-strategy '(priority-down category-up tag-up))
                 ))
          ))
        ("x" "Estimated versus clocked so far these 2 weeks" agenda ""
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
      org-refile-targets my-org-refile-targets
      org-agenda-todo-keyword-format ""
      org-archive-location (concat "archive/%s." (format-time-string "%Y" (current-time)) ".archive::")
      org-capture-templates
      '(("t" "TODO"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  %u")
        ("n" "TODO NOW - clock in immediately"
         entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %u" :clock-in t :clock-keep t)  ;; Clock in immediately
        ("o" "outcome"
         entry (file+olp "outcomes.org" "Goals / objectives")
         "* %?\n  %u")
        ("p" "project"
         entry (file+olp "projects.org" "Projects")
         "* PROJECT %?\n  %u")
        ("i" "incubating (someday/maybe)"
         entry (file+olp "someday-maybe.org" "Someday / maybe ideas")
         "* %?\n  %u")
        ("l" "TODO with link to current buffer"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  %i\n   %a")
        ("d" "reminder for today"
         entry (file+headline org-default-notes-file  "Tasks")
         "* TODO %?\n  DEADLINE: <%(org-read-date nil nil \"+1d\")>"))
      org-catch-invisible-edit 'show-and-error
      org-clock-into-drawer t          ;; Log clocking data into drawer
      org-clock-out-remove-zero-time-clocks t ;; Remove logdata without time
      org-columns-default-format "#+COLUMNS: %60ITEM(Task) %8Effort(estimate){:} %8CLOCKSUM(clocked){:} %8CLOCKSUM_T(today){:}"
      org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
      org-cycle-separator-lines 0      ;; No empty lines needed between subtrees
      org-default-priority ?E          ;; By default, make new TODO items lowest priority ('Eliminate')
      org-default-notes-file my-capture-file
      org-descriptive-links nil        ;; Display full links
      org-duration-format '(("h" . nil) (special . h:mm))  ;; Don't show days
      org-file-apps (quote             ;; Add several file handlers
                     ((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.odt\\'" . system)
                      ("\\.pdf\\'" . default)
                      ("\\.x?html?\\'" . default)))
      org-fontify-done-headline t      ;; change headline face when marked DONE
      org-global-properties '(("Effort_ALL" . "0 00:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))
      org-hide-leading-stars t         ;; Only show one star per heading
      org-log-into-drawer t            ;; Insert notes & time stamps into drawer
      org-lowest-priority ?E           ;; Use a 5-scale rating for priorities

      org-src-fontify-natively t       ;; Fontify code in blocks
      org-support-shift-select t       ;; Keep using shift as selector
      org-todo-keywords '((sequence "TODO(t)" "DELEGATED(e)" "PROJECT(p)" "|" "CANCELLED(c)" "DONE(d)" ))                ;; ! indicates timestamp, @ note & timestamp
      org-use-speed-commands t         ;; Enable speed commands
      )
;; associate certain files with modes
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.prf\\'" . conf-mode))

;;; KEY BINDINGS
(global-unset-key (kbd "M-1")) ; digit-argument
(global-unset-key (kbd "M-2")) ; digit-argument
(global-unset-key (kbd "M-3")) ; digit-argument
(global-unset-key (kbd "M-4")) ; digit-argument
(global-unset-key (kbd "M-5")) ; digit-argument
(global-unset-key (kbd "M-6")) ; digit-argument
(global-unset-key (kbd "M-7")) ; digit-argument
(global-unset-key (kbd "M-8")) ; digit-argument
(global-unset-key (kbd "M-9")) ; digit-argument
(global-unset-key (kbd "M-0")) ; digit-argument

(global-unset-key (kbd "C-1")) ; digit-argument
(global-unset-key (kbd "C-2")) ; digit-argument
(global-unset-key (kbd "C-3")) ; digit-argument
(global-unset-key (kbd "C-4")) ; digit-argument
(global-unset-key (kbd "C-5")) ; digit-argument
(global-unset-key (kbd "C-6")) ; digit-argument
(global-unset-key (kbd "C-7")) ; digit-argument
(global-unset-key (kbd "C-8")) ; digit-argument
(global-unset-key (kbd "C-9")) ; digit-argument
(global-unset-key (kbd "C-0")) ; digit-argument

(global-set-key (kbd "<scroll>") 'scroll-lock-mode)
(global-set-key (kbd "M-<f11>") (lambda () (interactive) (find-file user-init-file)))

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
(global-set-key (kbd "M-<f1>") 'my-copy-source-code-region)

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
(if (bound-and-true-p initial-buffer-choice)
    (global-set-key (kbd "S-<f4>") (lambda () (interactive) (switch-to-buffer (file-name-nondirectory initial-buffer-choice))))
  (global-set-key (kbd "S-<f4>") (lambda () (interactive) (switch-to-buffer "*scratch*"))))
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

(global-set-key (kbd "<f10>") 'my-repeat-compile-command-anywhere)
;; bookmarks
(global-set-key (kbd "C-c b") 'bookmark-jump)
(global-set-key (kbd "S-<f11>") 'xah-run-current-file)
(global-set-key (kbd "C-<f11>") 'bookmark-set)

;; open dayplanner file
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file my-dayplanner-file)))


;;; SSH / PUTTY HACKS
(if (eq system-uses-terminfo t)         ;; terminal
    (progn                              ;; PuTTY needs to be in SCO mode
      (xterm-mouse-mode 0)              ;; use mouse even in terminal mode

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
  (set-buffer-file-coding-system 'utf-8-unix))

(defun my-alerter (title message)
  "Alert a user using the my-toast-app, when it is set."
  (if my-toast-app
      (shell-command (concat my-toast-app " -p \"" my-alerter-icon "\" -t \"" title "\" -m \"" message "\" -appid \"Emacs\" -s ms-winsoundevent:Notification.SMS")))
  )

(defun my-align-org-tags ()
  "Align 'org-mode' tags to the right border of the screen."
  (interactive)
  (setq org-tags-column (- 15 (window-width))))

(defun my-apply-colors ()
  "Apply theme colors to tabbar and powerline."
  (interactive)
  (when (bound-and-true-p tabbar-mode)
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
                        :background "#AD4900"
                        :foreground "#FEFEFE")
    (set-face-attribute 'powerline-active2 nil
                        :background "#FE9003"
                        :foreground "#000000")
    (set-face-attribute 'powerline-bold nil :weight 'bold :inherit `powerline-active1)
    (set-face-attribute 'powerline-inactive1 nil :inherit `mode-line-inactive)
    (set-face-attribute 'powerline-inactive-bold nil
                        :inherit `mode-line-inactive :weight 'bold)
    (set-face-attribute 'powerline-inactive2 nil :inherit `mode-line-inactive)
    (set-face-attribute 'powerline-inactive-bold nil :weight 'bold)
    (set-face-attribute 'powerline-alert nil :inherit `powerline-active1
                        :weight 'bold
                        :foreground `,(face-attribute 'font-lock-warning-face :foreground))
    (set-face-attribute 'powerline-inactive-alert nil
                        :background `,(face-attribute 'mode-line-inactive :background))
    )
  )

(defun my-beautify-json ()
  "Beautify json using Python."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun my-cancel-running-timer ()
  "Cancel running timer."
  (when (and (boundp 'my-running-timer) (timerp my-running-timer))
    (cancel-timer my-running-timer))
  (setq my-running-timer nil))

(defun my-compile-anywhere ()
  "Search for a Makefile in directories recursively, and compile when found."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (compile "make"))))

(defun my-countdown-timer (minutes title message)
  "Start a countdown timer with TITLE and MESSAGE for MINUTES, and cancel any running timers."
  (my-cancel-running-timer)
  (setq my-running-timer (run-with-timer (* 60 minutes) nil 'my-alerter title message)))

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

(defun my-open-custom-agenda ()
  "Open custom agenda view."
  (interactive)
  (org-agenda nil "f")                 ;; Open forthnight overview by default
  )

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


(defun my-repeat-compile-command-anywhere ()
  "Repeat last compile command without leaving the current frame."
  (interactive)
  (switch-to-buffer "*compilation*")
  (recompile)
  (my-switch-to-previous-buffer))

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

(defun my-reset-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))


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

(defun my-sort-words-on-line ()
  "Sort words in a line."
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/b0DT8fb5Ieo
  (interactive)
  (insert (mapconcat 'identity
                     (sort (split-string
                            (delete-and-extract-region
                             (point) (1+ (line-end-position))))
                           'string<)
                     " ")
          ))


(defun my-titlecase-converter ()
  "Convert region to titlecase."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "titlecase" (current-buffer) t)))

(defun my-rstlint()
  "Perform a syntax check over reStructuredText files."
  (interactive)
  ;; (compile (concat "python -mrstcheck " (buffer-file-name)))
  (compile (concat "rst-lint " (buffer-file-name))) ;; use rst-lint
  )

(defun my-prettier-overwrite()
  "Automatically overwrite file with opinionated prettier formatting."
  (interactive)
  (shell-command (concat "prettier --write " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm)
  )

(defun my-prettier-diff()
  "Perform a diff with the opinionated prettier formatted version."
  (interactive)
  (compile (concat "diff " (buffer-file-name) " <(prettier " (buffer-file-name) ") "))
  (with-current-buffer "*compilation*" (diff-mode)))

(defun my-compilation-exit-autoclose (STATUS code msg)
  "Close the compilation window if there was no error at all."
  (when (and (eq STATUS 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))

(setq compilation-exit-message-function 'my-compilation-exit-autoclose)

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
(defun my-copy-source-code-region (beg end)
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
      (my-open-custom-agenda)))
  )

(defun my-org-confirm-babel-evaluate (lang body)
  "When LANG is trusted, return false (don't ask for confirmation).
   If true, ask for confirmation to evaluate code."
  (message "Check language %s" lang)
  (not (member lang '("bash" "dot" "http" "plantuml" "python" "restclient" "sh" "shell"))))

(defun my-xml-formatter ()
  "Pretty format XML file."
  (interactive)
  ;; (save-buffer)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (xml-mode)
    (indent-region 0 (count-lines (point-min) (point-max)))
    )
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
         ("m" bm-toggle)
         ("M" bm-next)
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
          (lambda ()                   ;; Reclaim keybindings for shell mode
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
            (setq comint-process-echoes t))) ;; prevent echoing

(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "C-;") nil)
            (define-key flyspell-mode-map (kbd "C-.") 'flyspell-goto-next-error)))

(add-hook 'git-commit-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))           ;; No need for line numbers within commit files

(add-hook 'imenu-list-minor-mode-hook
          (lambda ()                   ;; Activate newly generated buffer immediately
            (switch-to-buffer-other-window "*Ilist*")))

(add-hook 'json-mode-hook
          (lambda ()
            (my-prettier-diff)
            (add-hook 'after-save-hook 'my-prettier-diff nil t)
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (my-prettier-diff)
            (add-hook 'after-save-hook 'my-prettier-diff nil t)
            (auto-fill-mode 1)
            ))

(add-hook 'nxml-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (highlight-indentation-mode t)
            (yas-minor-mode 1)
            ))

(add-hook 'prog-mode-hook (lambda ()
                            (flyspell-prog-mode)
                            (display-line-numbers-mode)
                            (which-function-mode)))

(add-hook 'rst-mode-hook (lambda ()    ;; Automatically perform syntax check after saving
                           (my-rstlint)
                           (add-hook 'after-save-hook 'my-rstlint nil t)
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
                                        ;            (flyspell-mode 1)
            (display-line-numbers-mode)
            (visual-line-mode 0)       ;; show a symbol for wrapping lines,
            (setq word-wrap 1)))       ;; but still wrap words nicely



;; builtin hooks
(add-hook 'emacs-startup-hook          ;; After loading init file and packages
          (lambda ()
            (if (bound-and-true-p my-theme)
                (load-theme my-theme t))
            ;; make sure that utf8 Unix line endings (LF) are default
            (setq-default default-buffer-file-coding-system 'utf-8-unix
                          buffer-file-coding-system 'utf-8-unix)
            (prefer-coding-system 'utf-8-unix)
            (my-set-default-font my-font)
            ;; Check if desktop mode is active or not
            (if (bound-and-true-p my-presentation-mode)
                (progn
                  (blink-cursor-mode 0)
                  (menu-bar-mode 0)
                  (tool-bar-mode 0)
                  (setq auto-save-default nil
                        visible-cursor nil))
              (helm-mode t)
              (projectile-mode t)
              (global-company-mode)
              (recentf-mode 1)                       ;; Enable recently opened files mode
              (tool-bar-mode 0)                      ;; Disable toolbar
              ;; (desktop-save-mode 1)                  ;; Automatically save / restore 'desktop' (buffers)
              (if (bound-and-true-p initial-buffer-choice)
                  (if (get-buffer "*scratch*")
                      (kill-buffer "*scratch*")))
              (when (bound-and-true-p my-start-with-agenda)
                (my-open-custom-agenda))
              (raise-frame)
              (require 'server)
              (or (server-running-p)     ;; Start server if not already running
                  (server-start))
              (setq gc-cons-threshold 800000) ;; Reset to default value
              )
            )
          )
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
            (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
            (local-set-key (kbd "C-c o") 'org-agenda-open-link)
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.1)
            (set-face-attribute 'org-level-2 nil :inherit 'outline-1 :height 1.1)
            (which-function-mode)      ;; Show contextual location in the mode line
            (yas-minor-mode 1)
            ))

(define-minor-mode textwriting-mode
  "Formatted text documents and automatic spelling check."
  :lighter "textwriting"
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (make-local-variable 'fill-column)
  (setq fill-column 95))

;;; init.el ends here
