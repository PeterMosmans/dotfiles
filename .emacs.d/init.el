;;; init.el --- Emacs initialization file

;; Copyright (c) 2011-2016 Peter Mosmans

;; Author: Peter Mosmans <support AT go-forward.net>
;; Created: 2011
;; Version: (see git tag)
;; Keywords: emacs, dotfile, config

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Please contact support AT go-forward.net for questions and/or feedback

;;; Commentary:

;; Customized Emacs initialization file

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.

;; The following parameters are recognized, and can be added to
;; ~/.emacs.d/variables.el
;;
;; Note that the defaults will be used if they aren't defined...

(defvar my-font "Source Code Pro"
  "Font that will be used (if it is installed)")
(defvar my-scratch-file "~/scratch.txt"
  "Persistent scratch file that automatically will be opened on startup")
(defvar my-snippets-dir nil
  "A list of snippet directories that will be loaded by yasnippet")
(defvar my-theme 'misterioso
  "Theme that will be used")

(if (file-exists-p "~/.emacs.d/variables.el")
    (load "~/.emacs.d/variables.el"))

;; uncomment for some debugging options
;; (setq debug-on-error t)

(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load this keybinding first to facilitate editing init.el
(global-set-key (kbd "M-<f11>") (lambda () (interactive) (find-file user-init-file)))
(setq gc-cons-threshold 500000000)
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold 800000)
                       (require 'server)
                       (or (server-running-p)
                           (server-start))
                       (message "gc-cons-threshold restored to %S"
                                gc-cons-threshold)))

(defun enable-programmer-mode ()
  "Enable handy programming features / defaults."
  (interactive)
  (linum-mode 1)
  (if (featurep 'higlight-indentation-mode)
      (higlight-indentation-mode 1))
  (if (featurep 'yafolding-mode)
      (progn (
              (yafolding-mode 1)
              (yas-minor-mode 1))))
  (if (featurep 'flycheck-mode)
      (flycheck-mode 1))
  (if (featurep 'fci-mode)
      (fci-mode 1)))               ;; fci last, as it interferes with linum-mode


;; bootstrap
;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; add this first, as some packages need to be installed from unstable sources
(use-package package
  :config
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")))

;; define all necessary EXTERNAL alphabetically
;; bind:     keybindings
;; config:   execute code after a package is loaded
;; defer:    defer loading (automatic when comands and bind
;; disabled: (temporarily) disable a package
;; ensure:   make sure the package is installed
;; init:     execute code before a package is loaded

(use-package bm
  :ensure t
  :bind (("M-b" . bm-toggle)
         ("C-b" . bm-next))
  )

(use-package dash
  :defer t
  )

(use-package fill-column-indicator
  :commands fci-mode
  :config
  (setq fci-rule-color "light slate grey")
  )

(use-package flycheck
  :commands flycheck-mode
  :config (setq flycheck-highlighting-mode 'lines) ;; highlight whole line
  )

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-c p" "C-x r" "C-c !"
                                       (artist-mode "C-c C-a")
                                       (neotree-mode "C-c"))
        guide-key/idle-delay 0.2)
  (guide-key-mode t)
  :ensure t
  )

(use-package helm
  :bind ("M-x" . helm-M-x)
  :config
  (helm-mode t)
  (helm-autoresize-mode t)
  :defer t
  :ensure t
  )

(use-package helm-projectile
  :ensure t
  )

(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "light slate grey")
  (set-face-background 'highlight-indentation-current-column-face "light slate grey")
  (highlight-indentation-mode t)
  :ensure t
  )

(use-package let-alist
  :defer t
  )

(use-package magit
  :config
  (setq magit-diff-auto-show nil)
  :defer t
  :ensure t
  )

(use-package markdown-mode
  :defer t
  :ensure t
  )

(use-package neotree
  :commands (neotree)
  :config
  (setq neo-window-width 30)
  (setq neo-theme 'ascii)              ;; Don't use fancy icons
  :ensure t
  )

(use-package projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (projectile-global-mode 1)
  :defer t
  :ensure t
  )

(use-package pylint
  :defer t
  :ensure t
  )

(use-package smart-mode-line
  :config
  (setq sml/theme 'dark)
  (setq sml/mode-width 10)             ;; adjust the width of the smart-mode-line
  (add-to-list 'custom-safe-themes '"3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa")
  (add-to-list 'custom-safe-themes '"6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f")
  (sml/setup)
  :ensure t
  )

(use-package tabbar
  :config
  (set-face-attribute
   'tabbar-default nil       ;; left hand side tabbar...
   :background "#0c191c"
   :foreground "#0c191c"
   :box '(:line-width 1 :style nil))
  (set-face-attribute
   'tabbar-button nil                     ;; ...top & bottom
   :box '(:line-width 1 :color "#1f2427" :style nil))
  (set-face-attribute
   'tabbar-separator nil                  ;; generic left & right separator
   :background  "#1f2427"
   :height 0.1)
  (set-face-attribute
   'tabbar-selected nil                   ;; currently active tab
   :background "#262b2c"
   :foreground "#f57900"
   :bold 1                                ;; top & bottom separator
   :box '(:line-width 1 :color "#262b2c" :style nil))
  (set-face-attribute
   'tabbar-unselected nil                 ;; passive tab
   :background "#1f2427"
   :foreground "#555753"       ;; top & bottom separator
   :box '(:line-width 5 :color "#1f2427" :style nil))
  (set-face-attribute
   'tabbar-modified nil                 ;; modified tab
   :background "#1f2427"
   :foreground "#c4a000"
   :box '(:line-width 5 :color "#1f2427" :style nil))
  (set-face-attribute
   'tabbar-highlight nil                ;; highlighted
   :background "#ef2929"
   :foreground "#2e3436"
   :underline nil
   :box '(:line-width 5 :color "#f57900" :style nil))
  (if (boundp 'my-font)
      (when (member my-font (font-family-list))
        (set-face-attribute 'default nil :font my-font)))
  (when (display-graphic-p)
    (tool-bar-mode -1))                ;; enable the tabbar by default
  (tabbar-mode t)
  :ensure t
  )

(use-package yafolding
  :defer t
  :ensure t
  )

(use-package yasnippet
  :commands yas-minor-mode
  :config
  (if (boundp 'my-snippet-dirs)
      (dolist (item my-snippet-dirs)   ;; add item per item
        (add-to-list 'yas-snippet-dirs item)))
  (yas-reload-all)
  :ensure t
  )


;; let's get the show on the road
(if (boundp 'my-theme)
    (load-theme my-theme t))

;; file locations

;; workaround for removed function
;; (defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)
(if (boundp 'my-font)
    (when (member my-font (font-family-list))
      (set-face-attribute 'default nil :font my-font)
      (set-frame-font my-font nil t)))
;; define font for Unicode Private Use Area block
(when (member "Symbol" (font-family-list))
  (set-fontset-font "fontset-default" '(#xf000 . #xffff) (font-spec :name "Symbol")))
(setq
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
    (progn
      (setq find-program "c:/tools/find.exe" ;; use GNU find instead of Microsoft's
            w32-enable-caps-lock nil)))      ;; free the capslock key for useful stuff


(setq-default fill-column 80           ;; width of the screen for wrapping
              line-spacing 0
              indent-tabs-mode nil)    ; spaces are used when indenting

;; make sure that utf8 Unix line endings (LF) are default
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq
 dired-listing-switches "-agoh"
 recentf-save-file "~/.emacs.d/recentfiles.emacs"
 bookmark-default-file "~/.emacs.d/bookmarks.emacs"
 ediff-window-setup-function 'ediff-setup-windows-plain
 inhibit-startup-message t        ; remove welcome message
 inhibit-startup-echo-area-message nil
 visible-bell nil                 ; mute bell sound
 message-log-max t                ; keep and log all messages
 bookmark-save-flag nil           ; do not save bookmark when emacs quits
 make-backup-files nil            ;; do not create backups
 completion-ignore-case t         ;; ignore case when completing
 read-file-name-completion-ignore-case t
 comint-prompt-read-only t        ;; read only prompt for shell mode
 show-paren-style 'expression     ;; highlight entire bracket expression
 tab-width 4                      ;; default tab width
 recentf-max-saved-items 100      ;; recent files max save 100
 recentf-max-menu-items 15        ;; recent files max 15 in menu
 auto-save-interval 1000            ;; automatically save after 500 characters
 sentence-end-double-space nil    ;; don't use double spaces after a dot
 scroll-margin 1
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 use-package-always-ensure t           ;; always install missing packages
 )
(fset 'yes-or-no-p 'y-or-n-p)          ;; enable y/n answers to yes/no
(show-paren-mode t)                    ;; show matching parentheses
(global-font-lock-mode 1)              ;; syntax highlighting on by default
(global-linum-mode 0)                  ;; disable global line numbers
(global-visual-line-mode 1)            ;; act on visual lines, enable word wrap
(global-set-key [capslock] 'helm-M-x)
(cua-mode t)                           ;; enable CUA mode (C-z, C-x, C-c, C-v)
(recentf-mode 1)                       ;; turn recently opened files mode on
;; show week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian
                   (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
;; the modeline
(column-number-mode 1)                 ;; show column-number
;; these can be selectively enabled using linum-mode 1
(size-indication-mode 0)               ;; disable file size mode
(setq display-time-24hr-format t
      display-time-format "%H:%m"
      display-time-default-load-average nil)  ;; hide load average
(display-time)                         ;; show time in modeline

;; org mode settings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo-list)
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        ;; order todo list based on the state
        (todo todo-state-down priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-catch-invisible-edit 'show-and-error
      org-fontify-done-headline t      ;; change headline face when marked DONE
      org-log-into-drawer t            ;; insert notes & time stamps into drawer
      org-src-fontify-natively t       ;; fontify code in blocks
      org-time-clocksum-format         ;; don't show days
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      org-todo-keywords                ;; ! indicates timestamp, @ note & timestamp
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
(custom-set-faces
 '(org-done ((t (:strike-through t))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-1 :height 1.1)))))
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(add-hook 'kill-emacs-query-functions 'my/org-query-clock-out)

;; associate certain files with modes
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))
(add-to-list 'auto-mode-alist '("\\.ini\\'" . ntcmd-mode))

;; hooks for various BUILT IN modes (alphabetically)
(add-hook 'c-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "<tab>") 'calendar-forward-month)
            (define-key calendar-mode-map (kbd "S-<tab>") 'calendar-backward-month)
            (copy-face font-lock-keyword-face 'calendar-iso-week-face)
            (setq calendar-week-start-day 1   ;; DD/MM/YYYY
                  calendar-date-style 'european ;; easy navigation
                  )))

(add-hook 'comint-mode-hook
          (lambda ()                    ;; reclaim keybindings for shell mode
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
            (setq comint-process-echoes t))) ;; prevent echoing

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'javascript-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (highlight-indentation-mode 1)
            (linum-mode 1)
            (yas-minor-mode 1)))

(add-hook 'ntcmd-mode-hook
          (lambda ()
            (enable-programmer-mode)))

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (save-buffer)))

;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (save-current-buffer
;;               (dolist (buffer (buffer-list t))
;;                 (set-buffer buffer)
;;                 (when (member (buffer-file-name)
;;                               (mapcar 'expand-file-name (org-agenda-files t)))
;;                   (save-buffer))))))

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0)
            (yas-minor-mode 1)
            (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)))

(add-hook 'org-clock-in-hook           ;; autmatically save buffer when clocking in..
          (lambda ()
            (save-buffer)))

(add-hook 'org-clock-out-hook          ;; ..and clocking out
          (lambda ()
            (save-buffer)))

(add-hook 'perl-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'php-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            (enable-programmer-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (reveal-mode 1)
            (enable-programmer-mode)))

(add-hook 'shell-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (linum-mode 0)
            (toggle-truncate-lines 1))) ;; turn off word wrap for shell mode

(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 0)       ;; show a symbol for wrapping lines,
            (setq word-wrap 1)))       ;; but still wrap words nicely

(add-hook 'whitespace-mode
          (lambda ()
            (setq whitespace-style
                  (quote
                   (empty        ;; remove empty lines at beginning and end
                    spaces
                    tabs
                    trailing     ;; remove trailing characters
                    indentation  ;; fix indentation according to tab-mode
                    ;;                    newline      ;; remove new line char ?
                    space-mark
                    tab-mark
                    )))))

(setq org-replace-disputed-keys t
      org-disputed-keys
      '(([(shift up)] . [(ctrl up)])
        ([(shift down)] . [(ctrl down)])
        ([(shift left)] . [(ctrl left)])
        ([(shift right)] . [(ctrl right)])
        ([(shift meta right)] . [(shift control right)])
        ([(shift meta left)] . [(shift control left)])
        ([(shift meta up)] . [(shift control up)])
        ([(shift meta down)] . [(shift control down)])
        ([(shift control right)] . [(shift meta right)])
        ([(control a)] . [(shift control a)])
        ([(shift control left)] . [(shift meta left)]))
      )


;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(c-basic-offset 4)
 '(current-language-environment "UTF-8")
 '(cursor-color "#839496")
 '(ediff-patch-options "-f -N --strip=1 --binary")
 '(foreground-color "#839496")
 '(line-spacing nil)
 '(package-selected-packages
   (quote
    (speed-type bm helm-config helm-ag org-ref org-bullets auto-complete typit elfeed-org flylisp helm-projectile projectile guide-key php-mode helm esup aggressive-indent highlight-indentation yasnippet use-package atom-dark-theme aurora-theme cyberpunk-theme flycheck-pyflakes json-reformat web-mode flycheck-color-mode-line pylint neotree pandoc-mode markdown-mode yaml-mode vbasense rainbow-mode git-timemachine xcscope ecb yafolding fill-column-indicator bind-key pkg-info ace-jump-mode unison-mode tabbar smart-mode-line ntcmd nav naquadah-theme magit load-theme-buffer-local icicles gitignore-mode git-gutter-fringe+ flycheck flatland-theme firebelly-theme f expand-region display-theme dired-details deft darkburn-theme color-theme-solarized color-theme-sanityinc-solarized color-theme-buffer-local charmap calmer-forest-theme busybee-theme arduino-mode apache-mode)))
 '(safe-local-variable-values
   (quote
    ((pandoc/write . "html")
     (pandoc/output . t)
     (pandoc/write . "dzslides")
     (pandoc/read . "markdown")
     (pandoc/read . markdown)
     (pandoc/write . dzslides)
     (pandoc/output-format . dzslides)
     (pandoc/output-mode . dzslides))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))


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


;;; KEY BINDINGS

;; tabbar
(global-set-key (kbd "C-<tab>") 'tabbar-forward)
(global-set-key (kbd "C-S-<tab>") 'tabbar-backward)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tabbar-backward)
(global-set-key (kbd "M-<down>") 'tabbar-forward-group)
(global-set-key (kbd "M-<up>") 'tabbar-backward-group)
(global-set-key (kbd "<scroll>") 'scroll-lock-mode)

;; miscellaneous (for consistency)
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-n") 'find-file)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-c u") 'set-bfr-to-utf-8-unix)
(global-set-key (kbd "C-w") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-S-b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-(") 'check-parens) ;; matching parens
(global-set-key (kbd "C-=") 'er/expand-region) ;; make selection bigger and bigger
(global-set-key (kbd "C-|") 'yafolding-toggle-element)
(global-set-key (kbd "C-\\") 'yafolding-toggle-all)
(global-set-key (kbd "C-+") 'yafolding-toggle-element)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; function keys
(global-set-key (kbd "<f1>") 'magit-status)
                                        ;(global-set-key (kbd "S-<f1>") (lambda () (interactive) (bookmark-jump "keys")))
(global-set-key (kbd "S-<f1>") 'petermosmans/cleanup)
                                        ;(global-set-key (kbd "C-<f1>") 'org-ctrl-c-ctrl-c)
(global-set-key (kbd "C-<f1>") 'show-file-name)
                                        ;(global-set-key (kbd "M-<f1>") 'compile-quietly)
(global-set-key (kbd "M-<f1>") 'code-review-region)
                                        ;'neotree-toggle)
                                        ;(global-set-key (kbd "M-<f1>") 'org-align-all-tags)
                                        ;(global-set-key (kbd "C-<f1>") 'shrink-window)
                                        ;(global-set-key (kbd "M-<f1>") 'enlarge-window)

;; navigation in buffer (file)
(global-set-key (kbd "<f2>") 'switch-to-previous-buffer)
(global-set-key (kbd "S-<f2>") 'describe-line-endings)
                                        ; word count on region
(global-set-key (kbd "C-<f2>") (lambda () (interactive) (shell-command-on-region (point) (mark) "wc -w")))
                                        ;(global-set-key (kbd "C-<f2>") (lambda () (interactive) (
                                        ; (narrow-to-region (point-min) (point-max)) (count-matches "\\sw+"))))
                                        ;(global-set-key (kbd "M-<f2>") 'view-echo-area-messages)
                                        ;(global-set-key (kbd "M-<f2>") 'linum-mode)
(global-set-key (kbd "M-<f2>") (lambda () (interactive) (browse-url-of-file (buffer-name))))

;; searching
(global-set-key (kbd "<f3>") 'isearch-repeat-forward)
(global-set-key (kbd "S-<f3>") 'find-grep-dired)
(global-set-key (kbd "C-<f3>") 'diff)
(global-set-key (kbd "M-<f3>") 'org-show-todo-key)

;; scratchpad, text modes, closing
(global-set-key (kbd "<f4>") 'yas-insert-snippet)
(global-set-key (kbd "S-<f4>") (lambda () (interactive) (switch-to-buffer "scratch.txt")))
(global-set-key (kbd "C-<f4>") 'org-mode)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)

;; buffer (file) operations
                                        ;(global-set-key (kbd "<f5>") (lambda () (interactive) (load-file (buffer-name))))
(global-set-key (kbd "<f5>") 'projectile-find-file)
(global-set-key (kbd "S-<f5>") 'revert-buffer)
(global-set-key (kbd "C-<f5>") 'save-buffer)
(global-set-key (kbd "M-<f5>") 'helm-find-files)

;; windows
(global-set-key (kbd "<f6>") 'other-window)
(global-set-key (kbd "S-<f6>") 'split-window-vertically)
(global-set-key (kbd "C-<f6>") 'delete-other-windows)
(global-set-key (kbd "M-<f6>") 'split-window-horizontally)
;; balance-windows
;; enlarge window

;; frames
(global-set-key (kbd "<f7>") 'other-frame)
(global-set-key (kbd "S-<f7>") 'make-frame)
(global-set-key (kbd "C-<f7>") 'delete-frame)
(global-set-key (kbd "M-<f7>") 'balance-windows)

;; tabbar navigation
(global-set-key (kbd "<f8>") 'tabbar-backward)
(global-set-key (kbd "S-<f8>") 'tabbar-backward-group)
(global-set-key (kbd "C-<f8>") 'open-dired)
(global-set-key (kbd "M-<f8>") 'neotree-toggle)

;; browsing / exploring
(global-set-key (kbd "<f9>") 'tabbar-forward)
(global-set-key (kbd "S-<f9>") 'tabbar-forward-group)
(global-set-key (kbd "C-<f9>") 'browse-url-of-buffer)
(global-set-key (kbd "M-<f9>") 'color-theme-select)

;; show / display
(global-set-key (kbd "<f10>") 'ibuffer)
(global-set-key (kbd "S-<f10>") 'recentf-open-files)
(global-set-key (kbd "C-<f10>") 'calendar)
(global-set-key (kbd "M-<f10>") 'projectile-ibuffer)

;; bookmarks
(global-set-key (kbd "<f11>") 'bookmark-jump)
(global-set-key (kbd "S-<f11>") 'xah-run-current-file)
(global-set-key (kbd "C-<f11>") 'bookmark-set)


;; date / time
(global-set-key (kbd "<f12>") 'insert-current-date-time)
(global-set-key (kbd "S-<f12>") 'org-clock-in)
(global-set-key (kbd "C-<f12>") 'org-clock-out)
(global-set-key (kbd "M-<f12>") 'org-dblock-update)


;;; PUTTY HACKS

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

;; final statements

;; steps which can be performed after loading all repository functions
(add-hook 'after-init-hook
          (lambda ()
            (if (boundp 'my-scratch-file)
                (progn
                  (find-file my-scratch-file)  ;; only show it if it's the only file
                  (if (get-buffer "*scratch*")
                      (kill-buffer "*scratch*"))))
            ))


;; tango theme for tabbar-mode
;; tango colors defined, see http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines
(defconst aluminium-1 "#eeeeec")
(setq aluminium-2 "#d3d7cf"
      aluminium-3 "#babdb6"
      aluminium-4 "#888a85"
      aluminium-5 "#555753"
      aluminium-6 "#2e3436"
      butter-1 "#fce94f"
      butter-2 "#edd400"
      butter-3 "#c4a000"
      orange-1 "#fcaf3e"
      orange-2 "#f57900"
      orange-3 "#ce5c00"
      chocolate-1 "#e9b96e"
      chocolate-2 "#c17d11"
      chocolate-3 "#9f5902"
      chameleon-1 "#8ae234"
      chameleon-2 "#73d216"
      chameleon-3 "#4e9a06"
      sky-blue-1 "#729fcf"
      sky-blue-2 "#3465a4"
      sky-blue-3 "#204a87"
      plum-1 "#ad7fa8"
      plum-2 "#75507b"
      plum-3 "#5c3566"
      scarlet-red-1 "#ef2929"
      scarlet-red-2 "#cc0000"
      scarlet-red-3 "#a40000"
      tabbar-inactive "#1f2427"
      tabbar-inactive-text "#555753"
      tabbar-active "#262b2c"
      tabbar-active-text orange-2
      tabbar-background "#0c191c")
;; note that specifying color names doesn't work in the box attribute
(add-hook 'tabbar-mode-hook
          (lambda ()
            (set-face-attribute
             'tabbar-default nil
             :background tabbar-background            ;; left hand side tabbar...
             :foreground tabbar-background
             :box '(:line-width 1 :style nil))
            (set-face-attribute
             'tabbar-button nil                     ;; ...top & bottom
             :box '(:line-width 1 :color "#1f2427" :style nil))
            (set-face-attribute
             'tabbar-separator nil                  ;; generic left & right separator
             :background tabbar-inactive
             :height 0.1)
            (set-face-attribute
             'tabbar-selected nil                   ;; currently active tab
             :background tabbar-active
             :foreground tabbar-active-text
             :bold 1                                ;; top & bottom separator
             :box '(:line-width 1 :color "#262b2c" :style nil))
            (set-face-attribute
             'tabbar-unselected nil                 ;; passive tab
             :background tabbar-inactive
             :foreground tabbar-inactive-text       ;; top & bottom separator
             :box '(:line-width 5 :color "#1f2427" :style nil))
            (set-face-attribute
             'tabbar-modified nil                 ;; modified tab
             :background tabbar-inactive
             :foreground butter-3
             :box '(:line-width 5 :color "#1f2427" :style nil))
            (set-face-attribute
             'tabbar-highlight nil
             :background scarlet-red-1                  ;; highlighted
             :foreground aluminium-6
             :underline nil
             :box '(:line-width 5 :color "#f57900" :style nil))
            ))

;; testcode ediff
;; http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version

(defmacro ediff-char-to-buftype (arg)
  `(cond ((memq ,arg '(?a ?A)) 'A)
         ((memq ,arg '(?b ?B)) 'B)
         ((memq ,arg '(?c ?C)) 'C)
         ((memq ,arg '(?d ?D)) 'D)
         ))

                                        ;(require 'ediff)

;; Literally copied from ediff-util
;; need to re-evaluate because it uses the macro defined above
;; and the compiled version needs to be re-compiled with the new definition
;; why a macro????
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

;; add keybinding in a hook b/c the keymap isn't defined until the hook is run
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


;; functions --- Custom functions and variables

(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(defun set-bfr-to-utf-8-unix ()
  (interactive)
  (set-buffer-file-coding-system
   'utf-8-unix)
  )

(defun my-beautify-json ()
  "Beautify json using Python."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun my-reset-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

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

(defun compile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))

;; ask the user if they wish to clock out before killing Emacs
;; http://comments.gmane.org/gmane.emacs.orgmode/81781
(defun my/org-query-clock-out ()
  "Ask the user before clocking out.
    This is a useful function for adding to `kill-emacs-query-functions'."
  (if (and (featurep 'org-clock)
           (funcall 'org-clocking-p)
           (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t))                             ; only fails on keyboard quit or error

;; http://emacswiki.org/emacs/SwitchingBuffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

                                        ; http://www.blogbyben.com/2015/04/the-joy-of-elisp-powered-code-review.html
(defun code-review-region (beg end)
  "Copies a region of source code, adds line numbers."
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

;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)))


(defun petermosmans/cleanup ()
  "Enable handy programming features / defaults"
  (interactive)
  (whitespace-cleanup)
                                        ;  (fill-paragraph)
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

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string "%d-%m-%Y %H:%M" (current-time))))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defun describe-line-endings ()
  (interactive)
  (message (describe-variable buffer-file-coding-system)))

;; (defvar hs-all-hidden nil "Current state of hideshow for toggling all.")

;; (defun toggle-hide-all ()
;;   (interactive)
;;   (setq hs-all-hidden (not hs-all-hidden))
;;   (if hs-all-hidden
;;       (hs-hide-all)
;;     (hs-show-all)))
;;  (setq explicit-bash-args '("--login" "-i"))
;; Note that M-x shell uses explicit-PROGRAM-args,
;; where PROGRAM is the filename part of the shell's pathname.
;; This is why you should not include the .exe when setting the shell.

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

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
                                        ;            ("cmd" . ,(if (getenv "ProgramW6432") "c:\\windows\\sysnative\\cmd.exe /c" "c:\\windows\\system32\\cmd.exe /c"))
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
                                        ;         (fName (subst-char-in-string ?/ ?\\ (expand-file-name buffer-file-name)))
         (fName (expand-file-name buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer) ) )
    (if (string-equal fSuffix "md") ; special case for markdown file
        (progn
                                        ; run pandoc on the file, and, convert the filename .md to .html
                                        ;          (pandoc-run-pandoc fName)
                                        ;          (shell-command (concat "\"c:\\program files (x86)\\mozilla firefox\\firefox.exe\" file://" (concat (file-name-sans-extension fName) ".html"))))
          (shell-command (concat (concat "bash " (file-name-directory fName) "build.sh"))))
                                        ;    (cmdStr (concat progName fName))
      (if (string-equal fSuffix "el") ; special case for emacs lisp
          (load (file-name-sans-extension fName))
        (message "Running...")
        (shell-command (concat progName fName) "*xah-run-current-file output*" )
        (message "No recognized program file suffix for this file.")
        ) ) ))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-D-to-C))

;; end testcode ediff
(put 'downcase-region 'disabled nil)



;;; init.el ends here
