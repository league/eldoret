;;; init.el --- user-init-file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Preliminaries

;;;;; Init speed-up

;; Doom Emacs init speed-up secrets, from
;; https://github.com/doomemacs/doomemacs/issues/310#issuecomment-354424413

(defvar cl/file-name-handler-alist file-name-handler-alist
  "Temporarily store ‘file-name-handler-alist’ during init.")

(setq gc-cons-threshold (* 384 1048576)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun cl/startup-hook ()
  "Restore GC settings after init, and report startup time."
  (message "Init took %.3fs"
           (float-time (time-subtract after-init-time before-init-time)))
  (setq gc-cons-threshold (* 16 1048576)
        gc-cons-percentage 0.1
        file-name-handler-alist cl/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'cl/startup-hook)

;;;;; Init profiling support

(defmacro cl/profile-init (enable)
  "Optionally ENABLE profiling during init.
Get the report from the built-in profiler using \\[profiler-report].  If the
‘benchmark-init’ package is available, we can also call
\\[benchmark-init/show-durations-tree]."
  (if enable
      `(progn
         (require 'profiler)
         (profiler-start 'cpu)
         (add-hook 'after-init-hook #'profiler-stop)
         (define-key help-map (kbd "C-p") #'profiler-report)
         ,(if (locate-library "benchmark-init")
              `(progn
                 (require 'benchmark-init)
                 (benchmark-init/activate)
                 (add-hook 'after-init-hook #'benchmark-init/deactivate)
                 (define-key help-map (kbd "C-b")
                   'benchmark-init/show-durations-tree))))))

(cl/profile-init t)

;;;;; Package configuration utilities

(eval-and-compile
  (require 'use-package) ;; Configuration macros
  (require 'general) ;; Convenient macros for keybindings
  (setq use-package-verbose t
        use-package-compute-statistics t))

;; ‘bind-key’ is a run-time dependency of ‘use-package’.  Without it, ‘:bind’
;; throws “Symbol’s value as variable is void: ‘personal-keybindings’”.
(require 'bind-key)

(use-package use-package ;; Configuration macros
  :general
  ("C-h C-u" #'use-package-report))

;; Automatically (re-)compile elisp code.
(use-package auto-compile
  :init
  (setq auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t)
  (auto-compile-on-load-mode)
  :hook (emacs-lisp-mode . auto-compile-on-save-mode)
  :commands auto-compile-on-load-mode)

(use-package no-littering) ;; Keep ‘user-emacs-directory’ clean

;;;;; Startup dashboard/scratch screen

(setq inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message "league"
      initial-buffer-choice t
      initial-major-mode #'fundamental-mode
      initial-scratch-message "")

;;;; Key bindings

;;;;; MacOS modifiers

;; Need ‘defvar’ to avoid byte-compile warnings on Linux, but setting values within the here does not
;; take effect on Darwin.
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-right-option-modifier)
(when (eq window-system 'ns)
  ;; So then need ‘setq’ to change settings on Darwin.
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier 'control))

;;;; Visual interface

;;;;; Fonts and themes

(use-package default-text-scale ;; Adjust font size in all frames
  :no-require t
  :if (locate-library "default-text-scale")
  :init
  (setq frame-resize-pixelwise t)
  ;; Nice to make bindings the same as kitty (requires ctrl-shift) and
  ;; firefox (works with or without shift) — they don't need to be
  ;; usable from TTY.
  :general
  ("C-+" #'default-text-scale-increase)
  ("C-_" #'default-text-scale-decrease)
  ("C-|" #'default-text-scale-reset))

(use-package ef-themes ;; Colorful and legible themes
  :defer t
  :init
  (setq ef-themes-mixed-fonts t))

(defun cl/load-initial-theme ()
  "Load initial theme."
  (load-theme 'ef-cyprus 'no-confirm))

(add-hook 'emacs-startup-hook #'cl/load-initial-theme)

;;;;; Mode line

(use-package telephone-line ;; A pretty and configurable mode-line
  :hook
  (emacs-startup . telephone-line-mode)
  :config
  (telephone-line-defsegment* cl/telephone-line-position-segment ()
    "Displays buffer position, line, and column number.
If buffer has line numbers already, omit line number from mode line."
    (progn
      (ignore face)
      '((-3 "%p") ;; Portion of buffer show: XX%, Top, Bot, All
        ;; For alignment of Line:Column position, let’s assume 2-digit column
        ;; number and 4-digit line number, plus the space and leading char.
        (display-line-numbers-mode
         (column-number-mode (4 " C%C"))
         (line-number-mode
          (column-number-mode (10 " L%l C%C") (6 " L%l"))
          (column-number-mode (4 " C%C")))))))
  ;; (telephone-line-defsegment* cl/telephone-line-evil-tag-segment ()
  ;;   "Displays the current evil state, abbreviated."
  ;;   (if (evil-visual-state-p)
  ;;       (cl-case evil-visual-selection
  ;;         ('block "Vb")
  ;;         ('line "Vl")
  ;;         (t "Vi"))
  ;;     (ignore face)
  ;;     (capitalize (seq-take (symbol-name evil-state) 2))))
  :custom
  (telephone-line-evil-use-short-tag t)
  (telephone-line-primary-left-separator 'telephone-line-tan-left)
  (telephone-line-primary-right-separator 'telephone-line-tan-right)
  (telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left)
  (telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
  (telephone-line-lhs
   '(;; (evil cl/telephone-line-evil-tag-segment)
     (accent telephone-line-buffer-segment)
     (nil telephone-line-major-mode-segment) ;; Maybe omit major-mode?
     (nil telephone-line-minor-mode-segment)))
  (telephone-line-rhs
   '((nil telephone-line-misc-info-segment)
     (accent telephone-line-vc-segment)
     (evil cl/telephone-line-position-segment))))

(add-hook 'prog-mode-hook #'column-number-mode)

;;;;; Margins

(use-package display-line-numbers ;; Line numbers in left margin
  :hook
  (prog-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-grow-only t))

(use-package display-fill-column-indicator ;; Light line at right margin
  :init
  (add-hook 'prog-mode-hook #'cl/set-fill-column)
  :hook
  (prog-mode . display-fill-column-indicator-mode))

(defun cl/set-fill-column (&optional col)
  "Set ‘fill-column’ to COL (default 80) unless already set locally."
  (unless (assoc 'fill-column (buffer-local-variables))
    (setq fill-column (or col 80))))

;;;;; Full screen

(use-package echo-bar ;; Turn echo area into a custom status bar
  :defer t
  :commands echo-bar-mode
  :init
  (setq echo-bar-minibuffer t
        echo-bar-update-interval 3
        echo-bar-right-padding 2 ;; ≤1 looks bad on TTY
        echo-bar-format
        '((:eval
           (when battery-status-function
             (let* ((bs (funcall battery-status-function))
                    (bt (battery-format "%B" bs)))
               (concat
                (battery-format " %p%%%%" bs)
                (pcase bt
                  ("charging" "+")
                  ("discharging" "-")
                  ("fully-charged" "")
                  (_ bt))))))
          (:eval
           (format-time-string " %H:%M %a %1e %b")))) ; 23:19 Tue 5 Jul
  (add-hook 'window-configuration-change-hook #'cl/echo-bar-when-fullscreen)
  :config
  (require 'battery))

(defun cl/echo-bar-when-fullscreen ()
   "Make echo area display status info whenever we are fullscreen.
Full-screen mode activated with \\[toggle-frame-fullscreen] hides
the date/time and battery display on the desktop's panel, so this
can help."
   ;; Are we full-screen?
   (let ((fs (eq (frame-parameter nil 'fullscreen) 'fullboth)))
     (if fs
         (echo-bar-mode 1)
       ;; We're not full-screen, but don't force the echo-bar package to load
       ;; just to turn it off, if it hasn't been used yet!
       (if (bound-and-true-p echo-bar-mode)
           (echo-bar-mode -1)))))

;;;; File system

;;;;; Track recent files

(use-package recentf ;; Track recently-opened files
  :defer t
  :init
  (setq recentf-max-saved-items 1024)
  :hook
  (emacs-startup . recentf-mode)
  :commands
  recentf-save-list
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; Generally, recentf-save-list is only run when exiting emacs, but we should
  ;; save periodically during idle time.
  (add-hook 'recentf-mode-hook #'cl/recentf-save-hook))

(defvar cl/recentf-changes-since-save 0
  "How many times ‘recentf-list’ has been modified since last save.")

(defun cl/recentf-change ()
  "Record a change to ‘recentf-list’."
  (cl-incf cl/recentf-changes-since-save)
  nil)

(defun cl/recentf-save ()
  "Save ‘recentf-list’ if it has changed since last save."
  (unless (zerop cl/recentf-changes-since-save)
    (recentf-save-list)
    (setq cl/recentf-changes-since-save 0)))

(defvar cl/recentf-save-timer nil
  "Timer info for ‘cl/recentf-save’.")

(defun cl/recentf-save-hook ()
  "Mode hook to enable/disable saving ‘recentf-list’ periodically."
  (if recentf-mode
      (progn
        (add-hook 'find-file-hook #'cl/recentf-change)
        (add-hook 'write-file-functions #'cl/recentf-change)
        (add-hook 'kill-buffer-hook #'cl/recentf-change)
        (setq cl/recentf-save-timer
              (run-with-idle-timer 10 'repeat #'cl/recentf-save)))
    (remove-hook 'find-file-hook #'cl/recentf-change)
    (remove-hook 'write-file-functions #'cl/recentf-change)
    (remove-hook 'kill-buffer-hook #'cl/recentf-change)
    (setq cl/recentf-save-timer (cancel-timer cl/recentf-save-timer))))


;;;; Programming modes

(use-package rainbow-mode ;; Colorize color specs like #bff
  :hook emacs-lisp-mode nix-mode
  :config
  ;; Usually, colorizing plain words like red and RoyalBlue is distracting.
  (setq rainbow-x-colors-major-mode-list nil))

(use-package avy ;; Jump to arbitrary positions in visible text
  :bind (("C-c a" . avy-goto-line)))

;;;;; Languages

(use-package nix-mode :defer)

;;;; Afterword
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-error-on-warn: t
;; End:

;;; init.el ends here
