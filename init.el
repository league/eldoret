;;; init.el --- user-init-file  -*- lexical-binding: t -*-

;;; Commentary:

;; DONE Need an alternative to ‹ESC› to return to normal state.

;; DONE I'd like magit to open full-window, rather than a new, half other-window

;; DONE Implement hl-todo.

;; DONE In TTY Emacs, support cursor changes

;; DONE Possible to do default-text-scale just for current frame? → Doesn't seem
;; so, but anyway I rarely use multiple frames.

;; DONE evil-undo-system

;; DONE Also bind M-z, M-c, M-g to go to normal state, like evil-escape

;; TODO Need evil in more places, including minibuffer.

;; DONE Want a shortcut for saving, probably just ‹s›.

;; TODO ‘outline-minor-mode’ is missing keys I’m accustomed to, like ‹zB›

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

(use-package no-littering ;; Keep ‘user-emacs-directory’ clean
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

(use-package diminish
  :defer t
  :commands diminish
  :config
  (diminish 'eldoc-mode "edoc")
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode)))

;;;;; Startup dashboard/scratch screen

(setq inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message "league"
      initial-buffer-choice t
      initial-major-mode #'fundamental-mode
      initial-scratch-message ""
      confirm-kill-emacs #'y-or-n-p
      blink-cursor-interval 0.25)

(use-package server
  :defer t
  :commands server-running-p
  :init
  (add-hook 'emacs-startup-hook #'cl/start-server)
  :config
  ;; In a client frame, ‹C-x C-c› is ‘save-buffers-kill-terminal’ which leaves
  ;; the server/daemon running.  So make ‹C-x x c› exit the server too.
  (general-define-key
   :keymaps 'ctl-x-x-map
   "c" #'save-buffers-kill-emacs))

(defun cl/start-server ()
  "Start the server, unless already running."
  (unless (server-running-p)
    (server-mode)))

;;;; “Yep, here's your problem – someone set this thing to ‘evil’!”

(eval-and-compile
  (require 'evil-macros))

(use-package evil ;; I guess I joined the Dark Side™
  :init
  ;; DONE consider ‘evil-respect-visual-line-mode’
  ;; TODO I was never happy with ‘evil-complete-next-func’ ‹C-n›
  (setq evil-echo-state nil
        evil-want-C-u-delete t
        evil-want-C-u-scroll t          ; DONE Need to bind ‘universal-argument’
        evil-want-C-h-delete t
        evil-want-fine-undo t       ; I hope fine undo isn’t what wrecks repeat.
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree
        evil-want-keybinding nil)       ; Use evil-collection instead.
  :hook
  (emacs-startup . evil-mode)
  :config
  (evil-set-undo-system evil-undo-system)
  (general-define-key
   ;; Keys bound in motion state are inherited in normal, visual, and
   ;; operator state keymaps if they are not shadowed.
   :states 'motion
   ;; Like vim, we’re using ‹C-u› in normal state for scroll-up, and in insert
   ;; state for delete-to-beginning.  So let's bind capital ‹U› (otherwise
   ;; undefined) to ‘universal-argument’ in normal state. In insert state, I
   ;; think we'd only want an argument for repeating characters, and for that you
   ;; could do ‹M-1 M-0 *›, for example.
   "U" #'universal-argument)
  (general-define-key
   :states '(motion normal)
   ;; Saving: The ‹s› key in normal/visual states is pretty redundant.  In
   ;; normal, it’s equivalent to ‹cl› (substitute one character and continue
   ;; inserting).  In visual mode, it’s equivalent to ‹c› (change region).
   "s" #'save-buffer
   "M-s" #'save-some-buffers)

  (evil-define-text-object cl/a-whole-buffer (count &optional beg end type)
    "Select whole buffer, like \\[mark-whole-buffer]."
    (evil-range (point-min) (point-max)))
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "h" 'cl/a-whole-buffer) ;; ‹a h› selects entire buffer
  )

(use-package undo-tree
  :diminish ; DONE Probably completely diminish once I've settled on ‘undo-tree’
  :hook     ; DONE Bind ‘undo-tree-visualize’, default on ‹C-x u›
  (evil-local-mode . turn-on-undo-tree-mode)
  :general
  (:states 'motion
           "g/" #'undo-tree-visualize))

;;;; Key bindings

;;;;; MacOS modifiers

;; Need ‘defvar’ to avoid byte-compile warnings on Linux, but setting values
;; within the here does not take effect on Darwin.
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-right-option-modifier)
(when (eq window-system 'ns)
  ;; So then need ‘setq’ to change settings on Darwin.
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier 'control))

;;;;; Which-key

(use-package which-key
  :hook
  (emacs-startup . which-key-mode)
  :diminish
  :init
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 0.7
        which-key-idle-secondary-delay 0.3
        ;; Arrows and ellipses are too wide in Iosevka.  Setting the following
        ;; means arrow keys will show up as “left” and “right”.
        which-key-dont-use-unicode nil
        which-key-separator " "
        which-key-add-column-padding 1
        ;; Unfortunately, operator-state support flubs key seqs?
        which-key-show-operator-state-maps nil)
  :config
  ;; Disable ‘help-for-help’ because which-key does a better job when ‘help-map’
  ;; is open (and ‹C-h› interferes with which-key scrolling).
  (general-unbind :keymaps 'help-map "?" "<f1>" "<help>" "C-h" "C-s" "q"))

;;;;; Some evil shortcuts

(use-package evil-escape
  :diminish
  :init
  ;; Grepping words file to find a 2-letter sequence that's easy to type on
  ;; Dvorak but rare in words:
  ;; 3618 th – Obviously not workable, but just for comparison
  ;;  918 ht – This would be ideal position, but far too common.
  ;;  317 hl
  ;;   93 lh
  ;;   49 wm – I like this one, except “lawmaker” “sawmill”, “showman”, etc.
  ;;   11 kj
  ;;   11 gc – Mostly “eggcup” and “dogcart”, might be usable.
  ;;    2 jk – Good, but might prefer right hand [Notice the 2× “ht”]
  ;;    1 cg – Bingo!? Just the string “cg” itself is in the words file.
  (setq evil-escape-key-sequence "cg")

  ;; The ‘evil-escape-delay’ seems pretty well-tuned at 0.1.  I can type a
  ;; sequence of them in insert state – cgcgcgcgcgcgcgcg – even fairly quickly,
  ;; as long as I use a regular rhythm.  Activating the escape is more like
  ;; hitting a “grace note.”  Also should check how it escapes from things other
  ;; than insert mode.  When in a visual selection, ‹c› to change, and then ‹g›
  ;; to insert that letter seems okay.

  ;; Let’s allow unordered, so both ‹cg› and ‹gc› work.  Then treat it as a
  ;; chord in addition to grace note, hitting both more-or-less simultaneously.
  (setq evil-escape-unordered-key-sequence t)

  ;; Remember, we can also use the ‘evil-escape’ to quit transients like help
  ;; mode, magit, etc.
  :hook
  (emacs-startup . evil-escape-mode)
  :general
  (:states '(motion insert)
           "M-z" #'evil-escape
           "M-g" #'evil-escape
           "M-c" #'evil-escape))

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
  ("C-+" #'default-text-scale-increase
   "C-_" #'default-text-scale-decrease
   "C-|" #'default-text-scale-reset))

(use-package fontaine ;; Set font configurations using presets
  :no-require t
  :if (locate-library "fontaine")
  :defines fontaine-presets
  :commands fontaine-set-preset
  :defer t
  :init
  (setq fontaine-presets
        '((plex
           :default-family "IBM Plex Mono"
           :default-height 160
           :line-spacing 0
           :variable-pitch-family "IBM Plex Serif"
           :variable-pitch-height 1.0)))
  ;; Apply the initial preset when starting a graphical frame right away.
  (add-hook 'emacs-startup-hook #'cl/fontaine-initial-preset)
  ;; Apply the initial preset when creating a new frame (perhaps via daemon).
  (add-hook 'after-make-frame-functions #'cl/fontaine-initial-preset))

(defun cl/fontaine-initial-preset (&optional frame)
  "Configure initial font set for FRAME."
  (unless frame
    (setq frame (selected-frame)))
  (when (display-multi-font-p frame)
    ;; We’re seeing "Face height does not produce a positive integer" during
    ;; ‘fontaine--apply-bold-preset’, but otherwise it seems to work.
    (with-demoted-errors "Fontaine error: %S"
      (fontaine-set-preset 'plex frame))))

(use-package ef-themes ;; Colorful and legible themes
  :defer t
  :init
  (setq ef-themes-mixed-fonts t))

(defun cl/load-initial-theme ()
  "Load initial theme."
  (load-theme 'ef-cyprus 'no-confirm))

(add-hook 'emacs-startup-hook #'cl/load-initial-theme)

;;;;; Mode line

(use-package cyphejor
  :defer t
  :commands
  cyphejor--cypher
  :init
  (setq cyphejor-rules
        '(("mode" "")
          ("emacs" "e")
          ("fundamental" "Ø")
          ("inferior" "i" :prefix)
          ("interaction" "i" :prefix)
          ("interactive" "i" :prefix)
          ("python" "Py")
          ("magit" "Mg" :prefix)
          :upcase)))

(use-package telephone-line ;; A pretty and configurable mode-line
  :hook
  (emacs-startup . telephone-line-mode)
  :commands
  telephone-line-minor-mode-segment
  :config
  (telephone-line-defsegment* cl/telephone-line-position-segment ()
    "Displays buffer position, line, and column number.
If buffer has line numbers already, omit line number from mode line."
    `((-3 "%p") ;; Portion of buffer show: XX%, Top, Bot, All
      (ignore ,face)
      (display-line-numbers-mode
       (column-number-mode " C%2C")
       (line-number-mode
        (column-number-mode " L%2l C%2C" " L%2l")
        (column-number-mode " C%2C")))))
  (telephone-line-defsegment* cl/telephone-line-modes-segment ()
    "Displays abbreviated major and then minor mode information."
    (let ((rec-edit-help "Recursive edit, ‹C-M-c› to pop")
          (maj-name (symbol-name major-mode)))
      `((:propertize "%[" help-echo ,rec-edit-help face ,face)
        (:propertize
         ,(cyphejor--cypher maj-name cyphejor-rules)
         help-echo ,(concat maj-name "\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes")
         mouse-face mode-line-highlight
         local-map ,mode-line-major-mode-keymap
         face ,face)
        (:propertize "%]" help-echo ,rec-edit-help face ,face)
        ,(funcall (telephone-line-minor-mode-segment) face))))
  (telephone-line-defsegment* cl/telephone-line-evil-tag-segment ()
    "Displays the current evil state, abbreviated."
    (if (evil-visual-state-p)
        (cl-case evil-visual-selection
          ('block "Vb")
          ('line "Vl")
          (t "Vi"))
      (ignore face)
      (capitalize (seq-take (symbol-name evil-state) 2))))
  (setq
   telephone-line-evil-use-short-tag t
   telephone-line-primary-left-separator 'telephone-line-tan-left
   telephone-line-primary-right-separator 'telephone-line-tan-right
   telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
   telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right
   telephone-line-lhs
   '((evil cl/telephone-line-evil-tag-segment)
     (accent telephone-line-buffer-segment)
     (nil cl/telephone-line-modes-segment))
   telephone-line-rhs
   '((nil telephone-line-misc-info-segment)
     (accent telephone-line-vc-segment)
     (evil cl/telephone-line-position-segment))
   ;; This portion of mode-line disrupted the accent color scheme.
   mode-line-client
   `(:propertize ("" (:eval (if (frame-parameter nil 'client) "@" "")))
		 help-echo ,(purecopy "emacsclient frame")
                 face telephone-line-accent-active)))

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

;;;;; TTY

(use-package evil-terminal-cursor-changer ;; Cursor shape & color in terminal
  :defer t
  :commands
  evil-terminal-cursor-changer-activate)

(defun cl/tty-setup ()
  "Configure stuff related to terminal use, such as cursor and border glyphs."
  (evil-terminal-cursor-changer-activate)
  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00159.html
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'vertical-border
                          (make-glyph-code ?│)))

(add-hook 'after-init-hook #'cl/tty-setup)

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
  :diminish
  :config
  ;; Usually, colorizing plain words like red and RoyalBlue is distracting.
  (setq rainbow-x-colors-major-mode-list nil))

(use-package avy ;; Jump to arbitrary positions in visible text
  :general
  ("C-c a" #'avy-goto-line))

(use-package flymake
  :defer t
  :commands
  flymake-goto-prev-error flymake-goto-next-error
  :config ;; TODO maybe bind ‘flymake-show-buffer-diagnostics’
  (general-define-key
   :keymaps 'flymake-mode-map
   :states 'motion
   "[c" #'flymake-goto-prev-error
   "]c" #'flymake-goto-next-error))

;;;;; Revision control

(use-package magit ;; A git porcelain inside Emacs
  :config
  (add-hook 'magit-post-display-buffer-hook #'cl/magit-post-display-buffer)
  :general
  (:states 'normal
           "ZG" #'magit-status))

(defun cl/magit-post-display-buffer ()
  "Ensure that ‘magit-status’ interface takes up entire frame."
  ;; For some discussion on this issue, see
  ;; https://emacs.stackexchange.com/questions/17724/
  (when (provided-mode-derived-p major-mode 'magit-status-mode)
    (delete-other-windows)))

;;;;; Documentation

(use-package hl-todo ;; Highlight “TO-DO” and similar keywords
  :hook
  ((prog-mode ledger-mode latex-mode) . hl-todo-mode)
  :commands
  hl-todo-next hl-todo-previous
  :config
  (general-define-key
   :keymaps 'hl-todo-mode-map
   :states 'motion
   "]t" #'hl-todo-next
   "[t" #'hl-todo-previous))

;;;; Afterword

(when (file-exists-p custom-file)
  (load custom-file))

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-error-on-warn: t
;; eval: (setq elisp-flymake-byte-compile-load-path load-path)
;; eval: (flymake-mode)
;; End:

;;; init.el ends here
