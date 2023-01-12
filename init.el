;;; init.el --- user-init-file  -*- lexical-binding: t -*-

;;; Commentary:

;; DONE Also bind M-z, M-c, M-g to go to normal state, like evil-escape
;; DONE evil-undo-system
;; DONE I'd like magit to open full-window, rather than a new, half other-window
;; DONE Implement hl-todo.
;; DONE In TTY Emacs, support cursor changes
;; DONE Need an alternative to ‹ESC› to return to normal state.
;; DONE Need evil in more places, including minibuffer.
;; DONE Possible to do default-text-scale just for current frame? → Doesn't seem
;;      so, but anyway I rarely use multiple frames.
;; DONE repeat-mode and some relevant maps
;; DONE Want a shortcut for saving, probably just ‹s›.
;; DONE Add evil-numbers
;; DONE Binding for ‘find-library’
;; DONE evil-surround with quotes etc.
;; TODO ‘outline-minor-mode’ is missing keys I’m accustomed to, like ‹zB›
;; TODO Add toggles for themes
;; TODO Is leader available in magit? → No.
;; TODO Ensure sendmail works with message-mode

;;; Code:

;;;; Preliminaries

;;;;; Init speed-up

;; Some highlights from (elisp)Startup Summary:
;;    6. Load early-init.el
;;    9. Run ‘before-init-hook’
;;   10. Create graphical frame
;;   13. Load site-start
;;   14. Load init.el
;;   15. Load default
;;   17. Set ‘after-init-time’
;;   18. Run ‘after-init-hook’
;;   20. Run ‘tty-setup-hook’
;;   26. Run ‘emacs-startup-hook’
;;   28. Run ‘window-setup-hook’, with updated frame parameters

;; Doom Emacs init speed-up secrets, from
;; https://github.com/doomemacs/doomemacs/issues/310#issuecomment-354424413

(defconst cl/file-name-handler-alist file-name-handler-alist
  "Temporarily store ‘file-name-handler-alist’ during init.")

(setq gc-cons-threshold (* 384 1048576)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun cl/restore-gc-settings ()
  "Return to sensible GC settings after initialization."
  (setq gc-cons-threshold (* 16 1048576)
        gc-cons-percentage 0.1
        file-name-handler-alist cl/file-name-handler-alist))

(add-hook 'window-setup-hook #'cl/restore-gc-settings 95)

(defvar cl/after-after-init-time nil
  "Value of ‘current-time’ at end of ‘after-init-hook’.")

(defvar cl/after-startup-time nil
  "Value of ‘current-time’ at end of ‘window-setup-hook’.")

(defun cl/report-init-time ()
  "Report up to three elapsed time intervals during startup process."
  (let* ((t1 (float-time (time-subtract after-init-time before-init-time)))
         (accum t1)
         (mesg (format "init %.2fs" t1)))
    (when cl/after-after-init-time
      (let ((t2 (float-time
                 (time-subtract cl/after-after-init-time after-init-time))))
        (setq accum (+ accum t2))
        (setq mesg (concat mesg (format " + after %.2fs" t2))))
      (when cl/after-startup-time
        (let ((t3 (float-time (time-subtract cl/after-startup-time
                                             cl/after-after-init-time))))
          (setq accum (+ accum t3))
          (setq mesg (concat mesg (format " + startup %.2fs" t3)))))
      (setq mesg (concat mesg (format " = %.2fs" accum))))
    (message mesg)))

(defun cl/after-after-init-time ()
  "Report time at end of ‘after-init-hook’."
  (setq cl/after-after-init-time (current-time))
  (cl/report-init-time))

(defun cl/after-startup-time ()
  "Report time at end of ‘window-setup-hook’ – also after ‘emacs-startup-hook’."
  (setq cl/after-startup-time (current-time))
  (cl/report-init-time))

(add-hook 'after-init-hook #'cl/report-init-time -99)
(add-hook 'after-init-hook #'cl/after-after-init-time 99)
(add-hook 'window-setup-hook #'cl/after-startup-time 99)

;;;;; Init profiling support

(defmacro cl/profile-init ()
  "Optionally ENABLE profiling during init.
Get the report from the built-in profiler using \\[profiler-report].  If the
‘benchmark-init’ package is available, we can also call
\\[benchmark-init/show-durations-tree]."
  `(progn
     (require 'profiler)
     (profiler-start 'cpu)
     (add-hook 'window-setup-hook #'profiler-stop)
     (define-key help-map (kbd "C-p") #'profiler-report)
     ,(if (locate-library "benchmark-init")
          `(progn
             (require 'benchmark-init)
             (benchmark-init/activate)
             (add-hook 'window-setup-hook #'benchmark-init/deactivate)
             (define-key help-map (kbd "C-b")
               'benchmark-init/show-durations-tree)))))

;; (cl/profile-init)

;;;;; Package configuration utilities

(eval-and-compile
  (require 'use-package) ;; Configuration macros
  (require 'general) ;; Convenient macros for keybindings
  (require 'diminish)
  (setq use-package-verbose t
        use-package-compute-statistics t))

;; ‘bind-key’ is a run-time dependency of ‘use-package’.  Without it, ‘:bind’
;; throws “Symbol’s value as variable is void: ‘personal-keybindings’”.
(require 'bind-key)

(use-package use-package ;; Configuration macros
  :general
  ("C-h C-u" #'use-package-report))


(use-package auto-compile ;; Automatically (re-)compile elisp code.
  :init
  (setq auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t)
  ;; Not sure I need ‘auto-compile-on-load-mode’. All the packages I pull from
  ;; will be reliably pre-compiled (and read-only anyway).  The exception is
  ;; just this init file, and it should be reliably compiled on save.  Even if
  ;; it’s occasionally skipped, ‘load-prefer-newer’ will DTRT.
  :ghook
  ('emacs-lisp-mode-hook #'auto-compile-on-save-mode))

(use-package no-littering ;; Keep ‘user-emacs-directory’ clean
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

(use-package eldoc
  :defer t
  :diminish "edoc")

(use-package autorevert
  :defer t
  :config
  (diminish 'auto-revert-mode))

;;;;; Startup dashboard/scratch screen

(setq inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message "league"
      initial-buffer-choice nil
      initial-major-mode #'fundamental-mode
      initial-scratch-message ""
      confirm-kill-emacs #'y-or-n-p
      blink-cursor-interval 0.25)

(use-package server
  :defer t
  :commands server-running-p
  :init
  (add-hook 'after-init-hook #'cl/start-server)
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
  (setq evil-want-keybinding nil)
  (require 'evil-macros))

(defconst cl/leader "'")

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
        evil-undo-system 'undo-tree)
  :ghook 'after-init-hook
  :config
  (evil-set-undo-system evil-undo-system)
  ;; Initial setup for leader key: it clobbers ‘evil-goto-mark-line’ ‹'›, but
  ;; it's not a big loss because ‹'X› is the same as ‹`X0› and anyway let's
  ;; double it up in the leader map so we can use ‹''X› too.
  (when (symbolp (lookup-key evil-motion-state-map "'"))
    (general-unbind :states 'motion "'")
    (general-define-key
      :states 'motion :prefix cl/leader "'" 'evil-goto-mark-line))

  (general-define-key
   :states 'motion
   :prefix cl/leader
   "b" '(nil :wk "Buffers")
   "bb" #'switch-to-buffer
   "bl" #'buffer-menu-other-window
   "bx" #'kill-buffer
   "f" '(nil :wk "Files")
   "ff" #'find-file
   "fl" #'find-library
   "ft" #'load-theme
   "m" '(nil :wk "Mail")
   "t" '(nil :wk "Toggles"))

  (general-define-key
   :keymaps 'evil-window-map
   :prefix cl/leader
   "b" #'switch-to-buffer-other-window
   "f" #'find-file-other-window
   "l" #'find-library-other-window)

  (general-define-key
   ;; Keys bound in motion state are inherited in normal, visual, and
   ;; operator state keymaps if they are not shadowed.
   :states 'motion
   ;; Like vim, we’re using ‹C-u› in normal state for scroll-up, and in insert
   ;; state for delete-to-beginning.  So let's bind capital ‹U› (otherwise
   ;; undefined) to ‘universal-argument’ in normal state. In insert state, I
   ;; think we'd only want an argument for repeating characters, and for that
   ;; you could do ‹M-1 M-0 *›, for example.
   "U" #'universal-argument
   "g '" #'switch-to-buffer)

  (general-define-key
   :states '(motion normal) ;; Needs normal too, or ‘evil-substitute’ dominates.
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
  :defer t
  :diminish ; DONE Probably completely diminish once I've settled on ‘undo-tree’
  :ghook    ; DONE Bind ‘undo-tree-visualize’, default on ‹C-x u›
  ('evil-local-mode-hook #'turn-on-undo-tree-mode)
  :config
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  ;; This package binds ‹C-_› to ‘undo-tree-undo’, but I want to clear it for
  ;; use as ‘default-text-scale-decrease’.
  (general-unbind :keymaps 'undo-tree-map "C-_")
  (general-define-key
   :keymaps 'undo-tree-map
   :states 'motion
   "g/" 'undo-tree-visualize))

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
  :ghook 'emacs-startup-hook
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
  :ghook 'evil-local-mode-hook
  :general
  (:states '(motion insert replace)
           "M-z" #'evil-escape
           "M-g" #'evil-escape
           "M-c" #'evil-escape))

;;;;; Evil all the things!

(use-package evil-collection ;; Further keybindings and tools for Evil
  :after evil
  :commands evil-collection-init
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-always-run-setup-hook-after-load t)
  (evil-collection-init)
  :config
  (diminish 'evil-collection-unimpaired-mode)
  ;; The commands that begin with ‘evil-collection-unimpaired’ should have
  ;; abbreviations applied to ‘which-key’ presentation.
  (with-eval-after-load 'which-key
    (add-to-list 'which-key-replacement-alist
                 '((nil . "^evil-collection-unimpaired-")
                   nil . "ecu⠶"))))

(use-package evil-collection-unimpaired
  :defer t
  :diminish
  :init
  (setq evil-collection-unimpaired-want-repeat-mode-integration t))

;;;;; Repeatable bindings

(use-package repeat ;; Simple way to repeat previous & related commands
  :init
  ;; Can also set timeout separately for different commands using symbol
  ;; properties.
  (setq repeat-exit-timeout 3)
  :ghook 'emacs-startup-hook)

(defmacro cl/make-repeat-map (ksym doc binds)
  "Create a ‘repeat-map’ KSYM with given DOC string and BINDS.
BINDS is a list of elements like (KEY SYM) to place in the map.  But also each
command SYM will be linked to the map for use with ‘repeat-mode’."
  `(defconst ,ksym
     (let ((map (make-sparse-keymap)))
       (pcase-dolist (`(,key ,sym) ,binds)
         (define-key map key sym)
         (put sym 'repeat-map (quote ,ksym)))
       map)
     ,doc))

(cl/make-repeat-map
 cl/evil-win-next-repeat-map
 "Keymap for repetition of Evil next/previous window commands."
 '(("w" evil-window-next)
   ("W" evil-window-prev)))

(cl/make-repeat-map
 cl/evil-win-size-repeat-map
 "Keymap for repetition of Evil window resizing commands."
 '(("-" evil-window-decrease-height)
   ("+" evil-window-increase-height)
   ("=" evil-window-increase-height)
   ("_" evil-window-set-height)
   ("<" evil-window-decrease-width)
   ("," evil-window-decrease-width)
   (">" evil-window-increase-width)
   ("." evil-window-increase-width)
   ("|" evil-window-set-width)
   ("/" shrink-window-if-larger-than-buffer)))

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
  ;; DONE ‹C-_› conflicts with ‘undo-tree-undo’
  :general
  ("C-+" #'default-text-scale-increase
   "C-_" #'default-text-scale-decrease
   "C-|" #'default-text-scale-reset))

(use-package fontaine ;; Set font configurations using presets
  :no-require t
  :if (locate-library "fontaine")
  :commands fontaine-set-preset
  :defer t
  :init
  (general-setq
   fontaine-presets
   '((plex
      :default-family "IBM Plex Mono"
      :default-height 160
      :line-spacing 0
      :variable-pitch-family "IBM Plex Serif"
      :variable-pitch-height 1.0)))
  (general-after-gui
    ;; Getting "Face height does not produce a positive integer"
    ;; in ‘fontaine--apply-bold-preset’ but seems to work anyway.
    (with-demoted-errors "Fontaine error: %S"
      (fontaine-set-preset 'plex)))
  :general
  (:states 'motion :prefix cl/leader
           "fo" #'fontaine-set-preset))

(use-package ef-themes ;; Colorful and legible themes
  :defer t
  :init
  (setq ef-themes-mixed-fonts t))

(defun cl/load-initial-theme ()
  "Load initial theme."
  (load-theme 'ef-frost 'no-confirm))

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
  :ghook 'after-init-hook
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
        (pcase evil-visual-selection
          ('block "Vb")
          ('line "Vl")
          (_ "Vi"))
      (ignore face)
      (pcase evil-state
        ;; When evil doesn't get initialized correctly, ‘evil-state’ is nil but
        ;; telephone still tries to display it.
        ((pred null) "()")
        (_ (capitalize (seq-take (symbol-name evil-state) 2))))))
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
  :ghook 'prog-mode-hook
  :init
  (general-setq display-line-numbers-grow-only t)
  :general
  (:states 'motion :prefix cl/leader
           "tl" #'display-line-numbers-mode))

(use-package display-fill-column-indicator ;; Light line at right margin
  :ghook 'prog-mode-hook
  :gfhook #'cl/set-fill-column)

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
           (format-time-string " %1H:%M %a %1e %b")))) ; 23:19 Tue 5 Jul
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

(use-package olivetti ;; Centered, constrained-width editing
  :no-require t
  :diminish "›o‹"
  :config
  (general-setq
   ;; The body width can be specified as a proportion of the window, but unless
   ;; ‘visual-line-mode’ and/or ‘variable-pitch-mode’ are on, probably best to
   ;; use number of columns. Needs to be bigger than ‘fill-column’ to account
   ;; for line numbers.
   olivetti-body-width 90
   ;; Style is nil to use just margins, t to use fringes, or 'fancy for both.
   olivetti-style nil)
  :general
  (:prefix cl/leader :states 'motion
           "to" #'olivetti-mode))

(cl/make-repeat-map
 cl/olivetti-width-map
 "Key map for repetition of olivetti shrink/expand commands."
 '(("{" olivetti-shrink)
   ("}" olivetti-expand)
   ("[" olivetti-shrink)
   ("]" olivetti-expand)))

;;;;; TTY

(use-package evil-terminal-cursor-changer ;; Cursor shape & color in terminal
  :defer t
  :commands evil-terminal-cursor-changer-activate
  :init
  (general-after-tty
    (evil-terminal-cursor-changer-activate)
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00159.html
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (set-display-table-slot standard-display-table 'vertical-border
                            (make-glyph-code ?│))))


;;;; File system

;;;;; Track recent files

(use-package recentf ;; Track recently-opened files
  :defer t
  :init
  (setq recentf-max-saved-items 1024)
  :ghook 'emacs-startup-hook
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
  :ghook 'emacs-lisp-mode-hook 'nix-mode-hook
  :diminish
  :init
  ;; Usually, colorizing plain words like red and RoyalBlue is distracting.
  (general-setq rainbow-x-colors-major-mode-list nil))

(use-package envrc ;; Load environment based on .envrc in working dir
  :if (executable-find "direnv")
  :ghook ('emacs-startup-hook #'envrc-global-mode))

(use-package avy ;; Jump to arbitrary positions in visible text
  :defer t)

(use-package flymake
  :defer t
  :config ;; TODO maybe bind ‘flymake-show-buffer-diagnostics’
  (general-define-key
   :keymaps 'flymake-mode-map
   :states 'motion
   "[c" 'flymake-goto-prev-error
   "]c" 'flymake-goto-next-error))

(use-package evil-numbers ;; Increment/decrement numbers near point
  :general
  (:states 'motion
           "C-a" #'evil-numbers/inc-at-pt
           "C-q" #'evil-numbers/dec-at-pt
           "g C-a" #'evil-numbers/inc-at-pt-incremental
           "g C-q" #'evil-numbers/dec-at-pt-incremental))

;;;;; Delimiters

(defmacro cl/pair (key open close name)
  "Define a matching pair OPEN/CLOSE with trigger KEY and slug NAME."
  `(progn
     (setq-default evil-surround-pairs-alist
                   (append
                    '((,key ,open . ,close)
                      (,(aref close 0) ,open . ,close)
                      (,(aref open 0) ,(concat open " ") . ,(concat " " close)))
                    (default-value 'evil-surround-pairs-alist)))
     ,(let ((a-name (intern (concat "cl/evil-a-" name)))
            (i-name (intern (concat "cl/evil-i-" name)))
            (skey (char-to-string key)))
        `(progn
           (evil-define-text-object ,a-name (count &optional beg end type)
             :extend-selection nil
             (evil-select-paren ,open ,close beg end type count t))
           (general-define-key :keymaps 'evil-outer-text-objects-map
                               ,skey ',a-name)
           (evil-define-text-object ,i-name (count &optional beg end type)
             :extend-selection nil
             (evil-select-paren ,open ,close beg end type count))
           (general-define-key :keymaps 'evil-inner-text-objects-map
                               ,skey ',i-name)))))

(use-package evil-surround ;; Add/remove/change paired delimiters around point
  :after evil
  :ghook 'evil-local-mode-hook
  ;; Here is a summary of default ‘evil-surround-pairs-alist’:
  ;;   (       Parens ( with spaces )
  ;;   ) or b  Parens (without spaces)
  ;;   [       Brackets [ with spaces ]
  ;;   ]       Brackets [without spaces]
  ;;   {       Braces { with spaces }
  ;;   } or B  Braces {without spaces}
  ;;   < or t  HTML-like tag, <b>like this</b>
  ;;   >       Angles <without spaces>
  ;;   f       C-style function w/parens, func(like this)
  ;;   C-f     Lisp-style function w/parens, (func like this)
  :config
  (cl/pair ?c "‹" "›" "chevron")        ; U+2039/203A TeX \flq    Compose .<
  (cl/pair ?g "᚛" "᚜" "feather")        ; U+169B/169C
  (cl/pair ?d "“" "”" "dquote")         ; U+201C/201D TeX \ldq    Compose "<
  (cl/pair ?q "‘" "’" "quote")          ; U+2018/2019 TeX \lq     Compose '<
  (cl/pair ?u "᚛" "᚜" "guillemet")      ; U+00AB/00BB TeX \flqq   Compose <<
  (cl/pair ?r "｢" "｣" "corner")         ; U+FF62/FF63             Compose !<
  (cl/pair ?m "⟨" "⟩" "math-angle")     ; U+27E8/27E9 TeX \langle Compose ,<
  (setq-default evil-surround-pairs-alist
                (cl-remove-duplicates
                 (default-value 'evil-surround-pairs-alist)
                 :key #'car :from-end t)))

;;;;; Revision control

(use-package magit ;; A git porcelain inside Emacs
  :config
  (add-hook 'magit-post-display-buffer-hook #'cl/magit-post-display-buffer)
  :general
  (:states 'motion
           "ZG" #'magit-status))

(defun cl/magit-post-display-buffer ()
  "Ensure that ‘magit-status’ interface takes up entire frame."
  ;; For some discussion on this issue, see
  ;; https://emacs.stackexchange.com/questions/17724/
  (when (provided-mode-derived-p major-mode 'magit-status-mode)
    (delete-other-windows)))

(use-package diff-hl ;; Highlight uncommitted changes using VC
  :init
  (general-setq diff-hl-draw-borders nil)
  :ghook
  ('magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ('emacs-startup-hook #'global-diff-hl-mode)
  :config
  (general-define-key
   :keymaps 'diff-hl-mode-map
   :states 'motion
   "]d" 'diff-hl-next-hunk
   "[d" 'diff-hl-previous-hunk))

;;;;; Documentation

(use-package evil-commentary ;; Evil operators for (un-)commenting
  :diminish
  :ghook 'prog-mode-hook)

(use-package hl-todo ;; Highlight “TO-DO” and similar keywords
  :ghook
  'prog-mode-hook
  'ledger-mode-hook
  'latex-mode-hook
  :config
  (general-define-key
   :keymaps 'hl-todo-mode-map
   :states 'motion
   "]t" 'hl-todo-next
   "[t" 'hl-todo-previous))

;;;; Email

(use-package notmuch ;; Run notmuch email indexer within emacs
  :no-require t
  :if (locate-library "notmuch")
  :defer t
  :commands
  notmuch-search
  :init
  (defvar notmuch-saved-searches)
  (general-setq
   notmuch-hello-thousands-separator ","
   notmuch-show-all-tags-list t
   notmuch-always-prompt-for-sender t
   notmuch-fcc-dirs
   '(("league@contrapunctus.net" . "cng/Sent +sent -unread -inbox")
     ("christopher.league@liu.edu" . "\"liu/Sent Items\" +sent -unread -inbox")
     ("heychris@commandline.tv" . "cltv/Sent +sent -unread"))
   notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("flagged" . notmuch-search-flagged-face)
     ("deleted" . (:strike-through t)))
   notmuch-tagging-keys
   '(("a" notmuch-archive-tags "Archive")
     ("c" ("+chime" "-inbox" "-unread") "CHIME FRB")
     ("d" ("+deleted" "-inbox" "-unread") "Delete")
     ("f" ("+flagged") "Flag")
     ("n" ("+needs-filter" "-inbox" "-unread") "Needs filter")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("t" ("+tca" "-inbox" "-unread") "TCA message")
     ("u" notmuch-show-mark-read-tags "Mark read")
     ("w" ("+waiting" "-inbox" "-unread") "Waiting"))
   notmuch-search-sort-order 'newest-first
   notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox and (not tag:bulk)" :key "i")
     (:name "bulk" :query "tag:inbox and tag:bulk" :key "b")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent recently" :query "tag:sent and date:-2M..now" :key "r")
     (:name "tca" :query "tag:tca and date:2021-12-01.." :key "t")
     (:name "needs filter (gandi)" :query "tag:needs-filter and folder:/^cng/"
            :key "n")
     (:name "needs filter (liu)" :query "tag:needs-filter and folder:/^liu/"
            :key "l")
     (:name "waiting" :query "tag:waiting" :key "w")
     (:name "emacs-bug unread" :query "tag:emacs-bug is:unread" :key "e")
     (:name "orgmode unread" :query "tag:orgmode is:unread" :key "o")))
  ;; Add saved searches directly to keymap, as an alternative to binding
  ;; ‘notmuch-jump-search’. So I'm using ‹'mi› for Inbox rather than ‹'mji›.
  (dolist (item notmuch-saved-searches)
    (when (plist-get item :key)
      (general-define-key
       :states 'motion :prefix cl/leader
       (concat "m" (plist-get item :key))
       (cons (concat "notmuch⠶" (plist-get item :name))
             #'cl/notmuch-saved-search))))
  (general-define-key
   :states 'motion :prefix cl/leader
   "mm" 'notmuch
   "mc" 'notmuch-mua-new-mail
   "mg" 'notmuch-poll
   "ms" 'notmuch-search)
  :general
  ([remap compose-mail] #'notmuch-mua-new-mail))

(defun cl/notmuch-saved-search (key)
  "Invoke a saved search according to the last KEY pressed."
  (interactive (list last-command-event))
  (let (match (keystr (char-to-string key)))
    (dolist (item notmuch-saved-searches)
      (when (equal keystr (plist-get item :key))
        (setq match item)))
    (when match
      (notmuch-search (plist-get match :query)))))

(use-package ol-notmuch
  :no-require t
  :if (locate-library "ol-notmuch")
  :after ol)

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
