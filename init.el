;;; init.el --- user-init-file -*- lexical-binding: t -*-
(menu-bar-mode 0)

;; When compiling in a non-graphical Emacs, apparently ‘tool-bar-mode’ may not
;; be defined.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

;; ‘bind-key’ is a run-time dependency of ‘use-package’.  Without it, ‘:bind’
;; throws “Symbol’s value as variable is void: ‘personal-keybindings’”.
(require 'bind-key)

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

;; ‘rainbow-mode’ highlights color specs like #f7a1c8, which should have a pink
;; background.  (Words like red, RoyalBlue should not be highlighted, because we
;; removed ‘emacs-lisp-mode’ from ‘rainbow-x-colors-major-mode-list’).
(use-package rainbow-mode
  :hook emacs-lisp-mode
  :config
  (setq rainbow-x-colors-major-mode-list nil))

;; This is (for now) an example of configuring a package when it’s available,
;; but not requiring it.
(use-package avy
  :no-require t
  :if (locate-library "avy")
  :bind (("C-c a" . avy-goto-line)))

(message "We have loaded init...")

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-error-on-warn: t
;; End:
