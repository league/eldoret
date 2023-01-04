;;; init.el --- user-init-file -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'use-package))

;; ‘bind-key’ is a run-time dependency of ‘use-package’.  Without it, ‘:bind’
;; throws “Symbol’s value as variable is void: ‘personal-keybindings’”.
(require 'bind-key)

(use-package use-package
  :init
  (setq use-package-verbose t
        use-package-compute-statistics t)
  :bind
  (("C-h C-u" . use-package-report)))

;;;; Frame settings

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars)))

;;;; Programming modes

;; ‘rainbow-mode’ highlights color specs like #f7a1c8, which should have a pink
;; background.  (Words like red, RoyalBlue should not be highlighted, because we
;; removed ‘emacs-lisp-mode’ from ‘rainbow-x-colors-major-mode-list’).
(use-package rainbow-mode
  :hook emacs-lisp-mode
  :config
  (setq rainbow-x-colors-major-mode-list nil))

(use-package avy
  :bind (("C-c a" . avy-goto-line)))

;;;; Afterword
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-error-on-warn: t
;; End:

;;; init.el ends here

