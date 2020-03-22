;;; init.el --- jos's emacs configuration

;; Copyright (C) 2016 Josaphat Valdivia

;; Author: Josaphat Valdivia <jos@josaphat.co>
;; Created: 18 Aug 2016

;; This file is not part of GNU Emacs.

;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at

;;       http://www.apache.org/licenses/LICENSE-2.0

;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;; Commentary:

;; The general way we're structuring our init files is to put all of
;; our initialization code modules in files named init-*.el under the
;; "lisp" subdirectory (i.e. inside ~/.emacs.d/lisp).  However, most
;; of the lifting for configuring packages is going to be done by
;; use-package.el which allows us to keep our package configurations
;; neat.  If they don't make sense to put into a use-package call or
;; they start to get big or unwieldy, then they should get their own
;; file in lisp/.

;;; Code:

;;;; General editor configuration


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; We already do this in our `lisp/init-elpa.el' file.
;(package-initialize)

(tool-bar-mode -1)			; We don't need all those
					; fancy buttons
;; By default, parens are highlighted when you insert a `)`.  We want
;; to make it so that they'll be highlighted any time Point is on
;; them.
(show-paren-mode t)
;; Put emacs customizations into their own file.  We don't need them
;; cluttering up our init.el, especially if we release it for use by
;; others.  If we change its location, we have to load it ourselves.
;; Using the parameter 'noerror makes sure that we ignore the irror
;; for the case where the file doesn't yet exist.
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(load custom-file 'noerror)
;; This makes sure that backup files aren't in the way but that they
;; still exist in case we ever need them.  You always think you can
;; get away without backups until you lose something.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transorms
      `((".*" ,temporary-file-directory t)))

;; If you're keeping the splash screen, might as well spice it up :^)
;; I couldn't find good documentation on requirements, but this didn't
;; work for me until I tried an image that was 249px in height.
(setq fancy-splash-image (expand-file-name "~/Pictures/hills_sing.png"))
(setq inhibit-startup-screen t)		; I'm not keeping it...

;; Add ~/.emacs.d/lisp to the load path. This makes it so that we can
;; (require) the things we put into that directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Don't commit any secrets like API tokens or secret keys.  Keep them
;; all in a separate file. If it doesn't exist that's fine.
(load (locate-user-emacs-file "emacs-secrets.el") 'noerror)

;; Prefer UTF-8
(prefer-coding-system 'utf-8)

;; Enable highlighting in grep results
(setq grep-highlight-matches t)

;; Make sure extra dired stuff is available (specifically for renaming
;; the files of open buffers)
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

;;;; Package configurations

;; Load our file that configures the package manager and
;; use-package.el
(require 'init-elpa)

;; Auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use the projectile package which helps us get around in our
;; project.
;; Find the documentation at http://batsov.com/projectile/
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Projectile supports caching so that it doesn't have to re-index a
  ;; project all the time (which can be slow).  It is enabled by
  ;; default when the "native" Emacs Lisp indexing implementation is
  ;; used but we'll enable it everywhere because why not?
  (setq projectile-enable-caching t))

;; TODO: Add helm-projectile?

;; Modern C++ highlighting
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; golang
(use-package go-mode
  :ensure t)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; COMPlete ANY (company) will serve as our completion system.
(use-package company
  :ensure t
  :diminish "cmp"
  :init
  (global-company-mode))
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package flycheck
  :ensure t
  :init
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)
  ;; (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'objc-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode 'flycheck-mode))

;; Integrate clang-format, but not if we're on windows.
(unless (eq system-type 'windows-nt)
  (load-library "clang-format.el")
  (global-set-key [C-M-tab] 'clang-format-region))

;; Spell checking on windows
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "aspell"))

;; rust-lang
(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(use-package cargo :ensure t)
(use-package rust-mode
  :ensure t
  :after cargo
  :init
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (setq-local show-trailing-whitespace t)
	      (setq-local indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (local-set-key [C-M-tab] 'rust-format-buffer)))
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package racer
  :ensure t
  :after rust-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

;; scheme/guile
(use-package geiser :ensure t)

;; magit is a great git porcelain
(use-package magit :ensure t)

;; I edit markdown quite often
(use-package markdown-mode :ensure t)

;; A lot of projects use CMake
(use-package cmake-font-lock :ensure t)

;; LaTeX editing
(use-package magic-latex-buffer :ensure t)
(use-package tex :ensure auctex
  :init
  (setq TeX-view-program-selection '((output-pdf "xdg-open")))
  (setq TeX-PDF-mode t))

;; Allow emacs to be a pdf reader
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;; Tidy the up the mode line by diminishing the Abbrev minor mode
;; indicator in the mode line.
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package
  ansi-color :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


(require 'init-c++)
(require 'init-org)
(require 'jos-utils)

;; Appearance
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-Iosvkem t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(message "init.el complete")

;;; init.el ends here
