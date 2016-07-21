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

;;;; Package configurations

;; Load our file that configures the package manager and
;; use-package.el
(require 'init-elpa)

;; Use the projectile package which helps us get around in our
;; project.
;; Find the documentation at http://batsov.com/projectile/
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  ;; Projectile supports caching so that it doesn't have to re-index a
  ;; project all the time (which can be slow).  It is enabled by
  ;; default when the "native" Emacs Lisp indexing implementation is
  ;; used but we'll enable it everywhere because why not?
  (setq projectile-enable-caching t))

;; TODO: Add helm-projectile?

;; Irony is a great backbone for a better C/C++ editing experience.
;; It can provide code completion as well as syntax checking (not
;; entirely on its own).  I've run into a quirk where I have to run
;; M-x irony-cdb-autosetup-compile-options myself when I first pull up
;; a buffer.  It might be a lack of understanding of how use-package
;; should be used.  If it works better by skipping use-package
;; entirely, I might just go with that and put this into a separate
;; file.
(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun jos/irony-mode-hook ()
    ;; Replace the completion bindings in irony-mode buffer with the
    ;; irony mode counterparts:
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'jos/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; COMPlete ANY (company) will serve as our completion system.  It's
;; not that great on its own, though, so we combine it with irony
;; using company-irony
(use-package company
  :ensure t
  :diminish "cmp"
  :config
  (global-company-mode))
(use-package company-irony
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

;; Flycheck gives us quick diagnostics for our C/C++ programs.  If we
;; hook it up to the irony backend it's a breeze to set up.  This will
;; verify the file we're editing compiles and highlights any errors.
(use-package flycheck
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'objc-mode-hook 'flycheck-mode))
(use-package flycheck-irony
  :ensure t
  :after flycheck irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; TODO: Add better TAGS handling

;; Tidy the up the mode line by diminishing the Abbrev minor mode
;; indicator in the mode line.
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; Appearance
(use-package moe-theme
  :ensure t
  :config
  (moe-dark))

(require 'init-c++)

(message "init.el complete")

;;; init.el ends here