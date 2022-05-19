;;; init-elpa.el --- jos's elpa configuration

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

;;; Code:

;; Load emacs 24+ built-in package manager, package.el, and disable
;; loading at startup because we load the packages ourselves with
;; package-initialize in this file so that we can modify them.
(require 'package)
(setq package-enable-at-startup nil)

;; Add additional repositories
(setq package-archives
      '(("GNU elpa" . "https://elpa.gnu.org/packages/")
	("org elpa" . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa" . 10)
	("melpa-stable" . 9)
	("GNU ELPA" . 8)
	("org" . 10)))
;; (add-to-list 'package-archives '("GNU ELPA" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

;; If it isn't already, install use-package.el, which helps us manage
;; things like when a package gets initialized relative to other
;; packages.  We want it to take advantage of diminish package so we
;; make sure that's installed as well.
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Not entirely sure what the purpose of this is, but I suspect it's
;; useful if you compile everything and you don't need to fetch the
;; packages at runtime anymore. Supposedly this will reduce load
;; times.
;(eval-when-compile
;  (require 'use-package))

(provide 'init-elpa)

;;; init-elpa.el
