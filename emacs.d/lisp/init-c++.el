;;; init-c++.el --- jos's configuration for C/C++ editing

;; Copyright (C) 2016 Josaphat Valdivia

;; Author: Josaphat Valdivia <jos@josaphat.co>
;; Created: 21 Aug 2016

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License.  You
;; may obtain a copy of the License at

;;      http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied.  See the License for the specific language governing
;; permissions and limitations under the License.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-default-style '((java-mode . "java")
			("awk-mode" . "awk")
			(other . "stroustrup")))
(setq c-basic-offset 4)
(c-set-offset 'innamespace 0)
(c-set-offset 'inline-open 0)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq fill-column 80)
	    (setq-local show-trailing-whitespace t)
	    (setq-local column-number-mode t)
	    (setq-local indent-tabs-mode nil)))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'init-c++)

;;; init-c++.el ends here
