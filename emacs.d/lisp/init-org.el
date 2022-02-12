;;; init-org.el --- jos's configuration for org mode

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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "/home/jvaldivi/Documents/org/")

(setq org-log-done t)
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-agenda-files (list org-directory))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(add-hook 'org-mode-hook 'flyspell-mode)

(provide 'init-org)
