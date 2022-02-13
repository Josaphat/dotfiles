;;; jos-utils.el --- Collection of utility functions to do nice things

;; Copyright (C) 2016 Josaphat Valdivia

;; Author: Josaphat Valdivia <jos@josaphat.co>
;; Created: 24 Aug 2016

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

;;; Code:
(defun jos/c-guards (macroname)
  "Inserts C/C++ header guards at the current location of Point.
This currently doesn't try to generate the macro name for you."
  (interactive "sGuard macro: ")
  (beginning-of-buffer)
  (insert "#ifndef " (upcase macroname) "\n#define " (upcase macroname) "\n")
  (end-of-buffer)
  (insert "\n#endif\n")
  (previous-line)
  (previous-line))

(defun jos/string-to-array (strgiven)
  "Transforms the given text into a C/C++ character array
  inserted at the current location of Point."
  (interactive "sString: ")
  (seq-do (lambda (char)
	    "Insert each character as a C-style char literal"
	    (insert "'" char "', ")) strgiven))

(defun jos/c++-RO5 (classname)
  "Inserts the 5 special member functions for the C++ class named
CLASSNAME into the current location of Point, all explicitly
defaulted."
  (interactive "sClass name: ")
  ;; 1. destructor
  (newline-and-indent)
  (insert "virtual ~" classname "() = default;")
  (newline-and-indent)
  ;; 2. Copy constructor
  (insert classname "(const " classname "&) = default;")
  (newline-and-indent)
  ;; 3. Move constructor
  (insert classname "(" classname "&&) = default;")
  (newline-and-indent)
  ;; 4. Copy-assignment operator
  (insert classname "& operator=(const " classname " &) = default;")
  (newline-and-indent)
  ;; 5. Move-assignment operator
  (insert classname "& operator=(" classname " &&) = default;")
  (newline-and-indent))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(provide 'jos-utils)

;;; jos-utils.el ends here
