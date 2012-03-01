;;; hs-tags.el — TAGS-based features.

;; Copyright (C) 2011 Chris Done

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'hs-lang-en)
(require 'hs-types)

(require 'cl)

(defun hs-tags ())

(defun hs-tags-generate-interactive (&optional cmd)
  "Send a (silent; don't tell me about it in the REPL) arbitrary command."
  (let ((project (hs-project)))
    (unless (hs-process-current-dir (hs-project-process project))
      (hs-process-cd-interactive)
      (hs-process-reset (hs-project-process project)))
    (message "Generating tags...")
    (hs-process-queue-command
     (hs-project)
     'tags-generate
     (concat (if cmd
                 cmd
               hs-config-tags-cmd) "\n"))))

(defun hs-tags-find (&optional next-p)
  "The tag find function."
  (interactive "P")
  (find-tag (hs-ident-at-point) next-p))

(provide 'hs-tags)
