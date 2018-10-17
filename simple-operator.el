;;; simple-operator.el --- simple electric-operator  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: This is a still naive prototype

;;

;;; Code:

(require 'cl-extra)

(setq electric-operator-known-operator-chars (list ?- ?+ ?* ?: ?. ?, ?! ?$ ?% ?& ?/ ?=))

(setq electric-operator-known-defeat-chars (list ?\( ))

(setq electric-operator-known-operator-chars-strg (cl-map 'string 'identity electric-operator-known-operator-chars-strg))

(defun simple-operator--beginning-of-op ()
  "Jump to the beginning of an operator at point.

Return position if successful"
  (and (< 0 (abs (skip-chars-backward electric-operator-known-operator-chars-strg)))
       (point)))

(defun electric-operator--setup-haskell-mode ()
  )

(defun electric-operator-setup ()
  (pcase major-mode
    (`inferior-sml-mode
     (electric-operator--setup-inferior-sml))
    (`sml-mode
     (electric-operator--setup-sml pps))
    (`python-mode
     (electric-operator--setup-python-mode))
    (`emacs-lisp-mode
     (electric-operator--setup-emacs-lisp-mode))
    (`ruby-mode
     (electric-operator--setup-ruby-mode))
    (`haskell-mode
     (electric-operator--setup-haskell-mode))
    (`haskell-interactive-mode
     (electric-operator--setup-haskell-mode))
    (`inferior-haskell-mode
     (electric-operator--setup-haskell-mode))))

(defun simple-operator-do ()
  ""
  (interactive "*")
  (when (member (char-before) electric-operator-known-operator-chars)
    (let ((orig (copy-marker (point))))
      (when (simple-operator--beginning-of-op)
	(if (member (char-before) electric-operator-known-defeat-chars)
	    (goto-char orig)
	  (just-one-space)
	  (goto-char orig)
	  (just-one-space))))))

(provide 'simple-operator)
;;; simple-operator.el ends here
