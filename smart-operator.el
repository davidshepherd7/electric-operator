;;; smart-operator.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010, 2011 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.0a
;; Url: http://xwl.appspot.com/ref/smart-operator.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This extension tries to insert operators with surrounding spaces smartly.
;; e.g., `=' becomes ` = ', `+=' becomes ` += '.  This is handy for writing
;; C-style sources.

;; To use, put this file to your load-path and the following to your
;; ~/.emacs:
;;             (require 'smart-operator)
;;
;; Then `M-x smart-operator-mode' for toggling this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)

;;; smart-operator minor mode

(defvar smart-operator-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "=" 'smart-operator-self-insert-command)
    (define-key keymap "<" 'smart-operator-<)
    (define-key keymap ">" 'smart-operator->)
    (define-key keymap "%" 'smart-operator-%)
    (define-key keymap "+" 'smart-operator-+)
    (define-key keymap "-" 'smart-operator--)
    (define-key keymap "*" 'smart-operator-*)
    (define-key keymap "/" 'smart-operator-self-insert-command)
    (define-key keymap "&" 'smart-operator-&)
    (define-key keymap "|" 'smart-operator-self-insert-command)
    ;; (define-key keymap "!" 'smart-operator-self-insert-command)
    (define-key keymap ":" 'smart-operator-:)
    (define-key keymap "?" 'smart-operator-?)
    (define-key keymap "," 'smart-operator-\,)
    (define-key keymap "." 'smart-operator-.)
    keymap)
  "Keymap used my `smart-operator-mode'.")

;;;###autoload
(define-minor-mode smart-operator-mode
  "Insert operators with surrounding spaces smartly."
  nil " _+_" smart-operator-mode-map)

;;;###autoload
(defun smart-operator-mode-on ()
  "Turn on `smart-operator-mode'.  "
  (smart-operator-mode 1))

;;;###autoload
(defun smart-operator-self-insert-command (arg)
  "Insert the entered operator plus surrounding spaces."
  (interactive "p")
  (smart-operator-insert (string last-command-event)))

(defvar smart-operator-list
  '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":" "?" "," "."))

(defun smart-operator-insert (op &optional only-where)
  "Insert operator OP with surrounding spaces.
e.g., `=' becomes ` = ', `+=' becomes ` += '.

When `only-where' is 'after, we will insert space at back only;
when `only-where' is 'before, we will insert space at front only;
when `only-where' is 'middle, we will not insert space."
  (delete-horizontal-space)
  (case only-where
    ((before) (insert " " op))
    ((middle) (insert op))
    ((after) (insert op " "))
    (t
     (let ((begin? (bolp)))
       (unless (or (looking-back (regexp-opt smart-operator-list)
                                 (line-beginning-position))
                   begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (indent-according-to-mode))))))

(defun smart-operator-c-types ()
  (concat c-primitive-type-key "?"))

(if (fboundp 'python-comment-line-p)
    (defalias 'smart-operator-comment-line-p 'python-comment-line-p)
  (defun smart-operator-comment-line-p ()
    "Return non-nil if and only if current line has only a comment."
    (save-excursion
      (end-of-line)
      (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
        (back-to-indentation)
        (looking-at (rx (or (syntax comment-start) line-end))))))
  )


;;; Fine Tunings

(defun smart-operator-< ()
  "See `smart-operator-insert'."
  (interactive)
  (cond
   ((or (and c-buffer-is-cc-mode
             (looking-back
              (concat "\\("
                      (regexp-opt
                       '("#include" "vector" "deque" "list" "map" "stack"
                          "multimap" "set" "hash_map" "iterator" "template"
                          "pair" "auto_ptr" "static_cast"
                          "dynmaic_cast" "const_cast" "reintepret_cast"

                          "#import"))
                      "\\)\\ *")
              (line-beginning-position)))
        (eq major-mode 'sgml-mode))
    (insert "<>")
    (backward-char))
   (t
    (smart-operator-insert "<"))))

(defun smart-operator-: ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         (if (looking-back "\\?.+")
             (smart-operator-insert ":")
           (smart-operator-insert ":" 'middle)))
        ((memq major-mode '(haskell-mode))
         (smart-operator-insert ":"))
        (t
         (smart-operator-insert ":" 'after))))

(defun smart-operator-\, ()
  "See `smart-operator-insert'."
  (interactive)
  (smart-operator-insert "," 'after))

(defun smart-operator-. ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((smart-operator-comment-line-p)
         (smart-operator-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (and (or c-buffer-is-cc-mode
                      (memq major-mode '(python-mode)))
                  (looking-back "[a-z]")))
         (insert "."))
        ((memq major-mode '(cperl-mode perl-mode))
         (insert " . "))
        (t
         (smart-operator-insert "." 'after)
         (insert " "))))

(defun smart-operator-& ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----[ cases ]
         ;; | char &a = b; // FIXME
         ;; | void foo(const int& a);
         ;; | char *a = &b;
         ;; | int c = a & b;
         ;; | a && b;
         ;; `----
         (cond ((looking-back (concat (smart-operator-c-types) " *" ))
                (smart-operator-insert "&" 'after))
               ((looking-back "= *")
                (smart-operator-insert "&" 'before))
               (t
                (smart-operator-insert "&"))))
        (t
         (smart-operator-insert "&"))))

(defun smart-operator-* ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a * b;
         ;; | char *a;
         ;; | char **b;
         ;; | (*a)->func();
         ;; | *p++;
         ;; | *a = *b;
         ;; `----
         (cond ((looking-back (concat (smart-operator-c-types) " *" ))
                (smart-operator-insert "*" 'before))
               ((looking-back "\\* *")
                (smart-operator-insert "*" 'middle))
               ((looking-back "^[ (]*")
                (smart-operator-insert "*" 'middle)
                (indent-according-to-mode))
               ((looking-back "= *")
                (smart-operator-insert "*" 'before))
               (t
                (smart-operator-insert "*"))))
        (t
         (smart-operator-insert "*"))))

(defun smart-operator-> ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back " - "))
         (delete-char -3)
         (insert "->"))
        (t
         (smart-operator-insert ">"))))

(defun smart-operator-+ ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back "\\+ *"))
         (when (looking-back "[a-zA-Z0-9_] +\\+ *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (smart-operator-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (smart-operator-insert "+"))))

(defun smart-operator-- ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back "\\- *"))
         (when (looking-back "[a-zA-Z0-9_] +\\- *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (smart-operator-insert "-" 'middle)
         (indent-according-to-mode))
        (t
         (smart-operator-insert "-"))))

(defun smart-operator-? ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         (smart-operator-insert "?"))
        (t
         (smart-operator-insert "?" 'after))))

(defun smart-operator-% ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a % b;
         ;; | printf("%d %d\n", a % b);
         ;; `----
         (if (and (looking-back "\".*")
                  (not (looking-back "\",.*")))
             (insert "%")
           (smart-operator-insert "%")))
        (t
         (smart-operator-insert "%"))))

(provide 'smart-operator)

;;; smart-operator.el ends here
