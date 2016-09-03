;;; nix-mode.el --- Major mode for editing Nix expressions

;; Author: Eelco Dolstra
;; Maintainer: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/matthewbauer/nix-mode
;; Version: 1.1
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for editing Nix expressions (.nix files).  See the Nix manual
;; for more information available at https://nixos.org/nix/manual/.

;;; Code:

;; Emacs 24.2 compatability
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    `(set (make-local-variable ',var) ,val)))

(defun nix-syntax-match-antiquote (limit)
  "Find antiquote within a Nix expression up to LIMIT."
  (let ((pos (next-single-char-property-change (point) 'nix-syntax-antiquote
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((char (char-after pos)))
        (pcase char
          (`?$
           (forward-char 2))
          (`?}
           (forward-char 1)))
        (set-match-data (list pos (point)))
        t))))

(defconst nix-font-lock-keywords
  '(
    ;; keywords
    ("\\_<if\\_>" . font-lock-keyword-face)
    ("\\_<then\\_>" . font-lock-keyword-face)
    ("\\_<else\\_>" . font-lock-keyword-face)
    ("\\_<with\\_>" . font-lock-keyword-face)
    ("\\_<let\\_>" . font-lock-keyword-face)
    ("\\_<in\\_>" . font-lock-keyword-face)
    ("\\_<rec\\_>" . font-lock-keyword-face)
    ("\\_<inherit\\_>" . font-lock-keyword-face)
    ("\\_<or\\_>" . font-lock-keyword-face)

    ;; builtin functions
    ;; taken from http://nixos.org/nix/manual/#ssec-builtins
    ("\\_<builtins\\(\.[a-zA-Z]+\\)?\\_>" . font-lock-builtin-face)
    ("\\_<baseNameOf\\_>" . font-lock-builtin-face)
    ("\\_<derivation\\_>" . font-lock-builtin-face)
    ("\\_<dirOf\\_>" . font-lock-builtin-face)
    ("\\_<false\\_>" . font-lock-builtin-face)
    ("\\_<fetchTarball\\_>" . font-lock-builtin-face)
    ("\\_<import\\_>" . font-lock-builtin-face)
    ("\\_<isNull\\_>" . font-lock-builtin-face)
    ("\\_<map\\_>" . font-lock-builtin-face)
    ("\\_<null\\_>" . font-lock-builtin-face)
    ("\\_<removeAttrs\\_>" . font-lock-builtin-face)
    ("\\_<toString\\_>" . font-lock-builtin-face)
    ("\\_<true\\_>" . font-lock-builtin-face)

    ;; warnings
    ("\\_<assert\\_>" . font-lock-warning-face)
    ("\\_<abort\\_>" . font-lock-warning-face)
    ("\\_<throw\\_>" . font-lock-warning-face)

    ("[a-zA-Z][a-zA-Z0-9\\+-\\.]*:[a-zA-Z0-9%/\\?:@&=\\+\\$,_\\.!~\\*'-]+"
     . font-lock-constant-face)
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_'\-\.]*\\)[ \t]*="
     (1 font-lock-variable-name-face nil nil))
    ("<[a-zA-Z0-9._\\+-]+\\(/[a-zA-Z0-9._\\+-]+\\)*>"
     . font-lock-constant-face)
    ("[a-zA-Z0-9._\\+-]*\\(/[a-zA-Z0-9._\\+-]+\\)+"
     . font-lock-constant-face)
    (nix-syntax-match-antiquote 0 font-lock-preprocessor-face t))
  "Font lock keywords for nix.")

(defvar nix-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Nix mode.")

(defun nix-syntax-propertize-escaped-antiquote ()
  "Set syntax properies for an escaped antiquote mark."
  nil)

(defun nix-syntax-propertize-multiline-string ()
  "Set syntax properies for multiline string delimiters."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (context (save-excursion (save-match-data (syntax-ppss start))))
         (string-type (nth 3 context)))
    (pcase string-type
      (`t
       ;; inside a multiline string
       ;; ending multi-line string delimiter
       (put-text-property (1- end) end
                          'syntax-table (string-to-syntax "|")))
      (`nil
       ;; beginning multi-line string delimiter
       (put-text-property start (1+ start)
                          'syntax-table (string-to-syntax "|"))))))

(defun nix-syntax-propertize-antiquote ()
  "Set syntax properties for an antiquote mark."
  (let* ((start (match-beginning 0)))
    (put-text-property start (1+ start)
                       'syntax-table (string-to-syntax "|"))
    (put-text-property start (+ start 2)
                       'nix-syntax-antiquote t)))

(defun nix-syntax-propertize-close-brace ()
  "Set syntax properties for close braces.
If a close brace `}' ends an antiquote, the next character begins a string."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (context (save-excursion (save-match-data (syntax-ppss start))))
         (open (nth 1 context)))
    (when open ;; a corresponding open-brace was found
      (let* ((antiquote (get-text-property open 'nix-syntax-antiquote)))
        (when antiquote
          (put-text-property (+ start 1) (+ start 2)
                             'syntax-table (string-to-syntax "|"))
          (put-text-property start (1+ start)
                             'nix-syntax-antiquote t))))))

(defun nix-syntax-propertize (start end)
  "Special syntax properties for Nix from START to END."
  ;; search for multi-line string delimiters
  (goto-char start)
  (remove-text-properties start end '(syntax-table nil nix-syntax-antiquote nil))
  (funcall
   (syntax-propertize-rules
    ("''\\${"
     (0 (ignore (nix-syntax-propertize-escaped-antiquote))))
    ("''"
     (0 (ignore (nix-syntax-propertize-multiline-string))))
    ("\\${"
     (0 (ignore (nix-syntax-propertize-antiquote))))
    ("}"
     (0 (ignore (nix-syntax-propertize-close-brace)))))
   start end))

(defun nix-indent-level ()
  "Get current indent level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:space:]")
    (let ((baseline (* 2 (nth 0 (syntax-ppss)))))
      (cond
       ((looking-at "[]})]") (- baseline tab-width))
       ;; ((nix-inside-args) (- baseline tab-width))
       ;; ((nix-inside-let) (+ baseline tab-width))
       (t baseline)))))

(defun nix-indent-line ()
  "Indent current line in a Nix expression."
  (interactive)
  (indent-line-to (nix-indent-level)))


(defvar nix-mode-map
  (let ((map (make-sparse-keymap "Nix")))
    (define-key nix-mode-map [menu-bar] (make-sparse-keymap))
    (define-key nix-mode-map [menu-bar nix] (cons "Nix" map))
    (define-key map [about]
      '("Indent line" . nix-indent-line)))
 "Keymap for nix-mode.")

;;;###autoload
(define-derived-mode nix-mode prog-mode "Nix"
  "Major mode for editing Nix expressions.

The following commands may be useful:

  '\\[newline-and-indent]'
    Insert a newline and move the cursor to align with the previous
    non-empty line.

  '\\[fill-paragraph]'
    Refill a paragraph so that all lines are at most `fill-column'
    lines long.  This should do the right thing for comments beginning
    with `#'.  However, this command doesn't work properly yet if the
    comment is adjacent to code (i.e., no intervening empty lines).
    In that case, select the text to be refilled and use
    `\\[fill-region]' instead.

The hook `nix-mode-hook' is run when Nix mode is started.

\\{nix-mode-map}
"
  (set-syntax-table nix-mode-syntax-table)

  ;; Disable hard tabs and set tab to 2 spaces
  ;; Recommended by nixpkgs manual: https://nixos.org/nixpkgs/manual/#sec-syntax
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)

  ;; Font lock support.
  (setq-local font-lock-defaults '(nix-font-lock-keywords nil nil nil nil))

  ;; Special syntax properties for Nix
  (setq-local syntax-propertize-function 'nix-syntax-propertize)

  ;; Look at text properties when parsing
  (setq-local parse-sexp-lookup-properties t)

  ;; Automatic indentation [C-j].
  (setq-local indent-line-function 'nix-indent-line)

  ;; Indenting of comments.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (setq-local comment-multi-line t)

  ;; Filling of comments.
  (setq-local adaptive-fill-mode t)
  (setq-local paragraph-start "[ \t]*\\(#+[ \t]*\\)?$")
  (setq-local paragraph-separate paragraph-start)

  ;; Local keymap and menu
  (use-local-map nix-mode-map))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode)))

(defun nix-mode-reload ()
  "Reload Nix mode."
  (interactive)
  (unload-feature 'nix-mode)
  (require 'nix-mode)
  (nix-mode))

(provide 'nix-mode)

;;; nix-mode.el ends here
