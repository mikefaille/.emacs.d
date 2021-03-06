(require-package 'auctex)
(require-package 'cdlatex)
(require-package 'auctex-latexmk)
;; (require-package 'latex-extra)
(require 'smartparens-latex)            ; smartparens suppose to be instaled automaticly
(require 'tex-mik)



(require-package 'company-auctex)
(company-auctex-init)
;; TODO https://github.com/cgroll/dot_emacs.d/blob/master/init-latex.org

;; sensible defaults for OS X, other OSes should be covered out-of-the-box
(when (eq system-type 'darwin)
  (setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))
(setq TeX-view-program-list
      '(("DVI Viewer" "open %o")
        ("PDF Viewer" "open %o")
        ("HTML Viewer" "open %o"))))

(defcustom latex-fast-math-entry 'LaTeX-math-mode
  "Method used for fast math symbol entry in LaTeX."
  :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  :group 'pkg
  :type '(choice (const :tag "None" nil)
                 (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
                 (const :tag "CDLaTeX" cdlatex)))

(defun prelude-latex-mode-defaults ()
  "Default Prelude hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (smartparens-mode +1)
  (case latex-fast-math-entry
    (LaTeX-math-mode (LaTeX-math-mode 1))
    (cdlatex (turn-on-cdlatex))))
(setq prelude-latex-mode-hook 'latex-mode-defaults)


(add-hook 'LaTeX-mode-hook (lambda ()

			     (add-to-list 'company-backends 'company-math-symbols-unicode)
			     (defun my-latex-mode-setup ()
			       (setq-local company-backends
					   (append '((company-math-symbols-latex company-latex-commands))
						   company-backends)))
			     (add-hook 'LaTex-mode-hook 'my-latex-mode-setup)


			     (push
			      '("Latexmk" "latexmk -pdf %s" TeX-run-command nil t
				:help "Run Latexmk on file")
			      TeX-command-list)))

(setq pdf-latex-command "lualatex")

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-engine 'luatex)
(setq-default TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)


;; ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
;; (setq org-latex-default-packages-alist (list
;;  ("AUTO" "inputenc" t)
;;         ("T1" "fontenc" t)
;;         ("" "fixltx2e" nil)
;;         ("" "graphicx" t)
;;         ("" "longtable" nil)
;;         ("" "float" nil)
;;         ("" "wrapfig" nil)
;;         ("" "rotating" nil)
;;         ("normalem" "ulem" t)
;;         ("" "amsmath" t)
;;         ("" "textcomp" t)
;;         ("" "marvosym" t)
;;         ("" "wasysym" t)
;;         ("" "amssymb" t)
;;         ("" "xunicode" t)
;;         ("" "url" t)
;;         ("" "rotating" t)
;;         ("american" "babel" t)
;;         ("babel" "csquotes" t)
;;         ("" "soul" t)
;;         ("xetex" "hyperref" nil)
;;         "\\tolerance=1000"))


(setq reftex-plug-into-AUCTeX t)

(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))


(provide 'pkg-latex)
