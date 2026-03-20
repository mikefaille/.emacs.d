;; -*- lexical-binding: t; -*-
;;; pkg-latex.el --- LaTeX configuration using AUCTeX

(require 'use-package)

;; AUCTeX
(use-package auctex
  :ensure t
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-engine 'luatex)
  (TeX-PDF-mode t)
  (pdf-latex-command "lualatex"))

;; CDLaTeX
(use-package cdlatex
  :ensure t
  :defer t)

;; LaTeXmk support
(use-package auctex-latexmk
  :ensure t
  :after auctex
  :config
  (auctex-latexmk-setup))

;; Smartparens LaTeX support
(use-package smartparens
  :ensure t
  :defer t
  :config
  (require 'smartparens-latex))

;; Configure TeX-view-program based on system type
(setq TeX-view-program-selection
      (cond ((eq system-type 'darwin)
             '((output-dvi "DVI Viewer")
               (output-pdf "PDF Viewer")
               (output-html "HTML Viewer")))
            (t '(output-dvi "open %o"
                            output-pdf "open %o"
                            output-html "open %o"))))

(setq TeX-view-program-list
      '(("DVI Viewer" "open %o")
        ("PDF Viewer" "open %o")
        ("HTML Viewer" "open %o")))

;; LaTeX mode hooks
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (abbrev-mode +1)
            (smartparens-mode +1)
            (visual-line-mode 1)
            (flyspell-mode 1)
            (LaTeX-math-mode 1)
            (turn-on-reftex)
            (setq-local completion-at-point-functions
                        '(orderless-completion-at-point
                          prescient-completion-at-point
                          cape-tex
                          cape-keyword
                          cape-dabbrev
                          cape-file))
            ;; Push Latexmk to command list
            (push '("Latexmk" "latexmk -pdf %s" TeX-run-command nil t :help "Run Latexmk on file")
                  TeX-command-list)))

(setq reftex-plug-into-AUCTeX t)

(provide 'pkg-latex)
;;; pkg-latex.el ends here
