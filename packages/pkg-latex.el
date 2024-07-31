;; Required packages
(require-package 'auctex)
(require-package 'cdlatex)
(require-package 'auctex-latexmk)
(require 'smartparens-latex)
;; (require 'tex-mik)
;; (require-package 'company-auctex)

;; (company-auctex-init)

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


;; Adding specific backends and LaTeXmk for LaTeX
(add-hook 'LaTeX-mode-hook (lambda ()
  ;; Prioritize Orderless completion with Carpe and Prescient in LaTeX-mode
  (setq-local completion-at-point-functions '(orderless-completion-at-point prescient-completion-at-point cape-tex cape-keyword cape-dabbrev cape-file))

  ;; Latexmk command for easy compilation
  (push '("Latexmk" "latexmk -pdf %s" TeX-run-command nil t :help "Run Latexmk on file")
        TeX-command-list)))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(latex-mode . ("digestif" :build
;;                                (:executable:
;; 				(tectonic)
;; 				))
;; 			    )))


;; LaTeX mode defaults
(defun prelude-latex-mode-defaults ()
  "Default Prelude hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (smartparens-mode +1)
  (pcase latex-fast-math-entry
    (`LaTeX-math-mode (LaTeX-math-mode 1))
    (`cdlatex (turn-on-cdlatex))))

(add-hook 'LaTeX-mode-hook 'prelude-latex-mode-defaults)

;; Set lualatex as the default compiler
(setq pdf-latex-command "lualatex")

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-engine 'luatex)
(setq-default TeX-PDF-mode t)

;; Hooks for LaTeX mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)

;; Flymake configuration for LaTeX
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(provide 'pkg-latex)
