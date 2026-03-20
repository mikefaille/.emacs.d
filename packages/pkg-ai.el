;;; pkg-ai.el --- AI Coding Assistants -*- lexical-binding: t; -*-

(require 'use-package)

;; --- GPTel (Chat & LLM Interaction) ---
(use-package gptel
  :ensure t
  :bind (("C-c C-c" . gptel-send)
         ("C-c RET" . gptel-menu))
  :config
  ;; Configure default backend (e.g., OpenAI, Anthropic, or Local)
  ;; (setq gptel-default-mode 'org-mode)
  )

;; --- Copilot (Ghost Text Completion) ---
(use-package copilot
  :ensure (:host github :repo "zerolfx/copilot.el")
  :init
  ;; Ensure the server executable is looked for in ~/.emacs.d/var/copilot
  (setq copilot-server-executable 
        (expand-file-name "dist/agent.js" 
                          (expand-file-name "copilot/nil/node_modules/copilot-node-server" 
                                            michael-savefile-dir)))
  :hook (prog-mode . (lambda () 
                       (when (and copilot-server-executable 
                                  (file-exists-p copilot-server-executable))
                         (copilot-mode 1))))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :custom
  (copilot-idle-delay 0.5)
  :config
  ;; Set default indentation to silence warnings
  (setq copilot-indent-offset 2)
  ;; Explicitly define indentation for common modes if needed
  (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
  (add-to-list 'copilot-indentation-alist '(org-mode . 2))
  (add-to-list 'copilot-indentation-alist '(text-mode . 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))
  (add-to-list 'copilot-indentation-alist '(dart-mode . 2))
  
  ;; Force indentation setting to silence warnings
  (defun my/copilot-set-indent ()
    (setq-local copilot-indent-offset
                (cond ((derived-mode-p 'dart-mode) 2)
                      ((derived-mode-p 'yaml-mode) 2)
                      ((derived-mode-p 'python-mode) 4)
                      (t 2))))
  (add-hook 'copilot-mode-hook 'my/copilot-set-indent))

;; --- Aidermacs (Agentic Coding) ---
(use-package aidermacs
  :ensure (:host github :repo "clhenrick/aidermacs")
  :bind (("C-c a" . aidermacs-transient-menu)))

(provide 'pkg-ai)
;;; pkg-ai.el ends here
