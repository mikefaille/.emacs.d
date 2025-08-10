;; -*- lexical-binding: t; -*-
;; core-font.el --- Simplified font configuration for Monaspace

;; --- Basic Font Setup ---
;; Set default font for new frames (ideally place this in early-init.el)
;; Assumes "Monaspace Neon" size 12 is installed and available.
(set-frame-font "Monaspace Neon 12" nil t)

;; --- Configure Monaspace Features ---
;; Enable texture healing / ligatures via OpenType 'calt' feature
;; Applies to the default fontset range (covers typical programming characters)
(set-fontset-font t nil (font-spec :family "Monaspace Neon"
                                   :weight 'normal
                                   :slant 'normal
                                   :size 12 ; Hardcoded size
                                   ;; Enable Contextual Alternates ('calt')
                                   :features '(("calt" . t)
                                               ;; Add other features if needed, e.g., stylistic sets
                                               ;; ("ss01" . t) ("ss02" . t) ...
                                               ))
                    nil 'append) ; Append to existing fontset specs

;; Semantic font family variations for syntax highlighting
;; Assumes "Monaspace Radon", "Xenon", "Argon" are installed.
(set-face-attribute 'font-lock-comment-face nil
                    :family "Monaspace Radon" ; Italic variant for comments
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :family "Monaspace Xenon" ; Bold/Script variant for keywords
                    ;; :weight 'bold ; Uncomment if Xenon should also be bold
                    )
(set-face-attribute 'font-lock-string-face nil
                    :family "Monaspace Argon" ; Mono variant for strings
                    )
;; Add more face customizations as needed...
;; (set-face-attribute 'font-lock-function-name-face nil :family "Monaspace Krypton" :weight 'bold)


;; --- General Font/Display Settings ---

;; Line spacing (adjust vertical spacing between lines)
(setq-default line-spacing 0.2) ; Adjust value as needed (e.g., 0.1, 0.3)


;; Mark this file as provided
(provide 'core-font)
;;; core-font.el ends here
