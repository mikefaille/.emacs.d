;;; pkg-emms.el --- Emacs Multimedia System configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures EMMS, the Emacs Multimedia System.

;;; Code:

(use-package emms
  :defer t
  :config
  (defun mike-emms ()
    "Mike's emms launcher'"
    (interactive)
    (require 'emms-setup)
    (emms-all)
    (emms-default-players)

    (setq emms-info-asynchronously t)))

(provide 'pkg-emms)
