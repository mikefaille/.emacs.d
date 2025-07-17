;;; pkg-flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Flycheck for on-the-fly syntax checking.

;;; Code:

(require-package 'flycheck)
(use-package flycheck
  :defer t)

(provide 'pkg-flycheck)
