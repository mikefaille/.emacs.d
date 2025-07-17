;;; pkg-php.el --- PHP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures PHP support with php-mode.

;;; Code:

(require-package 'php-mode)
(use-package php-mode
  :defer t)

(provide 'pkg-php)
