;; -*- coding: utf-8; lexical-binding: t; -*-

;; Common Lisp
(use-package slime
  :init
  (progn
    (require 'slime-autoloads)
    (load (expand-file-name "~/.quicklisp/slime-helper.el"))
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")))

(provide 'programming-commonlisp)
