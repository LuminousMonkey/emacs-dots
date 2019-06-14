;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org-static-blog
  :init
  (setq org-static-blog-publish-title "LuminousMonkey")
  (setq org-static-blog-publish-url "https://luminousmonkey.org/")
  (setq org-static-blog-publish-directory "~/Projects/luminousmonkey-blog/")
  (setq org-static-blog-posts-directory "~/Projects/luminousmonkey-blog/posts/")
  (setq org-static-blog-drafts-directory "~/Projects/luminousmonkey-blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil))

(provide 'social-blog)
