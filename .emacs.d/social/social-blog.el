;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org-static-blog
  :load-path "override"
  :init
  (setq org-static-blog-publish-title "LuminousMonkey")
  (setq org-static-blog-publish-url "https://luminousmonkey.org/")
  (setq org-static-blog-publish-directory "~/Projects/monkey-blog/blog/")
  (setq org-static-blog-posts-directory "~/Projects/monkey-blog/posts/")
  (setq org-static-blog-drafts-directory "~/Projects/monkey-blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  (setq org-static-blog-use-semantic-html t))

(use-package htmlize
  :commands
  (htmlize-buffer
   htmlize-file
   htmlize-many-files
   htmlize-many-files-dired
   htmlize-region)
  :init
  (setq org-html-htmlize-output-type 'css))

(provide 'social-blog)
