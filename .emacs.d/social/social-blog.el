;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org-static-blog
  :init
  (setq org-static-blog-publish-title "LuminousMonkey")
  (setq org-static-blog-publish-url "https://luminousmonkey.org/")
  (setq org-static-blog-publish-directory "~/Projects/monkey-blog/blog/")
  (setq org-static-blog-posts-directory "~/Projects/monkey-blog/posts/")
  (setq org-static-blog-drafts-directory "~/Projects/monkey-blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)

  (setq org-static-blog-page-header
	"
<meta name=\"author\" content=\"Mike Aldred\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href=\"style/default.css\" rel=\"stylesheet\" type=\"text/css\" />


"
	)
  (setq org-static-blog-page-preamble
	"<div id=\"masthead\">
<h1>LuminousMonkey</h1>
<div id=\"nav\"><ul><li><a href=\"/\">Home</a></li><li><a href=\"/about.html\">About</a></li><li><a href=\"/archives/\">Archives</a></li></ul></div>
</div>
")
  (setq org-static-blog-page-postamble "")
  (setq org-static-blog-use-semantic-html t)
  :config
  (defun org-static-blog-post-preamble (post-filename)
    "Returns the formatted date and headline of the post."
    (concat
     "<h2 class=\"entry-title\">"
     "<a href=\"" (org-static-blog-get-url post-filename) "\">"
     (org-static-blog-get-title post-filename) "</a></h2>\n"
     "<div class=\"entry-meta\">"
     "<span class=\"meta-prep meta-author\">Posted on</span>&nbsp;"
     "<a href=\"" (org-static-blog-get-url post-filename) "\" rel=\"bookmark\">"
     (format-time-string "%d %b %Y"
			 (org-static-blog-get-date post-filename))
     "</a></div><div class=\"entry-content\">"))
  (defun org-static-blog-post-postamble (post-filename)
    "Returns the tag list of the post.
This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
    (let ((closing-entry "</div>")
	  (taglist-content ""))
      (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
	(setq taglist-content (concat "<div class=\"taglist\">"
				      "<a href=\""
				      org-static-blog-tags-file
				      "\">Tags:</a> "))
	(dolist (tag (org-static-blog-get-tags post-filename))
	  (setq taglist-content (concat taglist-content "<a href=\""
					"tag-" (downcase tag) ".html"
					"\">" tag "</a> ")))
	(setq taglist-content (concat taglist-content "</div>")))
      (concat
       closing-entry
       taglist-content)))
  (defun org-static-blog-get-body (post-filename &optional exclude-title)
    "Get the rendered HTML body without headers from POST-FILENAME.
Preamble and Postamble are excluded, too."
    (with-temp-buffer
      (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
      (buffer-substring-no-properties
       (progn
	 (goto-char (point-min))
	 (if exclude-title
	     (progn (search-forward "<h2 class=\"entry-title\">")
		    (search-forward "</h2>"))
	   (search-forward "<div id=\"content\">"))
	 (point))
       (progn
	 (goto-char (point-max))
	 (search-backward "<div id=\"postamble\" class=\"status\">")
	 (search-backward "</div>")
	 (point)))))
  )

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
