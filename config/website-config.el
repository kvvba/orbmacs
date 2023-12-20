(require 'ox-publish)

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

(defun my/org-sitemap-date-entry-format (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
              entry
              filename))))

(setq org-publish-project-alist
      `(("pages"
         :base-directory "~/website/org/"
         :base-extension "org"
         :recursive nil
         :publishing-directory "~/website/html/"
         :publishing-function org-html-publish-to-html)

        ("static"
         :base-directory "~/website/org/"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png"
         :recursive t
         :publishing-directory  "~/website/html/"
         :publishing-function org-publish-attachment)

				("blog"
				 :base-directory "~/website/org/blog/"
				 :base-extension "org"
				 :publishing-directory "~/website/html/blog/"
				 :publishing-function org-html-publish-to-html

				 :auto-sitemap t
				 :sitemap-title "Blog posts"
				 :sitemap-filename "index.org"
				 :sitemap-format-entry my/org-sitemap-date-entry-format
				 :sitemap-sort-files anti-chronologically)
				
        ("website" :components ("pages" "blog" "static"))))

