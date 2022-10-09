;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name    "Yuki"
      user-mail-address "hi@yukiisbo.red")

(setq doom-font  (font-spec :family "Julia Mono" :size 14)
      doom-theme 'doom-dracula)

(setq display-line-numbers-type 'relative)

(setq org-roam-directory "~/roam")

(use-package! direnv
  :hook (after-init . direnv-mode))

(use-package! company-tabnine
  :after company
  :init
  (setq company-tabnine-install-static-binary 't
        +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet)
        company-show-numbers t
        company-idle-delay 0))

(use-package! lsp
  :init
  (defun yuki/lsp-hacks ()
    (direnv-update-environment)
    (setq lsp-clangd-binary-path (executable-find "clangd")))
  (advice-add 'lsp :before 'yuki/lsp-hacks))

(use-package! org
  :config
  ;; Publishing
  (require 'ox-publish)

  (defun yuki/format-date-subtitle (file project)
    (format-time-string "Published on %Y-%m-%d" (org-publish-find-date file project)))

  (defun yuki/org-html-close-tag (tag &rest attrs)
    (concat "<" tag " "
            (mapconcat (lambda (attr)
                         (format "%s=\"%s\"" (car attr) (cadr attr)))
                       attrs
                       " ")
            ">"))

  (defun yuki/html-head-extra (file project)
    (let* ((info (cdr project))
           (org-export-options-alist
            `((:title "TITLE" nil nil parse)
              (:date "DATE" nil nil parse)
              (:author "AUTHOR" nil ,(plist-get info :author) space)
              (:description "DESCRIPTION" nil nil newline)
              (:keywords "KEYWORDS" nil nil space)
              (:meta-image "META_IMAGE" nil ,(plist-get info :meta-image) nil)
              (:meta-type "META_TYPE" nil ,(plist-get info :meta-type) nil)))
           (title (org-publish-find-title file project))
           (date (org-publish-find-date file project))
           (author (org-publish-find-property file :author project))
           (description (org-publish-find-property file :description project))
           (link-home (file-name-as-directory (plist-get info :html-link-home)))
           (extension (or (plist-get info :html-extension) org-html-extension))
           (rel-file (org-publish-file-relative-name file info))
           (full-url (concat link-home (file-name-sans-extension rel-file) "." extension))
           (image (concat link-home (org-publish-find-property file :meta-image project)))
           (type (org-publish-find-property file :meta-type project)))
      (mapconcat 'identity
                 `(,(yuki/org-html-close-tag "link" '(rel me) '(href "mailto:hi@yukiisbo.red"))
                   ,(yuki/org-html-close-tag "link" '(rel webmention) '(href "https://webmention.io/notes.yukiisbo.red/webmention"))
                   ,(yuki/org-html-close-tag "link" '(rel pingback) '(href "https://webmention.io/notes.yukiisbo.red/xmlrpc"))
                   ,(yuki/org-html-close-tag "link" '(rel alternate) '(type application/atom+xml) '(href "/feed.xml") '(title "Atom feed"))
                   ,(yuki/org-html-close-tag "link" '(rel alternate) '(type application/rss+xml) '(href "/rss.xml") '(title "RSS feed"))
                   ,(yuki/org-html-close-tag "meta" '(property og:title) `(content ,title))
                   ,(yuki/org-html-close-tag "meta" '(property og:url) `(content ,full-url))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property og:description) `(content ,description)))
                   ,(yuki/org-html-close-tag "meta" '(property og:image) `(content ,image))
                   ,(yuki/org-html-close-tag "meta" '(property og:type) `(content ,type))
                   ,(and (equal type "article")
                         (yuki/org-html-close-tag "meta" '(property article:author) `(content ,author)))
                   ,(and (equal type "article")
                         (yuki/org-html-close-tag "meta" '(property article:published_time) `(content ,(format-time-string "%FT%T%z" date))))

                   ,(yuki/org-html-close-tag "meta" '(property twitter:title) `(content ,title))
                   ,(yuki/org-html-close-tag "meta" '(property twitter:url) `(content ,full-url))
                   ,(yuki/org-html-close-tag "meta" '(property twitter:image) `(content ,image))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property twitter:description) `(content ,description)))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property twitter:card) '(content summary)))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://gongzhitaao.org/orgcss/org.css"))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "/static/stylesheet.css"))
                   )
                 "\n")))

  (defun yuki/org-html-publish-to-html (plist filename pub-dir)
    (let ((project (cons 'rw plist)))
      (plist-put plist :html-head-extra
                 (yuki/html-head-extra filename project))
      (when (not (equal "index.org" (file-name-nondirectory filename)))
        (plist-put plist :subtitle
                   (yuki/format-date-subtitle filename project)))
      (org-html-publish-to-html plist filename pub-dir)))

  (defun yuki/org-sitemap (title list)
    (concat "#+INCLUDE: ~/org/intro.org\n"
            (org-list-to-org list)))

  (defun yuki/org-sitemap-format (entry style project)
    (cond ((not (directory-name-p entry))
           (format "[[file:%s][%s — %s]]"
                   entry
                   (format-time-string "%Y-%m-%d"
                                       (org-publish-find-date entry project))
                   (org-publish-find-title entry project)))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

  (setq org-publish-project-alist
        (list
         (list "org-posts"
               :base-directory "~/org"
               :exclude (regexp-opt '("index.org" "intro.org" "drafts/"))
               :base-extension "org"
               :publishing-directory "~/org/public/"
               :recursive t
               :publishing-function 'yuki/org-html-publish-to-html
               :headline-levels 3
               :auto-preamble t
               :auto-sitemap t
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :sitemap-filename "index.org"
               :sitemap-style 'list
               :sitemap-sort-files 'anti-chronologically
               :sitemap-function 'yuki/org-sitemap
               :sitemap-format-entry 'yuki/org-sitemap-format
               :author "Yuki"
               :email "hi@yukiisbo.red"
               :meta-image "static/images/profile.png"
               :meta-type "article")
         (list "org-drafts"
               :base-directory "~/org/drafts/"
               :base-extension "org"
               :publishing-directory "~/org/public/drafts/"
               :auto-preamble t
               :recursive t
               :publishing-function 'yuki/org-html-publish-to-html
               :headline-levels 3
               :auto-preamble t
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :author "Yuki"
               :email "hi@yukiisbo.red"
               :meta-image "static/images/profile.png"
               :meta-type "article")
         (list "org-static"
               :base-directory "~/org/static"
               :exclude (regexp-opt '("public/"))
               :base-extension (regexp-opt '("css" "js" "png" "jpg" "gif" "webm" "mp4"))
               :publishing-directory "~/org/public/static"
               :recursive t
               :publishing-function 'org-publish-attachment)
         (list "org" :components '("org-posts" "org-drafts" "org-static")))))

(use-package! webfeeder
  :after org
  :init
  (defun yuki/org-generate-feed ()
    (webfeeder-build
     "feed.xml"
     "~/org/public"
     "https://notes.yukiisbo.red"
     (mapcar (lambda (f) (replace-regexp-in-string ".*/?public/" "" f))
             (directory-files-recursively "~/org/public/posts" ".*\.html$"))
     :title "Yuki's Notes"
     :description "The writings of a computer hobbyist dork"
     :author "Yuki <hi@yukiisbo.red>")
    (webfeeder-build
     "rss.xml"
     "~/org/public"
     "https://notes.yukiisbo.red"
     (mapcar (lambda (f) (replace-regexp-in-string ".*/?public/" "" f))
             (directory-files-recursively "~/org/public/posts" ".*\.html$"))
     :title "Yuki's Notes"
     :description "The writings of a computer hobbyist dork"
     :author "Yuki <hi@yukiisbo.red>"
     :builder 'webfeeder-make-rss))

  (defun yuki/publish ()
    (interactive)
    (org-publish "org")
    (yuki/org-generate-feed)))

