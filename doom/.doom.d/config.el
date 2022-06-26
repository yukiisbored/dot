;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name    "Yuki"
      user-mail-address "hi@yukiisbo.red")

(setq doom-font (font-spec :family "Iosevka Term Slab" :size 14))

(setq display-line-numbers-type 'relative)

(custom-theme-set-faces!
 '(org-level-3 :height 1.1)
 '(org-level-2 :height 1.25)
 '(org-level-1 :height 1.5)
 '(org-document-title :height 1.75 :underline nil))

(setq indent-tabs-mode nil)

(use-package! swiper
  :bind ("C-s" . swiper))

(use-package! modus-themes
  :init
  (setq modus-themes-syntax               '(yellow-comments green-strings alt-syntax)
        modus-themes-mode-line            '(borderless padded)
        modus-themes-subtle-line-numbers  t
        modus-themes-scale-headings       t
        modus-themes-italic-constructs    t
        modus-themes-mixed-fonts          t)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

(use-package! telephone-line
  :hook (after-init . telephone-line-mode)
  :init
  (setq telephone-line-lhs                      '((evil   . (telephone-line-evil-tag-segment))
                                                  (accent . (telephone-line-vc-segment
                                                             telephone-line-erc-modified-channels-segment
                                                             telephone-line-process-segment))
                                                  (nil    . (telephone-line-buffer-segment)))
        telephone-line-rhs                      '((nil    . (telephone-line-misc-info-segment))
                                                  (accent . (telephone-line-major-mode-segment))
                                                  (evil   . (telephone-line-airline-position-segment)))
        telephone-line-primary-left-separator   'telephone-line-flat
        telephone-line-primary-right-separator  'telephone-line-flat
        telephone-line-height                   24))

(use-package! direnv
  :hook (after-init . direnv-mode))

(use-package! lsp
  :init
  (advice-add 'lsp :before 'direnv-update-environment))

(use-package! emacsql-sqlite
  :init
  (setq emacsql-sqlite-executable (locate-file "emacsql-sqlite" exec-path)))

(use-package! org
  :hook
  ((org-mode . (lambda () (hl-line-mode nil)))
   (org-mode . auto-fill-mode))
  :init
  (setq initial-major-mode                  'org-mode
        org-startup-indented                t
        org-pretty-entities                 t
        org-hide-emphasis-markers           t
        org-fontify-whole-heading-line      t
        org-fontify-done-headline           t
        org-fontify-quote-and-verse-blocks  t
        org-ellipsis                        "  "

        org-src-tab-acts-natively           t
        org-src-fontify-natively            t
        org-src-window-setup                'current-window

        org-latex-to-pdf-process            '("xelatex -interaction nonstopmode %f"
                                              "xelatex -interaction nonstopmode %f")
        org-latex-listings                  'minted

        org-confirm-babel-evaluate          nil

        org-export-with-smart-quotes        t
        org-export-with-section-numbers     nil
        org-export-with-toc                 nil

        org-startup-with-inline-images      t

        org-html-divs                       '((preamble  "header" "top")
                                              (content   "main"   "content")
                                              (postamble "footer" "postamble"))
        org-html-container-element          "section"
        org-html-validation-link            nil
        org-html-head-include-default-style nil
        org-html-html5-fancy                t
        org-html-doctype                    "html5")
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

(use-package! org-bullets
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list '(" ")))

(use-package! org-roam
  :init (setq org-roam-directory "~/roam"))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :init
  (setq org-roam-ui-sync-theme     t
        org-roam-ui-follow         t
        org-roam-ui-update-on-save t))
