;;; init-org.el --- Setup org-mode

(use-package org
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

        org-latex-to-pdf-process            `("xelatex -interaction nonstopmode %f"
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
  ;; Babel
  (defvar load-language-list `((emacs-lisp . t)
                               (perl       . t)
                               (python     . t)
                               (ruby       . t)
                               (js         . t)
                               (css        . t)
                               (sass       . t)
                               (C          . t)
                               (java       . t)
                               (plantuml   . t)))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Use bullet points for lists
  (font-lock-add-keywords 'org-mode
                            '(("^ *\\(-\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Heading fonts
  (let* ((variable-tuple (cond ((x-list-fonts "Fira Sans")    '(:font   "Fira Sans"))
                               ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font. Install Fira Sans."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  ;; Publishing
  (require 'ox-publish)
  (require 'ox-rss)

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
                 `(,(yuki/org-html-close-tag "link" '(rel alternate) '(type application/rss+xml) '(href "rss.xml") '(title "RSS feed"))
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
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css"))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://code.cdn.mozilla.net/fonts/fira.css"))
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

  (defun yuki/org-rss-publish-to-rss (plist filename pub-dir)
    (if (equal "feed.org" (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename pub-dir)))

  (defun yuki/org-feed (title sitemap)
    (let* ((title "Yuki's Notes"))
      (concat (format "#+TITLE: %s\n\n" title)
              (org-list-to-subtree
               sitemap
               1
               '(:splice t :istart nil :icount nil
                 :dtstart " " :dtend " ")))))

  (defun yuki/org-feed-format (entry style project)
    (let* ((file (org-publish--expand-file-name entry project))
           (title (org-publish-find-title entry project))
           (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
           (link (concat (file-name-sans-extension entry) ".html")))
      (with-temp-buffer
        (insert (format "* [[file:%s][%s]]\n" entry title))
        (org-set-property "RSS_PERMALINK" link)
        (org-set-property "RSS_TITLE" title)
        (org-set-property "PUBDATE" date)
        (insert (format "#+INCLUDE: \"%s\" :lines \"3-\"" file))
        (buffer-string))))

  (setq org-publish-project-alist
        (list
         (list "org-posts"
               :base-directory "~/org"
               :exclude (regexp-opt '("index.org" "setup.org" "intro.org" "drafts/" "feed.org"))
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
               :recursive t
               :publishing-function 'org-html-publish-to-html
               :headline-levels 3
               :auto-preamble t)
         (list "org-feed"
               :base-directory "~/org"
               :exclude (regexp-opt '("index.org" "setup.org" "intro.org" "drafts/" "feed.org"))
               :base-extension "org"
               :publishing-directory "~/org/public/"
               :recursive t
               :publishing-function 'yuki/org-rss-publish-to-rss
               :headline-levels 3
               :auto-sitemap t
               :rss-extension "xml"
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :sitemap-filename "feed.org"
               :sitemap-title "Yuki's notes"
               :sitemap-style 'list
               :sitemap-sort-files 'anti-chronologically
               :sitemap-function 'yuki/org-feed
               :sitemap-format-entry 'yuki/org-feed-format
               :author "Yuki"
               :email "hi@yukiisbo.red")
         (list "org-static"
               :base-directory "~/org/static"
               :exclude (regexp-opt '("public/"))
               :base-extension (regexp-opt '("css" "js" "png" "jpg" "gif" "webm" "mp4"))
               :publishing-directory "~/org/public/static"
               :recursive t
               :publishing-function 'org-publish-attachment)
         (list "org" :components '("org-posts" "org-drafts" "org-feed" "org-static")))))

(use-package ein
  :after org
  :if (executable-find "jupyter")
  :init (cl-pushnew '(ein . t) load-language-list))

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode))
  :init
  (setq org-bullets-bullet-list '(" ")))

(use-package htmlize)

(use-package org-contrib)

(provide 'init-org)
