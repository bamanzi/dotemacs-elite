;; * org-mdoe

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      ;; org-list-indent-offset 4
      org-src-fontify-natively t
      )

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)

(eval-after-load "org"
  `(progn
     (setq org-CUA-compatible t)
     ;;fix compatibility with CUA mode
     (require 'org-cua-dwim nil t)

     (define-key org-mode-map (kbd "M-p") 'drag-stuff-up)
     (define-key org-mode-map (kbd "M-n") 'drag-stuff-down)

     ;; with the following settings, no need to switch off IME when adding new headings
     (define-key org-mode-map (kbd "×") (kbd "*"))
     (define-key org-mode-map (kbd "－") (kbd "-"))

     (set-face-attribute 'org-level-1 nil :height 1.6 :bold t)
     (set-face-attribute 'org-level-2 nil :height 1.4 :bold t)
     (set-face-attribute 'org-level-3 nil :height 1.2 :bold t)))


;; ** archive
(defun org-archive-subtree-to-file (file)
  "Move the current subtree to the archive.

Different with `org-archive-subtree', this would ask for target file each time.
If called with C-u prefix, it would archive to file \"%s_archive::\". "
  (interactive (list
                (if current-prefix-arg
                    (concat (buffer-file-name) "_archive")
                  (read-file-name "Archive to file:"
                                  (file-name-directory org-archive-location)
                                  (file-name-nondirectory org-archive-location)))))
  (require 'org-archive)
  (set (make-variable-buffer-local 'org-archive-location) (concat file "::"))
  (unless (string= (org-get-local-archive-location) (concat file "::"))
    (message (org-get-local-archive-location))
    (message (concat file "::"))
    (error "Can't set `org-archive-location' properly. Maybe you have `#+ARCHIVE:' in header?"))
  (call-interactively 'org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-subtree-to-file)

;; ** export

;; *** html
(setq org-export-with-section-numbers t ;; no numbers in export headings (num:t)
      org-export-with-toc 3 ;; no ToC in export (toc: 3)
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      ;; org-export-htmlize-output-type 'inline-css ;; use inline css for source code
      )

;; let ~foo~ generate "<kbd>foo</kbd>"
(setq org-emphasis-alist
  `(("*" bold "<b>" "</b>")
    ("/" italic "<i>" "</i>")
    ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
    ("=" org-code "<code>" "</code>" verbatim)
    ("~" org-verbatim "<kbd>" "</kbd>" verbatim)  ;;changed
    ("+" ,(if (featurep 'xemacs) 'org-table '(:strike-through t))
     "<del>" "</del>")
    ))

;; *** markdown
(eval-after-load "org"
  `(progn
    (if (string< org-version "8")
        ;; 1. based on org-export-generic (C-c C-e g M)
        (if (and (featurep 'org-exp)   ;; already in org core since 6.36
                 (or (require 'org-export-generic nil t) ;; contrib/lisp/org-export-generic.el since 6.36
                     (load-file (concat dotemacs-elite-dir "text/org-contrib/org-export-generic.el")))
                 (locate-library "ORGMODE-markdown"))
            (if (load-library "ORGMODE-markdown")  ;;https://github.com/alexhenning/ORGMODE-Markdown
                (message "Package `org-export-generic' loaded. now you can publish org-mode to `Markdown' with <C-c C-e g M>")
              )

          ;; 2. `org-md' provides `org-md-export-as-markdown' and `org-md-export-to-markdown'
          ;; (only for org-mode 7.9.2 - 7.9.4)
          (if (and (featurep 'org-element)
                   (require 'org-export nil t) ;; contrib/lisp/org-export.el since 7.8
                   (require 'org-e-html nil t)) ;; contrib/lisp/org-e-html since 7.9
              (load-library "org-md")))  ;; contrib/lisp/org-md.el in 7.9.2 - 7.9.x

      ;; 3. org 8.x can export to Markdown directly (no contrib needed)
      (require 'ox-md nil t))  ;; expose Markdown to C-c C-e menu
      ))


;; *** asciidoc
(eval-after-load "org"
  `(progn
    (if (string< org-version "8")
        ;; 1. export `org-export-as-ascii' to C-c C-e a/n/u  or C-c C-e A/N/U
        (if (require 'org-ascii nil t) ;;since org-6.33 core
            (message "Package `org-ascii' loaded. now you can publish org-mode to `asciidoc' with <C-c C-e a> or <C-c C-e A>"))

      ;; 2. org 8.x can export to ASCII directly
      (if (require 'ox-ascii nil t)  ;; expose ASCIIdocto C-c C-e menu
          (message "Package `ox-ascii' loaded. now you can publish org-mode to `asciidoc' with <C-c C-e t a> or <C-c C-e t A>"))
      )))


;; ** misc
;; make sure Org table line could align well when English & Chinese used together
(when nil
  (set-default-font "Dejavu Sans Mono 10")
  (if (eq system-type 'windows-nt)
      (set-fontset-font "fontset-default" 'unicode "宋体 12")
    (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12"))
  )


(global-set-key (kbd "<f8> <f8>") #'(lambda ()
                                      (interactive)
                                      (find-file org-default-notes-file)))



;; * markdown
(autoload 'markdown-mode  "markdown-mode"
  "Major mode for editing Markdown files." t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files." t)

(add-to-list 'auto-mode-alist '("\\.mk?d" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;; ** faces
(eval-after-load "markdown-mode"
  `(progn
     (set-face-attribute 'markdown-header-face-1 nil :inherit 'org-level-1)
     (set-face-attribute 'markdown-header-face-2 nil :inherit 'org-level-2)
     (set-face-attribute 'markdown-header-face-3 nil :inherit 'org-level-3)
     (set-face-attribute 'markdown-header-face-4 nil :inherit 'org-level-4)
     (set-face-attribute 'markdown-header-face-5 nil :inherit 'org-level-5)
     (set-face-attribute 'markdown-header-face-6 nil :inherit 'org-level-6)

     (add-hook 'markdown-mode-hook 'buffer-face-mode) 
     ))

;; ** imenu
;;stolen from http://tychoish.com/rhizome/imenu-for-markdown-and-writing/
(setq markdown-imenu-generic-expression
     '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
 ))

 (add-hook 'markdown-mode-hook
           (lambda ()
             (setq imenu-generic-expression markdown-imenu-generic-expression)))

;; * asciidoc
(autoload 'adoc-mode "adoc-mode"
  "Major mode for editing AsciiDoc text files." t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))



