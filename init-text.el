;; * general text-mode
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

(eval-after-load "org"
  `(progn
    (define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-subtree-to-file)
    ))

;; ** export

;; *** html
(setq org-export-with-section-numbers t ;; no numbers in export headings (num:t)
      org-export-with-toc 3 ;; no ToC in export (toc: 3)
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-with-sub-superscripts nil ;; required by org>=8
                                        ; (org-use-sub-superscripts no longer working for exporting)
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
      (require 'ox-md nil t) ;; expose Markdown to C-c C-e menu
      (require 'ox-gfm nil t) ;; enhanced for GitHub Flavored MarkDown
                                        ; https://github.com/larstvei/ox-gfm
      )))


;; ** org-babel
(setq org-confirm-babel-evaluate nil)
(idle-require 'ob-tangle)
(eval-after-load "ob-tangle"
  `(progn
     ;; org 8.2.x renamed `ob-sh.el' to `ob-shell.el'
     (when (require 'ob-sh nil t)
       (provide 'ob-shell))
     (org-babel-do-load-languages 'org-babel-load-languages
                                  '((shell . t)
                                    (emacs-lisp . t)
                                    (python . t)
                                    (ruby . t)))
     ))


;; ** misc

(global-set-key (kbd "<f4> <f4>") #'(lambda ()
                                      (interactive)
                                      (find-file org-default-notes-file)))

(eval-after-load "org"
  `(progn
    (define-key org-mode-map (kbd "<C-f4>") 'org-toggle-checkbox)
    (define-key org-mode-map (kbd "<S-f4>") 'org-toggle-checkbox-presence)

    (define-key org-mode-map (kbd "C-c M-%") 'org-replace-markdown-link)
    ))

(defun org-toggle-checkbox-presence ()
  (interactive)
  (org-toggle-checkbox '(4)))
 
(defun org-replace-markdown-link ()
  "Replace markdown-style link '[desc](link)' to org-mode-style link '[[link][desc]]'."
  (interactive)
  (query-replace-regexp "\\[\\([^\]]*\\)\\](\\([^)]*\\))"
                        "[[\\2][\\1]]"))


;; * markdown
(autoload 'markdown-mode  "markdown-mode"
  "Major mode for editing Markdown files." t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files." t)

(add-to-list 'auto-mode-alist '("\\.mk?d" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . gfm-mode))

;; ** faces
(eval-after-load "markdown-mode"
  `(progn
     (set-face-attribute 'markdown-header-face-1 nil :inherit 'org-level-1)
     (set-face-attribute 'markdown-header-face-2 nil :inherit 'org-level-2)
     (set-face-attribute 'markdown-header-face-3 nil :inherit 'org-level-3)
     (set-face-attribute 'markdown-header-face-4 nil :inherit 'org-level-4)
     (set-face-attribute 'markdown-header-face-5 nil :inherit 'org-level-5)
     (set-face-attribute 'markdown-header-face-6 nil :inherit 'org-level-6)

     ;; (add-hook 'markdown-mode-hook 'buffer-face-mode)

     ;; something like org-mode
     (define-key markdown-mode-map (kbd "M-RET")     'markdown-insert-list-item)
     (define-key markdown-mode-map (kbd "C-c C-l")   'markdown-insert-uri)
     (define-key markdown-mode-map (kbd "C-c C-x f") 'markdown-insert-footnote)
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




