;;* org-mdoe

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil ;; no ToC in export
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css

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


;;** archive
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


;;** misc
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



;;* markdown
(autoload 'markdown-mode  "markdown-mode"
  "Major mode for editing Markdown files." t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files." t)

(add-to-list 'auto-mode-alist '("\\.mk?d" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;;** faces
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

;;** imenu
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
