;;** minibuffer
;;*** easily insert buffer name (useful for `shell-command', `compile' etc)
(defun minibuffer-insert-buffer-filename (arg)
  (interactive "P")
  (let ((target-buffer (window-buffer (minibuffer-selected-window))))
    (if (and target-buffer
             (buffer-file-name target-buffer))
      (insert-string (if arg
                         (buffer-file-name target-buffer)
                       (file-name-nondirectory (buffer-file-name target-buffer))))
      (insert-string " "))))

(define-key minibuffer-local-map (kbd "C-c %") 'minibuffer-insert-buffer-filename)

;;*** insert current symbol to minibuffer
(defun minibuffer-insert-current-symbol (arg)
  (interactive "P")
  (let ((target-buffer (window-buffer (minibuffer-selected-window))))
    (if (and target-buffer
             (buffer-file-name target-buffer))
        (insert-string (with-current-buffer target-buffer
                         (if arg
                             (thing-at-point 'string) ;; thingatpt+.el needed
                           (thing-at-point 'symbol))))
      (insert-string " "))))

(define-key minibuffer-local-map (kbd "C-c M-s") 'minibuffer-insert-current-symbol)

;;*** multi-occur for buffers of same mod*;;;_.. multi-occur in same mode buffers
;; stolen from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun multi-occur-in-this-mode (symbol &optional arg)
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive
   (list (read-string "Occur in same mode: " (thing-at-point 'symbol))
         current-prefix-arg))
  (multi-occur (get-buffers-matching-mode major-mode)
               (format "%s" symbol)
               arg))

(define-key search-map (kbd "M-o") 'multi-occur-in-this-mode)

;;*** grin
(autoload 'grin "grin"
  "Use `grin' command for grepping text across files." t)
(autoload 'grind "grin"
  "Use `grind' command for find files." t)

;;** tabbar
(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward-tab)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward-tab)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
     (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)

     (define-key tabbar-mode-map (kbd "<f12> <right>") 'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<f12> <left>")  'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<f12> <up>")    'tabbar-press-home)

     (if (display-graphic-p)
         (require 'tabbar-ruler nil t))  
     ))

(idle-require 'tabbar)

;;** color-theme
(idle-require 'color-theme)

(eval-after-load "color-theme"
  `(progn
     (require 'color-theme-sanityinc)
     (require 'color-theme-tangotango)
     ))


;;** org-mdoe
(setq org-CUA-compatible t)

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil ;; no ToC in export
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css
      )

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)

(eval-after-load "org"
  `(progn
     (require 'org-cua-dwim nil t)

     (set-face-attribute 'org-level-1 nil :height 1.5 :bold t)
     (set-face-attribute 'org-level-2 nil :height 1.3 :bold t)
     (set-face-attribute 'org-level-3 nil :height 1.1)))

;;** shell

;;(idle-require 'esh-toggle)
;;(idle-require 'sh-toggle)

(autoload 'eshell-toggle "esh-toggle"
  "Toggles between the *eshell* buffer and the current buffer." t)
(autoload 'eshell-toggle-cd "esh-toggle"
  "Calls `eshell-toggle' and let it cd to path of current buffer." t)

(autoload 'shell-toggle "sh-toggle"
  "Toggles between the *shell* buffer and the current buffer." t)
(autoload 'shell-toggle-cd "sh-toggle"
  "Calls `shell-toggle' and let it cd to path of current buffer." t)


;;** misc
(idle-require 'mark-copy-something)

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point." t)

;;info+.el: more colors (and other enhancements) 
(eval-after-load "info"
  `(require 'info+)
  )
