;;* programming

;;** which-func-mode

(define-key global-map (kbd "<f10> w f") 'which-func-mode)

(which-func-mode t)

;; move which-func indicator to the start of mode line
(setcar mode-line-format '(which-func-mode which-func-format))

(eval-after-load "which-func"
  `(progn
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-1>") 'imenu)
     ))


;;**  imenu
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code "anythong-config"
  "Preconfigured anything to browse code. `imenu' + elisp/python improvements." t)

(global-set-key (kbd "<f5> i") 'anything-imenu)
(global-set-key (kbd "<f5> c") 'anything-browse-code)

(define-key goto-map "i" 'imenu)

(defun anything-goto-symbol ()
  "Show anything list, using current symbol as input to narrow the choices."
  (interactive)
  (anything
   :prompt "Go to:"
   :candidate-number-limit 10
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-imenu
         anything-c-source-browse-code
         anything-c-source-semantic
;;         anything-c-source-etags-select
         )))

(define-key goto-map "s" 'anything-goto-symbol)

;;** tags
;;*** etags
(global-set-key (kbd "<f5> .") 'anything-c-etags-select)

(defun anything-goto-etag ()
  "Show etags list, using current symbol as input to narrow the choices."
  (interactive)
  (anything
   :prompt "Go to:"
   :candidate-number-limit 20
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-etags-select
         anything-c-source-imenu
         )))

(define-key goto-map "." 'anything-goto-etag)

(autoload 'find-file-in-tags "find-file-in-tags"
  "find file in TAGS file.")

;;*** ctags
(defun anything-ctags-current-file ()
  "Show ctags list of current file, using current symbol as input to narrow the choices."
  (interactive)
  (anything
   :prompt "ctags:"
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-ctags
         anything-c-source-imenu
         )))

(eval-after-load "anything-config"
  `(add-to-list 'anything-c-ctags-modes 'ruby-mode)
  )

(global-set-key (kbd "<f5> t") 'anything-ctags-current-file)


;;**  compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(define-key global-map (kbd "<M-wheel-down>") 'next-error)
(define-key global-map (kbd "<M-wheel-up>")   'previous-error)


;;** flymake
(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))

(define-key goto-map (kbd "M-n") 'flymake-goto-next-error)
(define-key goto-map (kbd "M-p") 'flymake-goto-prev-error)


;;** project
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(idle-require 'projectile)
(eval-after-load "projectile"
  `(progn
     (require 'anything-projectile)
     
     (require 'projectile-ext)
     (define-key projectile-mode-map "e" 'projectile-eshell-cd-current)
     (define-key projectile-mode-map "E" 'projectile-eshell-cd-root)
     ))

