;;* programming

;;TODO: prog-mode

;;* automatically highlight current symbol
(eval-after-load "idle-highlight"
  `(progn
     (if (fboundp 'idle-highlight)
         (add-hook 'find-file-hook 'idle-highlight)
       (add-hook 'find-file-hook 'idle-highlight-mode))
     ))

(idle-require 'idle-highlight)

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

(eval-after-load "eshell"
  `(add-hook eshell-mode-hook
             #'(lambda ()
                 ;; `eshell-mode-map' is a local variable, damn!
                 (define-key eshell-mode-map (kbd "C-c <f9>") 'compilation-shell-minor-mode)
                 )))

(eval-after-load "comint"
  `(progn
     (define-key comint-mode-map (kbd "C-c <f9>") 'compilation-shell-minor-mode)))


;;** flymake
(setq flymake-log-level 2)  ;; -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG
(setq flymake-start-syntax-check-on-newline nil) ;;only syntax check when open/save

(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))

(define-key goto-map (kbd "M-n") 'flymake-goto-next-error)
(define-key goto-map (kbd "M-p") 'flymake-goto-prev-error)

;;*** flycheck
(autoload 'flycheck-mode "flycheck"
  "Flymake reloaded with useful checkers. " t)

;;load flycheck rather than flymake
(idle-require 'flycheck)

(global-set-key (kbd "<M-f9>") 'flycheck-mode)

;;fix endless loop bug of `flycheck-find-file-in-tree' on Windows
(eval-after-load "flycheck"
  `(progn
     (defun flycheck-find-file-in-tree (filename directory)
       "Find FILENAME in DIRECTORY and all of its ancestors.

Start looking for a file named FILENAME in DIRECTORY and traverse
upwards through all of its ancestors up to the file system root
until the file is found or the root is reached.

Return the absolute path of the file, or nil if the file was not
found in DIRECTORY or any of its ancestors."
       (let ((full-path (expand-file-name filename directory)))
         (cond ((or (string= directory "/")
                    (string= ":/" (substring directory 1 3)))
                (when (file-exists-p full-path) full-path))
               ((file-exists-p full-path)
                full-path)
               (t
                (let ((parent-directory (file-name-directory
                                         (directory-file-name
                                          (file-name-directory full-path)))))
                  (flycheck-find-file-in-tree filename parent-directory))))))
     ))


;;** projectile
(setq projectile-keymap-prefix (kbd "C-c C-p"))

;;(idle-require 'projectile)
(autoload 'projectile-global-mode "projectile"
  "Toggle Projectile mode in every possible buffer." t)

(eval-after-load "projectile"
  `(progn
     (require 'anything-projectile)
     
     (require 'projectile-ext)
     (define-key projectile-mode-map (kbd "C-c C-p e") 'projectile-eshell-cd-current)
     (define-key projectile-mode-map (kbd "C-c C-p E") 'projectile-eshell-cd-root)
     (define-key projectile-mode-map (kbd "C-c C-p G") 'projectile-grin)
     (define-key projectile-mode-map (kbd "C-c C-p a") 'projectile-ack)
     (define-key projectile-mode-map (kbd "C-c C-p A") 'projectile-ack-find-file)  
     ))

