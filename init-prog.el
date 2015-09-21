;; * programming

(unless (fboundp 'prog-mode)
  (defvar prog-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?\C-\M-q] 'prog-indent-sexp)
      map)
    "Keymap used for programming modes.")
  
  (define-derived-mode prog-mode fundamental-mode "Prog"
    "Major mode for editing programming language source code."
    (set (make-local-variable 'require-final-newline) mode-require-final-newline)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    ;; Any programming language is always written left to right.
    (setq bidi-paragraph-direction 'left-to-right))  

  ;;as in older Emacs, js-mode, python-mode is not derived from `prog-mode'
  ;;we had to call the hook manually
  (when nil    ;;FIXME: not work well
      (add-hook 'emacs-lisp-mode-hook 'prog-mode-run-hook)
      
      (eval-after-load "python"
        `(add-hook 'python-mode-hook 'prog-mode-run-hook))
      (eval-after-load "js"
        `(add-hook 'js-mode-hook     'prog-mode-run-hook))
      (eval-after-load "js2"
        `(add-hook 'js2-mode-hook    'prog-mode-run-hook))
      )
  )

(defun prog-mode-run-hook ()
  (interactive)  ;;allows manually invoke
  (run-hooks 'prog-mode-hook))

(global-set-key (kbd "<f10> P") 'prog-mode-run-hook)


;;TODO: test and enable this
(progn
  ;; (add-hook 'prog-mode-hook 'whitespace-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (if (fboundp 'turn-on-hideshow/bmz)
      (add-hook 'prog-mode-hook 'turn-on-hideshow/bmz))
  ;; (if (fboundp 'qtmstr-outline-mode)
  ;;     (add-hook 'prog-mode-hook 'qtmstr-outline-mode))
  )

;; ** automatically highlight current symbol
(eval-after-load "idle-highlight-mode"
  `(progn
     (when (boundp  'prog-mode-hook)
       (if (fboundp 'idle-highlight)
           (add-hook 'prog-mode-hook 'idle-highlight)
         (add-hook 'prog-mode-hook 'idle-highlight-mode)))
     ))

(idle-require 'idle-highlight-mode)


;; ** which-func-mode

(define-key global-map (kbd "<f10> w f") 'which-func-mode)

(which-func-mode t)
;;(add-hook 'prog-mode-hook 'which-func-mode)

;; move which-func indicator to the start of mode line
(setcar mode-line-format '(which-func-mode which-func-format))

(eval-after-load "which-func"
  `(progn
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-1>") 'imenu)
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-3>") 'imenu)

     (add-to-list 'which-func-modes 'js-mode)
     ))

(defun show-which-function ()
  "Show current function's name in echo area."
  (interactive)
  (message "Current function: %s" (which-function)))
  
(define-key global-map (kbd "<f9> w") 'show-which-function)


;; ** imenu
(define-key goto-map "i" 'imenu)

(if (boundp 'prog-mode-hook)
    (add-hook 'prog-mode-hook 'imenu-add-menubar-index))

;; *** anything-imenu
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code "anything-config"
  "Preconfigured anything to browse code. `imenu' + elisp/python improvements." t)

(global-set-key (kbd "<f9> i") 'anything-imenu)
(global-set-key (kbd "<f9> c") 'anything-browse-code)

(defun anything-goto-symbol-imenu ()
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

(define-key global-map (kbd "<f9> g i") 'anything-goto-symbol-imenu)

;; *** imenu-anywhere: imenu tags across all buffers
(autoload 'imenu-anywhere "imenu-anywhere"
  "Switch to a buffer-local tag from Imenu via Ido." t)

(define-key goto-map "I" 'imenu-anywhere)

(autoload 'helm-imenu-anywhere  "imenu-anywhere"
  "`helm' source for `imenu-anywhere'." t)

(global-set-key (kbd "<f9> I") 'helm-imenu-anywhere)


;; ** tags
;; *** etags
(global-set-key (kbd "<f9> . SPC") 'anything-c-etags-select)

(defun anything-goto-definition-etags ()
  "Show function/symbol list with etags & imenu.

Current symbol would be used as input to narrow the choices."
  (interactive)
  (anything
   :prompt "Go to:"
   :candidate-number-limit 20
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-etags-select
         anything-c-source-imenu
         )))

(global-set-key (kbd "<f9> g .") 'anything-goto-definition-etags)
(global-set-key (kbd "<f9> M-.") 'anything-c-etags-select)

;; *** tags history
;; tags-view works with etags.el & gtags.el
;; and for etags, it make use `find-tag-marker-ring' 
(autoload 'tv-view-history "tags-view"
  "Open a buffer listing locations on the tag stack." t)

(global-set-key (kbd "M-h .") 'tv-view-history)

(defalias 'find-tag-history 'tv-view-history)

;; *** anything-etags+
(autoload 'anything-etags+-select-at-point "anything-etags+"
  "Tag jump with current symbol using etags and `anything'." t)
(autoload 'anything-etags+-select "anything-etags+"
  "Tag jump using etags and `anything'." t)

(eval-after-load "anything-etags+"
  `(progn
     (global-set-key (kbd "<apps> M-.")   'anything-etags+-select-at-point)
     (global-set-key (kbd "<apps> . SPC") 'anything-etags+-select)

     ;; 'anything-etags+-history' has its own marker-ring. thus could
     ;; not be used with `find-tag'
     (global-set-key (kbd "<apps> .  M-h") 'anything-etags+-history)

     (global-set-key (kbd "<apps> . <") 'anything-etags+-history-go-back)
     (global-set-key (kbd "<apps> . >") 'anything-etags+-history-go-forward)
     ))

;; *** ctags
;; similar to 'taglist' plugin of vim, to use `anything-c-source-ctags'
;; you don't need to create tags file by yourself

;; (defun anything-ctags-current-file ()
;;   "Show ctags list of current file, using current symbol as input to narrow the choices."
;;   (interactive)
;;   (anything
;;    :prompt "ctags:"
;;    :input (thing-at-point 'symbol)
;;    :sources
;;       '( anything-c-source-ctags
;;          anything-c-source-imenu
;;          )))

(global-set-key (kbd "<f9> t") 'anything-ctags-current-file)


;; **  compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(define-key global-map (kbd "<M-wheel-down>") 'next-error)
(define-key global-map (kbd "<M-wheel-up>")   'previous-error)

(define-key global-map (kbd "<f9> n") 'next-error)
(define-key global-map (kbd "<f9> p") 'previous-error)

(global-set-key (kbd "C-c <f9>") 'compilation-shell-minor-mode)

(global-set-key (kbd "M-g <f9>") 'compile-goto-error)
(global-set-key (kbd "<f9> G") 'compile-goto-error)


;; ** flymake
(setq flymake-log-level 2)  ;; -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG
(setq flymake-start-syntax-check-on-newline nil) ;;only syntax check when open/save

(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))

(define-key goto-map (kbd "M-n") 'flymake-goto-next-error)
(define-key goto-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key global-map (kbd "ESC <wheel-down>") 'flymake-goto-next-error)
(define-key global-map (kbd "ESC <wheel-up>")   'flymake-goto-prev-error)

;; *** flycheck
(autoload 'flycheck-mode "flycheck"
  "Flymake reloaded with useful checkers. " t)

;;load flycheck rather than flymake
(idle-require 'flycheck)

(global-set-key (kbd "<M-f9>") 'flycheck-mode)

(define-key global-map (kbd "<f9> M-n") 'flymake-goto-next-error)
(define-key global-map (kbd "<f9> M-p") 'flymake-goto-prev-error)

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


;; ** projectile
(autoload 'projectile-global-mode "projectile"
  "Toggle Projectile mode in every possible buffer." t)
(global-set-key (kbd "<M-f12>") 'projectile-global-mode)

(setq projectile-keymap-prefix (kbd "<M-f12>"))

(idle-require 'projectile-ext)

(eval-after-load "projectile"
  `(progn
     (delete ".projectile" projectile-project-root-files)
     (add-to-list 'projectile-project-root-files ".projectile")

     (require 'projectile-ext nil t)
     (define-key projectile-mode-map (kbd "<M-f12> C-f") 'projectile-find-file-)
     (define-key projectile-mode-map (kbd "<M-f12> d") 'projectile-dired)     
     (define-key projectile-mode-map (kbd "<M-f12> e") 'projectile-eshell-cd-current)
     (define-key projectile-mode-map (kbd "<M-f12> E") 'projectile-eshell-cd-root)
     (define-key projectile-mode-map (kbd "<M-f12> G") 'projectile-grin)
     (define-key projectile-mode-map (kbd "<M-f12> a") 'projectile-ack)
     (define-key projectile-mode-map (kbd "<M-f12> A") 'projectile-ack-find-file)

     (require 'anything-projectile nil t)
     (define-key projectile-mode-map (kbd "<M-f12> B") 'anything-with-projectile-buffers)
     (define-key projectile-mode-map (kbd "<M-f12> F") 'anything-with-projectile-files)
     (define-key projectile-mode-map (kbd "<M-f12> D") 'anything-with-projectile-dirs)     
     ))

(defun eshell/cdprj ()
  "cd to the project root"
  (interactive)
  (require 'projectile)
  (let ((dir (projectile-get-project-root)))
    (eshell/cd dir)))

(autoload 'nv-speedbar-open-current-buffer-in-tree "projectile-speedbar"
  "Undocumented." t)
(defalias 'projectile-speedbar 'nv-speedbar-open-current-buffer-in-tree)


;; ** documentation lookup
(autoload 'keyword-help-lookup "bmz-keyword-help"
  "Invoke documentation query backends for KEYWORD." t)

(global-set-key (kbd "M-s <f1>") 'keyword-help-lookup)


;; ** repl-toggle
(autoload 'repl-toggle "bmz-repl-toggle"
  "Toggle between the REPL buffer and the source buffer." t)

(global-set-key (kbd "<f12> ~") 'repl-toggle)
