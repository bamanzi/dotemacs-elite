;; * programming

;; ** prog-mode
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

;;(progn
  ;; (add-hook 'prog-mode-hook 'whitespace-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;  )


;; ** code folding
;; *** folding by class/function
;; we use `hideshowvis' to show the fringe indicator
;; you need to customize `hs-special-modes-alist' for each major-mode
(defun turn-on-hideshowvis-maybe ()
  (if (and (display-graphic-p)
           (require 'hideshowvis nil t)
           (< (buffer-size) 50000))
      (hideshowvis-enable)))

(add-hook 'prog-mode-hook 'turn-on-hideshowvis-maybe)

;; (if (fboundp 'qtmstr-outline-mode)
;;     (add-hook 'prog-mode-hook 'qtmstr-outline-mode))


;; *** folding by org-mode like section header
;; this requires `orgstruct-mode' in org 8
;; http://www.orgmode.org/manual/Orgstruct-mode.html
(defun bmz/turn-on-orgstruct-mode-maybe ()
  (interactive)
  (require 'org)
  (if (string< org-version "8")
      (if (called-interactively-p 'interactive)
          (message "Only `orgstruct-mode' in org > 8.0 could be used as code folding. But currently install is %s" org-version))
    (setq orgstruct-heading-prefix-regexp
          (cond ((eq major-mode 'emacs-lisp-mode)
                 ";; ")
                ;;FIXME: other special cases?
                (t
                 comment-start)))
    (if orgstruct-heading-prefix-regexp
        (turn-on-orgstruct))))

;;(add-hook 'prog-mode-hook 'bmz/turn-on-orgstruct-mode-maybe)
(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "bmz/turn-on-orgstruct-mode"
                     :description "folding by org-mode like section header")
     t))

;; ** automatically highlight current symbol
(eval-after-load "idle-highlight-mode"
  `(progn
     (when (boundp  'prog-mode-hook)
       (if (fboundp 'idle-highlight)
           (add-hook 'prog-mode-hook 'idle-highlight)
         (add-hook 'prog-mode-hook 'idle-highlight-mode)))
     ))

(idle-require 'idle-highlight-mode)


;; ** indent guides
;;show guides for each indentation level
(autoload 'highlight-indentation-mode "highlight-indentation"
  "Highlight indentation minor mode highlights indentation based" t)
;;only the current column
(autoload 'highlight-indentation-current-column-mode "highlight-indentation"
  "Hilight Indentation minor mode displays" t)

(global-set-key (kbd "<f10> hI") 'highlight-indentation-current-column-mode)

;;(if (boundp 'prog-mode-hook)
;;    (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode))

;; *** highlight-indent-guides
;; https://github.com/DarthFennec/highlight-indent-guides
;; FIXME: this is a better implementation than `highlight-indentation'?
;; According to its README, it works fine on both space & tabs,
;; while `highlight-indentation.el' and `visual-indentation-mode.el' won't work on tabs.
;; Although `indent-guide.el' works on spaces & tabs, it is farily slow, and jittery
(autoload 'highlight-indent-guides-mode "highlight-indent-guides"
  "Display indent guides in a buffer." t)

(global-set-key (kbd "<f10> hi") 'highlight-indent-guides-mode)

(if (boundp 'prog-mode-hook)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(defun bmz/init-highlight-indent-guides-faces (&optional frame)
  (when (boundp 'highlight-indent-guides-method)
    (setq highlight-indent-guides-method
          (if (display-graphic-p)
              'character
            'column))
    (set-face-attribute 'highlight-indent-guides-odd-face frame
                        :inherit 'fringe
                        :background nil)
    (set-face-attribute 'highlight-indent-guides-even-face frame
                        :inherit 'default
                        :background nil)))
  
(eval-after-load "highlight-indent-guides"
  `(progn
     (add-hook 'after-make-frame-functions 'bmz/init-highlight-indent-guides-faces)
     ))


;; ** which-func-mode

(define-key global-map (kbd "<f10> w f") 'which-func-mode)

(which-func-mode t)
;;(add-hook 'prog-mode-hook 'which-func-mode)

(defun bmz/mode-line-move-which-func-indicator (&optional frame)
  "Move which-func indicator to the start of mode line."
  (interactive (list (selected-frame)))
  (set-face-attribute 'which-func nil :inherit 'font-lock-function-name-face
                      :background (face-background 'default))
  (if (boundp 'mode-line-misc-info)
      (setq mode-line-misc-info
            (remove '(which-func-mode ("" which-func-format " "))   mode-line-misc-info)))
  ;; current buffer
  (setq mode-line-format
        (cons '(which-func-mode ("" which-func-format " "))
              (remove '(which-func-mode ("" which-func-format " "))
                      (remove '(which-func-mode which-func-format) mode-line-format))))
  ;; default value
  (setq-default mode-line-format
                (cons '(which-func-mode ("" which-func-format " "))
                      (remove '(which-func-mode ("" which-func-format " "))
                              (remove '(which-func-mode which-func-format) (default-value 'mode-line-format))))))

(add-hook 'after-make-frame-functions 'bmz/mode-line-move-which-func-indicator)

(eval-after-load "which-func"
  `(progn
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-1>") 'imenu)
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-3>") 'imenu)

     (if (listp which-func-modes) ;;the default value in emacs>=24.3 is `t' (not a list)
         (add-to-list 'which-func-modes 'js-mode))

     (bmz/mode-line-move-which-func-indicator)
     ))

(defun show-which-function ()
  "Show current function's name in echo area."
  (interactive)
  (message "Current function: %s" (which-function)))
  
(define-key global-map (kbd "<f7> w") 'show-which-function)


;; ** imenu
(define-key goto-map "i" 'imenu)

(defun imenu-add-menubar-index-maybe ()
  (ignore-errors
    (imenu-add-menubar-index)))

(if (boundp 'prog-mode-hook)
    (add-hook 'prog-mode-hook 'imenu-add-menubar-index-maybe))

;; *** anything-imenu
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code "anything-config"
  "Preconfigured anything to browse code. `imenu' + elisp/python improvements." t)

(global-set-key (kbd "<f7> i") 'anything-imenu)
(global-set-key (kbd "<f7> c") 'anything-browse-code)

(defun anything-list-symbols ()
  "Show symbol list (imenu + semantic), using current symbol as input to narrow the choices."
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

(define-key global-map (kbd "<f7> l") 'anything-list-symbols)

;; *** imenu-anywhere: imenu tags across all buffers
(autoload 'imenu-anywhere "imenu-anywhere"
  "Switch to a buffer-local tag from Imenu via Ido." t)

(define-key goto-map "I" 'imenu-anywhere)

(autoload 'ido-imenu-anywhere "imenu-anywhere"
  "IDO interface for ‘imenu-anywhere’." t)

(defun imenu-anywhere+ ()
  (interactive)
  (require 'imenu-anywhere)
  (if (featurep 'helm-imenu)
      (call-interactively 'helm-imenu-anywhere)
    (if (featurep 'ivy)
        (call-interactively 'ivy-imenu-anywhere)
      (call-interactively 'ido-imenu-anywhere))))
      
(global-set-key (kbd "<f7> I") 'imenu-anywhere+)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "<f7> I"
                     :description "{helm,ivy}-imenu-anywhere (Show imenu tags across all buffers.)")
     t))


;; ** code jumpping
;; *** etags
;; **** select tag withing project
(autoload 'anything-c-etags-select "anything-config"
  "Preconfigured anything for etags." t)
(global-set-key (kbd "<f7> e") 'anything-c-etags-select)

(defun anything--etags+imenu ()
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

(global-set-key (kbd "<f7> E") 'anything--etags+imenu)


(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "<f7> e"
                     :description "anything-c-etags-select")
     (cheatsheet-add :group 'Programming/Tags
                     :key "<f5> a e"
                     :description "anything-c-etags-select")
     
     (cheatsheet-add :group 'Programming/Tags
                     :key "<f7> E"
                     :description "anything--etags+imenu")
     t))


;; **** tags history
;; tags-view works with etags.el & gtags.el,
;; and for etags, it make use `find-tag-marker-ring' 
(autoload 'tv-view-history "tags-view"
  "Open a buffer listing locations on the tag stack." t)

(global-set-key (kbd "M-h .") 'tv-view-history)

(defalias 'find-tag-history 'tv-view-history)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-h ."
                     :description "tv-view-history (tags-view.el)")
     t))


;; **** misc

;; automatically add a bookmark (bm.el)
(progn
  (defadvice find-tag (after bookmark-after-find-tag)
    (if (require 'bm nil t)
        (let ((bookmark (bm-bookmark-at (point))))
          (unless bookmark
            (bm-bookmark-add)))))
  (ad-activate 'find-tag)
  )

;; *** ctags
;; **** anything-ctags-current-file
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

(global-set-key (kbd "<f7> t") 'anything-ctags-current-file)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "<f7> t"
                     :description "anything-ctags-current-file")
     t))


;; *** dumb-jump
(autoload 'dumb-jump-mode "dumb-jump"
  "Minor mode for jumping to variable and function definitions" t)


;; ** compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(define-key global-map (kbd "<M-wheel-down>") 'next-error)
(define-key global-map (kbd "<M-wheel-up>")   'previous-error)

(define-key global-map (kbd "<f9> C-n") 'next-error)
(define-key global-map (kbd "<f9> C-p") 'previous-error)

(global-set-key (kbd "C-c <C-f9>") 'compilation-shell-minor-mode)

(global-set-key (kbd "M-g <C-f9>") 'compile-goto-error)
(global-set-key (kbd "<f9> M-g")   'compile-goto-error)


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
(global-set-key (kbd "<M-f9>") 'projectile-global-mode)

(setq projectile-keymap-prefix (kbd "<M-f9>"))

(idle-require 'projectile-ext)

(eval-after-load "projectile"
  `(progn
     (delete ".projectile" projectile-project-root-files)
     (add-to-list 'projectile-project-root-files ".projectile")

     (require 'projectile-ext nil t)
     (define-key projectile-mode-map (kbd "<M-f9> C-f") 'projectile-find-file-)
     (define-key projectile-mode-map (kbd "<M-f9> d") 'projectile-dired)     
     (define-key projectile-mode-map (kbd "<M-f9> e") 'projectile-eshell-cd-current)
     (define-key projectile-mode-map (kbd "<M-f9> E") 'projectile-eshell-cd-root)
     (define-key projectile-mode-map (kbd "<M-f9> G") 'projectile-ripgrep)
     (define-key projectile-mode-map (kbd "<M-f9> a") 'projectile-ack)
     (define-key projectile-mode-map (kbd "<M-f9> A") 'projectile-ack-find-file)

     (require 'anything-projectile nil t)
     (define-key projectile-mode-map (kbd "<M-f9> B") 'anything-with-projectile-buffers)
     (define-key projectile-mode-map (kbd "<M-f9> F") 'anything-with-projectile-files)
     (define-key projectile-mode-map (kbd "<M-f9> D") 'anything-with-projectile-dirs)     
     ))

(autoload 'projectile-ripgrep "projectile-ripgrep"
  "Run a Ripgrep search with ‘SEARCH-TERM’ rooted at the current projectile project root." t)

(defun eshell/cdprj ()
  "cd to the project root"
  (interactive)
  (require 'projectile)
  (let ((dir (projectile-get-project-root)))
    (eshell/cd dir)))

(autoload 'nv-speedbar-open-current-buffer-in-tree "projectile-speedbar"
  "Undocumented." t)
(defalias 'projectile-speedbar 'nv-speedbar-open-current-buffer-in-tree)


;; *** ibuffer-projectile
(autoload 'ibuffer-projectile-set-filter-groups "ibuffer-projectile"
  "Set the current filter groups to filter by project root dir." t)

(eval-after-load "ibuffer"
  `(progn
     ;; (add-hook 'ibuffer-hook
     ;;           #'(lambda ()
     ;;               (if (and (boundp 'projectile-global-mode)
     ;;                        projectile-global-mode)
     ;;                   (ibuffer-projectile-set-filter-groups)))
     ;;           'append)

     (define-key ibuffer-mode-map (kbd "G p") 'ibuffer-projectile-set-filter-groups)
     ))

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Ibuffer
                     :key "G p"
                     :description "M-x ibuffer-projectile-set-filter-groups")
     ))

;; ** documentation lookup
(autoload 'keyword-help-lookup "bmz-keyword-help"
  "Invoke documentation query backends for KEYWORD." t)

(global-set-key (kbd "M-s <f1>") 'keyword-help-lookup)


;; ** repl-toggle
(autoload 'repl-toggle "bmz-repl-toggle"
  "Toggle between the REPL buffer and the source buffer." t)

(global-set-key (kbd "<f9> ~") 'repl-toggle)



;; ** isend-mode
;; `isend-mode` allows interaction with code interpreters in `ansi-term` or
;; `term` buffers. Some language-specific modes (e.g. `python.el`) already
;; provide similar features;

;; NOTE: it also could be used to send region to normal buffers. (bamanzi)

(autoload 'isend-associate "isend-mode"
  "Set the buffer to which commands will be sent using `isend-send'." t)

(autoload 'isend-send "isend-mode"
  "Send the current region of line to a terminal." t)

(global-set-key (kbd "<apps> C-RET") 'isend-send)
(global-set-key (kbd "ESC C-RET")    'isend-send)

(autoload 'isend-display-buffer "isend-mode"
  "Undocumented." t)


(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Misc
                     :key "M-x isend-send"
                     :description "Send the current region of line to a buffer.")
     (cheatsheet-add :group 'Misc
                     :key "M-x isend-associate"
                     :description "Set the buffer to which commands will be sent using `isend-send'.")
     t))

