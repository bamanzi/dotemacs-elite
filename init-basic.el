(fset 'yes-or-no-p 'y-or-n-p)

(unless (fboundp 'idle-require)
    (defalias 'idle-require 'require))

(server-start)

;;** key bindings

;;***  key modifiers

;;***  <f1> .. <f12> as prefix key
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))

;;*** misc
(define-key key-translation-map (kbd "<left-fringe> <mouse-4>")   (kbd "<left-fringe> <wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <mouse-5>")   (kbd "<left-fringe> <wheel-down>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-4>") (kbd "<left-fringe> <C-wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-5>") (kbd "<left-fringe> <C-wheel-down>"))


;;** emacs enviroment
(global-set-key (kbd "ESC ESC e r") 'eval-region)
(global-set-key (kbd "ESC ESC e b") 'eval-buffer)
(global-set-key (kbd "ESC ESC l l") 'load-library)
(global-set-key (kbd "ESC ESC f l") 'find-library)

;;*** help
(define-key help-map "F" 'describe-face)
(define-key help-map "i" nil)
(define-key help-map "ii" 'info)
(define-key help-map "ic" 'Info-goto-emacs-command-node)
(define-key help-map "iF" 'Info-goto-emacs-command-node)
(define-key help-map "ik" 'Info-goto-emacs-key-command-node)
(define-key help-map "i " 'Info-goto-node)

(defun describe-keymap (keymap)
    (interactive
     (list (intern (completing-read "Keymap: " obarray
                                    (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
                                    t nil 'variable-name-history))))
    (with-output-to-temp-buffer "*Help*"
      (princ (substitute-command-keys (concat "\\{" (symbol-name keymap) "}")))
      ))
(define-key help-map (kbd "M-k") 'describe-keymap)

(define-key global-map (kbd "<C-f10> g") 'customize-group)
(define-key global-map (kbd "<C-f10> v") 'customize-variable)
(define-key global-map (kbd "<C-f10> f") 'customize-face)

;;** gui options

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

(global-set-key (kbd "<C-M-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-M-wheel-down>") 'text-scale-decrease)


;;** files & buffers
(define-key search-map (kbd "C-f") 'ffap)

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "C-c b") 'ibuffer)

(global-set-key (kbd "<C-tab>")   'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)
(global-set-key (kbd "<f12> <left>")    'previous-buffer)
(global-set-key (kbd "<f12> <right>")   'next-buffer)

(global-set-key (kbd "<f12> *")  (kbd "C-x b *scratch* RET"))


;;*** desktop
(require 'desktop)
(setq desktop-restore-eager 5)
(desktop-save-mode t)

;;***  recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-menu-path '("File"))
(recentf-mode t)

;;*** vc
(unless (fboundp 'vc-svn-root)  ;;fix emacs-23's svn support
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

;;*** nav
(autoload 'nav "nav"
  "Opens Nav in a new window to the left of the current one." t)

(autoload 'nav-toggle "nav"
  "Toggles the nav panel." t)

;;** windows
(setq split-width-threshold 120
      split-height-threshold 40)

;;***  winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "<f11> C-z") 'winner-undo)
(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;*** windmove
(idle-require 'windmove)
(progn
  (global-set-key (kbd "<f11> <up>")    'windmove-up)
  (global-set-key (kbd "<f11> <down>")  'windmove-down)
  (global-set-key (kbd "<f11> <left>")  'windmove-left)
  (global-set-key (kbd "<f11> <right>") 'windmove-right)
  )

;;*** windresize
(autoload 'windresize "windresize" "Resize windows interactively." t)
(setq windresize-default-increment 4)
(global-set-key (kbd "<f11> RET") 'windresize)

;;*** enlarge current window
;;(I don't use `golden-ratio-enable')
(autoload 'golden-ratio "golden-ratio"
  "Resizes current window to the golden-ratio's size specs" t)
(autoload 'golden-ratio-enable "golden-ratio"
  "Enables golden-ratio's automatic window resizing" t)

(defun golden-ratio+ ()
  "Enlarge current window, more than command `golden-ratio'."
  (interactive)
  (let ((-golden-ratio-value 1.3))
    (call-interactively 'golden-ratio)))

(global-set-key (kbd "<f11> x") 'golden-ratio)
(global-set-key (kbd "<f11> X") 'golden-ratio+)


(defun anything-enlarge-window ()
  (interactive)
  (with-anything-window
      (call-interactively 'golden-ratio)))

(defun anything-enlarge-window+ ()
  (interactive)
  (with-anything-window
    (let ((-golden-ratio-value 1.3))
      (call-interactively 'golden-ratio))))

(eval-after-load "anything"
  `(progn
     (define-key anything-map (kbd "<f11> x") 'anything-enlarge-window)
     (define-key anything-map (kbd "<f11> X") 'anything-enlarge-window+)
     ))


;;** editing

;;***  CUA

(transient-mark-mode t)
(setq shift-select-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'hyper)  ;;leave C-RET
(cua-mode t)

(global-set-key (kbd "C-x r RET") 'cua-set-rectangle-mark)

(when window-system
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)
    )

(setq mouse-yank-at-point t) ;;rather than the click point

;;***  tab key & indent
(setq tab-always-indent t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;***  parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;;***  newline & line-wrap
(setq require-final-newline 't)
(setq-default truncate-lines t)
(setq-default fill-column 100)
;;(auto-fill-mode t)

(global-set-key (kbd "<f10> C-w") 'toggle-truncate-lines)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;;***  changes
(require 'undo-tree nil 'noerror)
(eval-after-load "undo-tree"
  `(progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)
      ))

(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(define-key global-map (kbd "<f10> h c") 'highlight-changes-visible-mode)

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

;;***  quickly swap lines
(eval-after-load "drag-stuff"
  `(progn
     ;;    (setq drag-stuff-modifier 'hyper)
     (add-to-list 'drag-stuff-except-modes 'org-mode)
     (drag-stuff-global-mode t)))

(idle-require 'drag-stuff)


;;***  misc

(global-set-key (kbd "C-=") 'align-regexp)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;;** minibuffer

;;***  icomplete
(icomplete-mode t)  ;; completion for minibuffer
                                        ; commands (M-x)
                                        ; variables (C-h v, customize-variable)
                                        ; functions (C-h f)
                                        ; customize-group

;;***  ido
(require 'ido)

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
;;disable the merging (the "looking in other directories" in ido vulgo) 
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 'buffers)

;;***  anything
(autoload 'anything-recentf "anything-config"
  "Preconfigured `anything' for `recentf'." t)

(progn
  (global-set-key (kbd "<f5> r") 'anything-recentf)
  (global-set-key (kbd "<f5> b") 'anything-buffers+)
  (global-set-key (kbd "<f5> B") 'anything-bookmarks)
  (global-set-key (kbd "<f5> l") 'anything-locate)
  (global-set-key (kbd "<f5> c") 'anything-browse-code)
  (global-set-key (kbd "<f5> i") 'anything-imenu)
  (global-set-key (kbd "<f5> o") 'anything-occur)
  )

(eval-after-load "anything-config"    
    `(progn
       (global-set-key (kbd "<f5> a") anything-command-map)

       (define-key global-map (kbd "<f5> |") 'anything-toggle-resplit-window)
       
       ;;enable multiple keyword/regexp match
       ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
       ;;(global-set-key (kbd "M-x") 'anything-M-x)

       (define-key minibuffer-local-map (kbd "<f5> <f5>") 'anything-minibuffer-history)
      ))


;;(unless (require 'anything-config nil t)
;;  (message "%s: failed to load `anything'." load-file-name))
(idle-require 'anything-config)

;;*** misc


;;** completion
;;*** emacs built-in
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete))

;; Emacs default:
;;   M-TAB - lisp-complete-symbol(<24)/completion-at-point(v24)
;;   M-/ - dabbrev-expand

(global-set-key (kbd "M-/") 'hippie-expand)

;;***  auto-compelte
(eval-after-load "auto-complete-config"    
  `(progn
      (ac-config-default)
      (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)
      (define-key ac-completing-map (kbd "C-s") 'ac-isearch) 
      
      ;;(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)

      (if (load "auto-complete-scite-api" t)
          (add-to-list 'ac-sources 'ac-source-scite-api)
        (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))
  )

(define-key global-map (kbd "<f10> a c") 'auto-complete-mode)

(unless (and (load "auto-complete" t)
             (load "auto-complete-config" t))
  (message "%s: failed to load `auto-complete'." load-file-name))

(defun ac-toggle-source (source &optional desire)
  "Add or remove a SOURCE in `ac-sources'.

If DESIRE given, this source would be absolutely added (if DESIRE > 0) or
remove (if DESIRE <= 0). If DESIRE not given, it would be toggled."
  (interactive
   (list (intern-soft (ido-completing-read "Source: "
										   (loop for x being the symbols
												 if (and (boundp x)
														 (string-match "^ac-source-" (symbol-name x)))
												 collect (symbol-name x))))))
  (when (and source (symbolp source))
	(if desire
		(if (> desire 0)
			(add-to-list 'ac-sources source)
		  (setq ac-sources (remq source ac-sources)))
	  (if (memq source ac-sources)
		  (setq ac-sources (remq source ac-sources))
		(add-to-list 'ac-sources source)))
	(message "Source `%s' %s." source (if (memq source ac-sources)
										  "enabled"
										"disabled"))))

(progn
  (define-key global-map (kbd "C-. f") 'ac-complete-filename)
  (define-key global-map (kbd "C-. i") 'ac-complete-imenu)
  (define-key global-map (kbd "C-. a") 'ac-complete-scite-api)
  (define-key global-map (kbd "C-. y") 'ac-complete-yasnippet)

  ;; C-. not availiable on xterm, use C-^ instead
  (define-key key-translation-map (kbd "C-^") (kbd "C-."))
  )


;;** code folding

;;***  hideshow
(eval-after-load "hideshow"
  `(progn
     (define-key hs-minor-mode-map (kbd "M-+")  'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "<C-mouse-1>") 'hs-mouse-toggle-hiding)
     ))

(define-key global-map (kbd "<f10> h s") 'hs-minor-mode)

(defun bmz/turn-on-hideshow ()
  (interactive)
  (if (display-graphic-p)
      (if (require 'hideshowvis nil t)
          (progn
            (require 'hideshow-fringe nil t))
            (hs-minor-mode t)
            (hideshowvis-enable))
        (hs-minor-mode t))
     )

(eval-after-load "hideshowvis"
  ` (define-key global-map (kbd "<f10> h s") 'hideshowvis-minor-mode)
  )


;;***  outline
(idle-require 'outline)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<C-up>")   'outline-previous-visible-heading)
(global-set-key (kbd "<C-down>") 'outline-next-visible-heading)

(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")   outline-mode-prefix-map)
     
     (global-set-key (kbd "C-z <up>")     'outline-previous-visible-heading)
     (global-set-key (kbd "C-z <down>")   'outline-next-visible-heading)
     
     (global-set-key (kbd "<C-wheel-up>")   'outline-previous-visible-heading)
     (global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
     
     ;;for folding, `outline-minor-mode' must be turnned on
     (global-set-key (kbd "M-+")            'outline-toggle-children)
     (define-key outline-mode-prefix-map (kbd "<left>")  'hide-subtree)
     (define-key outline-mode-prefix-map (kbd "<right>") 'show-subtree)

     (global-set-key (kbd "<C-mouse-1>")    'outline-toggle-children)
     (global-set-key (kbd "<C-mouse-3>")    'show-subtree)
     (global-set-key (kbd "<C-mouse-2>")    'show-all)
  ))

(define-key global-map (kbd "<f10> o l") 'outline-minor-mode)

(autoload 'qtmstr-outline-mode "qtmstr-outline"
  "Add left-fringe +/- icons and line overlays for outline-sections." t)

(define-key global-map (kbd "<f10> q o") 'qtmstr-outline-mode)


;;***  highlight-symbol
(idle-require 'highlight-symbol)

(define-key search-map (kbd "j")     'highlight-symbol-at-point)
(define-key search-map (kbd "#")     'highlight-symbol-prev)
(define-key search-map (kbd "*")     'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

;;*** occur
(defun occur-current-symbol (arg)
  (interactive "P")
  (occur (thing-at-point 'symbol) arg))

(define-key search-map "O" 'occur-current-symbol)


;;*** idle-highlight
(autoload 'idle-highlight "idle-highlight"
  "highlight the word the point is on" t)


(define-key global-map (kbd "<f10> i h") 'idle-highlight)

;;*** iedit
(autoload 'iedit-mode "iedit"
  "Edit multiple regions in the same way simultaneously." t)

(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-c ;") 'iedit-mode)  ;;for terminal

;;** some visual effect

;;***  bm
(idle-require 'bm)

(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)
(autoload 'bm-toggle-mouse "bm" "Toggle a bookmark with a mouse click." t)

(progn
  (global-set-key (kbd "<C-f2>")     'bm-toggle)
  
  (global-set-key (kbd "<f2> <f2>") 'bm-toggle)
  (global-set-key (kbd "<f2> n")    'bm-next)
  (global-set-key (kbd "<f2> p")    'bm-previous)
  (global-set-key (kbd "<f2> l")    'bm-show)

  (global-set-key (kbd "<left-fringe> <C-mouse-1>")     'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <C-wheel-up>")    'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <C-wheel-down>")  'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <C-mouse-2>")     'bm-show)
  )


;;*** other highlighting
;; highlight url
(defun highlight-url/bmz ()
  (interactive)
  (require 'hi-lock)
  (highlight-regexp "https?://[^]
\n\|]+" 'link))

;;(add-hook 'find-file-hook 'highlight-url/bmz)
(add-hook 'find-file-hook 'goto-address-mode) ;;this one is better
(define-key goto-map "u" 'goto-address)

(global-set-key (kbd "M-s RET") 'browse-url)


;; highlight todo
(defun highlight-todo/bmz ()
  (interactive)
  (highlight-lines-matching-regexp "\\<FIXME\\>:" 'font-lock-warning-face)
  (highlight-lines-matching-regexp "\\<TODO\\>:" 'font-lock-warning-face)
  (highlight-lines-matching-regexp "\\<WARN" 'font-lock-warning-face)
  )

(add-hook 'find-file-hook 'highlight-todo/bmz)


;; highlight header
(defun highlight-outline-header/bmz ()
  (interactive)
  (highlight-lines-matching-regexp "^;;; \\w" 'hi-black-hb)
  (highlight-lines-matching-regexp "^;;\\*+ " 'hi-blue))

(eval-after-load "lisp-mode"
  `(add-hook 'emacs-lisp-mode-hook 'highlight-outline-header/bmz))


;;** programming


;;** buffer navigations
;;*** mark
(global-set-key (kbd "M-`")   'set-mark)
;;(global-set-key (kbd "M-`") 'exchange-point-and-mark)
;;(global-set-key (kbd "ESC M-`")   'pop-mark)

(global-set-key (kbd "M-`")   'set-mark-command)
;;(global-set-key (kbd "M-`")   'cua-exchange-point-and-mark)
(global-set-key (kbd "ESC M-`") 'pop-to-mark-command)


;;***  recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)


;;** major modes
;;*** emacs lisp mode
(eval-after-load "lisp-mode"
  `(progn
     (define-key goto-map (kbd "f") 'find-function-at-point)
     (define-key goto-map (kbd "F") 'find-function)
     (define-key goto-map (kbd "v") 'find-variable-at-point)
     (define-key goto-map (kbd "V") 'find-variable)

     (require 'eldoc)
     (require 'eldoc-extension nil t)
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     ))


;;** utils

;;*** eshell


;;** misc
(column-number-mode t)




	 
  
