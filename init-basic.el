(fset 'yes-or-no-p 'y-or-n-p)

(unless (fboundp 'idle-require)
    (defalias 'idle-require 'require))

(server-start)

;; ** key bindings

;; ***  key modifiers

;; ***  <f1> .. <f12> as prefix key
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

(global-set-key (kbd "<f3>") search-map)
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))
;(global-unset-key (kbd "M-`"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-l"))
;; NOTE: M-O is a xterm control sequence prefix. refer: (find-library "term/xterm")
(global-unset-key (kbd "M-o"))

(define-key key-translation-map (kbd "M-u") (kbd "<apps>"))

;; *** misc
(define-key key-translation-map (kbd "<left-fringe> <mouse-4>")   (kbd "<left-fringe> <wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <mouse-5>")   (kbd "<left-fringe> <wheel-down>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-4>") (kbd "<left-fringe> <C-wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-5>") (kbd "<left-fringe> <C-wheel-down>"))

(global-unset-key (kbd "<C-f10>")) ;;to make emacs-25 happy

;; ** emacs environment
(progn
  (global-unset-key (kbd "<M-f10>"))
  (global-set-key (kbd "<M-f10> e r") 'eval-region)
  (global-set-key (kbd "<M-f10> e b") 'eval-buffer)
  (global-set-key (kbd "<M-f10> l l") 'load-library)
  (global-set-key (kbd "<M-f10> f l") 'find-library)
  (global-set-key (kbd "<M-f10> f f") 'find-function)
  (global-set-key (kbd "<M-f10> f v") 'find-variable)
  (global-set-key (kbd "<M-f10> d f") 'describe-function)
  (global-set-key (kbd "<M-f10> d v") 'describe-variable)
  )

;; *** help
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

(eval-after-load "info"
  `(add-hook 'Info-mode-hook #'(lambda ()
                                 ;; cancel binding to `Info-history-forward' and `Info-history-back'
                                 (define-key Info-mode-map (kbd "<mouse-4>") nil)
                                 (define-key Info-mode-map (kbd "<mouse-5>") nil)
                                 )))

;; ** gui options

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; *** frame title & modeline
(setq eol-mnemonic-dos "DOS"
      eol-mnemonic-unix "UNIX"
      eol-mnemonic-mac "MAC")

(setq-default frame-title-format
              '("%b - (%m)"
                (:eval (format " - Emacs %s" emacs-version))
                (:eval (format " - [%s]" (or buffer-file-name default-directory)))
                ))

;; http://www.reddit.com/r/emacs/comments/1nihkt/how_to_display_full_charset_name_in_modeline_eg/
(defvar my-mode-line-coding-format
      '(:eval
        (let* ((code (symbol-name buffer-file-coding-system))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol (if (eq 0 eol-type) "UNIX"
                      (if (eq 1 eol-type) "DOS"
                        (if (eq 2 eol-type) "MAC"
                          "???")))))
          (concat " " code " " eol " "))))

(put 'my-mode-line-coding-format 'risky-local-variable t)
(if (require 'cl nil t)
    (setq-default mode-line-format (substitute
                                'my-mode-line-coding-format
                                'mode-line-mule-info
                                mode-line-format)))

;; *** maximize frame
(defun maximize-frame (&optional frame)
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (w32-send-sys-command #xf030))
   ((eq window-system 'x)
    (progn
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
               '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
               '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
       ;; adjust window borders, needed when running remote emacs on local X server (?)
       (unless (string-match "^\\(localhost\\)?:[0-9]." (getenv "DISPLAY"))
        ;; (set-frame-parameter nil 'fullscreen 'fullboth)
         (set-frame-position (selected-frame) 5 25)
         (set-frame-width nil (- (frame-parameter nil 'width) 3))
         (set-frame-height nil (- (frame-parameter nil 'height) 3)))))))

(add-hook 'window-setup-hook 'maximize-frame t)    

;;(run-with-idle-timer 2 nil 'maximize-frame)

(global-set-key (kbd "<C-f11> <C-f11>") 'maximize-frame)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'OS
                     :key "<C-f11> <C-f11>"
                     :description "maximize-frame")
     t))


;; *** font
(global-set-key (kbd "<C-M-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-M-wheel-down>") 'text-scale-decrease)

(defun bmz/frame-init-font (&optional frame)
  (interactive (list (selected-frame)))
  (when window-system
    (with-selected-frame frame
      (let ((font (cl-find-if 'font-exists-p
                              '("Source Code Pro"
                                "DejaVu Sans Mono"
                                "Inconsolata"
                                "Ubuntu Mono"))))
        (if font
            (set-frame-font (concat font " 11") 'keep-frame-size)
          (message "No suitable default font found. %s"
		   "We recommend you install `ttf-dejavu-core' or `ttf-inconsolata'"))))))

(defun font-exists-p (font)
  "Test if FONT is available."
   (if (null (list-fonts (font-spec :family font)))
              ;; 2008-02-26 function of the new font backend (Emacs 23),
              ;; instead of `x-list-fonts'
       nil
     t))

(add-hook 'after-make-frame-functions 'bmz/frame-init-font)

;; This is required to make sure tables in org-mode align well, even if
;; Chinese & English chars used together.
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; If changed, maybe you need to restart Emacs (or `redraw-frame'?)
(setq face-font-rescale-alist '(("宋体" . 1.25)
                                ("新宋体" . 1.25)
                                ("微软雅黑" . 1.25)
                                ("Microsoft Yahei" . 1.25)
                                ("WenQuanYi Zen Hei" . 1.25)))


;; ** files & buffers
(define-key search-map (kbd "C-f") 'ffap)

(global-set-key (kbd "<C-down-mouse-1>") nil)
(global-set-key (kbd "<C-mouse-1>") 'ffap-at-mouse)

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "C-c b") 'ibuffer)

(global-set-key (kbd "<C-tab>")   'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)
(global-set-key (kbd "<f12> <left>")    'previous-buffer)
(global-set-key (kbd "<f12> <right>")   'next-buffer)

(global-set-key (kbd "<C-f4>") 'kill-this-buffer)

(global-set-key (kbd "<f12> *")  (kbd "C-x 4 b *scratch* RET"))

(idle-require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; *** backups
(setq make-backup-files t) ;;to disable backup, set it to nil
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq backup-by-copying t
      backup-by-copying-when-linked nil)

(when nil
 (setq version-control t
       delete-old-versions t
       kept-new-versions 6
       kept-old-versions 2)
 )

;; **** make backup to a designated dir, mirroring the full path
;; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backups/")
        (filePath (replace-regexp-in-string ":" "/" fpath )) ; remove Windows driver letter in path, e.g. "C:"
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

;; *** desktop
(setq desktop-base-file-name (format ".emacs.desktop.%s" emacs-major-version))
(require 'desktop)
(setq desktop-restore-eager 5)
(desktop-save-mode t)

(global-set-key (kbd "<f12> C-l") 'desktop-change-dir)
(global-set-key (kbd "<f12> C-s") 'desktop-save)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Misc :key "<f12> C-l" :description "desktop-change-dir")
     (cheatsheet-add :group 'Misc :key "<f12> C-s" :description "desktop-save")
     t))


;; restore cursor position when re-opening a file
;; http://ergoemacs.org/emacs/emacs_save_cursor_position.html
(setq-default save-place t)
(idle-require 'saveplace)

;; ***  recentf
(require 'recentf)
(setq recentf-save-file (locate-user-emacs-file "recentf" ".recentf"))  ;; ~/.emacs.d/recentf rather than ~/.recentf
(setq recentf-max-saved-items 100)
(setq recentf-menu-path '("File"))
(recentf-mode t)

;; *** midnight-mode
(require 'midnight)
;; (midnight-delay-set 'midnight-delay "12:40")


;; ** windows
;;supress splitting on small window (works for `split-window-sensibly' and `display-buffer')
(setq split-width-threshold 150
      split-height-threshold 80)

(global-set-key (kbd "<f11> <tab>") 'other-window)
(global-set-key (kbd "<f11> <backtab>") (kbd "C-u -1 C-x o"))

;;NOTE: emacs 23 has no `delete-other-windows-vertically/horizontally'
(global-set-key (kbd "<f11> V") 'delete-other-windows-vertically)
(global-set-key (kbd "<f11> H") 'delete-other-windows-horizontally)

;; ***  winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "<f11> C-z") 'winner-undo)
(global-set-key (kbd "<f11> C-y") 'winner-redo)

;; *** windmove
(idle-require 'windmove)
(progn
  (global-set-key (kbd "<f11> <up>")    'windmove-up)
  (global-set-key (kbd "<f11> <down>")  'windmove-down)
  (global-set-key (kbd "<f11> <left>")  'windmove-left)
  (global-set-key (kbd "<f11> <right>") 'windmove-right)
  )

;; *** dedicated window
(defun toggle-window-dedicated (win)
  (interactive (list (selected-window)))
  (set-window-dedicated-p win (not (window-dedicated-p win)))
  (message "Dedicated state of window '%s' now set to %s" win (window-dedicated-p win)))

(global-set-key (kbd "<f11> *") 'toggle-window-dedicated)
  
;; *** windresize
(autoload 'windresize "windresize" "Resize windows interactively." t)
(setq windresize-default-increment 4)
(global-set-key (kbd "<f11> RET") 'windresize)


;; ** editing

;; ***  CUA

(transient-mark-mode t)
(setq shift-select-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)
(cua-mode t)

(setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)

(setq mouse-yank-at-point t) ;;rather than the click point

;; *** cua rectangle
;;(setq cua-rectangle-modifier-key 'hyper)  ;;leave C-RET

(global-set-key (kbd "C-x r RET") 'cua-set-rectangle-mark)

(eval-after-load "cua-rect"
  `(progn
     ;; as `C-?' already used by `undo-tree-redo'
     (define-key cua--rectangle-keymap (kbd "M-?") 'cua-help-for-rectangle)
     ))

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Rectangle/cua :key "C-x r RET" :description "cua-set-rectangle-mark")
     (cheatsheet-add :group 'Rectangle/cua :key "(cua-rect) M-?"       :description "(cua) cua-help-for-rectangle")
     t
     ))


;; ***  tab key & indent
(setq tab-always-indent t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun indent-region-or-line ()
  (interactive)
  (if mark-active
      (indent-region (region-beginning) (region-end)))
    (indent-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "<S-tab>") 'indent-region-or-line)
;(define-key key-translation-map (kbd "<backtab>") (kbd "<S-tab>"))
  

;; ***  parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;; ***  newline & line-wrap
(setq require-final-newline 't)
(setq-default truncate-lines t)

(eval-after-load "help-mode"
  `(progn
     (add-hook 'help-mode-hook 'visual-line-mode 'append)
     ))

;;(auto-fill-mode t)

(global-set-key (kbd "<f10> C-w") 'toggle-truncate-lines)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;; ***  changes
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
(global-set-key (kbd "C-c =") 'diff-buffer-with-file)

;; ***  quickly swap lines
(autoload 'drag-stuff-up "drag-stuff"
  "Drag stuff ARG lines up." t)
(autoload 'drag-stuff-down "drag-stuff"
  "Drag stuff ARG lines down." t)

(eval-after-load "drag-stuff"
  `(progn
     ;;    (setq drag-stuff-modifier 'hyper)
     (add-to-list 'drag-stuff-except-modes 'org-mode)
     (drag-stuff-define-keys)
     (drag-stuff-global-mode t)))

(idle-require 'drag-stuff)


;; ***  misc

(global-set-key (kbd "C-=") 'align-regexp)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(global-set-key (kbd "<M-S-backspace>") 'backward-kill-line)

(progn
  (global-unset-key (kbd "M-c"))
  (global-set-key (kbd "M-c c") 'capitalize-word)
  (global-set-key (kbd "M-c l") 'downcase-word)
  (global-set-key (kbd "M-c u") 'upcase-word)
  )


;; ** minibuffer
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; *** icomplete
;; completion for minibuffer
;; - commands (M-x, where-is)
;; - variables (describe-variable, set-variable, customize-variable, find-variable)
;; - functions (describe-function, find-function)
;; - groups (customize-group)
;; - libraries (find-library)
(icomplete-mode t) ;; obsolete in favor of `ivy-mode'

;; prevent icomplete from `read-file-name'
;; (`ido' is better (at least it allows you use 'C-f' to fallback to normal mode.
;;   with `ido-vertical-mode' it is much better)
(eval-after-load "icomplete"
  `(when (fboundp 'icomplete-simple-completing-p)
     
     (defun icomplete-simple-completing-p ()
       (unless executing-kbd-macro
	 (let ((table (icomplete--completion-table)))
	   (and table
		(or (not (functionp table))
		    (if (eq icomplete-with-completion-tables t)
			(not (eq 'read-file-name-internal table))
		      (member table icomplete-with-completion-tables)))))))
     ))
     

;; ***  ido
(require 'ido)

;;(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
;; disable the merging (the "looking in other directories" in ido vulgo) 
(setq ido-auto-merge-work-directories-length -1)
;; `ido-mode' for files is nasty :-(
(ido-mode 'buffers)  ;; obsolete in favor of `ivy-mode'

;; ***  anything
(autoload 'anything-recentf "anything-config"
  "Preconfigured `anything' for `recentf'." t)

(progn
  (global-set-key (kbd "M-o M-o") 'anything-resume)
  
  (global-set-key (kbd "M-o f") 'anything-for-files)
  (global-set-key (kbd "M-o b") 'anything-buffers+)
  
  (global-set-key (kbd "M-o r") 'anything-recentf)
  (global-set-key (kbd "M-o B") 'anything-bookmarks)
  (global-set-key (kbd "M-o l") 'anything-locate)
  (global-set-key (kbd "M-o c") 'anything-browse-code)
  (global-set-key (kbd "M-o i") 'anything-imenu)
  (global-set-key (kbd "M-o o") 'anything-occur)
  (global-set-key (kbd "M-o R") 'anything-register)
  (global-set-key (kbd "M-o M-y") 'anything-show-kill-ring)
  )

(eval-after-load "anything-config"
    `(progn
       (setq anything-enable-shortcuts nil)
     
       ;;enable multiple keyword/regexp match
       (require 'anything-match-plugin nil t)
       
       (global-set-key (kbd "M-o a") anything-command-map)

       (define-key global-map (kbd "M-o |") 'anything-toggle-resplit-window)       
       
       ;;(global-set-key (kbd "M-x") 'anything-M-x)

       (define-key minibuffer-local-map (kbd "M-o M-h") 'anything-minibuffer-history)
      ))


;;(unless (require 'anything-config nil t)
;;  (message "%s: failed to load `anything'." load-file-name))
(idle-require 'anything-config)


;; *** misc


;; ** completion
;; *** emacs built-in
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete))

;; Emacs default:
;;   M-TAB - lisp-complete-symbol(<24)/completion-at-point(v24)
;;   M-/ - dabbrev-expand

(global-set-key (kbd "M-/") 'hippie-expand)

;; *** auto-compelte
(eval-after-load "auto-complete-config"    
  `(progn
     (ac-config-default)

     ;;supress auto-starting completion. use TAB key instead
     (setq ac-auto-start nil)
     (ac-set-trigger-key "TAB")
     
      (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)
      (define-key ac-completing-map (kbd "C-s") 'ac-isearch) 
      
      (setq ac-quick-help-prefer-pos-tip t)
      (require 'pos-tip nil t)
      
      ;;redefine it to remove `ac-source-yasnippet'
      (defun ac-emacs-lisp-mode-setup ()
        (setq ac-sources (append '(ac-source-features
                                   ac-source-functions
                                   ;; ac-source-yasnippet
                                   ac-source-variables
                                   ac-source-symbols)
                                 ac-sources)))      
      ))

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
  (define-key global-map (kbd "M-c M-f") 'ac-complete-filename)
  (define-key global-map (kbd "M-c M-i") 'ac-complete-imenu)
  (define-key global-map (kbd "M-c M-a") 'ac-complete-scite-api)
  (define-key global-map (kbd "M-c M-y") 'ac-complete-yasnippet)
  ;; (define-key global-map (kbd "M-c M-s") 'ac-complete-ispell-word)

  ;; integrate completion-at-point to auto-compete
  (eval-after-load "auto-complete"
    `(progn
       (when (require 'ac-capf nil t)
         (define-key global-map (kbd "M-c TAB") 'ac-complete-capf)
         )))

  (eval-after-load "cheatsheet"
    `(progn
       (cheatsheet-add :group 'Auto-Complete :key "M-c M-f" :description "ac-complete-filename")
       (cheatsheet-add :group 'Auto-Complete :key "M-c M-i" :description "ac-complete-imenu")
       (cheatsheet-add :group 'Auto-Complete :key "M-c M-a" :description "ac-complete-scite-api")
       (cheatsheet-add :group 'Auto-Complete :key "M-c M-y" :description "ac-complete-yasnippet")
       (cheatsheet-add :group 'Auto-Complete :key "M-c TAB" :description "ac-complete-capf (completion-at-point)")
       t
       ))
  )


;; ** code folding

;; *** hideshow
(autoload 'hideshowvis-enable "hideshowvis"
  "Will enable hideshowvis minor mode" t)

(eval-after-load "hideshow"
  `(progn
     (or (require 'hideshow-fringe nil t)
         (ignore-errors
           (hideshowvis-symbols)))
     
     (define-key hs-minor-mode-map (kbd "M-h TAB")  'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "M-h M-h")  'hs-toggle-hiding)
     ;;     (define-key hs-minor-mode-map (kbd "<C-mouse-1>") 'hs-mouse-toggle-hiding)

     (define-key hs-minor-mode-map (kbd "M-h s")  'hs-show-block)
     (define-key hs-minor-mode-map (kbd "M-h h")  'hs-hide-block)
     (define-key hs-minor-mode-map (kbd "M-h S")  'hs-show-all)
     (define-key hs-minor-mode-map (kbd "M-h H")  'hs-hide-all)
     ))

(define-key global-map (kbd "<f10> h s") 'hs-minor-mode)

(defun turn-on-hideshow/bmz ()
  (interactive)
  (if (require 'hideshowvis nil t)
      (progn
        (or (require 'hideshow-fringe nil t)
            (ignore-errors
              (hideshowvis-symbols)))
        (hs-minor-mode t)
        (hideshowvis-enable))
    (hs-minor-mode t)))

(eval-after-load "hideshowvis"
  ` (progn
      (if (fboundp 'hideshowvis-symbols) ;;added in hideshowvis 0.5
          (hideshowvis-symbols))
      (ignore-errors
        (set-face-attribute 'hs-face nil :inherit 'font-lock-warning-face))
      
      (define-key global-map (kbd "<f10> h s") 'hideshowvis-minor-mode)
      (define-key hideshowvis-mode-map [left-fringe S-mouse-1] 'hs-mouse-hide-level)
      (define-key hideshowvis-mode-map [left-margin S-mouse-1] 'hs-mouse-hide-level)
      (set-face-attribute 'hs-face nil :inherit 'font-lock-warning-face)      
      ))

(defun hs-mouse-hide-level (e)
  "Mouse version of `hs-hide-level'."
  (interactive)
  (mouse-set-point e)
  (call-interactively 'hs-hide-level))

;; *** hideshow within view-mode
(setq view-read-only t) ;; open read-only file in view-mode

(defun toggle-view-mode-with-outline ()
  "Toggle `view-mode'. And `hs-minor-mode' always turned on."
  (interactive)
  (if view-mode
      (progn
        (view-mode -1) ;; quit `view-mode'
        (read-only-mode -1)) ;; switch `read-only' off
    (view-mode 1))
  (unless hs-minor-mode
    (hideshowvis-enable)) ;;enable `hs-minor-mode' with fringe indicator
  )

(global-set-key (kbd "<f10> V") 'toggle-view-mode-with-outline)

(eval-after-load "view"
  `(progn
     (define-key view-mode-map "i"  'view-exit)

     (define-key view-mode-map "s" 'hs-show-block)
     (define-key view-mode-map "h" 'hs-hide-hook)

     (define-key view-mode-map "S" 'hs-show-all)
     (define-key view-mode-map "H" 'hs-hide-all)

     ;;(define-key view-mode-map "z" 'hs-toggle-hiding)
     (define-key view-mode-map (kbd "TAB") 'hs-toggle-hiding)

     ;; hideshow doesn't support jumping to next/previous, we use outline here
     ;; (`outline-minor-mode' doesn't need to be turned on if we don't need folding operations)
     (define-key view-mode-map (kbd "n") 'outline-next-visible-heading)
     (define-key view-mode-map (kbd "p") 'outline-previous-visible-heading)
     (define-key view-mode-map (kbd "u") 'outline-up-heading)

     (define-key view-mode-map (kbd "*") 'highlight-symbol-next)
     (define-key view-mode-map (kbd "#") 'highlight-symbol-prev)
     ))

;; *** outline
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
     (define-key outline-mode-prefix-map (kbd "<left>")      'hide-subtree)
     (define-key outline-mode-prefix-map (kbd "<right>")     'show-subtree)
     (define-key outline-mode-prefix-map (kbd "<M-up>")      'outline-move-subtree-up)
     (define-key outline-mode-prefix-map (kbd "<M-down>")    'outline-move-subtree-down)

     (define-key outline-mode-prefix-map (kbd "<mouse-1>")   'outline-toggle-children)

     (define-key outline-minor-mode-map  (kbd "M-+")          'outline-toggle-children)
     (define-key outline-minor-mode-map (kbd "<C-mouse-3>")   'show-subtree)
     (define-key outline-minor-mode-map (kbd "<C-mouse-2>")   'show-all)
  ))

(define-key global-map (kbd "<f10> o l") 'outline-minor-mode)

(autoload 'qtmstr-outline-mode "qtmstr-outline"
  "Add left-fringe +/- icons and line overlays for outline-sections." t)

(define-key global-map (kbd "<f10> q o") 'qtmstr-outline-mode)


;; ** highlighting
;; *** highlight-symbol
(autoload 'highlight-symbol "highlight-symbol"
  "Toggle highlighting of the symbol at point." t)
(idle-require 'highlight-symbol)

(progn
  (define-key search-map "j"               'highlight-symbol)
  (define-key search-map "#"               'highlight-symbol-prev)
  (define-key search-map "*"               'highlight-symbol-next)
  (define-key search-map "O"               'highlight-symbol-occur)

  (global-set-key (kbd "<double-mouse-1>") 'highlight-symbol)
  (global-set-key (kbd "<S-wheel-up>")     'highlight-symbol-prev)
  (global-set-key (kbd "<S-wheel-down>")   'highlight-symbol-next)
  (global-set-key (kbd "<S-mouse-3>")      'highlight-symbol-occur)
  
  (global-set-key (kbd "<M-f3>")           'highlight-symbol-prev)
  (global-set-key (kbd "<S-f3>")           'highlight-symbol-next)
  (global-set-key (kbd "<C-f3>")           'highlight-symbol-occur)

  (global-set-key (kbd "<f3> j")           'highlight-symbol)
  (global-set-key (kbd "<f3> p")           'highlight-symbol-prev)
  (global-set-key (kbd "<f3> n")           'highlight-symbol-next)
  (global-set-key (kbd "<f3> O")           'highlight-symbol-occur)
)

;; *** idle-highlight
(autoload 'idle-highlight-mode "idle-highlight-mode"
  "Idle-Highlight Minor Mode" t)

(define-key global-map (kbd "<f10> i h") 'idle-highlight-mode)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Highlighting
                     :key "<f10> i h"
                     :description "idle-highlight-mode (auto highlight all occurences of current word)")
     t))


;; *** iedit
(autoload 'iedit-mode "iedit"
  "Edit multiple regions in the same way simultaneously." t)

(defun iedit-mode-on-function ()
  (interactive)
  (require 'iedit)
  (iedit-mode 0))

(global-set-key (kbd "C-;") 'iedit-mode-on-function)
(global-set-key (kbd "C-c ;") 'iedit-mode-on-function)  ;;for terminal


;; ** some visual effect

;; ***  bm
(idle-require 'bm)

(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)
(autoload 'bm-toggle-mouse "bm" "Toggle a bookmark with a mouse click." t)

(progn
  (global-set-key (kbd "<C-f2>")    'bm-toggle)
  
  (global-set-key (kbd "<f2> SPC")  'bm-toggle)
  (global-set-key (kbd "<f2> n")    'bm-next)
  (global-set-key (kbd "<f2> p")    'bm-previous)
  (global-set-key (kbd "<f2> l")    'bm-show)
  (global-set-key (kbd "<f2> <f2>") 'bm-next)

  (global-set-key (kbd "<left-fringe> <C-mouse-1>")     'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <C-wheel-up>")    'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <C-wheel-down>")  'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <C-mouse-3>")     'bm-show)
  )


;; *** highlight url
(add-hook 'find-file-hook 'goto-address-mode)
(define-key goto-map "u" 'goto-address)

(define-key search-map (kbd "RET") 'browse-url-at-point)
(defalias 'buap 'browse-url-at-point)

;; **** this supports more link types
;; http://orgmode.org/manual/External-links.html
;; file:/etc/fstab
;; file:/user@machine:/home/user/.bashrc
;; info:org
;; shell:ls -l
;; elisp:tool-bar-mode

(autoload 'org-link-minor-mode "org-link-minor-mode"
  "Toggle display of org-mode style bracket links in non-org-mode buffers." t)

;; (when (and (require 'org nil t)
;;          (string< "7.3" org-version))
;;   (remove-hook 'find-file-hook 'goto-address-mode)
;;   (add-hook 'find-file-hook 'org-link-minor-mode))

(global-set-key (kbd "C-c C-o") 'org-open-at-point-global)

;; *** other highlighting
;; highlight todo
(defun highlight-todo/bmz ()
  (interactive)
  (highlight-regexp (concat comment-start "+[ \t]*FIXME[: \t].*$") 'font-lock-warning-face)
  (highlight-regexp (concat comment-start "+[ \t]*TODO[: \t].*$") 'font-lock-warning-face)
  (highlight-regexp (concat comment-start "+[ \t]*WARN\\(ING\\)?[: \t].*$") 'font-lock-warning-face)  
  )

(add-hook 'find-file-hook 'highlight-todo/bmz)


;; ** buffer navigations
;; *** mark
;;(global-set-key (kbd "M-`")   'set-mark-command)
;;(global-set-key (kbd "M-`") 'exchange-point-and-mark)
(global-set-key (kbd "M-g `") 'pop-to-mark-command)

;; *** back-button: Visual navigation through mark rings
;;https://github.com/rolandwalker/back-button

(autoload 'back-button-mode "back-button"
  "Turn on back-button-mode." t)

(idle-require 'back-button)

(eval-after-load "back-button"
  `(progn
     (back-button-mode 1)

     (when (fbound 'cheatsheet-add)
       (cheatsheet-add :group 'Jump
                       :key "C-x <left>"
                       :description "back-button-local-backward")
       (cheatsheet-add :group 'Jump
                       :key "C-x <right>"
                       :description "back-button-local-forward")
       (cheatsheet-add :group 'Jump
                       :key "C-x <C-left>"
                       :description "back-button-global-backward")
       (cheatsheet-add :group 'Jump
                       :key "C-x <C-right>"
                       :description "back-button-global-forward"))
     t
     ))

;; ** major modes
;; *** emacs lisp mode
(eval-after-load "lisp-mode"
  `(progn
     (define-key emacs-lisp-mode-map (kbd "<f9> d")   'eval-defun)
     (define-key emacs-lisp-mode-map (kbd "<f9> SPC") 'eval-region)
     (define-key emacs-lisp-mode-map (kbd "<f9> (")   'eval-last-sexp)
     
     (define-key emacs-lisp-mode-map (kbd "<f4> f") 'find-function-at-point)
     (define-key emacs-lisp-mode-map (kbd "<f4> F") 'find-function)
     (define-key emacs-lisp-mode-map (kbd "<f4> v") 'find-variable-at-point)
     (define-key emacs-lisp-mode-map (kbd "<f4> V") 'find-variable)
     (define-key emacs-lisp-mode-map (kbd "<f4> SPC") 'find-symbol-at-point)

     (require 'eldoc)
     (require 'eldoc-extension nil t)
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     ))


(defalias 'fdap 'find-function-at-point)
(defalias 'fvap 'find-variable-at-point)

(autoload 'find-symbol-at-point  "bmz-elisp-misc"
  "Find the definiton of the SYMBOL near point." t)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Emacs-Lisp-Mode
                     :key "<f4> SPC"
                     :description "find-symbol-at-point")
     t))


;; ** utils
(autoload 'ifas "bmz-elisp-misc"
  "Insert the first line of documentation of a function." t)

(autoload 'load-and-execute  "bmz-elisp-misc"
  "load a library 'foobar' and execute the command with same name:" t)

(global-set-key (kbd "M-X") 'load-and-execute)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Minibuffer
                     :key "M-S-x"
                     :description "load library 'foobar' and execute 'M-x foobar-mode' or 'M-x foobar'.")
     t))


;; *** workaround for some display tweaking
(defun bmz/call-frame-init-functions ()
  "Nanually call all functions added into `after-make-frame-functions'."
  (interactive)
  (run-hook-with-args 'after-make-frame-functions
                      (selected-frame)))
  
;;put face-adjusting code to hook `after-make-frame-functions'
;;then use this to call them
(global-set-key (kbd "<f12> <f12>") 'bmz/call-frame-init-functions)
                
(run-with-timer 5 nil 'bmz/call-frame-init-functions)


;; ** misc
(setq scroll-margin 3)

(column-number-mode t)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "ESC M-%") 'query-replace-regexp)

(setq bookmark-save-flag 1) ;; save every time we make or delete a bookmark

;; set GC threshold to 80M (default value is 400k)
(setq gc-cons-threshold (* 80 1000 1000))

(global-set-key (kbd "<header-line> <C-mouse-3>") 'mouse-delete-window)
