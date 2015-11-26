(fset 'yes-or-no-p 'y-or-n-p)

(unless (fboundp 'idle-require)
    (defalias 'idle-require 'require))

(server-start)

;; ** key bindings

;; ***  key modifiers

;; ***  <f1> .. <f12> as prefix key
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

(global-unset-key (kbd "<f5>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "<f12>"))
(global-unset-key (kbd "M-u"))
(define-key key-translation-map (kbd "M-u") (kbd "<apps>"))

;; *** misc
(define-key key-translation-map (kbd "<left-fringe> <mouse-4>")   (kbd "<left-fringe> <wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <mouse-5>")   (kbd "<left-fringe> <wheel-down>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-4>") (kbd "<left-fringe> <C-wheel-up>"))
(define-key key-translation-map (kbd "<left-fringe> <C-mouse-5>") (kbd "<left-fringe> <C-wheel-down>"))

(global-unset-key (kbd "M-h"))

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
         (set-frame-parameter nil 'fullscreen 'fullboth)
         (set-frame-position (selected-frame) 5 25)
         (set-frame-width nil (- (frame-parameter nil 'width) 3))
         (set-frame-height nil (- (frame-parameter nil 'height) 3)))))))

(add-hook 'window-setup-hook 'maximize-frame t)    

;;(run-with-idle-timer 2 nil 'maximize-frame)

(global-set-key (kbd "<C-f11> <C-f11>") 'maximize-frame)

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

(global-set-key (kbd "<f12> *")  (kbd "C-x b *scratch* RET"))

(idle-require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; *** describe-buffer
;; based on `describe-buffer' from http://www.emacswiki.org/emacs/download/help-fns%2b.el
(defun describe-buffer- (&optional buffer-name)
    "Describe the existing buffer named BUFFER-NAME.
By default, describe the current buffer."
    ;; (interactive "bDescribe buffer: ")
    (interactive "@")
    (unless buffer-name (setq buffer-name  (buffer-name)))
    (help-setup-xref `(describe-buffer- ,buffer-name) (called-interactively-p 'interactive))
    (let ((buf  (get-buffer buffer-name)))
      (unless (and buf  (buffer-live-p buf))  (error(format "No such live buffer `%s'" buffer-name)))
      (let* ((file       (or (buffer-file-name buf)
                             (with-current-buffer buf
                               (and (eq major-mode 'dired-mode)  default-directory))))
             (help-text  (concat
                          (format "Buffer `%s'\n%s\n\n" buffer-name (make-string
                                                                     (+ 9 (length buffer-name)) ?-))
                          (and file  (format "File/directory: %s\n\n" file))
                          (format "Mode:           %s\n"
                                  (with-current-buffer buf (format-mode-line mode-name)))
                          (format "Encoding:       %s\n" buffer-file-coding-system)
                          (format "Line-ending:    %s\n"
                                (with-current-buffer buf
                                  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
                                    (cond
                                     ((eq 0 eol-type) "UNIX")
                                     ((eq 1 eol-type) "DOS")
                                     ((eq 2 eol-type) "MAC")
                                     (t "???")))))
                          (format "Lines:          %s\n"
                                  (with-current-buffer buf
                                      (count-lines (point-min) (point-max))))
                          (format "Chars:          %g\n" (buffer-size buf))
                          (format "Modified:       %s\n" (if (buffer-modified-p buf) "yes" "no"))
                          (with-current-buffer buf
                            (format "Read-only:      %s\n" (if buffer-read-only "yes" "no")))
                          (with-current-buffer buf
                            (if (not buffer-display-time)
                                "Never displayed\n"
                              (format "Last displayed: %s\n"
                                      (format-time-string
                                       ;; Could use this, for short format: "%02H:%02M:%02S"
                                       ;; Or this, for a bit longer: "%_3a %_2l:%02M:%02S %_2p"
                                       "%a %b %e %T %Y (%z)"
                                       buffer-display-time)))))))
        
        (with-help-window (help-buffer)
          (with-current-buffer (help-buffer)
            (insert help-text))))))

(global-set-key (kbd "C-h B") 'describe-buffer-)

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
(require 'desktop)
(setq desktop-restore-eager 5)
(desktop-save-mode t)

(global-set-key (kbd "<f12> C-l") 'desktop-change-dir)
(global-set-key (kbd "<f12> C-s") 'desktop-save)


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
(setq split-width-threshold 120
      split-height-threshold 60)

(global-set-key (kbd "<f11> <tab>") 'other-window)
(global-set-key (kbd "<f11> <backtab>") (kbd "C-u -1 C-x o"))

(global-set-key (kbd "<f11> 1 v") 'delete-other-windows-vertically)
(global-set-key (kbd "<f11> 1 h") 'delete-other-windows-horizontally)

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

;; *** enlarge current window
;;(I don't use `golden-ratio-enable')
(autoload 'golden-ratio "golden-ratio"
  "Resizes current window to the golden-ratio's size specs" t)
(autoload 'golden-ratio-enable "golden-ratio"
  "Enables golden-ratio's automatic window resizing" t)

(defun golden-ratio+ ()
  "Enlarge current window, more than command `golden-ratio'."
  (interactive)
  (require 'golden-ratio)
  (let ((-golden-ratio-value 1.25))
    (call-interactively 'golden-ratio)))

(global-set-key (kbd "<f11> x") 'golden-ratio)
(global-set-key (kbd "<f11> X") 'golden-ratio+)

(defun anything-enlarge-window ()
  (interactive)
  (require 'golden-ratio)
  (with-anything-window
    (call-interactively 'golden-ratio)))

(defun anything-enlarge-window+ ()
  (interactive)
  (require 'golden-ratio)
  (with-anything-window
    (let ((-golden-ratio-value 1.3))
      (call-interactively 'golden-ratio))))

(eval-after-load "anything"
  `(progn
     (define-key anything-map (kbd "<f11> x") 'anything-enlarge-window)
     (define-key anything-map (kbd "<f11> X") 'anything-enlarge-window+)
     ))


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
(define-key key-translation-map (kbd "<backtab>") (kbd "<S-tab>"))
  

;; ***  parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;; ***  newline & line-wrap
(setq require-final-newline 't)
(setq-default truncate-lines t)

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
     (drag-stuff-global-mode t)))

(idle-require 'drag-stuff)


;; ***  misc

(global-set-key (kbd "C-=") 'align-regexp)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(progn
  (global-unset-key (kbd "M-c"))
  (global-set-key (kbd "M-c c") 'capitalize-word)
  (global-set-key (kbd "M-c l") 'downcase-word)
  (global-set-key (kbd "M-c u") 'upcase-word)
  )


;; ** minibuffer
;; *** ivy-mode
;; better replacement for icomplete + ido + ido-vertical + ido-ubiquitous
;; - commands (M-x, where-is)
;; - variables (describe-variable, set-variable, customize-variable, find-variable)
;; - functions (describe-function, find-function)
;; - groups (customize-group)
;; - libraries (find-library)
;; - packages (describe-package, package-install)
;; - tags (find-tag)
;; - info nodes (Info-goto-node, info-lookup-symbol)

(autoload 'ivy-mode "ivy"
  "Toggle Ivy mode on or off." t)

(unless (fboundp 'setq-local)
  ;; emacs <= 24.2 doesn't have `setq-local'
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val)))

(idle-require 'ivy)

(eval-after-load "ivy"
  `(progn
     (icomplete-mode -1)
     (ido-mode -1)

     (ivy-mode 1)
     ))

;; *** icomplete
;; completion for minibuffer
;; - commands (M-x, where-is)
;; - variables (describe-variable, set-variable, customize-variable, find-variable)
;; - functions (describe-function, find-function)
;; - groups (customize-group)
;; - libraries (find-library)
(icomplete-mode t) ;; obsolete in favor of `ivy-mode'

;; ***  ido
(require 'ido)

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
;;disable the merging (the "looking in other directories" in ido vulgo) 
(setq ido-auto-merge-work-directories-length -1)
;; `ido-mode' for files is nasty :-(
(ido-mode 'buffers)  ;; obsolete in favor of `ivy-mode'

;; ***  anything
(autoload 'anything-recentf "anything-config"
  "Preconfigured `anything' for `recentf'." t)

(progn
  (global-set-key (kbd "<M-f5>") 'anything-resume)
  
  (global-set-key (kbd "<f5> f") 'anything-for-files)
  (global-set-key (kbd "<f5> b") 'anything-buffers+)
  
  (global-set-key (kbd "<f5> r") 'anything-recentf)
  (global-set-key (kbd "<f5> B") 'anything-bookmarks)
  (global-set-key (kbd "<f5> l") 'anything-locate)
  (global-set-key (kbd "<f5> c") 'anything-browse-code)
  (global-set-key (kbd "<f5> i") 'anything-imenu)
  (global-set-key (kbd "<f5> o") 'anything-occur)
  (global-set-key (kbd "<f5> R") 'anything-register)
  (global-set-key (kbd "<f5> M-y") 'anything-show-kill-ring)
  )

(eval-after-load "anything-config"    
    `(progn
       ;;enable multiple keyword/regexp match
       (require 'anything-match-plugin nil t)
       
       (global-set-key (kbd "<f5> a") anything-command-map)

       (define-key global-map (kbd "<f5> |") 'anything-toggle-resplit-window)       
       
       ;;(global-set-key (kbd "M-x") 'anything-M-x)

       (define-key minibuffer-local-map (kbd "<f5> M-h") 'anything-minibuffer-history)
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

;; ***  auto-compelte
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
  (define-key global-map (kbd "<apps> , f") 'ac-complete-filename)
  (define-key global-map (kbd "<apps> , i") 'ac-complete-imenu)
  (define-key global-map (kbd "<apps> , a") 'ac-complete-scite-api)
  (define-key global-map (kbd "<apps> , y") 'ac-complete-yasnippet)
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
     
     (define-key hs-minor-mode-map (kbd "M-+")  'hs-toggle-hiding)
;;     (define-key hs-minor-mode-map (kbd "<C-mouse-1>") 'hs-mouse-toggle-hiding)
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


;; *** outline-org-like (use org-like headings for outline)
;; FIXME: switch to orgstruct-mode of org-8.x?
(autoload 'outline-org-mode  "outline-org-like"
  "A special `outline-minor-mode' that use org-mode-style headings." t)

(autoload 'outline-org-headings-mode "outline-org-like"
  "org-mode like heading highlighting." t)

(autoload 'anything-outline-org-headings "outline-org-like"
  "Preconfigured anything to show org-mode-like headings." t)

(global-set-key (kbd "<f5> C-z") 'anything-outline-org-headings)

(idle-require 'outline-org-like)

(eval-after-load "outline-org-like"
  `(progn
     (if (boundp 'prog-mode-hook)
         (add-hook 'prog-mode-hook 'outline-org-headings-mode)
       (add-hook 'find-file-hook 'outline-org-headings-mode))
     
     (define-key outline-mode-prefix-map (kbd "<f5>") 'outline-org-headings-mode)
     ))

;; ;; highlight header
;; (defun highlight-outline-header/bmz ()
;;   (interactive)
;;   (highlight-lines-matching-regexp "^;;; \\w" 'hi-black-hb)
;;   ;; highlight headers in this file
;;   (highlight-lines-matching-regexp "^;; \\* "          'org-level-1)
;;   (highlight-lines-matching-regexp "^;; \\*\\* "       'org-level-2)
;;   (highlight-lines-matching-regexp "^;; \\*\\*\\* "    'org-level-3)
;;   (highlight-lines-matching-regexp "^;; \\*\\*\\*\\* " 'org-level-4))

;; (eval-after-load "lisp-mode"
;;   `(add-hook 'emacs-lisp-mode-hook 'highlight-outline-header/bmz))


;; ** highlighting
;; ***  highlight-symbol
(autoload 'highlight-symbol-at-point-ext "highlight-symbol"
  "Toggle highlighting of the symbol at point." t)
(idle-require 'highlight-symbol)

(define-key search-map (kbd "j")     'highlight-symbol-at-point-ext)
(define-key search-map (kbd "#")     'highlight-symbol-prev)
(define-key search-map (kbd "*")     'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point-ext)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)
(global-set-key (kbd "<S-mouse-3>")       'highlight-symbol-occur)

(define-key search-map "O" 'highlight-symbol-occur)

(eval-after-load "highlight-symbol"
  `(progn
     (if (version< emacs-version "24.4")
         ;; add an alias
         (defalias 'highlight-symbol-at-point-ext 'highlight-symbol-at-point)
       
       ;; Emacs 24.4 has a built-in `highlight-symbol-at-point`.
       (defalias 'highlight-symbol-at-point-built-in 'highlight-symbol-at-point)
         
       ;; we have to 'reimplemented' it (copied from `highlight-symbol.el`)
       (defun highlight-symbol-at-point-ext ()
         "Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'."
         (interactive)
         (let ((symbol (highlight-symbol-get-symbol)))
           (unless symbol (error "No symbol at point"))
           (if (highlight-symbol-symbol-highlighted-p symbol)
               (highlight-symbol-remove-symbol symbol)
             (highlight-symbol-add-symbol symbol))))
       )))

;; *** idle-highlight
(autoload 'idle-highlight-mode "idle-highlight-mode"
  "Idle-Highlight Minor Mode" t)

(define-key global-map (kbd "<f10> i h") 'idle-highlight-mode)

;; *** iedit
(autoload 'iedit-mode "iedit"
  "Edit multiple regions in the same way simultaneously." t)
(autoload 'iedit-mode-on-function "iedit"
  "Toggle Iedit mode on current function." t)

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
(add-hook 'find-file-hook 'goto-address-mode) ;;this one is better
(define-key goto-map "u" 'goto-address)

(global-set-key (kbd "M-s RET") 'browse-url)

;; **** this supports more link types
;; http://orgmode.org/manual/External-links.html
;; file:/etc/fstab
;; file:/user@machine:/home/user/.bashrc
;; info:org
;; shell:ls -l
;; elisp:tool-bar-mode

(autoload 'org-link-minor-mode "org-link-minor-mode"
  "Toggle display of org-mode style bracket links in non-org-mode buffers." t)

(when (and (require 'org nil t)
         (string< "7.3" org-version))
  (remove-hook 'find-file-hook 'goto-address-mode)
  (add-hook 'find-file-hook 'org-link-minor-mode))

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
(global-set-key (kbd "M-`")   'set-mark)
;;(global-set-key (kbd "M-`") 'exchange-point-and-mark)
;;(global-set-key (kbd "ESC M-`")   'pop-mark)

(global-set-key (kbd "M-`")   'set-mark-command)
;;(global-set-key (kbd "M-`")   'cua-exchange-point-and-mark)
(global-set-key (kbd "M-g `") 'pop-to-mark-command)


;; ** major modes
;; *** emacs lisp mode
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


(defalias 'fdap 'find-function-at-point)
(defalias 'fvap 'find-variable-at-point)

(autoload 'find-symbol-at-point  "bmz-elisp-misc"
  "Find the definiton of the SYMBOL near point." t)

(eval-after-load "lisp-mode"
  `(progn
     (define-key emacs-lisp-mode-map (kbd "C-c .") 'find-symbol-at-point)
     ))


;; ** utils
(autoload 'ifas "bmz-elisp-misc"
  "Insert the first line of documentation of a function." t)

(autoload 'load-and-execute  "bmz-elisp-misc"
  "load a library 'foobar' and execute the command with same name:" t)

(global-set-key (kbd "M-X") 'load-and-execute)


;; *** workaround for some display tweaking
(defun bmz/init-frame ()
  (interactive)
  (run-hook-with-args 'after-make-frame-functions
                      (selected-frame)))
  
;;put face-adjusting code to hook `after-make-frame-functions'
;;then use this to call them
(global-set-key (kbd "<f12> <f12>") 'bmz/init-frame)
                
(run-with-idle-timer 3 nil 'bmz/init-frame)


;; ** misc
(column-number-mode t)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "ESC M-%") 'query-replace-regexp)

(setq bookmark-save-flag 1) ;; save every time we make or delete a bookmark
