;; ** shell
(autoload 'shell-toggle "sh-toggle"
  "Toggles between the *shell* buffer and the current buffer." t)
(autoload 'shell-toggle-cd "sh-toggle"
  "Calls `shell-toggle' and let it cd to path of current buffer." t)

(global-set-key (kbd "<f12> s") 'shell-toggle-cd)
(global-set-key (kbd "<f12> S") 'shell-toggle)

;;TIP: init file for shells in emacs
;;    ~/.emacs_SHELLNAME   (e.g. ~/.emacs_bash)
;;    ~/.emacs.d/init_SHELLNAME.sh  (e.g. ~/.emacs.d/init_bash.sh)
;; (info "(emacs) Interactive Shell")

;; ** eshell
;; *** shell toggle

;;(idle-require 'esh-toggle)
;;(idle-require 'sh-toggle)

(autoload 'eshell-toggle "esh-toggle"
  "Toggles between the *eshell* buffer and the current buffer." t)
(autoload 'eshell-toggle-cd "esh-toggle"
  "Calls `eshell-toggle' and let it cd to path of current buffer." t)

(require 'esh-mode)
(global-set-key (kbd "<f12> e") 'eshell-toggle-cd)
(global-set-key (kbd "<f12> E") 'eshell-toggle)

(defun eshell-cd (dir)
  (interactive "Dcd to: ")
  (let ((default-directory dir))
    (eshell-toggle-cd)))


;; *** some eshell command
(defun eshell/edit (&rest args)
  "Invoke find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\\+\([0-9]+\)\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defalias 'eshell/vi 'eshell/edit)
(defalias 'eshell/vim 'eshell/edit)
(defalias 'eshell/emacs 'eshell/edit)


(defun eshell/open (file)
    "Invoke system's associated application for FILE.
On Windows, baskslashes is substituted with slashes."
    (if (eq system-type 'gnu/linux)
        (shell-command (concat "gnome-open "
                               (shell-quote-argument (file))))
      (w32-shell-execute "Open"
                       (subst-char-in-string ?\\ ?/ (expand-file-name file))
		       nil)))

(defalias 'eshell/start 'eshell/open)


(defun eshell/clear()
  "to clear the eshell buffer."
  (interactive)  
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defalias 'eshell/cls 'eshell/clear)

(defun eshell/nc (&optional dir)
  (interactive)
  (nc-goto-dir (or dir default-directory)))


;; *** pcomplete for shell commands & args
;;stolen from http://linuxtoy.org/archives/emacs-eshell.html
(eval-after-load "auto-complete"
  `(progn
     (ac-define-source pcomplete
       '((candidates . pcomplete-completions)))
     ;; NOTE: `pcomplete-complations' won't work for command args when used in auto-complete.
     ;; http://emacswiki.org/emacs/EshellCompletion#toc4 provides a better solution

     (add-to-list 'ac-modes 'eshell-mode)
     (add-to-list 'ac-modes 'shell-mode)
     ))

;;(define-key global-map (kbd "<apps> , p") 'ac-complete-pcomplete)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Eshell
                     :key "C-c TAB"
                     :description "ac-complete-pcomplete: complete shell commands & args with pcomplete + auto-complete.")
     (cheatsheet-add :group 'Eshell
                     :key "<f5> TAB"
                     :description "anything-esh-pcomplete")
     (cheatsheet-add :group 'Eshell
                     :key "<f5> M-h"
                     :description "anything-esh-history")
     
     (cheatsheet-add :group 'Shell
                     :key "M-x ac-complete-pcomplete"
                     :description "complete shell commands & args with pcomplete + auto-complete.")
     t))

;; *** history

(defun eshell-toggle-cursor-keybinding (arg)
  "Toggle up/down key between {previous,next}-line and {previous,next}-input."
  (interactive "P")
  (if (or arg (eq (key-binding (kbd "<up>")) 'previous-line))
      (progn
        (define-key eshell-mode-map (kbd "<up>")   'eshell-previous-input)
        (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)
        (define-key eshell-mode-map (kbd "M-p")    'previous-line)
        (define-key eshell-mode-map (kbd "M-n")    'next-line)
        (message "Now up/down key now binding to `eshell-{previous,next}-input'."))
    (progn
        (define-key eshell-mode-map (kbd "M-p")    'eshell-previous-input)
        (define-key eshell-mode-map (kbd "M-n")    'eshell-next-input)
        (define-key eshell-mode-map (kbd "<up>")   'previous-line)
        (define-key eshell-mode-map (kbd "<down>") 'next-line)
        (message "Now up/down key now binding to `{previous,next}-line."))))

;;stolen from http://comments.gmane.org/gmane.emacs.help/7319
(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))


(autoload 'anything-eshell-history "anything-config"
  "Preconfigured anything for eshell history." t)
(autoload 'anything-esh-pcomplete  "anything-config"
  "Preconfigured anything to provide anything completion in eshell." t)


;; *** misc
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))


(defun bmz/eshell-mode-init ()
  ;; swap <home> and C-a
  (define-key eshell-mode-map (kbd "C-a")    'eshell-maybe-bol)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)  

  (define-key eshell-mode-map (kbd "M-.") 'kai-eshell-insert-last-word)

  (eshell-toggle-cursor-keybinding 1)
  (define-key eshell-mode-map (kbd "<Scroll_Lock>") 'eshell-toggle-cursor-keybinding)
  (define-key eshell-mode-map (kbd "<scroll>") 'eshell-toggle-cursor-keybinding)
  (define-key eshell-mode-map (kbd "<f5> M-h") 'anything-eshell-history)

  (define-key eshell-mode-map (kbd "<f5> TAB") 'anything-esh-pcomplete)
  (define-key eshell-mode-map (kbd "C-c TAB")  'ac-complete-pcomplete)
  ;;(define-key eshell-mode-map (kbd "M-TAB")  'complete-symbol)
  
  (define-key eshell-mode-map (kbd "<M-up>")   'eshell-previous-matching-input)
  (define-key eshell-mode-map (kbd "<M-down>") 'eshell-next-matching-input)
  
  ;;I'd like M-s as highlight-xxx prefix key
  (define-key eshell-mode-map (kbd "M-s") nil)

  (if (fboundp 'drag-stuff-mode)
      (drag-stuff-mode -1))
  
  ;; disable `highlight-symbol'  as it would make eshell lose all colors
  (define-key eshell-mode-map (kbd "<double-mouse-1>") 'mouse-set-point)
  
  (setq outline-regexp "^.* $")
  (outline-minor-mode t)

  (eshell/alias "g" "grin --emacs --force-color $*")
  )

(add-hook 'eshell-mode-hook 'bmz/eshell-mode-init)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Eshell
                     :key "<f5> M-h"
                     :description "anything-eshell-history")
     (cheatsheet-add :group 'Eshell
                     :key "<f5> TAB"
                     :description "anything-esh-pcomplete")
     (cheatsheet-add :group 'Eshell
                     :key "M-."
                     :description "kai-eshell-insert-last-word")
     t))


;; *** eshell prompt
(setq eshell-prompt-function (lambda nil
                               (concat
                                (propertize (eshell/pwd) 'face 'font-lock-keyword-face)
                                (propertize " $ " 'face  'font-lock-type-face))))
(setq eshell-highlight-prompt nil)


;; *** bookmark integration
;; based on code from https://www.emacswiki.org/emacs/EshellBmk
(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute with argument '-h'."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (let ((all-bookmarks (bookmark-all-names)))
        (if all-bookmarks
            (mapconcat #'(lambda (bookmark)
                           (let ((filename (bookmark-get-filename bookmark)))
                             ;;TODO: surpress checking for existence for tramp files
                             (propertize (format "%16s\t%s" bookmark filename)
                                         'face (if (file-directory-p filename)
                                                   'eshell-ls-directory
                                                 (if (file-exists-p filename)
                                                     'default
                                                   'eshell-ls-missing)))))
                       all-bookmarks
                       "\n")
          "No bookmarks defined. Use 'C-x r m' to add one.")))
     ((or (string= "-h" bookmark)
          (string= "--help" bookmark)
          (string= "/?" bookmark))
      (format "Usage:
bmk BOOKMARK
    either change directory pointed to by BOOKMARK
    or bookmark-jump to the BOOKMARK if it is not a directory.
bmk . BOOKMARK
    bookmark current directory as name BOOKMARK.
Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
       ;; Check whether an existing bookmark has been specified
       (if (setq filename (bookmark-get-filename bookmark))
           ;; If it points to a directory, change to it.
           (if (file-directory-p filename)
               (eshell/cd filename)
             ;; otherwise, just jump to the bookmark 
             (bookmark-jump bookmark))
         (error "%s is not a bookmark" bookmark))))))


;; ** comint general

(defun comint-toggle-cursor-keybinding (arg)
  "Toggle up/down key between {previous,next}-line and {previous,next}-input."
  (if (or arg (eq (key-binding (kbd "<up>")) 'previous-line))
      (progn
        (local-set-key (kbd "<up>")   'comint-previous-input)
        (local-set-key (kbd "<down>") 'comint-next-input)
        (local-set-key (kbd "M-p")    'previous-line)
        (local-set-key (kbd "M-n")    'next-line))
    (progn
      (local-set-key (kbd "<up>")   'previous-line)
      (local-set-key (kbd "<down>") 'next-line)
      (local-set-key (kbd "M-p")    'comint-previous-input)
      (local-set-key (kbd "M-n")    'comint-next-input))))


(defun comint-bind-cursor-key-for-history ()
  "Bind up/down key to comint-{previous,next}-input."
  (interactive)
  (comint-toggle-cursor-keybinding 1)
  (message "Now up/down key bound to `comint-{previous,next}-input'."))

(defun comint-bind-cursor-key-for-lines ()
  "Bind up/down key to {previous,next}-line."
  (interactive)
  (comint-toggle-cursor-keybinding nil)
  (message "Now up/down key bound to `{previous,next}-line."))


;; stolen from http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/
(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(eval-after-load "comint"
  `(progn
     (add-hook 'comint-mode-hook 'comint-bind-cursor-key-for-history)
     ;; let's bind the new command to a keycombo
     (define-key comint-mode-map "\C-c\M-o" #'comint-clear-buffer)
     ))

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Shell
                     :key "C-c C-o"
                     :description "comint-clear-buffer")
     (cheatsheet-add :group 'Comint
                     :key "C-c C-o"
                     :description "comint-clear-buffer")     
     t))

;; ** misc
;; *** compilation-shell-minor-mode (also for grep/grin in shell)
(global-set-key (kbd "C-c <C-f9>") 'compilation-shell-minor-mode)

(global-set-key (kbd "M-g <C-f9>") 'compile-goto-error)

;; any other key will jump back to the prompt
(setq eshell-scroll-to-bottom-on-input t)
