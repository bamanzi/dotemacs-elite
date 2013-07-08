;;** shell toggle

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

(global-set-key (kbd "<f12> e") 'eshell-toggle-cd)
(global-set-key (kbd "<f12> E") 'eshell-toggle)
(global-set-key (kbd "<f12> s") 'shell-toggle-cd)
(global-set-key (kbd "<f12> S") 'shell-toggle)


;;*** some eshell command
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


;;*** pcomplete for shell commands & args
;;stolen from http://linuxtoy.org/archives/emacs-eshell.html
(eval-after-load "auto-complete"
  `(progn
     (ac-define-source pcomplete
       '((candidates . pcomplete-completions)))

     (add-to-list 'ac-modes 'eshell-mode)
     (add-to-list 'ac-modes 'shell-mode)
     ))

(eval-after-load "eshell"
  `(progn
     (require 'pcmpl-git nil t)
     (if (require 'pcase nil t)
         (require 'pcmpl-args nil t))
     ))

(define-key global-map (kbd "C-. p") 'ac-complete-pcomplete)

;;** compilation-shell-minor-mode (also for grep/grin in shell)
(global-set-key (kbd "C-c <f9>") 'compilation-shell-minor-mode)

(global-set-key (kbd "M-g <f9>") 'compile-goto-error)


;;** cursor keysc

(defun comint-toggle-cursor-keybinding (arg)
  "Toggle up/down key between {previous,next}-line and {previous,next}-input."
  (interactive "P")
  (if (or arg (eq (key-binding (kbd "<up>") 'previous-line)))
      (progn
        (define-key comint-mode-map (kbd "<up>")   'comint-previous-input)
        (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
        (message "up/down key now binding to `eshell-{previous,next}-input'."))
    (progn
        (define-key comint-mode-map (kbd "<up>")   'previous-line)
        (define-key comint-mode-map (kbd "<down>") 'next-line))))

(defun eshell-toggle-cursor-keybinding (arg)
  "Toggle up/down key between {previous,next}-line and {previous,next}-input."
  (interactive "P")
  (if (or arg (eq (key-binding (kbd "<up>") 'previous-line)))
      (progn
        (define-key eshell-mode-map (kbd "<up>")   'eshell-previous-input)
        (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)
        (message "up/down key now binding to `eshell-{previous,next}-input'."))
    (progn
        (define-key eshell-mode-map (kbd "<up>")   'previous-line)
        (define-key eshell-mode-map (kbd "<down>") 'next-line))))
          

;;** autojump
(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))
;;use command `j' to list your MRU path,
;;use command `j regexp' to jump to one


;;** misc
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

;;stolen from http://comments.gmane.org/gmane.emacs.help/7319
(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))


(defun bmz/eshell-mode-init ()
  ;; swap <home> and C-a
  (define-key eshell-mode-map (kbd "C-a")    'eshell-maybe-bol)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)  


  (eshell-toggle-cursor-keybinding 1)
  
  ;;I'd like M-s as highlight-xxx prefix key
  (define-key eshell-mode-map (kbd "M-s") nil)

  (if (fboundp 'drag-stuff-mode)
      (drag-stuff-mode -1))

  (define-key eshell-mode-map (kbd "<M-up>")   'eshell-previous-matching-input)
  (define-key eshell-mode-map (kbd "<M-down>") 'eshell-next-matching-input)
  
  (define-key eshell-mode-map (kbd "M-.") 'kai-eshell-insert-last-word)  
  
  (setq outline-regexp "^.* $")
  (outline-minor-mode t)
  )

(add-hook 'eshell-mode-hook 'bmz/eshell-mode-init)

;;*** eshell prompt
(setq eshell-prompt-function (lambda nil
                               (concat
                                (propertize (eshell/pwd) 'face 'font-lock-keyword-face)
                                (propertize " $ " 'face  'font-lock-type-face))))
(setq eshell-highlight-prompt nil)
