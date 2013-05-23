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
(defun eshell/vim (&rest args)
  "Invoke find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\\+\([0-9]+\)\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defalias 'eshell/vi 'eshell/vim)

(defun eshell/start (file)
    "Invoke system's associated application for FILE.
On Windows, baskslashes is substituted with slashes."
    (if (eq system-type 'gnu/linux)
        (shell-command (concat "gnome-open "
                               (shell-quote-argument (file))))
      (w32-shell-execute "Open"
                       (subst-char-in-string ?\\ ?/ (expand-file-name file))
		       nil)))

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
     (require 'pcmpl-args nil t)
     ))

(define-key global-map (kbd "C-. p") 'ac-complete-pcomplete)


;;** misc
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
  
  (define-key eshell-mode-map (kbd "<up>")   'eshell-previous-input)
  (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)
  
  (if (fboundp 'drag-stuff-mode)
      (drag-stuff-mode -1))
  (define-key eshell-mode-map (kbd "<M-up>")   'previous-line)
  (define-key eshell-mode-map (kbd "<M-down>") 'next-line)    

  (setq outline-regexp "^.* $")
  (outline-minor-mode t)
  )

(add-hook 'eshell-mode-hook 'bmz/eshell-mode-init)
