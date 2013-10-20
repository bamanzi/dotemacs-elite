
;;** menu bar
(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)

(defun anything-lacarte ()
  (interactive)
  (anything  '(anything-c-source-lacarte)))

(define-key global-map (kbd "<f5> <f10>") 'lacarte-execute-menu-command)


;;** x11 mouse
(progn
  (define-key key-translation-map (kbd "<S-mouse-4>") (kbd "<S-wheel-up>"))
  (define-key key-translation-map (kbd "<S-mouse-5>") (kbd "<S-wheel-down>"))
  (define-key key-translation-map (kbd "<M-mouse-4>") (kbd "<M-wheel-up>"))
  (define-key key-translation-map (kbd "<M-mouse-5>") (kbd "<M-wheel-down>"))
  (define-key key-translation-map (kbd "<C-mouse-4>") (kbd "<C-wheel-up>"))
  (define-key key-translation-map (kbd "<C-mouse-5>") (kbd "<C-wheel-down>"))
  (define-key key-translation-map (kbd "<C-M-mouse-4>") (kbd "<C-M-wheel-up>"))
  (define-key key-translation-map (kbd "<C-M-mouse-5>") (kbd "<C-M-wheel-down>"))
  )

;;** xterm mouse
(defun bmz/xterm-init-mouse (&optional frame)
  (if (not (display-graphic-p))
      (xterm-mouse-mode 1)))

(add-hook 'after-make-frame-functions 'bmz/xterm-init-mouse)


;;** xterm keys
(defun xterm-map-function-keys-csi ()
  "Map xterm control sequences for F1..F4 keys.

Only for CSI sequences, which are used by xterm-r6/gnome-terminal/mintty 
but not mapped by term/xterm.el"
  (interactive)
  (define-key input-decode-map "\e[1;2P" [S-f1])
  (define-key input-decode-map "\e[1;2Q" [S-f2])
  (define-key input-decode-map "\e[1;2R" [S-f3])
  (define-key input-decode-map "\e[1;2S" [S-f4])

  (define-key input-decode-map "\e[1;3P" [M-f1])
  (define-key input-decode-map "\e[1;3Q" [M-f2])
  (define-key input-decode-map "\e[1;3R" [M-f3])
  (define-key input-decode-map "\e[1;3S" [M-f4])

  (define-key input-decode-map "\e[1;4P" [S-M-f1])
  (define-key input-decode-map "\e[1;4Q" [S-M-f2])
  (define-key input-decode-map "\e[1;4R" [S-M-f3])
  (define-key input-decode-map "\e[1;4S" [S-M-f4])

  (define-key input-decode-map "\e[1;5P" [C-f1])
  (define-key input-decode-map "\e[1;5Q" [C-f2])
  (define-key input-decode-map "\e[1;5R" [C-f3])
  (define-key input-decode-map "\e[1;5S" [C-f4])

  (define-key input-decode-map "\e[1;6P" [C-S-f1])
  (define-key input-decode-map "\e[1;6Q" [C-S-f2])
  (define-key input-decode-map "\e[1;6R" [C-S-f3])
  (define-key input-decode-map "\e[1;6S" [C-S-f4])  
  
  (define-key input-decode-map "\e[1;7P" [C-M-f1])
  (define-key input-decode-map "\e[1;7Q" [C-M-f2])
  (define-key input-decode-map "\e[1;7R" [C-M-f3])
  (define-key input-decode-map "\e[1;7S" [C-M-f4])  
  )


(progn
  (define-key key-translation-map (kbd "<select>") (kbd "<end>"))

  (define-key key-translation-map  (kbd "ESC <up>")    (kbd "<M-up>"))
  (define-key key-translation-map  (kbd "ESC <down>")  (kbd "<M-down>"))
  (define-key key-translation-map  (kbd "ESC <left>")  (kbd "<M-left>"))
  (define-key key-translation-map  (kbd "ESC <right>") (kbd "<M-right>"))
  (define-key key-translation-map  (kbd "ESC <insertchar>")  (kbd "<M-insert>"))
  (define-key key-translation-map  (kbd "ESC <deletechar>")  (kbd "<M-delete>"))
  
  (xterm-map-function-keys-csi)
  )

(defun bmz/xterm-init-keys (&optional frame)
  (interactive)
  (when (not (display-graphic-p))
      (ignore-errors
        (load-library "term/xterm")
        (terminal-init-xterm))))
      
(add-hook 'after-make-frame-functions 'bmz/xterm-init-keys)


;;** sudo
(autoload 'sudo-find-file "sudo"
  "Open a file, which may or may not be readable. If we can't" t)

(autoload sudo-unset-ro-or-save "sudo"
  "Unset read-only flag for buffer, otherwise" t)

(global-set-key (kbd "C-x M-f") 'sudo-find-file)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)


   
