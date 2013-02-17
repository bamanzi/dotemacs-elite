

(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)

(defun xterm-map-function-keys-csi ()
  "Map xterm control sequences for F1..F4 keys.

Only for CSI sequences (\e[1;;..)"
  (interactive)
  (define-key input-decode-map "\e[1;2A" [S-f1])
  (define-key input-decode-map "\e[1;2B" [S-f2])
  (define-key input-decode-map "\e[1;2C" [S-f3])
  (define-key input-decode-map "\e[1;2D" [S-f4])

  (define-key input-decode-map "\e[1;3P" [M-f1])
  (define-key input-decode-map "\e[1;3Q" [M-f2])
  (define-key input-decode-map "\e[1;3R" [M-f3])
  (define-key input-decode-map "\e[1;3S" [M-f4])

  (define-key input-decode-map "\e[1;4P" [S-M-f1])
  (define-key input-decode-map "\e[1;4Q" [S-M-f2])
  (define-key input-decode-map "\e[1;4R" [S-M-f3])
  (define-key input-decode-map "\e[1;4S" [S-M-f4])

  (define-key input-decode-map "\e[1;5R" [C-f1])
  (define-key input-decode-map "\e[1;5Q" [C-f2])
  (define-key input-decode-map "\e[1;5R" [C-f3])
  (define-key input-decode-map "\e[1;5Q" [C-f4])

  (define-key input-decode-map "\e[1;6R" [C-S-f1])
  (define-key input-decode-map "\e[1;6Q" [C-S-f2])
  (define-key input-decode-map "\e[1;6R" [C-S-f3])
  (define-key input-decode-map "\e[1;6Q" [C-S-f4])  
  
  (define-key input-decode-map "\e[1;7R" [C-M-f1])
  (define-key input-decode-map "\e[1;7Q" [C-M-f2])
  (define-key input-decode-map "\e[1;7R" [C-M-f3])
  (define-key input-decode-map "\e[1;7Q" [C-M-f4])  
  )

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (define-key key-translation-map (kbd "<select>") (kbd "<end>"))

  (define-key key-translation-map  (kbd "ESC <up>")    (kbd "<M-up>"))
  (define-key key-translation-map  (kbd "ESC <down>")  (kbd "<M-down>"))
  (define-key key-translation-map  (kbd "ESC <left>")  (kbd "<M-left>"))
  (define-key key-translation-map  (kbd "ESC <right>") (kbd "<M-right>"))
  (define-key key-translation-map  (kbd "ESC <insertchar>")  (kbd "<M-insert>"))
  (define-key key-translation-map  (kbd "ESC <deletechar>")  (kbd "<M-delete>"))
  
  (define-key key-translation-map  (kbd "<mouse-8>")   (kbd "<S-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-9>")   (kbd "<S-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-12>")  (kbd "<M-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-13>")  (kbd "<M-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-20>")  (kbd "<C-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-21>")  (kbd "<C-wheel-down>"))

  (when (require 'xterm-extras nil t)
      (xterm-map-function-keys-csi))
  )

   
