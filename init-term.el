

(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)


(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (define-key key-translation-map (kbd "<select>") (kbd "<end>"))

  (define-key key-translation-map  (kbd "ESC <up>")    (kbd "<M-up>"))
  (define-key key-translation-map  (kbd "ESC <down>")  (kbd "<M-down>"))
  (define-key key-translation-map  (kbd "ESC <left>")  (kbd "<M-left>"))
  (define-key key-translation-map  (kbd "ESC <right>") (kbd "<M-right>"))

  (define-key key-translation-map  (kbd "<mouse-8>")   (kbd "<S-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-9>")   (kbd "<S-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-12>")  (kbd "<M-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-13>")  (kbd "<M-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-20>")  (kbd "<C-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-21>")  (kbd "<C-wheel-down>"))   
  )

   
