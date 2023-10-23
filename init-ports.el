
;; ** x11 mouse
(progn
  (define-key key-translation-map (kbd "<mouse-4>") (kbd "<wheel-up>"))
  (define-key key-translation-map (kbd "<mouse-5>") (kbd "<wheel-down>"))
  
  (define-key key-translation-map (kbd "<S-mouse-4>") (kbd "<S-wheel-up>"))
  (define-key key-translation-map (kbd "<S-mouse-5>") (kbd "<S-wheel-down>"))
  (define-key key-translation-map (kbd "<M-mouse-4>") (kbd "<M-wheel-up>"))
  (define-key key-translation-map (kbd "<M-mouse-5>") (kbd "<M-wheel-down>"))
  (define-key key-translation-map (kbd "<C-mouse-4>") (kbd "<C-wheel-up>"))
  (define-key key-translation-map (kbd "<C-mouse-5>") (kbd "<C-wheel-down>"))
  (define-key key-translation-map (kbd "<C-M-mouse-4>") (kbd "<C-M-wheel-up>"))
  (define-key key-translation-map (kbd "<C-M-mouse-5>") (kbd "<C-M-wheel-down>"))
  )

;; ** xterm mouse
(defun bmz/xterm-init-mouse (&optional frame)
  (when (not (display-graphic-p))
    (xterm-mouse-mode 1)
    (when (load "mwheel" t)
      (mwheel-install)
      (global-set-key (kdb "<wheel-up>") 'mwheel-scroll)
      (global-set-key (kdb "<wheel-down>") 'mwheel-scroll))
    ))

(add-hook 'after-make-frame-functions 'bmz/xterm-init-mouse)


;; ** xterm keys
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
  (xterm-map-function-keys-csi)
  
  (define-key key-translation-map (kbd "<select>") (kbd "<end>"))

  (define-key key-translation-map  (kbd "ESC <up>")    (kbd "<M-up>"))
  (define-key key-translation-map  (kbd "ESC <down>")  (kbd "<M-down>"))
  (define-key key-translation-map  (kbd "ESC <left>")  (kbd "<M-left>"))
  (define-key key-translation-map  (kbd "ESC <right>") (kbd "<M-right>"))
  (define-key key-translation-map  (kbd "ESC <insertchar>")  (kbd "<M-insert>"))
  (define-key key-translation-map  (kbd "ESC <deletechar>")  (kbd "<M-delete>"))

  (define-key key-translation-map  (kbd "ESC <f1>")   (kbd "<M-f1>"))
  (define-key key-translation-map  (kbd "ESC <f2>")   (kbd "<M-f2>"))
  (define-key key-translation-map  (kbd "ESC <f3>")   (kbd "<M-f3>"))
  (define-key key-translation-map  (kbd "ESC <f4>")   (kbd "<M-f4>"))
  (define-key key-translation-map  (kbd "ESC <f5>")   (kbd "<M-f5>"))
  (define-key key-translation-map  (kbd "ESC <f6>")   (kbd "<M-f6>"))
  (define-key key-translation-map  (kbd "ESC <f7>")   (kbd "<M-f7>"))
  (define-key key-translation-map  (kbd "ESC <f8>")   (kbd "<M-f8>"))
  (define-key key-translation-map  (kbd "ESC <f9>")   (kbd "<M-f9>"))
  (define-key key-translation-map  (kbd "ESC <f10>")  (kbd "<M-f10>"))
  (define-key key-translation-map  (kbd "ESC <f11>")  (kbd "<M-f11>"))
  (define-key key-translation-map  (kbd "ESC <f12>")  (kbd "<M-f12>"))

  ;; some PuTTY sequences:
;;  (define-key input-decode-map "\e[23~" [S-f1]) -> F11
;;  (define-key input-decode-map "\e[24~" [S-f2]) -> F12
  (define-key input-decode-map "\e[25~" [S-f3])
  (define-key input-decode-map "\e[26~" [S-f4])
  
  (define-key input-decode-map "\e[28~" [S-f5])
  (define-key input-decode-map "\e[29~" [S-f6])
  (define-key input-decode-map "\e[31~" [S-f7])
  (define-key input-decode-map "\e[32~" [S-f8])
  (define-key input-decode-map "\e[33~" [S-f9])
  (define-key input-decode-map "\e[34~" [S-f10])
  
 
  )

(defun bmz/xterm-init-keys (&optional frame)
  (interactive)
  (when (not (display-graphic-p))
      (ignore-errors
        (load-library "term/xterm")
        (terminal-init-xterm))))
      
(add-hook 'after-make-frame-functions 'bmz/xterm-init-keys)


;; ** sudo
(defun revert-buffer-with-sudo ()
  (interactive)
  (let ((pt        (point)))
    (if (or (not buffer-file-name)
            (string-match "^/sudo:" buffer-file-name))
        (call-interactively 'find-alternate-file)
      (find-alternate-file (concat "/sudo::" buffer-file-name))
      (goto-char pt))))

(global-set-key (kbd "C-c C-x C-v") 'revert-buffer-with-sudo)

;; *** sudo.el
(autoload 'sudo-find-file "sudo"
  "use sudo to open an otherwise unreadable file." t)
(autoload 'sudo-save-current-buffer "sudo"
  "Save current buffer, running sudo if necessary." t)

(autoload 'sudo-unset-ro-or-save "sudo"
  "unset read-only (if necessary), else sudo-save.")

(global-set-key (kbd "C-x M-f") 'sudo-find-file)
(global-set-key (kbd "C-x M-s") 'sudo-unset-ro-or-save)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Open/Save
                     :key "M-x revert-buffer-with-sudo"
                     :description "revert-buffer-with-sudo")

     (cheatsheet-add :group 'Open/Save
                     :key "M-x sudo-find-file"
                     :description "use sudo to open an otherwise unreadable file.")
     (cheatsheet-add :group 'Open/Save
                     :key "M-x sudo-save-current-buffer"
                     :description "save current buffer, running sudo if necessary.")

     (cheatsheet-add :group 'Open/Save
                     :key "M-x sudo-unset-ro-or-save"
                     :description "Unset read-only flag for buffer if necessary, otherwise sudo-save")
     t))


;; ** key modifiers
;; *** win32
;; prevent a single keypress on LWIN poping up the Start Menu
(setq w32-pass-lwindow-to-system nil)

;; this would let us bind win+o, win+i, win+y etc to emacs commands
;; but not system registered hot keys (such as win+1..9, win+r, win+l etc)
(setq w32-lwindow-modifier 'super)
;; (if you want to disable most system win+? hotkeys, follow this guide
;;  http://www.askvg.com/tip-how-to-disable-all-win-keyboard-shortcuts-hotkeys-in-windows/
;;  it would disable win+1..9, win+r, win+m etc, but not win+l, win+u, win+arrow)

(setq w32-apps-modifier 'super) ;; the same effect with linux (alt_super_win)

;; *** linux
;; **** <lwindow> & <rwindow>: by default they're bound to `super'.

;; (for Cygwin/X & Xming, option `-keyhook' should be used when starting X server.
;;  http://thread.gmane.org/gmane.os.cygwin.xfree/22869/focus=22869 )

;; **** <menu> key

;; a) the non-sticky way (e.g. to get `s-o', you can press `<menu>'
;;    and `o' at the same time):
(defun x-map-menu-key-to-super ()
  "Call `setxkbmap -option altwin:alt_super_win' to map <menu> key `super' modifier.

NOTE: this would also change LWIN to Super_L, and RWIN to
ALT_R/META_R. Refer [[file:/usr/share/X11/xkb/symbols/altwin]] for detail info."
  (interactive)
  (let ((result (shell-command-to-string "setxkbmap -option altwin:alt_super_win")))
    (if (> (length result) 0)
        (display-warning :warning (concat "Failed in (x-map-menu-key-to-super):\n" result)))))

(when (eq window-system 'x)
  (x-map-menu-key-to-super))

;; b) the sticky way (e.g. to get `s-o', you need to press `<menu>'
;;    and release it, then press `o')
; (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier)

;; ** win32 programs
(when (memq system-type '(windows-nt ms-dos))
  ;;http://sourceforge.net/projects/ezwinports/files/

  (setq find-program "gfind")

  (when (executable-find "ggrep")
      (setq grep-program "ggrep")
      (setq ispell-grep-program "ggrep"))

  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  ;; (setq ispell-extra-args '("-d" "en_US"))

  (eval-after-load "ispell"
    `(progn
       ;; `flyspell-mode' needs this
       (add-to-list 'ispell-local-dictionary-alist '("en_US"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "en_US")
                                              nil
                                              iso-8859-1))
       ;; `ispell-complete-word' needs this
       (if (executable-find "hunspell")
           (setq ispell-alternate-dictionary
                 (concat (file-name-directory (executable-find "hunspell"))
                         "..\\share\\hunspell\\en_US.dic")))
    ))
  
  )
   
