;;** eim
;;https://github.com/wenbinye/emacs-eim

(add-to-list 'load-path "~/.emacs.d/packages/eim")

(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

(eval-after-load "eim"
  `(progn
     ;; 用 ; 暂时输入英文
     (require 'eim-extra)
     (global-set-key ";" 'eim-insert-ascii)
     ))


;;** realgud
;;https://github.com/rocky/emacs-dbgr

(add-to-list 'load-path "~/.emacs.d/packages/realgud")

(idle-require 'realgud)

(autoload 'realgud-gdb    "realgud" nil t)

(autoload 'pdb            "realgud" nil t)
(autoload 'realgud-pdb    "realgud" nil t)
(autoload 'perldb         "realgud" nil t)
(autoload 'realgud-perldb "realgud" nil t)
(autoload 'rdebug         "realgud" nil t)
(autoload 'realgud-rdebug "realgud" nil t)

(autoload 'realgud-track-mode "realgud" nil t)


;;** w3m

;;a recommended to download latest snapshot:
;;http://packages.debian.org/sid/w3m-el-snapshot

(add-to-list 'load-path "~/.emacs.d/packages/w3m")

(idle-require 'w3m-load)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;(setq browse-url-browser-function 'w3m-browse-url)


;; default to new tab
(defun w3m-new-tab ()
  (require 'w3m)
  (interactive)
  (w3m-copy-buffer nil nil nil t))

 (defun w3m-browse-url-new-tab (url &optional new-session)
   (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
   (w3m-new-tab)
   (w3m-browse-url url new-session))
;;(setq browse-url-browser-function 'w3m-browse-url-new-tab)

 (eval-after-load "w3m"
   `(progn
      (setq w3m-use-title-buffer-name nil)
      (setq browse-url-browser-function 'w3m-browse-url)
	 
      (define-key w3m-mode-map (kbd "<down>") 'next-line)
      (define-key w3m-mode-map (kbd "<up>") 'previous-line)
      (define-key w3m-mode-map (kbd "<left>") 'backward-char)
      (define-key w3m-mode-map (kbd "<right>") 'forward-char)

      (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
      (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
      (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
      (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-next-page)       
    ))



