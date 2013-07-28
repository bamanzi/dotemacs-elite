;;** files

(defun ido-find-file-at-point ()
  (interactive)
  (let ((ido-use-filename-at-point t)
        (ido-use-url-at-point t))
     (call-interactively 'ido-find-file)))

(define-key search-map (kbd "C-f") 'ido-find-file-at-point)

(defun revert-buffer-with-sudo ()
  (interactive)
  (let ((pt        (point)))
    (if (or (not buffer-file-name)
            (string-match "^/sudo:" buffer-file-name))
        (call-interactively 'find-alternate-file)
      (find-alternate-file (concat "/sudo::" buffer-file-name))
      (goto-char pt))))


;;*** dired-single
(autoload 'joc-dired-single-buffer "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(autoload 'joc-dired-single-buffer-mouse "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map [return] 'joc-dired-single-buffer)
     (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
     
     (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
     (define-key dired-mode-map "^"
       (function
        (lambda nil (interactive) (joc-dired-single-buffer ".."))))
     )) 


;;** mark, copy & yank
(idle-require 'mark-copy-something)

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point." t)

(global-set-key (kbd "<M-insert>") 'copy-from-above-command)


;;*** back-button: Visual navigation through mark rings
;;https://github.com/rolandwalker/back-button
(idle-require 'back-button)
(eval-after-load "back-button"
  `(progn
     (back-button-mode 1)
     (define-key goto-map (kbd "<left>")    'back-button-local-backward)
     (define-key goto-map (kbd "<right>")   'back-button-local-backward)
     (define-key goto-map (kbd "<M-left>")  'back-button-global-backward)
     (define-key goto-map (kbd "<M-right>") 'back-button-global-backward)
     ))


;;*** copy/cut current line if nothing selected
;; http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;;*** copy/paste rectangle
;;stolen from http://ergoemacs.org/emacs/emacs_string-rectangle_ascii-art.html
(unless (functionp 'copy-rectangle-as-kill)  ;; emacs-24 has this
  (defun copy-rectangle-to-clipboard (p1 p2)
    "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
    (interactive "r")
    (let ((x-select-enable-clipboard t))
      (copy-rectangle-to-register ?0 p1 p2)
      (kill-new
       (with-temp-buffer
         (insert-register ?0)
         (buffer-string) )) ) )

  (define-key global-map (kbd "C-x r M-w") 'copy-rectangle-to-clipboard))


;;** minibuffer
;;*** easily insert buffer name (useful for `shell-command', `compile' etc)
(defun minibuffer-insert-buffer-filename (arg)
  (interactive "P")
  (let ((target-buffer (window-buffer (minibuffer-selected-window))))
    (if (and target-buffer
             (buffer-file-name target-buffer))
      (insert-string (if arg
                         (buffer-file-name target-buffer)
                       (file-name-nondirectory (buffer-file-name target-buffer))))
      (insert-string " "))))

(define-key minibuffer-local-map (kbd "<M-insert> %") 'minibuffer-insert-buffer-filename)

;;*** insert current symbol to minibuffer
(defun minibuffer-insert-current-symbol (arg)
  (interactive "P")
  (let ((target-buffer (window-buffer (minibuffer-selected-window))))
    (if (and target-buffer
             (buffer-file-name target-buffer))
        (insert-string (with-current-buffer target-buffer
                         (if arg
                             (thing-at-point 'string) ;; thingatpt+.el needed
                           (thing-at-point 'symbol))))
      (insert-string " "))))

(define-key minibuffer-local-map (kbd "<M-insert> M-s") 'minibuffer-insert-current-symbol)


;;** multi-occur for buffers of same mods
;; stolen from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun _get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun multi-occur-in-this-mode (symbol &optional arg)
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive
   (list (read-string "Occur in same mode: " (thing-at-point 'symbol))
         current-prefix-arg))
  (multi-occur (_get-buffers-matching-mode major-mode)
               (format "%s" symbol)
               arg))

(define-key search-map (kbd "M-o") 'multi-occur-in-this-mode)


;;** grin & ack: better grep replacement for source code project
(defun grep-on-dir (dir)
  (interactive "DGrep on dir: ")
  (require 'grep)
  (let ((default-directory dir))
    (call-interactively 'grep)))

(define-key search-map "g " 'grep-on-dir)
(define-key search-map " g" 'grep-on-dir)


(autoload 'grin "grin"
  "Use `grin' command for grepping text across files." t)
(autoload 'grind "grin"
  "Use `grind' command for find files." t)

(defun grin-on-dir (dir &optional pattern)
  (interactive "DGrin on dir: ")
  (require 'grin)
  (let* ((default-directory dir)
         (c (concat grin-cmd " \"" (or pattern (thing-at-point 'symbol)) "\""))
         (l (+ 3 (length grin-cmd)))
         (cmd (read-shell-command "Command: " (cons c l) 'grin-hist))
         (null-device nil))
    (grep cmd)))

(define-key search-map "gG" 'grin-on-dir)


(autoload 'ack "ack"
  "ack.el provides a simple compilation mode for the perl grep-a-like ack program." t)

(defun ack-on-dir (dir &optional pattern)
  (interactive "DAck on dir: ")
  (require 'ack)
  (let* ((default-directory dir)
         (ack-command (concat ack-command " \"" (or pattern (thing-at-point 'symbol)) "\"")))
    (call-interactively 'ack)))
   
(define-key search-map "ga" 'ack-on-dir)



;;** tabbar
(defun tabbar-buffer-groups/bmz ()
  "Return the list of group names the current buffer belongs to.
 Return a list of one element based on major mode."
  (list
   (cond
    ((or (memq major-mode '(dired-mode ibuffer-mode grep-mode occur-mode
                                       shell-mode eshell-mode lisp-interaction-mode))
         (get-buffer-process (current-buffer)))
     "Utils")
    ((= (aref (buffer-name) 0) ?*)
     "*temp*")
    (t
     "User"
     ))))


;;add extra leading & ending space to tab label
(defun tabbar-buffer-tab-label/xterm (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups/bmz)
     
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
     (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)

     (define-key tabbar-mode-map (kbd "<f12> <right>") 'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<f12> <left>")  'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<f12> <up>")    'tabbar-press-home)

     (if (display-graphic-p)
         (require 'tabbar-ruler nil t)
       (setq tabbar-tab-label-function 'tabbar-buffer-tab-label/xterm))
     ))

(eval-after-load "tabbar-ruler"
  `(progn
     ;;reset my grouping funciton
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups/bmz)
     ))

(idle-require 'tabbar)


;;** color-theme
(idle-require 'color-theme)

(if (string< emacs-version "24")
    (progn
      (require 'color-theme nil t)
      (eval-after-load "color-theme"
        `(progn
           (require 'color-theme-sanityinc-tomorrow)

           (if (require 'color-theme-tangotango)
               (color-theme-tangotango))         
           )))
  (progn
    (let ((theme-dir (locate-library "tangotango-theme")))
      (if theme-dir
          (add-to-list 'custom-theme-load-path (file-name-directory theme-dir))))
    (unless custom-enabled-themes
      (custom-set-variables
       '(custom-enabled-themes (quote (tango-dark)))))
    )
  )


;;** org-mdoe
(setq org-CUA-compatible t)

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil ;; no ToC in export
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css
      )

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)

(eval-after-load "org"
  `(progn
     (require 'org-cua-dwim nil t)

     (set-face-attribute 'org-level-1 nil :height 1.5 :bold t)
     (set-face-attribute 'org-level-2 nil :height 1.3 :bold t)
     (set-face-attribute 'org-level-3 nil :height 1.1)))

(idle-require 'org-cua-dwim)

;;** markdown
(autoload 'markdown-mode  "markdown-mode"
  "Major mode for editing Markdown files." t)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files." t)

(add-to-list 'auto-mode-alist '("\\.mk?d" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;;*** faces
(eval-after-load "markdown-mode"
  `(progn
     (set-face-attribute 'markdown-header-face-1 nil :inherit 'org-level-1)
     (set-face-attribute 'markdown-header-face-2 nil :inherit 'org-level-2)
     (set-face-attribute 'markdown-header-face-3 nil :inherit 'org-level-3)
     (set-face-attribute 'markdown-header-face-4 nil :inherit 'org-level-4)
     (set-face-attribute 'markdown-header-face-5 nil :inherit 'org-level-5)
     (set-face-attribute 'markdown-header-face-6 nil :inherit 'org-level-6)

     (add-hook 'markdown-mode-hook 'buffer-face-mode) 
     ))

;;*** imenu
;;stolen from http://tychoish.com/rhizome/imenu-for-markdown-and-writing/
(setq markdown-imenu-generic-expression
     '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
 ))

 (add-hook 'markdown-mode-hook
           (lambda ()
             (setq imenu-generic-expression markdown-imenu-generic-expression)))




;;** languages tools
;;*** spell
(define-key global-map (kbd "ESC M-$") 'ispell-complete-word)

(defun ac-ispell-get-candidates ()
  (let ((word (car (ispell-get-word nil "\\*")))
        (interior-frag nil))
    (lookup-words (concat (and interior-frag "*") word
                    (if (or interior-frag (null ispell-look-p))
                    "*"))
                  ispell-complete-word-dict)))

(eval-after-load "auto-complete"
  `(progn
     (ac-define-source ispell-word
       '((symbol . "i")
         (candidates . ac-ispell-get-candidates)))
     ))

(define-key global-map (kbd "C-. $") 'ac-complete-ispell-word)

;;*** sdcv
(autoload 'sdcv-search-input "sdcv"
  "Search WORD through the `command-line' tool sdcv." t)
(define-key search-map "D"  'sdcv-search-input)


;;(setq sdcv-dictionary-simple-list '("XDICT英汉辞典" "XDICT汉英辞典"))
(autoload 'sdcv-search-pointer+ "sdcv"
  "Translate current point word with command-line tool `sdcv'." t)
(define-key search-map "d"  'sdcv-search-pointer+)

(defun sdcv-search-word-at-pt-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (require 'sdcv)
  (call-interactively 'sdcv-search-pointer+))

(global-set-key (kbd "<C-down-mouse-1>") 'sdcv-search-word-at-pt-mouse)


;;*** dict protocol
(setq dictem-server "localhost")
(autoload 'dictem-run-search  "dictem" nil t)
(autoload 'dictem-run-match   "dictem" nil t)
(autoload 'dictem-run-define  "dictem" nil t)

(progn
  (define-key search-map (kbd "M-d s") 'dictem-run-search)
  (define-key search-map (kbd "M-d m") 'dictem-run-match)
  (define-key search-map (kbd "M-d d") 'dictem-run-define)
  )

(eval-after-load "dictem"
  `(progn
     (dictem-initialize)
     ))

;;*** google-translate
(autoload 'google-translate-at-point "google-translate"
  "Translate the word at point or the words in the active region." t)
(autoload 'google-translate-query-translate "google-translate"
  "Interactively translate text with Google Translate." t)

(defalias 'gtap 'google-translate-at-point)
(defalias 'gtqt 'google-translate-query-translate)

(setq google-translate-enable-ido-completion t
      google-translate-default-source-language "en"
      google-translate-default-target-language "zh-CN")

(define-key search-map "G" 'google-translate-at-point)
(define-key search-map " G" 'google-translate-query-translate)
    

;;** highlight changes
(autoload 'diff-hl-mode  "diff-hl"
  "Toggle VC diff fringe highlighting." t)
(autoload 'global-diff-hl-mode "diff-hl"
  "Toggle Diff-Hl mode in every possible buffer." t)

;;(add-hook 'prog-mode-hook 'diff-hl-mode)


(global-highlight-changes-mode 1)
(global-set-key (kbd "<f10> h c") 'highlight-changes-visible-mode)

;;*** git-gutter
;; https://github.com/syohex/emacs-git-gutter
(autoload 'git-gutter:toggle "git-gutter"
  "toggle to show diff information" t)

(defun _frame-reinit-git-gutter (&optional frame)
  "Choose `git-gutter' implementation from `git-gutter.el' or `git-gutter-fringe.el'."
  (interactive)
  (if (require 'git-gutter-fringe nil t)
      (if (display-graphic-p)
          (setq git-gutter:init-function 'git-gutter-fr:init
                git-gutter:view-diff-function 'git-gutter-fr:view-diff-infos
                git-gutter:clear-function 'git-gutter-fr:clear)
        (setq git-gutter:init-function nil
                git-gutter:view-diff-function 'git-gutter:view-diff-infos
                git-gutter:clear-function 'git-gutter:clear-diff-infos))))

;;(add-hook 'after-make-frame-functions '_frame-reinit-git-gutter)
 

;;** yasnippet
(autoload 'anything-yasnippet-2  "anything-yasnippet-2"
  "Find yasnippets." t)

(global-set-key (kbd "<f5> s")  'anything-yasnippet-2)


;;** windows
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(global-set-key (kbd "<f11> |") 'window-toggle-split-direction)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;** misc

;;info+.el: more colors (and other enhancements) 
(eval-after-load "info"
  `(require 'info+)
  )

