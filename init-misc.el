;; ** files

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


;; *** dired-single
(autoload 'dired-single-buffer "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(autoload 'dired-single-buffer-mouse "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map [return] 'dired-single-buffer)
     (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
     
     (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
     (define-key dired-mode-map "^"
       (function
        (lambda nil (interactive) (dired-single-buffer ".."))))
     )) 

;; *** nc.el: norton commander clone
(autoload 'nc "nc" "Major mode for File Browser in GNU emacs." t)

(eval-after-load "nc"
  `(progn
     (defadvice nc (around nc-window-configuration activate)
       ;;save window configuration before nc starts
       (frame-configuration-to-register ? )
       ad-do-it
       (let ( (nc-win1 (get-buffer-window "*NC 1*"))
              (nc-win2 (get-buffer-window "*NC 2*"))
              (nc-win3 (get-buffer-window "*NC shell*")) )
         (set-window-dedicated-p nc-win1 t)
         (set-window-dedicated-p nc-win2 t)
         (set-window-dedicated-p nc-win3 t)
         (unless (get-register ?C)
           (frame-configuration-to-register ?C))))
     ))

(defun nc-goto-dir (dir)
  (interactive "Dnc to: ")
  (nc)
  (with-current-buffer nc-active-nc-buffer
    (nc-display-new-dir dir)))


;; ** mark, copy & yank
(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point." t)

(global-set-key (kbd "<M-insert>") 'copy-from-above-command)

;; *** make/copy something
;;(idle-require 'mark-copy-something)

;; based on code stolen from https://github.com/m2ym/thingopt-el/blob/master/thingopt.el
(setq thing/name-map
  '((?w . word)
    (?e . sexp)
    (?s . symbol)
    (?S . sentence)
    (?p . paragraph)
    (?h . defun)
    (?F . filename)
    (?l . line)
    (?L . list)
    (?\" . string)
    (?u . url)
    (?P . page)))

(defun thing/read-thing (quick &optional prompt)
  "Ask user to select a THING name.

When QUICK is true, it read in one char matching key of `thing/name-map'.
Otherwise it requires user to input full thing name (value of `thing/name-map`)."
  (if quick
      (assoc-default (read-key (concat (or prompt "Thing")
                                       " ["
                                       (mapconcat #'(lambda (elem)
                                                      (format "%c:%s" (car elem) (cdr elem)))
                                                  thing/name-map " ")
                                       "]: "))
                     thing/name-map)
    (let ((thing-name (ido-completing-read (or prompt "Thing: ")
                                      (mapcar #'(lambda (elem)
                                                  (format "%s" (cdr elem)))
                                              thing/name-map)
                                      nil
                                      'match)))
      (if thing-name
          (intern thing-name)))))
                  

(defun thing/call-action (thing action &optional prompt)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (cond
     (bounds
      (funcall action bounds))
     (thing
      (message "There is no %s here." thing))
     (t
      (message "Nothing here.")))))

(defun thing/mark-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (car bounds))
                         (push-mark (cdr bounds) nil transient-mark-mode)
                         (message "Markd %s." thing))))

(global-set-key (kbd "C-`") 'thing/mark-one-thing)
(global-set-key (kbd "C-c `") 'thing/mark-one-thing) ;;for xterm

(defun thing/goto-beginning (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (car bounds)))
                     "Go to thing begin "))

(defun thing/goto-end (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (cdr bounds)))
                     "Go to thing end "))

(global-set-key (kbd "M-g <") 'thing/goto-beginning)
(global-set-key (kbd "M-g >") 'thing/goto-end)


;; *** copy buffer filename
(defun copy-buffer-file-name ()
  (interactive)
  (kill-new
   (if current-prefix-arg
       (file-name-nondirectory (buffer-file-name))
     (buffer-file-name))))

(define-key global-map (kbd "C-c c %") 'copy-buffer-file-name)

;; *** copy/cut current line if nothing selected
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

;; *** copy/paste rectangle
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


;; ** minibuffer
;; *** easily insert buffer name (useful for `shell-command', `compile' etc)
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

;; *** insert current symbol to minibuffer
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


;; ** multi-occur extensions
(defun moccur-all-buffers (regexp)
  (interactive "MRegexp: ")
  (multi-occur (buffer-list) regexp))

(defun moccur-all-files (regexp)
  "Call `multi-occur' on all buffers which has a filename."
  (interactive "MRegexp: ")
  (let ((buffers (remq nil (mapcar #'(lambda (buf)
                                       (if (buffer-file-name buf)
                                           buf))
                                   (buffer-list)))))
    (multi-occur buffers regexp)))

;; based on code from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun moccur-in-same-mode (symbol &optional arg)
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive
   (list (read-string "Occur in same mode: " (thing-at-point 'symbol))
         current-prefix-arg))
  (let* ((target-major-mode major-mode)
         (buffers (remq nil (mapcar #'(lambda (buf)
                                        (with-current-buffer buf
                                          (if (eq target-major-mode major-mode)
                                              buf)))
                                    (buffer-list)))))
    (multi-occur buffers
                 (format "%s" symbol)
                 arg)))

(define-key search-map (kbd "M-o") 'moccur-in-same-mode)


;; ** grin & ack: better grep replacement for source code project
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

(define-key search-map "gg" 'grin-on-dir)


(autoload 'ack "ack"
  "ack.el provides a simple compilation mode for the perl grep-a-like ack program." t)

(defun ack-on-dir (dir &optional pattern)
  (interactive "DAck on dir: ")
  (require 'ack)
  (let* ((default-directory dir)
         (ack-command (concat ack-command " \"" (or pattern (thing-at-point 'symbol)) "\"")))
    (call-interactively 'ack)))
   
(define-key search-map "ga" 'ack-on-dir)

;; ** windows
(autoload 'window-numbering-mode "window-numbering"
  "A minor mode that assigns a number to each window." t)

(idle-require 'window-numbering)

(eval-after-load "window-numbering"
  `(progn     
     (window-numbering-mode 1)

     ;; make window number more clear on mode-line
     ;; TODO: make sure new frame get the face correctly copied
     (copy-face 'mode-line-buffer-id 'window-numbering-face)              
     (defun window-numbering-get-number-string (&optional window)
       (let ((s (concat " "
                        (int-to-string (window-numbering-get-number window))
                        "□ ")))
         (propertize s 'face 'window-numbering-face)))

     (define-key window-numbering-keymap (kbd "M-0") 'select-minibuffer-window)
     ))

(defun select-minibuffer-window ()
  "Focus the minibuffer window (if it is active)"
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))))


;; ** tabbar
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

(defun tabbar-buffer-list-menu (event)
  "List all buffers of current tabset in a popup menu, let use choose to switch."
  (interactive "e")
  (let* ((buffers (mapcar '(lambda (tab) (tabbar-tab-value tab))
                          (tabbar-tabs (tabbar-current-tabset t))))
         (alist   (mouse-buffer-menu-alist buffers))
         (menu    (cons "Buffers in current tabset"
                        (mouse-buffer-menu-split "Select Buffer" alist)))
         (sel     (x-popup-menu event menu)))
    (if sel
        (switch-to-buffer sel))))

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

     (define-key tabbar-mode-map (kbd "<header-line> <C-mouse-1>") 'tabbar-buffer-list-menu)

     (require 'color nil t)
     (unless (fboundp 'color-name-to-rgb)
         ;;required by `tabbar-ruler', but provided by emacs-24
         (defun color-name-to-rgb (color &optional frame)
           "Convert COLOR string to a list of normalized RGB components."
           ;; `colors-values' maximum value is either 65535 or 65280 depending on the
           ;; display system. So we use a white conversion to get the max value.
           (let ((valmax (float (car (color-values "#ffffff")))))
             (mapcar (lambda (x) (/ x valmax)) (color-values color frame))))
       )
         
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

;; *** ido-jump-to-tab
(defun ido-jump-to-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (if (< emacs-major-version 24)
      (ido-common-initialization))  
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (mapcar '(lambda (tab) (buffer-name (tabbar-tab-value tab)))
                                  (tabbar-tabs (tabbar-current-tabset t))))
         (buffer-name (ido-completing-read "Tab: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (switch-to-buffer buffer-name))))

(define-key global-map (kbd "C-x B") 'ido-jump-to-tab)

;; *** anything-jump-to-tab
(defun anything-c-tab-list ()
  "Return a list of buffer names of current tabbar group. "
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (with-current-buffer anything-c-tab-current-buffer
    (mapcar #'(lambda (tab) (buffer-name (tabbar-tab-value tab)))
            (tabbar-tabs (tabbar-current-tabset t)))))

;;NOTE: without this, anything would change current buffer (to '*anything-tabs*'),
;; thus leads to wrong group
(setq anything-c-tab-current-buffer nil)

(defvar anything-c-source-tabs
  '((name . "Buffers of current Tabbar Group")
    (candidates . anything-c-tab-list)
    (type . buffer)))

(defun anything-tab-list ()
  "Preconfigured `anything' to list buffers.
It is an enhanced version of `anything-for-buffers'."
  (interactive)
  (setq anything-c-tab-current-buffer (current-buffer))
  (anything :sources '(anything-c-source-tabs
                       anything-c-source-buffer-not-found)
            :buffer "*anything tabs*"
            :keymap anything-c-buffer-map))

(define-key global-map (kbd "<f11> TAB") 'anything-tab-list)


;; ** color-theme
(idle-require 'color-theme)

(if (string< emacs-version "24")
    (progn
      (require 'color-theme nil t)
      (eval-after-load "color-theme"
        `(progn
           (if (require 'color-theme-tangotango nil t)
               (color-theme-tangotango)
             (if (require 'color-theme-molokai nil t)
               (color-theme-molokai)))
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





;; ** languages tools
;; *** spell
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

;; *** sdcv
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

;;(global-set-key (kbd "<C-down-mouse-1>") 'sdcv-search-word-at-pt-mouse)


;; *** dict protocol
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

;; *** google-translate
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


;; ** windows
(autoload 'window-toggle-split-direction "bmz-window-misc"
  "Switch window split from horizontally to vertically, or vice versa." t)

(global-set-key (kbd "<f11> |") 'window-toggle-split-direction)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; ** speedbar
(autoload 'sr-speedbar-toggle "sr-speedbar"
  "Toggle sr-speedbar window." t)

(global-set-key (kbd "<M-f11>") 'sr-speedbar-toggle)
                
(eval-after-load "speedbar"
  `(progn
     (setq speedbar-verbosity-level 1)
     
     (speedbar-disable-update)
     (global-set-key (kbd "ESC <M-f11>") 'speedbar-update-contents) ;;
     
     (setq speedbar-show-unknown-files t)
     (speedbar-add-supported-extension ".org")
     (speedbar-add-supported-extension ".md")     
     ))


;; ** indent guides
;;show guides for each indentation level
(autoload 'highlight-indentation-mode "highlight-indentation"
  "Highlight indentation minor mode highlights indentation based" t)
;;only the current column
(autoload 'highlight-indentation-current-column-mode "highlight-indentation"
  "Hilight Indentation minor mode displays" t)

(global-set-key (kbd "<f10> hi") 'highlight-indentation-mode)


;;`indent-guide-mode' only show guides on current section.
;; but it would actually insert a char (`indent-guide-char'),
;; thus it might not be suitable for terminal (if you use external copy (mouse or tmux))
(autoload 'indent-guide-mode  "indent-guide"
  "Show vertical lines to guide indentation." t)

(global-set-key (kbd "<f10> ig") 'indent-guide-mode)


;; `indent-guide-mode' actually use a char as the guide line,
;; thus if you use term's copy method (such as putty's or tmux's),
;; maybe `highlight-indentation-mode' is better.
(defun turn-on-highlight-indent ()
  (interactive)
  (if (display-graphic-p)
      (indent-guide-mode)
    (highlight-indentation-mode)))
  
(global-set-key (kbd "<f10> |") 'turn-on-highlight-indent)

(if (boundp 'prog-mode-hook)
    (add-hook 'prog-mode-hook 'turn-on-highlight-indent))


;; ** vi(m) emulation
;; *** viper
(global-set-key (kbd "<f6>") 'viper-mode)

(setq viper-expert-level 3)
(setq viper-inhibit-startup-message t)

(eval-after-load "viper"
  `(progn
     (require 'vimpulse nil t)

     (define-key viper-vi-global-user-map     (kbd "<f6>") 'viper-go-away)
     (define-key viper-insert-global-user-map (kbd "<f6>") 'viper-go-away)

     ;; fix some compartibility problems with CUA mode
     (define-key viper-vi-global-user-map [backspace] 'backward-delete-char-untabify)
     (define-key viper-vi-global-user-map "\C-d" 'delete-char)
     (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
     (define-key viper-insert-global-user-map "\C-d" 'delete-char) 
     ))


;; *** Ex commands without entering viper-mode
;; stoem from http://www.advogato.org/person/chalst/diary/277.html
;;for ex commands supported by viper, refer `ex-token-alist'

(autoload 'viper-ex "viper-ex" "Undocumented." t)

(eval-after-load "viper-ex"
  `(ignore-errors
     ;;(require 'viper-ex)
     (require 'viper-keym)
     (require 'viper-cmd)
     ))

(define-key global-map (kbd "ESC ESC :") 'viper-ex)

;; *** vim-region
(autoload 'vim-region-mode "vim-region"
  "Toggle Local-Vim-Region mode in every possible buffer." t)

(define-key global-map (kbd "<M-f6>") 'vim-region-mode)
(define-key global-map (kbd "M-`")    'vim-region-mode)
;;(define-key global-map (kbd "ESC `")  'vim-region-mode)


;; ** misc

;;info+.el: more colors (and other enhancements) 
(eval-after-load "info"
  `(require 'info+)
  )

(eval-after-load "info+"
  `(progn
     (defvar Info-next-link-keymap (make-sparse-keymap))
     (defvar Info-prev-link-keymap (make-sparse-keymap))
     (defvar Info-up-link-keymap   (make-sparse-keymap))
     (defvar Info-down-link-keymap   (make-sparse-keymap))))

;;--
;;(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> ws") 'whitespace-mode)

;;--
(global-set-key (kbd "<C-f4>") 'kill-buffer)

;;--
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (add-hook 'before-save-hook 'check-parens nil 'local)))

;--
(global-set-key (kbd "<C-down-mouse-1>") nil)
(global-set-key (kbd "<C-mouse-1>") 'ffap-at-mouse)
;; in case `outline-minor-mode' turned on
(define-key global-map (kbd "<C-M-down-mouse-1>") nil)
(define-key global-map (kbd "<C-M-mouse-1>") 'ffap-at-mouse)
