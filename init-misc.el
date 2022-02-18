;; ** files
;; *** ffap
(defun ido-find-file-at-point ()
  (interactive)
  (let ((ido-use-filename-at-point t)
        (ido-use-url-at-point t))
     (call-interactively 'ido-find-file)))

(define-key search-map (kbd "C-f") 'ido-find-file-at-point)

(defun ffap-at-mouse-other-window (e)
  (interactive "e")
  (let ((ffap-file-finder 'find-file-other-window))
    (ffap-at-mouse e)))

(global-set-key (kbd "<C-down-mouse-1>") nil)
(global-set-key (kbd "<C-mouse-1>") 'ffap-at-mouse-other-window)
;; in case `outline-minor-mode' turned on
(define-key global-map (kbd "<C-M-down-mouse-1>") nil)
(define-key global-map (kbd "<C-M-mouse-1>") 'ffap-at-mouse-other-window)

;; **** make ffap support line number
;;copied from http://www.emacswiki.org/emacs/FindFileAtPoint#toc6
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string 
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0)) 
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))


;; *** dired-single
(autoload 'dired-single-buffer "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(autoload 'dired-single-buffer-mouse "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(eval-after-load "dired"
  `(add-hook 'dired-mode-hook 'bmz/dired-init-keys 'append)
  )

(defun bmz/dired-init-keys ()
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
  
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^" ''(lambda ()
                                     (interactive)
                                     (dired-single-buffer ".."))))

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

(defun nc-goto-dir-at-point ()
  "Let NC show directory at point."
  (interactive)
  (let ((ffap-directory-finder 'nc-goto-dir))
    (call-interactively 'dired-at-point)))

(define-key goto-map (kbd "C") 'nc-goto-dir-at-point)

;; *** open file in desktop environment
(defun de-open-file-in-associated-app (file)
  (cond
   ((eq system-type 'windows-nt)
    (w32-browser file))
   ((eq system-type 'gnu/linux)
    (start-process "" nil "xdg-open" file))))

(defun de-open-current-file-in-associated-app ()
  (interactive)
  (de-open-file-in-associated-app buffer-file-name))

(defun de-open-current-dir-in-associated-app ()
  (interactive)
  (de-open-file-in-associated-app default-directory))

(global-set-key (kbd "<apps> C-o") 'de-open-current-file-in-associated-app)
(global-set-key (kbd "<apps> C-O") 'de-open-current-dir-in-associated-app)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'OS :key "<apps> C-o"   :description "de-open-current-file-in-associated-app")
     (cheatsheet-add :group 'OS :key "<apps> C-S-o" :description "de-open-current-dir-in-associated-app")
     t))


;; ** minibuffer
;; *** easily insert buffer name (useful for `shell-command', `compile' etc)
(defun minibuffer-insert-buffer-filename (arg)
  (interactive "P")
  (let ((target-buffer (window-buffer (minibuffer-selected-window))))
    (if (and target-buffer
             (buffer-file-name target-buffer))
      (insert (if arg
                         (buffer-file-name target-buffer)
                       (file-name-nondirectory (buffer-file-name target-buffer))))
      (insert " "))))

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


;; ** searching
;; *** multi-occur extensions
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


;; *** ripgrep/grin/ack: better grep replacement for source code project
(defun grep-on-dir (dir)
  (interactive "DGrep on dir: ")
  (require 'grep)
  (let ((default-directory dir))
    (call-interactively 'grep)))

(global-set-key (kbd "<apps> gg") 'grep-on-dir)

;;-- ripgrep
(autoload 'ripgrep-regexp "ripgrep"
  "Run a ripgrep search with ‘REGEXP’ rooted at ‘DIRECTORY’." t)

(global-set-key (kbd "<apps> gr") 'ripgrep-regexp)

;;-- grin
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

(global-set-key (kbd "<apps> gG") 'grin-on-dir)

;;-- ack
(autoload 'ack "ack"
  "ack.el provides a simple compilation mode for the perl grep-a-like ack program." t)

(defun ack-on-dir (dir &optional pattern)
  (interactive "DAck on dir: ")
  (require 'ack)
  (let* ((default-directory dir)
         (ack-command (concat ack-command " \"" (or pattern (thing-at-point 'symbol)) "\"")))
    (call-interactively 'ack)))

(global-set-key (kbd "<apps> ga") 'ack-on-dir)

;; ** windows
;; *** window splitting
(autoload 'window-toggle-split-direction "bmz-window-misc"
  "Switch window split from horizontally to vertically, or vice versa." t)

(global-set-key (kbd "<f11> |") 'window-toggle-split-direction)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; *** window jumping

(defun select-minibuffer-window ()
  "Focus the minibuffer window (if it is active)"
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))))

(global-set-key (kbd "<f11> 0") 'select-minibuffer-window)


;; ** buffers
;; *** describe-buffer
;; based on `describe-buffer' from http://www.emacswiki.org/emacs/download/help-fns%2b.el
(defun describe-buffer- (&optional buffer-name)
    "Describe the existing buffer named BUFFER-NAME.
By default, describe the current buffer."
    ;; (interactive "bDescribe buffer: ")
    (interactive "@")
    (unless buffer-name (setq buffer-name  (buffer-name)))
    (help-setup-xref `(describe-buffer- ,buffer-name) (called-interactively-p 'interactive))
    (let ((buf  (get-buffer buffer-name)))
      (unless (and buf  (buffer-live-p buf))  (error(format "No such live buffer `%s'" buffer-name)))
      (let* ((file       (or (buffer-file-name buf)
                             (with-current-buffer buf
                               (and (eq major-mode 'dired-mode)  default-directory))))
             (help-text  (concat
                          (format "Buffer `%s'\n%s\n\n" buffer-name (make-string
                                                                     (+ 9 (length buffer-name)) ?-))
                          (and file  (format "File/directory: %s\n\n" file))
                          (format "Mode:           %s\n"
                                  (with-current-buffer buf (format-mode-line mode-name)))
                          (format "Encoding:       %s\n" buffer-file-coding-system)
                          (format "Line-ending:    %s\n"
                                (with-current-buffer buf
                                  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
                                    (cond
                                     ((eq 0 eol-type) "UNIX")
                                     ((eq 1 eol-type) "DOS")
                                     ((eq 2 eol-type) "MAC")
                                     (t "???")))))
                          (format "Lines:          %s\n"
                                  (with-current-buffer buf
                                      (count-lines (point-min) (point-max))))
                          (format "Chars:          %g\n" (buffer-size buf))
                          (format "Modified:       %s\n" (if (buffer-modified-p buf) "yes" "no"))
                          (with-current-buffer buf
                            (format "Read-only:      %s\n" (if buffer-read-only "yes" "no")))
                          (with-current-buffer buf
                            (if (not buffer-display-time)
                                "Never displayed\n"
                              (format "Last displayed: %s\n"
                                      (format-time-string
                                       ;; Could use this, for short format: "%02H:%02M:%02S"
                                       ;; Or this, for a bit longer: "%_3a %_2l:%02M:%02S %_2p"
                                       "%a %b %e %T %Y (%z)"
                                       buffer-display-time)))))))
        
        (with-help-window (help-buffer)
          (with-current-buffer (help-buffer)
            (insert help-text))))))

(global-set-key (kbd "C-h B") 'describe-buffer-)

(eval-after-load "cheatsheet"
  `(cheatsheet-add :group 'Misc
                  :key "C-h B"
                  :description "describe-buffer-"))

;; *** swbuff
(autoload 'nswbuff-switch-to-next-buffer "nswbuff"
  "Switch to the next buffer in the buffer list." t)
(autoload 'nswbuff-switch-to-previous-buffer "nswbuff"
  "Switch to the previous buffer in the buffer list." t)

(progn
  (global-set-key (kbd "<M-f12>")   'nswbuff-switch-to-next-buffer)
  (global-set-key (kbd "<M-S-f12>") 'nswbuff-switch-to-previous-buffer)

  (when (and (< emacs-major-version 25)
             (not (featurep 'seq)))
    ;; http://elpa.gnu.org/packages/seq.html
    (require 'seq-24)
    (provide 'seq)
    ))


;; *** ibuffers
(eval-after-load "ibuffer"
  `(progn
     (define-key ibuffer-mode-map (kbd "G m")   'ibuffer-set-filter-groups-by-mode)
     (define-key ibuffer-mode-map (kbd "G ESC") 'ibuffer-clear-filter-groups)
     ))

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Ibuffer
                :key "G ESC"
                :description "M-x ibuffer-clear-filter-groups")
     (cheatsheet-add :group 'Ibuffer
                :key "G m"
                :description "M-x ibuffer-set-filter-groups-by-mode")
     ))

;; *** rings
(eval-after-load "rings"
  `(progn
     (dotimes (i 10)
       (eval `(progn
                (define-key global-map ,(read-kbd-macro (format "<f12> %d" i))  nil)
                (define-key global-map ,(read-kbd-macro (format "<f12> %d" i)) (rings-generate-cycler ,i))

                (define-key global-map ,(read-kbd-macro (format "<f12> M-%d" i))  nil)
                (define-key global-map ,(read-kbd-macro (format "<f12> M-%d" i)) (rings-generate-setter ,i))
                )))
     ))

(idle-require 'rings)


;; ** tabbar
;; *** basic setup
(idle-require 'tabbar)

(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
     (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)

     (define-key tabbar-mode-map (kbd "<f12> <right>") 'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<f12> <left>")  'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<f12> <up>")    'tabbar-press-home)

     (define-key tabbar-mode-map (kbd "<header-line> <C-mouse-1>") 'tabbar-buffer-list-menu)

     (setq tabbar-tab-label-function 'tabbar-buffer-tab-label/my)
     ))


;;add extra leading & ending space to tab label
(defun tabbar-buffer-tab-label/my (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (if window-system
                      (format "%s " (tabbar-tab-value tab))
                    (format "/%s\\" (tabbar-tab-value tab))))))
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


;; *** grouping
(defun tabbar-group-by-major-modes ()
  "Use tabbar.el's default grouping method: group by major-mode names."
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

(defun tabbar-buffer-groups/simple ()
  "Return the list of group names the current buffer belongs to.
 Return a list of one element based on major mode."
  (list
   (cond
    ((or (memq major-mode '(ibuffer-mode grep-mode occur-mode
                                         shell-mode eshell-mode lisp-interaction-mode
                                         diff-mode))
         (get-buffer-process (current-buffer))
         (member (buffer-name) '("*scratch*" "*messages*" "*Help*")))
     "utils")
    ((or (= (aref (buffer-name) 0) ?*)
         (= (aref (buffer-name) 0) ? ))
     "*temp*")
    (t
     "files"
     ))))

(defun tabbar-group-by-simple-rules ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups/simple))


;; **** grouping with rings
(defvar tabbar-buffer-groups-function/without-rings nil)

(defun tabbar-buffer-groups/simple-with-rings ()
  "Return the list of group names the current buffer belongs to.
 Return a list of one element based on major mode."
  (let ((rings (remove-if-not 'identity
                              (mapcar #'(lambda (var)
                                          (let ((varname (format "%s" (car var))))
                                            (if (string-match-p "^rings-[0-9]" varname)
                                                varname)))
                                      (buffer-local-variables (current-buffer))))))
    (append rings (if tabbar-buffer-groups-function/without-rings
                      (funcall tabbar-buffer-groups-function/without-rings)
                    (tabbar-buffer-groups/simple)))))

(defun tabbar-group-by-simple-with-rings ()
  (interactive)
  (setq tabbar-buffer-groups-function/without-rings 'tabbar-buffer-groups/simple)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups/simple-with-rings))



;; *** tabbar-ruler
(eval-after-load "tabbar"
  `(progn
     ;; FIXME: is there any problem if we load `tabbar-ruler' on xterm frame?
     (if (display-graphic-p)
         (require 'tabbar-ruler nil t))
     ))
       
(eval-after-load "tabbar-ruler"
  `(progn
     ;;reset my grouping funciton
     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups/simple-with-rings)

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

     (tabbar-ruler-remove-caches)
     ))
       

;; **** workaround for a bug
;; Emacs >= 24.4  would try to store frame configuration in `desktop-save-mode`
;; but couldn't make `tabbar-cache` persistent correctly ("Unprintable entity" error)
(defun tabbar-ruler-remove-caches ()
  (mapc #'(lambda (frame)
            (modify-frame-parameters frame '((tabbar-cache . nil))))
        (frame-list)))

(if (fboundp 'desktop-save-mode)
    (add-hook 'desktop-after-read-hook 'tabbar-ruler-remove-caches 'append))


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

(define-key global-map (kbd "<f12> TAB") 'anything-tab-list)


;; ** speedbar
(autoload 'sr-speedbar-toggle "sr-speedbar"
  "Toggle sr-speedbar window." t)

(global-set-key (kbd "<M-f11>") 'sr-speedbar-toggle)
                
(eval-after-load "speedbar"
  `(progn
     (setq speedbar-verbosity-level 1)
     
     (speedbar-disable-update)
     (global-set-key (kbd "<apps> <M-f11>") 'speedbar-update-contents) ;;
     
     (setq speedbar-show-unknown-files t)
     (speedbar-add-supported-extension ".org")
     (speedbar-add-supported-extension ".md")     
     ))




;; ** misc

;;--
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (add-hook 'before-save-hook 'check-parens nil 'local)))

;;--
(defun revert-buffer-keep-cursor-pos ()
  "revert bufer with close & reopen the file, so local variable would be re-inited."
  (interactive)
  (let ( (file-name (buffer-file-name))
         (pt        (point)) )
    (when (kill-buffer (current-buffer))
        (find-file file-name)
        (goto-char pt))))

(global-set-key (kbd "C-x M-r") 'revert-buffer-keep-cursor-pos)

;;--
(autoload 'htmlize-buffer "htmlize"
  "Convert BUFFER to HTML, preserving colors and decorations." t)
(autoload 'htmlize-file   "htmlize"
  "Load FILE, fontify it, convert it to HTML, and save the result." t)
(autoload 'htmlize-region "htmlize"
  "Convert the region to HTML, preserving colors and decorations." t)

;;--
;; copy buffer filename
(defun copy-buffer-file-name ()
  (interactive)
  (kill-new
   (if current-prefix-arg
       (file-name-nondirectory (buffer-file-name))
     (buffer-file-name))))

(define-key global-map (kbd "<apps> c %") 'copy-buffer-file-name)
