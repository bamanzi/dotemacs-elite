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
