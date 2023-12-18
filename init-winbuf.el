;; ** windows
;; *** enlarge current window
;;(I don't use `golden-ratio-enable')
(autoload 'golden-ratio "golden-ratio"
  "Resizes current window to the golden-ratio's size specs" t)
(autoload 'golden-ratio-enable "golden-ratio"
  "Enables golden-ratio's automatic window resizing" t)

(defun golden-ratio+ ()
  "Enlarge current window, more than command `golden-ratio'."
  (interactive)
  (require 'golden-ratio)
  (let ((-golden-ratio-value 1.25))
    (call-interactively 'golden-ratio)))

(global-set-key (kbd "<f11> x") 'golden-ratio)
(global-set-key (kbd "<f11> X") 'golden-ratio+)

;; **** golden-ratio on anything window
(defun anything-enlarge-window ()
  (interactive)
  (require 'golden-ratio)
  (with-anything-window
    (call-interactively 'golden-ratio)))

(defun anything-enlarge-window+ ()
  (interactive)
  (require 'golden-ratio)
  (with-anything-window
    (let ((-golden-ratio-value 1.3))
      (call-interactively 'golden-ratio))))

(eval-after-load "anything"
  `(progn
     (define-key anything-map (kbd "<f11> x") 'anything-enlarge-window)
     (define-key anything-map (kbd "<f11> X") 'anything-enlarge-window+)
     ))


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


