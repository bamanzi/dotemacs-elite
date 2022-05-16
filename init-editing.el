;; ** section folding
;; *** orgstruct-mode: folding by org-mode like section header
;; better than my 'outline-org-mode' (use TAB for visibility cycling),
;; but it lacks heading highlighting (use it with `outline-org-headings-mode' to fix this.)
;;
;; NOTE: it requires `orgstruct-mode' in org 8
;; http://www.orgmode.org/manual/Orgstruct-mode.html
(defun bmz/turn-on-orgstruct-mode-maybe ()
  (interactive)
  (require 'org)
  (if (string< org-version "8")
      (if (called-interactively-p 'interactive)
          (message "Only `orgstruct-mode' in org > 8.0 could be used as code folding. But currently install is %s" org-version))
    (setq orgstruct-heading-prefix-regexp
          (cond ((eq major-mode 'emacs-lisp-mode)
                 ";; ")
                ;;FIXME: other special cases?
                (t
                 comment-start)))
    (if orgstruct-heading-prefix-regexp
        (turn-on-orgstruct))))

(eval-after-load "org"
  `(progn
     (unless (string< org-version "8")
       (add-hook 'prog-mode-hook 'bmz/turn-on-orgstruct-mode-maybe 'append))))


(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Outline/orgstruct
                     :key "bmz/turn-on-orgstruct-mode"
                     :description "folding by org-mode like section header (org > 8.0 requried)")
     t))


;; deprecated: highlight header
;; (defun highlight-outline-header/bmz ()
;;   (interactive)
;;   (highlight-lines-matching-regexp "^;;; \\w" 'hi-black-hb)
;;   ;; highlight headers in this file
;;   (highlight-lines-matching-regexp "^;; \\* "          'org-level-1)
;;   (highlight-lines-matching-regexp "^;; \\*\\* "       'org-level-2)
;;   (highlight-lines-matching-regexp "^;; \\*\\*\\* "    'org-level-3)
;;   (highlight-lines-matching-regexp "^;; \\*\\*\\*\\* " 'org-level-4))

;; (eval-after-load "lisp-mode"
;;   `(add-hook 'emacs-lisp-mode-hook 'highlight-outline-header/bmz))

;; *** outline-org-headings-mode (highlight org-like headings)
(autoload 'outline-org-headings-mode "outline-org-like"
  "org-mode like heading highlighting." t)

(autoload 'anything-outline-org-headings "outline-org-like"
  "Preconfigured anything to show org-mode-like headings." t)

(global-set-key (kbd "<f5> C-z") 'anything-outline-org-headings)

(idle-require 'outline-org-like)

(eval-after-load "outline-org-like"
  `(progn
     (if (boundp 'prog-mode-hook)
         (add-hook 'prog-mode-hook 'outline-org-headings-mode)
       (add-hook 'find-file-hook 'outline-org-headings-mode))
     
     (define-key outline-mode-prefix-map (kbd "<f5>") 'outline-org-headings-mode)
     ))

(global-set-key (kbd "C-z <f5>") 'outline-org-headings-mode)


;; *** outline-org-mode
(autoload 'outline-org-mode  "outline-org-like"
  "A special `outline-minor-mode' that use org-mode-style headings." t)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z <f5>"
                     :description "toggle `outline-org-headings-mode'")
     
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z C-z ..."
                     :description "outline-org/outline-command-dispatcher")
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z C-z C-a"
                     :description "outline-hide-body (fold all)")  
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z C-z C-a"
                     :description "outline-show-all (unfold all)")
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z C-z C-s"
                     :description "outline-show-entry (unfold current)")
     (cheatsheet-add :group 'Outline/org-like
                     :key "C-z C-z C-d"
                     :description "outline-show-subtree (fold children)")
     ))


;; ** mark, copy & yank
;; *** make/copy something


(autoload 'thing/mark-one-thing "bmz-thing-op"
  "Undocumented." t)

(global-set-key (kbd "M-` SPC") 'thing/mark-one-thing)

(idle-require 'pulse) ;; for `pulse-momentary-highlight-region'

(idle-require 'bmz-thing-op)

(eval-after-load "bmz-thing-op"
  `(progn
     (global-set-key (kbd "C-c m") 'thing/mark-one-thing)
     (global-set-key (kbd "C-c c") 'thing/copy-one-thing)
     (global-set-key (kbd "C-c k") 'thing/kill-one-thing)

     (global-set-key (kbd "M-g <") 'thing/goto-beginning)
     (global-set-key (kbd "M-g >") 'thing/goto-end)

     (global-set-key (kbd "M-s M-w") 'thing/copy-symbol-or-word)
     (global-set-key (kbd "M-s C-w") 'thing/kill-symbol-or-word)
     ))


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

;; *** misc
(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point." t)

(global-set-key (kbd "<M-insert>") 'copy-from-above-command)
(global-set-key (kbd "C-c C-l") 'copy-from-above-command) ; in case there's no 'insert' on small keyboard

(defun backward-kill-line ()
  "Kill backward to the beginning of the line."
  (interactive)
  (kill-line 0))

(global-set-key (kbd "C-c C-u") 'backward-kill-line)


;; ** rectangle
;; *** C-x r ... (no speicial selection)

;; put rectangle-area to `killed-rectangle'
;; (same with `copy-rectangle-as-kill' in emacs>=24.3)
(autoload 'rectplus-copy-rectangle  "rect+"
  "Copy rectangle area." t)

;; copy content of `killed-rectangle' to `kill-ring'
(autoload 'rectplus-rectangle-to-kill-ring "rect+"
  "Killed rectangle to normal `kill-ring'." t)

(autoload 'rectplus-insert-number-rectangle  "rect+"
  "Insert incremental number into each left edges of rectangle's line." t)

(define-key ctl-x-r-map (kbd "M-n")   'rectplus-insert-number-rectangle)

(define-key global-map (kbd "C-x r M-w") 'rectplus-copy-rectangle)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Rectangle/rect
                     :key "C-x r M-w"
                     :description "rectplus-copy-rectangle - Copy rectangle-area to `killed-rectangle'.")
     (cheatsheet-add :group 'Rectangle/rect
                     :key "C-x r y"
                     :description "yank-rectangle - Yank the last killed rectangle (`killed-rectangle').")
  
     (cheatsheet-add :group 'Rectangle/kill-ring
                     :key "M-x rectplus-rectangle-to-kill-ring"
                     :description "Copy content of `killed-rectangle' to `kill-ring'")
     t
     ))

;; *** cua rectangle (visual)
(autoload 'cua-mouse-set-rectangle-mark "cua-rect"
  "Start rectangle at mouse click position." t)

;; *** rectangle-mark-mode (visual, but only available on emacs>=24.4)

;; use mouse to mark rectangle (r-m-m)
;; https://tangjunjie.wordpress.com/2015/07/10/enable-emacs-column-selection-using-mouse/
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(if (fboundp 'rectangle-mark-mode)
    (global-set-key (kbd "<C-M-down-mouse-1>") #'mouse-start-rectangle)
  (global-set-key (kbd "<C-M-down-mouse-1>")   'cua-mouse-set-rectangle-mark))

(eval-after-load "cheatsheet"
  `(cheatsheet-add :group 'Rectangle/cua
                   :key "<C-M-down-mouse-1>"
                   :description "mouse-start-rectangle (r-m-m) / cua-mouse-set-rectangle-mark (cua)")
  )

;; ** languages tools
;; *** spell
(define-key global-map (kbd "M-c $") 'ispell-complete-word)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Language-Tools
                     :key "M-c $"
                     :description "ispell-complete-word.")
     t))


;; **** ispell + auto-complete
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

(define-key global-map (kbd "M-c M-s") 'ac-complete-ispell-word)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Auto-Complete
                     :key "M-c M-s"
                     :description "ac-complete-ispell-word")
     t))


;; ** vi(m) emulation
;; *** viper
(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<f6> <f6>") 'viper-mode)

(setq viper-expert-level 3)
(setq viper-inhibit-startup-message t)

(eval-after-load "viper"
  `(progn
     (require 'vimpulse nil t)

     (define-key viper-vi-global-user-map     (kbd "<M-f6>") 'viper-go-away)
     (define-key viper-insert-global-user-map (kbd "<M-f6>") 'viper-go-away)

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
(define-key global-map (kbd "<f6> :") 'viper-ex)

;; *** vim-region
(autoload 'vim-region-mode "vim-region"
  "Toggle Local-Vim-Region mode in every possible buffer." t)

(define-key global-map (kbd "<f6> v") 'vim-region-mode)
(define-key global-map (kbd "M-` V")  'vim-region-mode)
;;(define-key global-map (kbd "ESC `")  'vim-region-mode)

;; *** misc vi(m) commands
(define-key global-map (kbd "<f6> *")   'highlight-symbol-next)
(define-key global-map (kbd "<f6> #")   'highlight-symbol-prev)

;; http://www.emacswiki.org/emacs/ParenthesisMatching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(define-key global-map (kbd "<f6> %")   'goto-match-paren)
(define-key global-map (kbd "M-g %")   'goto-match-paren)

(defun join-line ()
  "Join the following line with current line"
  (interactive)
  (delete-indentation 1))

(define-key global-map (kbd "<f6> j") 'join-line)


(autoload 'viper-info-on-file "viper-ex" nil nil)
 
(define-key global-map (kbd "<f6> C-g") 'viper-info-on-file)

(define-key global-map (kbd "<f6> y y")  #'(lambda ()
                                      (interactive)
                                      (thing/copy-one-thing 'line)))

(define-key global-map (kbd "<f6> d w") 'kill-word)
(define-key global-map (kbd "<f6> d t") 'zap-up-to-char)
(define-key global-map (kbd "<f6> d f") 'zap-to-char)
(define-key global-map (kbd "<f6> d d") 'kill-whole-line)

(define-key global-map (kbd "<f6> g g") 'beginning-of-buffer)
(define-key global-map (kbd "<f6> G")   'end-of-buffer)
(define-key global-map (kbd "<f6> g f") 'find-file-at-point)

(unless (fboundp 'zap-up-to-char)
    (defun zap-up-to-char (arg char)
      "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
      (interactive "p\ncZap to char: ")
      (zap-to-char arg char)
      (insert char)
      (forward-char -1)))

(global-set-key [remap zap-to-char] 'zap-up-to-char)
    
;; FIXME: any better one?
;; candicates:
;;    `anything-goto-definition-etags/imenu' from file:init-prog.el
(define-key global-map (kbd "<f6> g d") 'vimpulse-goto-definition)

;; ** some indicators
(global-set-key (kbd "<f10> -") 'hl-line-mode)

;;--
;;(setq-default fill-column 72)

(autoload 'fci-mode "fill-column-indicator"
  "Toggle fci-mode on and off." t)

(global-set-key (kbd "<f10> |") 'fci-mode)

;;--
(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Highlighting
                     :key "<f10> -"
                     :description "hl-line-mode")
     (cheatsheet-add :group 'Highlighting
                     :key "<f10> |"
                     :description "fci-mode (fill-column-indicator)")
     t))


;; ** misc
;;--
;;(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> ws") 'whitespace-mode)


;;--
;;goto-line with line number
;;stolen from http://whattheemacsd.com/key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
