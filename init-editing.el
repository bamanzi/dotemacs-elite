;; ** mark, copy & yank
;; *** make/copy something

;; based on code stolen from https://github.com/m2ym/thingopt-el/blob/master/thingopt.el
(setq thing/name-map
  '((?w . word)
    (?e . sexp)
    (?s . symbol)
    (?S . sentence)
    (?p . paragraph)
    (?h . defun)
    (?f . filename)
    (?l . line)
    (?\( . list)
    (?L . list)
    (?\" . string)
    (?u . url)
    (?P . page)
    (?n . number)))

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

(idle-require 'pulse) ;; for `pulse-momentary-highlight-region'
(defun thing/copy-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (let ((begin (car bounds))
                               (end   (cdr bounds)))
                           (if (require 'pulse nil t)
                               (pulse-momentary-highlight-region begin end))
                           (copy-region-as-kill begin end)
                           (message "Copied %s." thing)))))

(defun thing/kill-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (let ((begin (car bounds))
                               (end   (cdr bounds)))
                           (if (require 'pulse nil t)
                               (pulse-momentary-highlight-region begin end))
                           (kill-region begin end)
                           (message "Killed %s." thing)))))

(global-set-key (kbd "<M-delete>") 'thing/kill-one-thing)

(global-set-key (kbd "C-c m") 'thing/mark-one-thing)
(global-set-key (kbd "C-c c") 'thing/copy-one-thing)
(global-set-key (kbd "C-c k") 'thing/kill-one-thing)


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


(defun thing/copy-symbol-or-word ()
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'word))))
    (if bounds
        (let* ((begin (car bounds))
               (end   (cdr bounds))
               (content (buffer-substring begin end)))
          (if (require 'pulse nil t)
              (pulse-momentary-highlight-region begin end))
          (copy-region-as-kill begin end)
          (message "Copied '%s'." content)))))

(global-set-key (kbd "M-s M-w") 'thing/copy-symbol-or-word)

(defun thing/kill-symbol-or-word ()
  (interactive)
  (thing/kill-one-thing 'symbol))

(global-set-key (kbd "M-s C-w") 'thing/kill-symbol-or-word)

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
(define-key global-map (kbd "ESC M-$") 'ispell-complete-word)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Language-Tools
                     :key "ESC M-$"
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

(define-key global-map (kbd "<apps> , $") 'ac-complete-ispell-word)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Auto-Complete
                     :key "<apps> , $"
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
(define-key global-map (kbd "M-`")    'vim-region-mode)
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
