;;; outline-org-like.el - Use org-mode style headings in other major modes.

;;* Org-mode style headings in comment
;;** (comments & header)
;;
;; Author: bamanzi AT gmail DOT com
;;
;; All right reversed.

;;; Commentary:
;; This package allow you to use org-mode style headings in other
;; major modes. Visit this link for demo pictures:
;; http://www.cnblogs.com/bamanzi/archive/2011/10/09/emacs-outline-org-like.html
;;
;; Features:
;;   1. org-mode style heading highlighting
;;   2. org-mode style cycling heading visibilities with just one key
;;      (`H-tab' by default)
;;   3. org-mode style move-up/down/promote/demote heading (you need
;;      package `outline-magic'
;;   4. other outline operations proxied via another prefix key
;;      (`C-S-z' by default)
;;   5. `anything' source to list all org-mode style headings
;;
;; Usage:
;; 
;;   First, put `*' `**' `***' (with a leading space) in comments as
;;   outline headings:
;;
;;     # * heading 1
;;     # ** heading 2
;;     class Object1:
;;         # *** heading 1.2.1
;;         def method_2(a, b):
;;             ...
;;     # ** heading 2
;;
;;   Then use `outline-org-mode' to turn on a tweaked
;;   `outline-minor-mode'.  (Note: it would change current buffer's
;;   `outline-regexp'.) You can use all `outline-minor-mode' commands
;;   (such as `hide-all', `hide-entry', `hide-all', `hide-entry' etc)
;;   to navigate/fold these sections.
;;
;; Advanced usage:
;;
;;   1. To leave you own `outline-regexp' setting untouched, you use
;;      `outline-org-headings-mode' to turn on the heading
;;      highlighting, and use `outline-org/outline-command-dispatcher'
;;      to navigate/fold the org-mode-style headings.
;;
;;      for example, if you bind
;;      `outline-org/outline-command-dispatcher' to `C-z', then you
;;      can use:
;;
;; 	      C-z C-a  show-all
;; 	      C-z C-c  hide-entry
;; 	      C-z C-f  outline-forward-same-level
;; 	      C-z C-t  hide-body
;;
;;   2. To get an overview of all headings, you can use
;;   
;;     - `outline-org/cycle' (package `outline-magic' needed)
;;     - or, `anything-outline-org-headings' (package `anything' needed)

;;; Code:

(require 'outline)
(require 'outline-magic nil t) ;; for `outline-cycle'
(require 'org)           ;; for face org-level-1..6

;;** we'll use an internal `outline-regexp' value
(defun outline-org/get-heading-regexp ()
  "Calculate the outline regexp for the current mode."
  (when comment-start
    (let ((comment-starter (replace-regexp-in-string
                            "[[:space:]]+" "" comment-start)))
      (if (eq 'emacs-lisp-mode major-mode)
          ;; both ';; ***' and ';;;; ' would work
          "^;; ?[*;]"
        ;; e.g. for python: '# * title ...'
        (concat "^" comment-starter " [*]+")))))

;;** heading highlighting
(defun outline-org/get-heading-font-lock-keywords ()
  (let ( (heading-regexp (outline-org/get-heading-regexp)) )
    (let ( (heading-1-regexp   
            (concat heading-regexp "\\{1\\} \\(.*\\)"))
           (heading-2-regexp
            (concat heading-regexp "\\{2\\} \\(.*\\)"))
           (heading-3-regexp
            (concat heading-regexp "\\{3\\} \\(.*\\)"))
           (heading-4-regexp
            (concat heading-regexp "\\{4,\\} \\(.*\\)")) )
      `((,heading-1-regexp 1 'org-level-1 t)
        (,heading-2-regexp 1 'org-level-2 t)
        (,heading-3-regexp 1 'org-level-3 t)
        (,heading-4-regexp 1 'org-level-4 t)))))

(define-minor-mode outline-org-headings-mode
  "org-mode like heading highlighting."
  nil
  :group 'outline
  (let ( (keywords (outline-org/get-heading-font-lock-keywords)) )
    (when keywords
      (if outline-org-headings-mode
          ;; turn on
          (font-lock-add-keywords nil keywords)
        ;; turn off
      (font-lock-remove-keywords nil keywords))
      (font-lock-mode -1) ;;FIXME: any better way?
      (font-lock-mode 1)
      )))

;;** outline commands wrapper without changing user `outline-regexp'

(defun outline-org/outline-command-dispatcher (key)
  "Outline commands wrapper, but using org-mode like headings as `outline-regexp'.

This command use a temporary `outline-regexp' value inside to
find the headings, thus you don't need to change your own
settings.  For example, if you bind `C-S-z' to this command, you
can use `C-S-z C-u' to go to parent heading in org-mode style, but
`C-c @ C-u' remains the default `outline-up-heading'."
  (interactive "KOutline operation: ")
  (let ( (outline-regexp (outline-org/get-heading-regexp))
         (command (lookup-key outline-mode-prefix-map key)) )
    (if comment-start
        (if (or (equal key (kbd "<f1>"))
                (equal key (kbd "<f1> <f1>")))  
            (describe-variable 'outline-mode-prefix-map)
          (if (and command (commandp command))
              (progn
                (message "%s" command)
                (call-interactively command))
            (message "no command for that key in `outlint-mode-prefix-map'.")))
      (message "Can't determine `outline-regexp' according to `comment-start'."))))

;; default keybindings: C-c @ C-z
(define-key outline-mode-prefix-map (kbd "C-z") 'outline-org/outline-command-dispatcher)
(global-set-key (kbd "C-S-z") 'outline-org/outline-command-dispatcher)

;;*** our new `outline-cycle'
(defun outline-org/outline-cycle ()
  (interactive)
  (let ( (outline-regexp (outline-org/get-heading-regexp)) )
    (if (and (not outline-minor-mode) (not (eq major-mode 'outline-mode)))
        (outline-minor-mode t))
    (if (not outline-org-headings-mode)
        (outline-org-headings-mode t))
    (call-interactively 'outline-cycle)))

;;TODO: you can wrap more outline command as you like

;;** `anything' integration: list headings and jump to it
(defun anything-c-outline-org-heading-hide-others (elm)
  (anything-c-action-line-goto elm)
  (hide-other))

(defvar anything-c-source-outline-org-headings
  '((name . "Org-like Headings")
    (headline . (lambda ()
                  (outline-org/get-heading-regexp)))
    (migemo)
    (persistent-action . (lambda (elm)
                           (anything-c-action-line-goto elm)
                           (outline-org/show-subtree)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to line" . anything-c-action-line-goto)
           ("Go to section and fold otherse" . anything-c-outline-org-heading-hide-others)))))
  "Show Org-like headlines.
outline-org-mode is a special outline-minor-mode that uses org-mode like
headings.

See (find-library \"outline-org.el\") ")

(defun anything-outline-org-headings ()
  "Preconfigured anything to show org-mode-like headings."
  (interactive)
  (anything-other-buffer 'anything-c-source-outline-org-headings "*org-like headlines*"))


;;** `outline-org-mode': a special `outline-minor-mode' that use org-mode style headings
;; It supports heading highlighting and all outline commands.
;; NOTE: it would overtake your `outline-regexp' settings

(defvar outline-regexp-old nil "backup of value of `outline-regexp")

(define-minor-mode outline-org-mode
  "A special `outline-minor-mode' that use org-mode-style headings."
  nil
  :group 'outline
  (let ( (keywords (outline-org/get-heading-font-lock-keywords)) )
    (if outline-org-mode
        (progn  ;; to turn on
          (if (not outline-minor-mode)
              (outline-minor-mode t))
          (set (make-local-variable 'outline-regexp-old) outline-regexp)
          (setq outline-regexp (outline-org/get-heading-regexp))
          (if (not outline-org-headings-mode)
              (outline-org-headings-mode t))
          (hide-body))
      (progn
        (setq outline-regexp-old outline-regexp)
        (outline-org-headings-mode -1))
        )))


(provide 'outline-org-like)
;;; outline-org-like.el ends here
