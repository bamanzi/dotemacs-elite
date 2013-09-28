;;** vc
;;*** svn
(unless (fboundp 'vc-svn-root)  ;;fix emacs-23's svn support
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

;;*** git
(autoload 'magit-log  "magit"
  "Command for 'git log'." t)

(autoload 'magit-status "magit"
  "Open a Magit status buffer for the Git repository containing DIR." t)

;;*** hg
(autoload 'monky-log  "monky"
  "Undocumented." t)

(autoload 'monky-status  "monky"
  "Show the status of Hg repository." t)

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
