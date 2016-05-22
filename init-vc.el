;; ** vc
;; *** svn
(unless (fboundp 'vc-svn-root)  ;;fix emacs-23's svn support
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

;; *** git
(autoload 'magit-log  "magit"
  "Command for 'git log'." t)

(autoload 'magit-status "magit"
  "Open a Magit status buffer for the Git repository containing DIR." t)

;; *** hg
(autoload 'monky-log  "monky"
  "Undocumented." t)

(autoload 'monky-status  "monky"
  "Show the status of Hg repository." t)

;; ** highlight changes

(global-highlight-changes-mode 1)
(global-set-key (kbd "<f10> h c") 'highlight-changes-visible-mode)


;; *** diff-hl
(autoload 'diff-hl-mode  "diff-hl"
  "Toggle VC diff fringe highlighting." t)
(autoload 'global-diff-hl-mode "diff-hl"
  "Toggle Diff-Hl mode in every possible buffer." t)

(autoload 'diff-hl-margin-mode "diff-hl-margin"
  "Toggle displaying `diff-hl-mode' highlights on the margin." t)

(autoload 'diff-hl-dired-mode "diff-hl-dired"
  "Toggle VC diff highlighting on the side of a Dired window." t)
  
;;(add-hook 'prog-mode-hook 'diff-hl-mode)

(defun diff-hl-mode-fringe-or-margin ()
  "Toggle `diff-hl-mode' or `diff-hl-margin-mode' according to current frame's display."
  (interactive)
  (if window-system
      (diff-hl-mode)
    (diff-hl-margin-mode)))

(global-set-key (kbd "<f10> d h") 'diff-hl-mode-fringe-or-margin)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'VC
                     :key "M-x diff-hl-mode"
                     :description "Toggle VC diff fringe highlighting.")
     (cheatsheet-add :group 'VC
                     :key "M-x diff-hl-margin-mode"
                     :description "Toggle displaying `diff-hl-mode' highlights on the margin.")
     t))

;; *** smeargle
(autoload 'smeargle "smeargle"
  "Highlight regions by last updated time." t)

(autoload 'smeargle-commits "smeargle"
  "Highlight regions by age of commits." t)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'VC
                     :key "M-x smeargle"
                     :description "Highlight regions by last updated time." t)
     (cheatsheet-add :group 'VC
                     :key "M-x smeargle-commits"
                     :description "Highlight regions by age of commits." t)
     t
     ))



