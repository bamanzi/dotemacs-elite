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

(defun toggle-diff-hl-mode ()
  (if window-system
      (diff-hl-mode)
    (diff-hl-margin-mode)))

(global-set-key (kbd "<f10> d h") 'toggle-diff-hl-mode)


;; *** smeargle
(autoload 'smeargle "smeargle"
  "Highlight regions by last updated time." t)

(autoload 'smeargle-commits "smeargle"
  "Highlight regions by age of commits." t)
