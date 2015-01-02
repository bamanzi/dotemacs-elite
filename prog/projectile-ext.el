;;; projectile-ext.el -- some extensions for projectile

;; Author: Ba Manzi <bamanzi@gmail.com>
;; URL:    http://github.com/bamanzi/projectile
;;

;;; Code

(require 'projectile)
(require 'easymenu)

(unless (fboundp 'projectile-get-project-root)
  (defalias 'projectile-get-project-root 'projectile-project-root))
(unless (fboundp 'projectile-get-project-name)
  (defalias 'projectile-get-project-name 'projectile-project-name))


(defun projectile-create-project (dir)
  "Create a projectile project (actually create an empty file named .projectile)."
  (interactive "Dproject root dir:")
  (let ((prj-file (concat dir ".projectile")))
    (unless (file-exists-p prj-file)
      (with-temp-buffer
        (insert ";; This file is meant for projectile project.")
        (write-file prj-file))))
  (if (string-prefix-p dir (buffer-file-name))
      (projectile-on)))

(defun projectile-show-project-info ()
  "Show project info of current project."
  (interactive)
  (if projectile-mode
      (if (projectile-get-project-root)
          (message "Project: %s. Root dir: %s. Type: %s"
                   (propertize (projectile-get-project-name) :bold t)
                   (projectile-get-project-root)
                   (loop for file in projectile-project-root-files
                         when (locate-dominating-file default-directory file)
                         do (return file)))
        (message "No project found along the path of current file."))
    (message "projectile-mode not turned on.")))


(defun projectile-eshell-cd (dir)
  "If there is an EShell buffer, cd to DIR in that buffer."
  (interactive "D")
  (let* ((eshell-buf-p (lambda (buf)
                         (with-current-buffer buf (eq major-mode 'eshell-mode))))
         (eshell-win-p (lambda (win)
                         (let ((buf (window-buffer win)))
                           (with-current-buffer buf (eq major-mode 'eshell-mode)))))
         (eshell-win (find-if eshell-win-p (window-list)))
         (eshell-buf (find-if eshell-buf-p (buffer-list))))
    (if eshell-win
        (setq eshell-buf (window-buffer eshell-win)))
    (unless eshell-buf
      (eshell)
      (setq eshell-buf (current-buffer)))
    (with-current-buffer eshell-buf
      (goto-char (point-max))
      (eshell/cd dir)
      (eshell-send-input nil t)
      eshell-buf ;; returns eshell-buf so you can focus
      ; the window if you want
      )
    (if eshell-win
        (select-window eshell-win)
      (switch-to-buffer eshell-buf))))

(defun projectile-eshell-cd-root ()
  (interactive)
  (projectile-eshell-cd (projectile-get-project-root)))

(defun projectile-eshell-cd-current ()
  (interactive)
  (projectile-eshell-cd default-directory))

(defun projectile-ack-on-dir (dir)
  "Use `ack' to grep text across source files."
  (interactive
   (list (read-directory-name "Ack on dir: "
                              (projectile-get-project-root))))
  (unless (fboundp 'ack)
    (or (require 'ack)
        (require 'ack-and-a-half)))
  (let ((default-directory dir))
    (call-interactively 'ack)))

(defun projectile-grin-on-dir (dir)
  "Use `grin' to grep text across source files."
  (interactive
   (list (read-directory-name "Grin on dir: "
                              (projectile-get-project-root))))   
  (require 'grin)
  (let ((default-directory dir))
    (call-interactively 'grin)))

(defun projectile-grind-on-dir (dir)
  "Use `grind' to find file by name."
  (interactive
   (list (read-directory-name "Grind on dir: "
                              (projectile-get-project-root))))   
  (require 'grin)
  (let ((default-directory dir))
    (call-interactively 'grind)))


(defun projectile-dired (dir)
  "If there is an EShell buffer, cd to DIR in that buffer."
  (interactive
   (list (let ((default-directory (projectile-get-project-root)))
           (if (and (symbolp 'ido-mode) ido-mode)
               (ido-read-directory-name "Dired to: " nil nil 'must-match)
             (read-directory-name "Dired to: " nil nil 'must-match)))))
  (dired dir "-al"))

(defun projectile-find-file- (file)
  "Similar to `projectile-find-file', but not building all files as candidates."
  (interactive
   (list (let ((default-directory (projectile-get-project-root)))
           (if (and (symbolp 'ido-mode) ido-mode)
               (read-file-name "Find file: " nil nil 'must-match)))))
  (find-file file))


(easy-menu-define projectile-mode-menu projectile-mode-map
  "Menu for Projectile mode"
  '("Projectile"
    ["Find file" 'projectile-find-file-]
    ["Find file (grind)" projectile-grind-on-dir]
    ["Dired" projectile-dired]
    ["Recent files" projectile-recentf]    
    "--"
    ["Grep in project" (if (fboundp 'projectile-grep)
                           (projectile-grep)
                         (projectile-grep-in-project))]
    ["Grep in project (ack)"  projectile-ack-on-dir]
    ["Grep in project (grin)"  projectile-grin-on-dir]
    ["Replace in project" (if (fboundp 'projectile-replace)
                              (projectile-replace)
                            (projectile-replace-in-project))]
    ["Multi-occur in project" projectile-multi-occur]
    "--"
    ["Switch buffer" projectile-switch-to-buffer]
    ["Kill all buffers"  projectile-kill-buffers]
    "--"
    ["Find tag..."  projectile-find-tag]
    ["Regenerate etags" projectile-regenerate-tags]
    "--"
    ["Eshell cd to project root" projectile-eshell-cd-root]
    ["Eshell cd to current folder" projectile-eshell-cd-current]
    "--"
    ["Invalidate cache" (if (fboundp 'projectile-invalidate-cache)
                            (projectile-invalidate-cache)
                          (projectile-invalidate-project-cache))]
    ["Show project info" projectile-show-project-info]
    ["About" projectile-version]))


;;** integration with desktop-mode
(defun projectile-save-desktop ()
  (interactive)
  (if (projectile-get-project-root)
      (let ((desktop-file-name-format 'local) ;;Relative to directory of desktop file.
            (desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^\\.\\./\\)")) ;;exclude files not in project folder
        (desktop-save (projectile-get-project-root)))
    (message "Current directory is not in a project.")))
        
(defun projectile-load-desktop()
  (interactive)
  (if (projectile-get-project-root)
      (let ((desktop-restore-eager 5))
        (desktop-read (projectile-get-project-root)))
    (message "Current directory is not in a project.")))

;;** tabbar: group by project name
(defvar tabbar-buffer-projectile-project nil
  "project name. used as cache in `tabbar-buffer-groups-by-projectile'")
(make-variable-buffer-local 'tabbar-buffer-projectile-project)
  
(defun tabbar-buffer-groups-by-projectile ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on projectile's project."
  (list (cond
         ((and projectile-mode
               (or
                (buffer-file-name)
                (memq major-mode '(dired-mode comint-mode occur-mode grep-mode compilation-mode eshell-mode shell-mode))))
          (or tabbar-buffer-projectile-project
              (setq tabbar-buffer-projectile-project
                    (projectile-get-project-name))))
         ((= (aref (buffer-name) 0) ?*)
            (if (or (member (buffer-name) '("*scratch*" "*Buffer List*" "*Help*"))
                    (memq major-mode '()))
                "*utils*"
              "*temp*"))
         (t
          "*files*"))))

(defun tabbar-group-by-project ()
  (interactive)
  (when (or projectile-mode
          (yes-or-no-p "Projectile-mode seems not turned-on yet. Continue to enable it?"))
    (projectile-global-on)
    (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-projectile))
  )


(provide 'projectile-ext)
;;; projectile-ext.el ends here
