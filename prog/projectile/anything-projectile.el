(require 'projectile)
(require 'anything)

;;; project files
(defun anything-c-projectile-files-candidates ()
  (with-anything-current-buffer
    (projectile-current-project-files)))

(defvar anything-c-source-projectile-files
  `((name . "Projectile Files")
    (candidates . anything-c-projectile-files-candidates)
    (type . file)
    (match . anything-c-match-on-basename)
    (mode-line . "Projectile Files")
    )
  "Anything source definition for projectile files.")

(defun anything-with-projectile-files ()
  "Call `anything' with the projectile file source."
  (interactive)
  (cd (projectile-project-root))
  (anything :sources '(anything-c-source-projectile-files)))

;;; project dirs
(defun anything-c-projectile-dirs-candidates ()
  (with-anything-current-buffer
    (projectile-current-project-dirs)))

(defvar anything-c-source-projectile-dirs
  `((name . "Projectile Dirs")
    (candidates . anything-c-projectile-dirs-candidates)
    (type . file)
    (match . anything-c-match-on-basename)
    (mode-line . "Projectile Dirs")
    )
  "Anything source definition for projectile dirs.")

(defun anything-with-projectile-dirs ()
  "Call `anything' with the projectile dir source."
  (interactive)
  (cd (projectile-project-root))
  (anything :sources '(anything-c-source-projectile-dirs)))

;;; project buffers
(defun anything-c-projectile-buffers-candidates ()
  (with-anything-current-buffer
    (projectile-project-buffer-names)))

(defvar anything-c-source-projectile-buffers
  `((name . "Projectile buffers")
    (candidates . anything-c-projectile-buffers-candidates)
    (type . buffer)
    (mode-line . "Projectile Buffers")
    )
  "Anything source definition for projectile buffers")

(defun anything-with-projectile-buffers ()
  "Call `anything' with the projectile buffer source."
  (interactive)
  (anything :sources '(anything-c-source-projectile-buffers)))

(provide 'anything-projectile)
