(when nil
  (progn
    ;; highlight headers in this file
    (highlight-lines-matching-regexp "^;;\\* "    'org-level-1)
    (highlight-lines-matching-regexp "^;;\\*\\* " 'org-level-2)
    (highlight-lines-matching-regexp "^;;\\*\\*\\* " 'org-level-3)
    (setq outline-regexp "^;;[\*]+ ")
    )    ;;<- put cursor here, press C-x C-e
  )

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq dotemacs-elite-dir (if load-file-name
                    (file-name-directory load-file-name)
                  default-directory))
(progn
  (add-to-list 'load-path dotemacs-elite-dir)
  (mapc #'(lambda (file)
            (when (and (file-directory-p file)
                       (not (file-exists-p (concat file "/.nosearch"))))
              (message "Prepending %s to load-path" file)
              (add-to-list 'load-path file)
              (let ((default-directory file))
                (normal-top-level-add-subdirs-to-load-path))))
        (directory-files dotemacs-elite-dir 'full "^[a-z][^\\.]+"))

  ;;for cl-lib
  (if (< emacs-major-version 24)
      (add-to-list 'load-path (concat dotemacs-elite-dir "_extra")))

  (add-to-list 'Info-default-directory-list
               (concat dotemacs-elite-dir "_extra"))
  (setq Info-directory-list nil)

  
  (setq idle-require-idle-delay 5
        idle-require-load-break 1)

  (unless (load "idle-require" t)
    (defun idle-require (feature)
      (require feature)))
  
  (mapc #'(lambda (file)
;;	    (unless (ignore-errors          
                  (load-file file))
;;          (message "Failed %s" file)))
	(directory-files dotemacs-elite-dir 'full "^init-\.*.el$"))

  (if (fboundp 'idle-require-mode)
      (idle-require-mode 1)))
      
      
