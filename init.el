(when nil
  (progn
    ;; highlight headers in this file
    (highlight-lines-matching-regexp "^;;\\* "    'org-level-1)
    (highlight-lines-matching-regexp "^;;\\*\\* " 'org-level-2)
    (highlight-lines-matching-regexp "^;;\\*\\*\\* " 'org-level-3)
    (setq outline-regexp "^;;[\*]+ ")
    )    ;;<- put cursor here, press C-x C-e
  )


(let ((this_dir (if load-file-name
                    (file-name-directory load-file-name)
                  default-directory)))
  (let ((default-directory this_dir))
    (add-to-list 'load-path this_dir)
    (normal-top-level-add-subdirs-to-load-path))

  (setq idle-require-idle-delay 10
        idle-require-load-break 1)

  (unless (load "idle-require" t)
    (defun idle-require (feature)
      (require feature)))
  
  (mapc #'(lambda (file)
;;	    (unless (ignore-errors          
                  (load-file file))
;;          (message "Failed %s" file)))
	(directory-files this_dir 'full "^init-\.*.el$"))

  (if (fboundp 'idle-require-mode)
      (idle-require-mode 1)))
      
