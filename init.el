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

  ;;(require 'idle-require nil t)

  (if (fboundp 'idle-require)
      (defun idle-require (feature)
	(require feature)))
  (mapc #'(lambda (file)
	    (unless (ignore-errors
                  (load-file file))
          (message "Failed %s" file)))
	(directory-files this_dir 'full "^init-\.*.el$")))
