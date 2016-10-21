(when nil
  (progn
    ;; highlight headers in this file
    (highlight-lines-matching-regexp "^;;\\* "    'org-level-1)
    (highlight-lines-matching-regexp "^;;\\*\\* " 'org-level-2)
    (highlight-lines-matching-regexp "^;;\\*\\*\\* " 'org-level-3)
    (setq outline-regexp "^;;[\*]+ ")
    )    ;;<- put cursor here, press C-x C-e
  )

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

  ;;for cl-lib, pcase etc
  (add-to-list 'load-path (concat dotemacs-elite-dir "_libs") 'append)
  
  (add-to-list 'Info-default-directory-list
               (concat dotemacs-elite-dir "_info"))
  ;; reset `Info-directory-list' to force `info' to re-init it
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

  (add-to-list 'load-path "~/.emacs.d/lisp")
  
  (if (fboundp 'idle-require-mode)
      (idle-require-mode 1)))
      
;; ###################################################################################
;; TIPS: Sometimes you want to find out where a particular error,
;; warning or just plain annoying message in Messages is coming from.
;;
;; - [[http://www.emacswiki.org/emacs/DebugMessages][Debug Messages - EmacsWiki]]
;; - [[http://emacs.stackexchange.com/questions/5302/find-elisp-origin-of-warning/5306#530][debugging - Find elisp origin of warning - Emacs Stack Exchange]]

