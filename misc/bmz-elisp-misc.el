;; bmz-elisp-misc.el -- some elisp utilities

;; Author: Ba Manzi <bamanzi AT gmail.com>

;;** load-and-execute
(defun load-and-execute (library)
  "load a library 'foobar' and execute the command with same name:
`foobar' or `foobar-mode'"
  (interactive
   (list (completing-read "Load library: "
                          (apply-partially 'locate-file-completion-table
                                           load-path
                                           (get-load-suffixes)))))
  (when (load library)
    (let ( (command (if (fboundp (intern library))
                        (intern library)
                      (intern (concat library "-mode")))) )
      (message "try to execute `%s'" command)
      (call-interactively command))))

(global-set-key (kbd "M-X") 'load-and-execute)


;;** helper for writing "(autoload ...." statement
(defun insert-function-autoload-spec (function)
  "Insert the first line of documentation of a function.

Useful when writing autoload spec."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t nil nil
				(and fn (symbol-name fn))))
     (list (if (equal val "")
	       fn (intern val)))))
  (require 'eldoc)
  (if (null function)
      (message "You didn't specify a function")
    (insert (format " \"%s\"\n  \"%s\" t)"
                    (replace-regexp-in-string ".elc?$" "" (file-name-nondirectory (symbol-file function 'defun)))
                    (or (elisp--docstring-first-line (documentation function t))
                        "Undocumented.")    ))))

(defalias 'ifas 'insert-function-autoload-spec)

;;** modeline helper
(defun mode-line-install-element (element &optional position)
  "Install an ELEMENT to mode-line.

POSITION could be an integer or another element (such as `mode-line-buffer-identification').
New ELEMENT would be installed *after* POSITION."
  (let ((mode-line (default-value 'mode-line-format))
        (res)
	(position (or position 1)))
    (while (and position mode-line)
      (push (car mode-line) res)
      (if (integerp position)
	  (progn
	    (setq position (- position 1))
	    (if (eq position 0)
		(setq position nil)))
	(if (equal (car mode-line) position)
	    (setq position nil)))
      (pop mode-line))
    (push element res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res))
    (force-mode-line-update t))
    )


(defun mode-line-uninstall-element (element)
  "Remove an ELEMENT from the mode-line.

ELEMENT could be the same value of an existing element,
or the car of it, if that element is a cons cell.

e.g. 
 (mode-line-uninstall-element 
 '(which-func-mode
  ("" which-func-format
   #(" " 0 1
     (help-echo ...))))
or just:
  (mode-line-uninstall-element 'which-func-mode)
"
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (if (consp item)
                    (equal (car item) element)
                  (equal item element))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))


;;** if nothing marked, use current line as region 
;; http://pastebin.com/G7N4F4eE
;; from aquamacs emacs
;; if nothing marked, use current line as region
(defmacro allow-line-as-region-for-function (orig-function)
  `(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
     ()
     ,(format "Like `%s', but acts on the current line if mark is not active." orig-function)
     (interactive)
     (if mark-active
         (call-interactively (function ,orig-function))
       (save-excursion
         ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
         (beginning-of-line)
         (set-mark (point))
         (end-of-line)
         (call-interactively (function ,orig-function))))))

(allow-line-as-region-for-function kill-ring-save)
(allow-line-as-region-for-function kill-region)

;; (allow-line-as-region-for-function comment-region)
;; (allow-line-as-region-for-function uncomment-region)
;; (allow-line-as-region-for-function comment-or-uncomment-region)


;;** loaddefs.el generator
;;based on code from http://stackoverflow.com/a/4189794
(defun _do-update-autoloads-for-dir (basedir recursively)
  (require 'autoload)         ;ironic, i know
  (when (not (file-exists-p generated-autoload-file))
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert ";;") ;; create the file with non-zero size to appease autoload
      (save-buffer)))
  (update-directory-autoloads basedir)
  (if recursively
      (mapc #'(lambda (subdir)
                (message "=== %s" subdir)
                (when (and (file-directory-p subdir)
                           (not (string= "." subdir))
                           (not (string= ".." subdir)))
                  (cd subdir)
                  (_do-update-autoloads-for-dir subdir recursively)))
            (directory-files basedir 'full))))
  
(defun update-autoloads-for-dir (basedir)
  "Update autoloads for files in the diretory BASE."
  (interactive "DDirectory: ")
  (let ((generated-autoload-file (concat basedir "loaddefs.el")))
    (_do-update-autoloads-for-dir basedir 'recursive)))


;;** find-symbol-at-point
(defun find-symbol-at-point (symbol)
  "Find the definiton of the SYMBOL near point.

This is a front-end function for `find-function', `find-variable',
`find-face-definition' & `find-library'."
  (interactive
   (list (intern-soft (read-string "Symbol: "
                                   (thing-at-point 'symbol)))))
  (cond
   ((fboundp symbol)
    (find-function symbol))
   ((boundp symbol)
    (find-variable symbol))
   ((facep symbol)
    (find-face-definition symbol))
   ((featurep symbol)
    ;;not accurate (if package name is different than feature name), but in most cases that's enough
    (find-library (symbol-name symbol)))    
   (t
    (message "Unknown symbol: %s" symbol))))

(defun describe-symbol-at-point (symbol)
  "Describe the SYMBOL near point.

This is a front-end function for `describe-function', `describe-variable',
`describe-face' & `describe-package'."
  (interactive
   (list (intern-soft (read-string "Symbol: "
                                   (thing-at-point 'symbol)))))
  (let ((ffap-file-finder 'find-file-other-window))
    (cond
     ((fboundp symbol)
      (describe-function symbol))
     ((boundp symbol)
      (describe-variable symbol))
     ((facep symbol)
      (describe-face symbol))
     ((featurep symbol)
      ;;not accurate (if package name is different than feature name), but in most cases that's enough
      (if (fboundp 'describe-package) ;; emacs-24 provides this
          (describe-package (symbol-name symbol))
        (finder-commentary (symbol-file symbol))))
     (t
      (message "Unknown symbol: %s" symbol))))
  )
  

(eval-after-load "lisp-mode"
  `(progn
     (define-key emacs-lisp-mode-map (kbd "C-c .")         'find-symbol-at-point)
     (define-key emacs-lisp-mode-map (kbd "C-h M-s")       'describe-symbol-at-point)
     (define-key lisp-interaction-mode-map (kbd "C-c .")   'find-symbol-at-point)
     (define-key lisp-interaction-mode-map (kbd "C-h M-s") 'describe-symbol-at-point)
     ))

(provide 'bmz-elisp-misc)
;;; bmz-elisp-misc.el ends here

