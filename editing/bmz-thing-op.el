;;; bmz-thing-op --- mark, copy, kill thing at point

;; based on code stolen from https://github.com/m2ym/thingopt-el/blob/master/thingopt.el

;;; Code:

(require 'thingatpt)

(setq thing/name-map
  '((?w . word)
    (?e . sexp)
    (?s . symbol)
    (?S . sentence)
    (?p . paragraph)
    (?h . defun)
    (?f . filename)
    (?l . line)
    (?\( . list)
    (?L . list)
    (?\" . string)
    (?u . url)
    (?P . page)
    (?n . number)
    ))

(defun thing/read-thing (quick &optional prompt)
  "Ask user to select a THING name.

When QUICK is true, it read in one char matching key of `thing/name-map'.
Otherwise it requires user to input full thing name (value of `thing/name-map`)."
  (if quick
      (assoc-default (read-key (concat (or prompt "Thing")
                                       " ["
                                       (mapconcat #'(lambda (elem)
                                                      (format "%c:%s" (car elem) (cdr elem)))
                                                  thing/name-map " ")
                                       "]: "))
                     thing/name-map)
    (let ((thing-name (ido-completing-read (or prompt "Thing: ")
                                      (mapcar #'(lambda (elem)
                                                  (format "%s" (cdr elem)))
                                              thing/name-map)
                                      nil
                                      'match)))
      (if thing-name
          (intern thing-name)))))
                  

(defun thing/call-action (thing action &optional prompt)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (cond
     (bounds
      (funcall action bounds))
     (thing
      (message "There is no %s here." thing))
     (t
      (message "Nothing here.")))))

(defun thing/mark-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (car bounds))
                         (push-mark (cdr bounds) nil transient-mark-mode)
                         (message "Markd %s." thing))))

(global-set-key (kbd "M-` SPC") 'thing/mark-one-thing)

(idle-require 'pulse) ;; for `pulse-momentary-highlight-region'
(defun thing/copy-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (let ((begin (car bounds))
                               (end   (cdr bounds)))
                           (if (require 'pulse nil t)
                               (pulse-momentary-highlight-region begin end))
                           (copy-region-as-kill begin end)
                           (message "Copied %s." thing)))))

(defun thing/kill-one-thing (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (let ((begin (car bounds))
                               (end   (cdr bounds)))
                           (if (require 'pulse nil t)
                               (pulse-momentary-highlight-region begin end))
                           (kill-region begin end)
                           (message "Killed %s." thing)))))

(defun thing/goto-beginning (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (car bounds)))
                     "Go to thing begin "))

(defun thing/goto-end (thing)
  (interactive (list (thing/read-thing 'quick)))
  (thing/call-action thing
                     #'(lambda (bounds)
                         (goto-char (cdr bounds)))
                     "Go to thing end "))


(defun thing/copy-symbol-or-word ()
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                    (bounds-of-thing-at-point 'word))))
    (if bounds
        (let* ((begin (car bounds))
               (end   (cdr bounds))
               (content (buffer-substring begin end)))
          (if (require 'pulse nil t)
              (pulse-momentary-highlight-region begin end))
          (copy-region-as-kill begin end)
          (message "Copied '%s'." content)))))


(defun thing/kill-symbol-or-word ()
  (interactive)
  (thing/kill-one-thing 'symbol))

;;; bmz-thing-op.el ends here
