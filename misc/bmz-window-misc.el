;;; bmz-window-misc.el --- misc commands for manuplating windows
;;
;; Filename: bmz-window-misc.el
;; Description: Misc commands for manipulating windows
;; Author: Ba Manzi <bamanzi@gmail.com>
;; Copyright (C) 2013, Ba Manzi, all rights reserved.
;; Created: <2013-10-09 Tue>
;; Version: 0.5
;; URL: http://bitbucket.org/bamanzi/dotemacs-elite/src/default/misc/bmz-window-misc.el
;; Keywords: windows
;; Compatibility: GNU Emacs:  23.x


;;** jump by buffer name
;;;###autoload
(defun ido-jump-to-window ()
  "Jump to window by current buffer name."
  (interactive)
  (defun swap(l)
    (if (cdr l)
	(cons (cadr l) (cons (car l) (cddr l)))
      l))
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (swap (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
                (delq nil (mapcar '(lambda (window)
                                       (if (equal buffer-name (buffer-name (window-buffer window)))
                                           window
                                         nil))
                                  (window-list))))
      (select-window (car window-of-buffer)))))

;;;###autoload
(defun focus-or-switch-to-buffer (buffer)
  "If buffer currently shows in a window of current frame, focus it.
Otherwise call `switch-to-buffer'."
  (interactive "bBuffer: ")
  (let ((win (find-if (lambda (win)
                        (string= buffer
                                 (buffer-name (window-buffer win))))
                      (window-list))))
    (if win
        (select-window win)
      (switch-to-buffer buffer))))

;;;###autoload
(defun move-buffer-to-largest-window()
  "Open current buffer in largest window"
  (interactive)
  (let ((oldbuf (current-buffer)))
  (select-window (get-largest-window))
  (switch-to-buffer oldbuf))
)

;;** swap with or move to
(defun swap-or-move-buffer-between-windows (other-window swap &optional this-window)
  "If SWAP, swap WINDOW's buffer with OTHER-WINDOW's.
Otherwise, move WINDOW's buffer to OTHER-WINDOW."
  (let ( (window (or this-window
                     (selected-window))) )
    (cond ( (null other-window)
            (error "No window %s from selected window" dir))
          ( (and (window-minibuffer-p other-window)
                 (not (minibuffer-window-active-p other-window)))
            (error "Minibuffer is inactive"))
          ( (window-dedicated-p window)
            (error "Current window is dedicated, can't be moved") )
          ( (window-dedicated-p other-window)
            (error "Target window is dedicated, can't be swapped") )
          (t
           (let ( (this-buffer (window-buffer window))
                  (other-buffer (window-buffer other-window)))
             (if (eq this-buffer other-buffer)
                 (let ( (this-point (window-point window))
                        (other-point (window-point other-window)) )
                   (progn
                     (set-window-point window other-point)
                     (set-window-point other-window this-point)))
               (progn
                 (if swap
                     (set-window-buffer window (window-buffer other-window)))
                 (set-window-buffer other-window this-buffer)))
             (select-window other-window))))))


;;*** move/swap by direction (two windows)
;; modified from windmove-do-window-select
(defun windmove-do-swap-window (dir swap &optional arg window)
  "Move the buffer to the window at direction DIR.

If SWAP is non-nil, the buffers in the source window and target
window would be swapped, otherwise only the source buffer be
moved to target window.  DIR, ARG, and WINDOW are handled as by
`windmove-other-window-loc'.  If no window is at direction DIR,
an error is signaled."
  (let ((other-window (windmove-find-other-window dir arg window)))
    (swap-or-move-buffer-between-windows other-window swap window)))


(defun swap-buffer-up (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'up t arg))

(defun swap-buffer-down (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'down t arg))

(defun swap-buffer-left (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'left t arg))

(defun swap-buffer-right (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'right t arg))

(defun move-buffer-up (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'up nil arg))

(defun move-buffer-down (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'down nil arg))

(defun move-buffer-left (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'left nil arg))

(defun move-buffer-right (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'right nil arg))

;;*** move/swap by window name
(defun ido-move-or-swap-window-buffer (justmove)
  (let (windows)
    (mapc '(lambda (window)
             (if (and (not (window-dedicated-p window))
                      (not (eq window (selected-window))))
               (add-to-list 'windows (format "%s" window))))
          (window-list))
;;    (message "%s" (car (window-list)))
    (let ( (target-window (ido-completing-read
                           (if justmove
                               "Move window to: "
                             "Swap window with: ")
                           windows)) )
      (mapc '(lambda (window)
               (if (string= target-window (format "%s" window))
                   (swap-or-move-buffer-between-windows window (not justmove))))
            (window-list)))
    ))

(defun ido-swap-window-buffer-with ()
  "Swap the window's buffer with another window."
  (interactive)
  (ido-move-or-swap-window-buffer nil))

(defun ido-move-window-buffer-to ()
  (interactive)
  (ido-move-or-swap-window-buffer 'justmove))

;;** rotate
;; https://github.com/banister/window-rotate-for-emacs
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(provide 'bmz-window-misc)
;;; bmz-window-misc.el
