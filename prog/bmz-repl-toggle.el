;;; bmz-repl-toggle --- toggle focus bwteen source buffer and repl buffer

;;
;; Copyright (C) 2013-2014 Ba Manzi
;;
;; Author: Ba Manzi <bamanzi * gmail com>
;; Version: 1.0
;; Keywords: shell repl buffers
;; URL: http://bitbucket.org/bamanzi/dotemacs-extra/src/default/prog/
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; This package provides the command `repl-toggle' which toggles between the
;; REPL buffer and file buffer you are editing.
;;
;; This is done in an "intelligent" way.  Features are:
;;
;;  - Starts a REPL buffer if non is existing (according to configuration in
;;    `repl-toggle-start-methods'
;;
;;  - Minimum distortion of your window configuration.
;;
;;  - When done in the repl-buffer you are returned to the same
;;    window configuration you had before you toggled to the repl buffer.
;;
;;  - You can convinently choose if you want to have the repl buffer in
;;    another window or in the whole frame.  Just invoke `repl-toggle'
;;    again to make the REPL buffer in the whole frame.
;;
;; This file has been tested under Emacs 24.3.
;;
;; To use, call the functions `repl-toggle'. It's most helpful to bind these
;; to a key.
;;
;; This package was inspired by `esh-toggle.el' and some code was stolen from
;; it.

;;; Code:

(defvar repl-toggle--target-buffer nil
  "The REPL buffer associated with current buffer.

If not nil, `repl-toggle` would switch to it.
Otherwise, `repl-toggle` would try to find or create one according to `repl-buffer-pairs`.

Command `repl-buffer-associate` would set this variable.")

(make-variable-buffer-local 'repl-toggle--target-buffer)

(defun repl-toggle-associate-repl-buffer (buffer)
  "Associate a REPL buffer with current buffer."
  (interactive "b")
  (setq repl-toggle--target-buffer buffer))


(defvar repl-toggle--last-source-buffer nil)
(defvar repl-toggle--pre-win-conf nil)

(defun repl-toggle--goto-repl-buffer (repl-mode new-repl-buffer-cmd)
  (let ((source-buffer (current-buffer))
        (repl-buffer (or (and repl-toggle--target-buffer (get-buffer repl-toggle--target-buffer))
                    (find-if #'(lambda (buffer)
                                       (with-current-buffer buffer
                                         (eq repl-mode major-mode)))
                             (buffer-list)))))
    (if repl-buffer
        (progn
          (message "repl-toggle: Switching to %s" repl-buffer)
          (setq repl-toggle--pre-win-conf (current-window-configuration))

          (make-variable-buffer-local 'repl-toggle--last-source-buffer)

          (with-current-buffer repl-buffer
            (setq repl-toggle--last-source-buffer source-buffer))
          (switch-to-buffer-other-window repl-buffer))
      (call-interactively new-repl-buffer-cmd)
      ;; FIXME: automatically switch to it?
      (message "New repl buffer created. Call `repl-toggle` again to switch to it"))))

(defun repl-toggle--return-to-source-buffer ()
  (let ((source-buffer repl-toggle--last-source-buffer))
    (if source-buffer        
        (if (and (eq (count-windows) 1)
                 (window-configuration-p repl-toggle--pre-win-conf))
            (set-window-configuration repl-toggle--pre-win-conf)
          (switch-to-buffer-other-window source-buffer)
          )
      (message "no source buffer found."))))


(setq repl-buffer-pairs
      '((python-mode     . inferior-python-mode)
        (emacs-lisp-mode . lisp-interaction-mode)
        ))

(defun repl-toggle--is-current-buffer-repl ()
  "Check whether current buffer is a REPL buffer
according to configurationin `repl-buffer-pairs."
  ;;FIXME: parent mode?
  (let ((current-mode major-mode))
    (cond
     ((assoc-default current-mode repl-buffer-pairs)
      nil)
     ((rassq current-mode repl-buffer-pairs)
      t)
     )))

;;;###autoload
(defun repl-toggle ()
  "Toggle between the REPL buffer and the source buffer.

Call twice in a row to get a full screen window for the REPL
buffer.

When called in the REPL buffer returns you to the buffer you were
editing before caling the first time."
  (interactive)
  (if (repl-toggle--is-current-buffer-repl)
      (if (and (eq last-command 'repl-toggle)
               (not (eq (count-windows) 1)))
          (progn
            ;; maximize the repl window
            (delete-other-windows)
            (message "repl-toggle: maximize the repl buffer."))
        (progn
          ;; switch back to source buffer            
          (repl-toggle--return-to-source-buffer)
          (message "repl-toggle: return to source buffer.")))
    ;; switch to repl buffer
    (let ((repl-mode (assoc-default major-mode repl-buffer-pairs)))
      (if repl-mode
          ;;FIXME: passing `new-repl-buffer-cmd`
          (repl-toggle--goto-repl-buffer repl-mode nil)))))

;;; bmz-repl-toggle.el ends here
