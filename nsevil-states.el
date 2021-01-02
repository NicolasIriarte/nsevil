;;; nsevil-states.el --- States -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.14.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Nsevil.
;;
;; Nsevil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Nsevil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Nsevil.  If not, see <http://www.gnu.org/licenses/>.

(require 'nsevil-core)

;;; Code:

;;; Normal state

(nsevil-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " <N> "
  :enable (motion)
  ;; :exit-hook (nsevil-repeat-start-hook)
  (cond
   ((nsevil-normal-state-p)
    (overwrite-mode -1)
    (add-hook 'post-command-hook #'nsevil-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook #'nsevil-normal-post-command t))))

(defun nsevil-normal-post-command (&optional command)
  "Reset command loop variables in Normal state.
Also prevent point from reaching the end of the line.
If the region is activated, enter Visual state."
  (unless (or (nsevil-initializing-p)
              (null this-command))
    (setq command (or command this-command))
    (when (nsevil-normal-state-p)
      (setq nsevil-this-type nil
            nsevil-this-operator nil
            nsevil-this-motion nil
            nsevil-this-motion-count nil
            nsevil-inhibit-operator nil
            nsevil-inhibit-operator-value nil)
      (unless (memq command '(nsevil-use-register
                              digit-argument
                              negative-argument
                              universal-argument
                              universal-argument-minus
                              universal-argument-more
                              universal-argument-other-key))
        (setq nsevil-this-register nil))
      ;; (nsevil-adjust-cursor)
      )))
(put 'nsevil-normal-post-command 'permanent-local-hook t)

;;; Insert state

(nsevil-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  ;; :entry-hook (nsevil-start-track-last-insertion)
  :exit-hook (nsevil-cleanup-insert-state ;; nsevil-stop-track-last-insertion
                                          )
  :input-method t
  (cond
   ((nsevil-insert-state-p)
    ;; (add-hook 'post-command-hook #'nsevil-maybe-remove-spaces)
    ;; (add-hook 'pre-command-hook #'nsevil-insert-repeat-hook)
    ;; (setq nsevil-maybe-remove-spaces t)
    ;; (unless (eq nsevil-want-fine-undo t)
    ;;   (nsevil-start-undo-step))
    )
   (t
    ;; (remove-hook 'post-command-hook #'nsevil-maybe-remove-spaces)
    ;; (remove-hook 'pre-command-hook #'nsevil-insert-repeat-hook)
    ;; (nsevil-maybe-remove-spaces t)
    ;; (setq nsevil-insert-repeat-info nsevil-repeat-info)
    ;; (nsevil-set-marker ?^ nil t)
    ;; (unless (eq nsevil-want-fine-undo t)
    ;;   (nsevil-end-undo-step))
    )))

;; (defun nsevil-insert-repeat-hook ()
;;   "Record insertion keys in `nsevil-insert-repeat-info'."
;;   (setq nsevil-insert-repeat-info (last nsevil-repeat-info))
;;   (remove-hook 'pre-command-hook #'nsevil-insert-repeat-hook))
;; (put 'nsevil-insert-repeat-hook 'permanent-local-hook t)

(defun nsevil-cleanup-insert-state ()
  "Called when Insert state is about to be exited.
Handles the repeat-count of the insertion command."

  )

;;; Operator-Pending state

(nsevil-define-state operator
  "Operator-Pending state."
  :tag " <O> "
  :cursor nsevil-half-cursor
  :enable (nsevil-operator-shortcut-map operator motion normal))

(nsevil-define-keymap nsevil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq nsevil-operator-shortcut-map (make-sparse-keymap))
  (nsevil-initialize-local-keymaps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun nsevil-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let ((height (/ (window-pixel-height) (* (window-height) 2))))
    (setq cursor-type (cons 'hbar height))))

;;; Replace state

(nsevil-define-state replace
  "Replace state."
  :tag " <R> "
  :cursor hbar
  :message "-- REPLACE --"
  :input-method t
  (cond
   ((nsevil-replace-state-p)
    (overwrite-mode 1)
    (add-hook 'pre-command-hook #'nsevil-replace-pre-command nil t)
    ;; (unless (eq nsevil-want-fine-undo t)
    ;;   (nsevil-start-undo-step))
    )
   (t
    (overwrite-mode -1)
    (remove-hook 'pre-command-hook #'nsevil-replace-pre-command t)
    ;; (unless (eq nsevil-want-fine-undo t)
    ;;   (nsevil-end-undo-step))
    ;; (nsevil-move-cursor-back)
    ))
  (setq nsevil-replace-alist nil))

(defun nsevil-replace-pre-command ()
  "Remember the character under point."
  (when (nsevil-replace-state-p)
    (unless (assq (point) nsevil-replace-alist)
      (add-to-list 'nsevil-replace-alist
                   (cons (point)
                         (unless (eolp)
                           (char-after)))))))
(put 'nsevil-replace-pre-command 'permanent-local-hook t)

(defun nsevil-replace-backspace ()
  "Restore character under cursor."
  (interactive)
  (let (char)
    (backward-char)
    (when (assq (point) nsevil-replace-alist)
      (setq char (cdr (assq (point) nsevil-replace-alist)))
      (save-excursion
        (delete-char 1)
        (when char
          (insert char))))))

;;; Motion state

(nsevil-define-state motion
  "Motion state."
  :tag " <M> "
  :suppress-keymap t)

;;; Emacs state

(nsevil-define-state emacs
  "Emacs state."
  :tag " <E> "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(provide 'nsevil-states)

;;; nsevil-states.el ends here
