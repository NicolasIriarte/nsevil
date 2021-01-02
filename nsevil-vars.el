;;; nsevil-vars.el --- Settings and variables -*- lexical-binding: t -*-

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

;;; Code:

;; (declare-function nsevil-add-command-properties "nsevil-common"
;;                   (command &rest properties))
;; (declare-function nsevil-update-insert-state-bindings "nsevil-maps"
;;                   (&optional _option-name remove force))

;; ;;; Hooks

;; (defvar nsevil-after-load-hook nil
;;   "Functions to be run when loading of Nsevil is finished.
;; This hook can be used the execute some initialization routines
;; when Nsevil is completely loaded.")

;; (defcustom nsevil-goto-definition-functions
;;   '(nsevil-goto-definition-imenu
;;     nsevil-goto-definition-semantic
;;     nsevil-goto-definition-xref
;;     nsevil-goto-definition-search)
;;   "List of functions run until success by `nsevil-goto-definition'."
;;   :type 'hook
;;   :group 'nsevil)

;; ;;; Initialization

;; (defvar nsevil-pending-custom-initialize nil
;;   "A list of pending initializations for custom variables.
;; Each element is a triple (FUNC VAR VALUE). When Nsevil is
;; completely loaded then the functions (funcall FUNC VAR VALUE) is
;; called for each element. FUNC should be a function suitable for
;; the :initialize property of `defcustom'.")

;; (defun nsevil-custom-initialize-pending-reset (var value)
;;   "Add a pending customization with `custom-initialize-reset'."
;;   (push (list 'custom-initialize-reset var value)
;;         nsevil-pending-custom-initialize))

;; (defun nsevil-run-pending-custom-initialize ()
;;   "Executes the pending initializations.
;; See `nsevil-pending-custom-initialize'."
;;   (dolist (init nsevil-pending-custom-initialize)
;;     (apply (car init) (cdr init)))
;;   (remove-hook 'nsevil-after-load-hook 'nsevil-run-pending-custom-initialize))
;; (add-hook 'nsevil-after-load-hook 'nsevil-run-pending-custom-initialize)

;; ;;; Setters

;; (defun nsevil-set-toggle-key (key)
;;   "Set `nsevil-toggle-key' to KEY.
;; KEY must be readable by `read-kbd-macro'."
;;   (let ((old-key (read-kbd-macro
;;                   (if (boundp 'nsevil-toggle-key)
;;                       nsevil-toggle-key
;;                     "C-z")))
;;         (key (read-kbd-macro key)))
;;     (with-no-warnings
;;       (dolist (pair '((nsevil-motion-state-map nsevil-emacs-state)
;;                       (nsevil-insert-state-map nsevil-emacs-state)
;;                       (nsevil-emacs-state-map nsevil-exit-emacs-state)))
;;         (when (boundp (car pair))
;;           (let ((map (symbol-value (car pair)))
;;                 (fun (cadr pair)))
;;             (when (keymapp map)
;;               (define-key map key fun)
;;               (define-key map old-key nil))))))))

;; (defun nsevil-set-custom-state-maps (var pending-var key _make newlist)
;;   "Changes the list of special keymaps.
;; VAR         is the variable containing the list of keymaps.
;; PENDING-VAR is the variable containing the list of the currently pending
;;             keymaps.
;; KEY         the special symbol to be stored in the keymaps.
;; MAKE        the creation function of the special keymaps.
;; NEWLIST     the list of new special keymaps."
;;   (set-default pending-var newlist)
;;   (when (default-boundp var)
;;     (dolist (map (default-value var))
;;       (when (and (boundp (car map))
;;                  (keymapp (default-value (car map))))
;;         (define-key (default-value (car map)) (vector key) nil))))
;;   (set-default var newlist)
;;   (nsevil-update-pending-maps))

;; (defun nsevil-update-pending-maps (&optional _file)
;;   "Tries to set pending special keymaps.
;; This function should be called from an `after-load-functions'
;; hook."
;;   (let ((maps '((nsevil-make-overriding-map . nsevil-pending-overriding-maps)
;;                 (nsevil-make-intercept-map . nsevil-pending-intercept-maps))))
;;     (while maps
;;       (let* ((map (pop maps))
;;              (make (car map))
;;              (pending-var (cdr map))
;;              (pending (symbol-value pending-var))
;;              newlist)
;;         (while pending
;;           (let* ((map (pop pending))
;;                  (kmap (and (boundp (car map))
;;                             (keymapp (symbol-value (car map)))
;;                             (symbol-value (car map))))
;;                  (state (cdr map)))
;;             (if kmap
;;                 (funcall make kmap state)
;;               (push map newlist))))
;;         (set-default pending-var newlist)))))

;; (defun nsevil-set-visual-newline-commands (var value)
;;   "Set the value of `nsevil-visual-newline-commands'.
;; Setting this variable changes the properties of the appropriate
;; commands."
;;   (with-no-warnings
;;     (when (default-boundp var)
;;       (dolist (cmd (default-value var))
;;         (nsevil-set-command-property cmd :exclude-newline nil)))
;;     (set-default var value)
;;     (dolist (cmd (default-value var))
;;       (nsevil-set-command-property cmd :exclude-newline t))))

;; (defun nsevil-set-custom-motions (var values)
;;   "Sets the list of motion commands."
;;   (with-no-warnings
;;     (when (default-boundp var)
;;       (dolist (motion (default-value var))
;;         (nsevil-add-command-properties motion :keep-visual nil :repeat nil)))
;;     (set-default var values)
;;     (mapc #'nsevil-declare-motion (default-value var))))

;; ;;; Customization group

(defgroup nsevil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'nsevil-)

;; (defcustom nsevil-auto-indent t
;;   "\\<nsevil-normal-state-map>
;; Whether to auto-indent when opening lines with \\[nsevil-open-below] \
;; and \\[nsevil-open-above]."
;;   :type  'boolean
;;   :group 'nsevil)
;; (make-variable-buffer-local 'nsevil-auto-indent)

;; (defcustom nsevil-shift-width 4
;;   "\\<nsevil-normal-state-map>
;; The number of columns by which a line is shifted.
;; This applies to the shifting operators \\[nsevil-shift-right] and \
;; \\[nsevil-shift-left]."
;;   :type 'integer
;;   :group 'nsevil)
;; (make-variable-buffer-local 'nsevil-shift-width)

;; (defcustom nsevil-shift-round t
;;   "\\<nsevil-normal-state-map>
;; Whether shifting rounds to the nearest multiple.
;; If non-nil, \\[nsevil-shift-right] and \\[nsevil-shift-left] adjust line
;; indentation to the nearest multiple of `nsevil-shift-width'."
;;   :type 'boolean
;;   :group 'nsevil)
;; (make-variable-buffer-local 'nsevil-shift-round)

;; (defcustom nsevil-indent-convert-tabs t
;;   "\\<nsevil-normal-state-map>
;; If non-nil, the \\[nsevil-indent] operator converts between leading tabs and spaces.
;; Whether tabs are converted to spaces or vice versa depends on the
;; value of `indent-tabs-mode'."
;;   :type 'boolean
;;   :group 'nsevil)

(defcustom nsevil-default-cursor t
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'nsevil)

(defvar nsevil-force-cursor nil
  "Overwrite the current states default cursor.")

;; (defcustom nsevil-repeat-move-cursor t
;;   "\\<nsevil-normal-state-map>
;; Whether repeating commands with \\[nsevil-repeat] may move the cursor.
;; If nil, the original cursor position is preserved, even if the command
;; normally would have moved the cursor."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-cross-lines nil
;;   "\\<nsevil-motion-state-map>
;; Whether horizontal motions may move to other lines.  If non-nil,
;; certain motions that conventionally operate in a single line may move
;; the cursor to other lines.  Otherwise, they are restricted to the
;; current line.  This applies to \\[nsevil-backward-char], \
;; \\[nsevil-forward-char], \\[nsevil-find-char], \
;; \\[nsevil-find-char-backward], \\[nsevil-find-char-to], \
;; \\[nsevil-find-char-to-backward], \
;; \\<nsevil-normal-state-map>\\[nsevil-invert-char]."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-backspace-join-lines t
;;   "Whether backward delete in insert state may join lines."
;;   :type 'boolean
;;   :group 'nsevil)

(defcustom nsevil-move-cursor-back t
  "Whether the cursor is moved backwards when exiting insert state.
If non-nil, the cursor moves \"backwards\" when exiting insert state,
so that it ends up on the character to the left.  Otherwise it remains
in place, on the character to the right."
  :type 'boolean
  :group 'nsevil)

(defcustom nsevil-move-beyond-eol nil
  "Whether the cursor can move past the end of the line.
If non-nil, the cursor is allowed to move one character past the
end of the line, as in Emacs."
  :type 'boolean
  :group 'nsevil)

;; (defcustom nsevil-respect-visual-line-mode nil
;;   "\\<nsevil-motion-state-map>
;; Whether movement commands respect `visual-line-mode'.
;; If non-nil, `visual-line-mode' is generally respected when it is
;; on.  In this case, motions such as \\[nsevil-next-line] and
;; \\[nsevil-previous-line] navigate by visual lines (on the screen) rather
;; than \"physical\" lines (defined by newline characters).  If nil,
;; the setting of `visual-line-mode' is ignored.

;; This variable must be set before Nsevil is loaded."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-repeat-find-to-skip-next t
;;   "Whether a repeat of t or T should skip an adjacent character."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-kbd-macro-suppress-motion-error nil
;;   "\\<nsevil-motion-state-map>
;; Whether left/right motions signal errors in keyboard macros.
;; This variable only affects beginning-of-line or end-of-line errors
;; regarding the motions \\[nsevil-backward-char] and \\[nsevil-forward-char]
;; respectively.  This may be desired since such errors cause macro
;; definition or execution to be terminated.  There are four
;; possibilities:

;; - `record': errors are suppressed when recording macros, but not when
;;   replaying them.
;; - `replay': errors are suppressed when replaying macros, but not when
;;   recording them.
;; - `t': errors are suppressed in both cases.
;; - `nil': errors are never suppressed."
;;   :type '(radio (const :tag "No" :value nil)
;;                 (const :tag "Record" :value record)
;;                 (const :tag "Replay" :value replay)
;;                 (const :tag "Both" :value t))
;;   :group 'nsevil)

;; (defcustom nsevil-track-eol t
;;   "\\<nsevil-motion-state-map>
;; Whether \\[nsevil-end-of-line] \"sticks\" the cursor to the end of the line.
;; If non-nil, vertical motions after \\[nsevil-end-of-line] maintain the cursor at the
;; end of the line, even if the target line is longer.  This is analogous
;; to `track-eol', but respects Nsevil's interpretation of end-of-line."
;;   :type 'boolean
;;   :group 'nsevil)

(defcustom nsevil-mode-line-format 'before
  "The position of the state tag in the mode line.
If set to `before' or `after', the tag is placed at the beginning
or the end of the mode-line, respectively.  If nil, there is no
tag.  Otherwise it should be a cons cell (WHERE . WHICH), where
WHERE is either `before' or `after', and WHICH is a symbol in
`mode-line-format'.  The tag is then placed before or after that
symbol, respectively."
  :type '(radio :value 'before
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'nsevil)

;; (defcustom nsevil-mouse-word 'nsevil-word
;;   "The thing-at-point symbol for double click selection.
;; The double-click starts visual state in a special word selection
;; mode. This symbol is used to determine the words to be
;; selected. Possible values are `nsevil-word' or `nsevil-WORD'."
;;   :type 'symbol
;;   :group 'nsevil)

;; (defcustom nsevil-bigword "^ \t\r\n"
;;   "The set of characters to be interpreted as WORD boundaries.
;; This is enclosed with square brackets and used as a regular
;; expression.  By default, whitespace characters are considered
;; WORD boundaries."
;;   :type 'string
;;   :group 'nsevil)
;; (make-variable-buffer-local 'nsevil-bigword)

;; (defcustom nsevil-want-fine-undo nil
;;   "Whether actions are undone in several steps.
;; There are two possible choices: nil (\"no\") means that all
;; changes made during insert state, including a possible delete
;; after a change operation, are collected in a single undo step.
;; Non-nil (\"yes\") means that undo steps are determined according
;; to Emacs heuristics, and no attempt is made to aggregate changes.

;; For backward compatibility purposes, the value `fine' is
;; interpreted as `nil'.  This option was removed because it did not
;; work consistently."
;;   :type '(radio (const :tag "No" :value nil)
;;                 (const :tag "Fine (obsolete)" :value fine)
;;                 (const :tag "Yes" :value t))
;;   :group 'nsevil)

;; (defcustom nsevil-regexp-search t
;;   "\\<nsevil-motion-state-map>
;; Whether to use regular expressions for searching in \
;; \\[nsevil-search-forward] and \\[nsevil-search-backward]."
;;   :type  'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-search-wrap t
;;   "\\<nsevil-motion-state-map>
;; Whether search with \\[nsevil-search-forward] and \
;; \\[nsevil-search-backward] wraps around the buffer.
;; If this is non-nil, search stops at the buffer boundaries."
;;   :type  'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-flash-delay 2
;;   "\\<nsevil-motion-state-map>
;; Time in seconds to flash search matches after \\[nsevil-search-next] and \
;; \\[nsevil-search-previous]."
;;   :type  'number
;;   :group 'nsevil)

;; (defcustom nsevil-auto-balance-windows t
;;   "If non-nil window creation and deletion trigger rebalancing."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-split-window-below nil
;;   "If non-nil split windows are created below."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-vsplit-window-right nil
;;   "If non-nil vertically split windows with are created to the right."
;;   :type 'boolean
;;   :group 'nsevil)

(defcustom nsevil-esc-delay 0.01
  "The time, in seconds, to wait for another key after escape.
If no further event arrives during this time, the event is
translated to `ESC'.  Otherwise, it is translated according to
`input-decode-map'.  This does not apply in Emacs state, and may
also be inhibited by setting `nsevil-inhibit-esc'."
  :type 'number
  :group 'nsevil)

(defvar nsevil-esc-mode nil
  "Non-nil if `nsevil-esc-mode' is enabled.")

;; (defvar nsevil-esc-map nil
;;   "Original ESC prefix map in `input-decode-map'.
;; Used by `nsevil-esc-mode'.")

(defvar nsevil-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom nsevil-intercept-esc 'always
  "Whether Nsevil should intercept the escape key.
In the terminal, escape and a meta key sequence both generate the
same event.  In order to distingush these, Nsevil uses
`input-decode-map'.  It is not necessary to do this in a graphical
Emacs session.  However, if you prefer to use `C-[' as escape (which
is identical to the terminal escape key code), this interception must
also happen in graphical Emacs sessions.  Set this variable to
`always', t (only in the terminal) or nil (never intercept)."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'nsevil)

;; (defcustom nsevil-show-paren-range 0
;;   "The minimal distance between point and a parenthesis
;; which causes the parenthesis to be highlighted."
;;   :type 'integer
;;   :group 'nsevil)

;; (defcustom nsevil-ex-hl-update-delay 0.02
;;   "Time in seconds of idle before updating search highlighting.
;; Setting this to a period shorter than that of keyboard's repeat
;; rate allows highlights to update while scrolling."
;;   :type 'number
;;   :group 'nsevil)

;; (defcustom nsevil-highlight-closing-paren-at-point-states
;;   '(not emacs insert replace)
;;   "The states in which the closing parenthesis at point should be highlighted.
;; All states listed here highlight the closing parenthesis at
;; point (which is Vim's default behavior).  All others highlight the
;; parenthesis before point (which is Emacs default behavior). If
;; this list contains the symbol `not' then its meaning is inverted,
;; i.e. all states listed here highlight the closing parenthesis
;; before point."
;;   :type '(repeat symbol)
;;   :group 'nsevil)

;; (defcustom nsevil-kill-on-visual-paste t
;;   "Whether pasting in visual state adds the replaced text to the
;; kill ring, making it the default for the next paste. The default,
;; replicates the default Vim behavior."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-want-C-i-jump t
;;   "Whether `C-i' jumps forward in the jump list (like Vim).
;; Otherwise, `C-i' inserts a tab character."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-motion-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-motion-state-map (kbd "C-i"))
;;                         'nsevil-jump-forward))
;;                (define-key nsevil-motion-state-map (kbd "C-i") nil))
;;               ((and value
;;                     (not (lookup-key nsevil-motion-state-map (kbd "C-i"))))
;;                (define-key nsevil-motion-state-map (kbd "C-i") 'nsevil-jump-forward))))))

;; (defcustom nsevil-want-C-u-scroll nil
;;   "Whether `C-u' scrolls up (like Vim).
;; Otherwise, `C-u' applies a prefix argument.  The binding of
;; `C-u' mirrors Emacs behaviour by default due to the relative
;; ubiquity of prefix arguments."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-motion-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-motion-state-map (kbd "C-u"))
;;                         'nsevil-scroll-up))
;;                (define-key nsevil-motion-state-map (kbd "C-u") nil))
;;               ((and value
;;                     (not (lookup-key nsevil-motion-state-map (kbd "C-u"))))
;;                (define-key nsevil-motion-state-map (kbd "C-u") 'nsevil-scroll-up))))))

;; (defcustom nsevil-want-C-d-scroll t
;;   "Whether `C-d' scrolls down (like Vim)."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-motion-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-motion-state-map (kbd "C-d"))
;;                         'nsevil-scroll-down))
;;                (define-key nsevil-motion-state-map (kbd "C-d") nil))
;;               ((and value
;;                     (not (lookup-key nsevil-motion-state-map (kbd "C-d"))))
;;                (define-key nsevil-motion-state-map (kbd "C-d") 'nsevil-scroll-down))))))

;; (defcustom nsevil-want-C-u-delete nil
;;   "Whether `C-u' deletes back to indentation in insert state.
;; Otherwise, `C-u' applies a prefix argument.  The binding of
;; `C-u' mirrors Emacs behaviour by default due to the relative
;; ubiquity of prefix arguments."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-insert-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-insert-state-map (kbd "C-u"))
;;                         'nsevil-delete-back-to-indentation))
;;                (define-key nsevil-insert-state-map (kbd "C-u") nil))
;;               ((and value
;;                     (not (lookup-key nsevil-insert-state-map (kbd "C-u"))))
;;                (define-key nsevil-insert-state-map (kbd "C-u") 'nsevil-delete-back-to-indentation))))))

;; (defcustom nsevil-want-C-w-delete t
;;   "Whether `C-w' deletes a word in Insert state."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-insert-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-insert-state-map (kbd "C-w"))
;;                         'nsevil-delete-backward-word))
;;                (define-key nsevil-insert-state-map (kbd "C-w") 'nsevil-window-map))
;;               ((and value
;;                     (eq (lookup-key nsevil-insert-state-map (kbd "C-w"))
;;                         'nsevil-window-map))
;;                (define-key nsevil-insert-state-map (kbd "C-w") 'nsevil-delete-backward-word))))))

;; (defcustom nsevil-want-C-g-bindings nil
;;   "Whether `C-g' postfix can be used in bindings."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-want-C-w-in-emacs-state nil
;;   "Whether `C-w' prefixes windows commands in Emacs state."
;;   :type 'boolean
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (when (boundp 'nsevil-emacs-state-map)
;;              (cond
;;               ((and (not value)
;;                     (eq (lookup-key nsevil-emacs-state-map (kbd "C-w"))
;;                         'nsevil-window-map))
;;                (define-key nsevil-emacs-state-map (kbd "C-w") nil))
;;               ((and value
;;                     (not (lookup-key nsevil-emacs-state-map (kbd "C-w"))))
;;                (define-key nsevil-emacs-state-map (kbd "C-w") 'nsevil-window-map))))))

;; (defcustom nsevil-want-change-word-to-end t
;;   "Whether `cw' behaves like `ce'."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-want-Y-yank-to-eol nil
;;   "Whether `Y' yanks to the end of the line.
;; The default behavior is to yank the whole line, like Vim."
;;   :group 'nsevil
;;   :type 'boolean
;;   :initialize #'nsevil-custom-initialize-pending-reset
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (nsevil-add-command-properties
;;             'nsevil-yank-line
;;             :motion (if value
;;                         'nsevil-end-of-line-or-visual-line
;;                       'nsevil-line-or-visual-line))))

;; (defcustom nsevil-disable-insert-state-bindings nil
;;   "Whether insert state bindings should be used.
;; Bindings for escape, delete and `nsevil-toggle-key' are always
;; available. If this is non-nil, default Emacs bindings are by and
;; large accessible in insert state."
;;   :group 'nsevil
;;   :type 'boolean
;;   :initialize #'nsevil-custom-initialize-pending-reset
;;   :set #'(lambda (sym value)
;;            (set-default sym value)
;;            (nsevil-update-insert-state-bindings sym value)))

(defcustom nsevil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'nsevil)

;; (defcustom nsevil-complete-all-buffers t
;;   "\\<nsevil-insert-state-map>
;; Whether completion looks for matches in all buffers.
;; This applies to \\[nsevil-complete-next] and \\[nsevil-complete-previous] \
;; in insert state."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defvar dabbrev-search-these-buffers-only)
;; (defvar dabbrev-case-distinction)
;; (defcustom nsevil-complete-next-func
;;   #'(lambda (arg)
;;       (require 'dabbrev)
;;       (let ((dabbrev-search-these-buffers-only
;;              (unless nsevil-complete-all-buffers
;;                (list (current-buffer))))
;;             dabbrev-case-distinction)
;;         (condition-case nil
;;             (if (eq last-command this-command)
;;                 (dabbrev-expand nil)
;;               (dabbrev-expand (- (abs (or arg 1)))))
;;           (error (dabbrev-expand nil)))))
;;   "Completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-next]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-complete-previous-func
;;   #'(lambda (arg)
;;       (require 'dabbrev)
;;       (let ((dabbrev-search-these-buffers-only
;;              (unless nsevil-complete-all-buffers
;;                (list (current-buffer))))
;;             dabbrev-case-distinction)
;;         (dabbrev-expand arg)))
;;   "Completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-previous]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-complete-next-minibuffer-func 'minibuffer-complete
;;   "Minibuffer completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-next]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-complete-previous-minibuffer-func 'minibuffer-complete
;;   "Minibuffer completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-previous]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-complete-next-line-func
;;   #'(lambda (arg)
;;       (let ((hippie-expand-try-functions-list
;;              '(try-expand-line
;;                try-expand-line-all-buffers)))
;;         (hippie-expand arg)))
;;   "Minibuffer completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-next-line]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-complete-previous-line-func
;;   nsevil-complete-next-line-func
;;   "Minibuffer completion function used by \
;; \\<nsevil-insert-state-map>\\[nsevil-complete-previous-line]."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-lookup-func #'woman
;;   "Lookup function used by \
;; \"\\<nsevil-motion-state-map>\\[nsevil-lookup]\"."
;;   :type 'function
;;   :group 'nsevil)

;; (defcustom nsevil-toggle-key "C-z"
;;   "The key used to change to and from Emacs state.
;; Must be readable by `read-kbd-macro'. For example: \"C-z\"."
;;   :type 'string
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (nsevil-set-toggle-key value)
;;            (set-default sym value)))

(defcustom nsevil-default-state 'normal
  "The default Nsevil state.
This is the state a buffer starts in when it is not otherwise
configured (see `nsevil-set-initial-state' and
`nsevil-buffer-regexps').  The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and `emacs'."
  :type  'symbol
  :group 'nsevil)

(defcustom nsevil-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expressions determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and
`nil'.  If STATE is `nil', Nsevil is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'nsevil)

;; (defcustom nsevil-emacs-state-modes
;;   '(5x5-mode
;;     archive-mode
;;     bbdb-mode
;;     biblio-selection-mode
;;     blackbox-mode
;;     bookmark-bmenu-mode
;;     bookmark-edit-annotation-mode
;;     browse-kill-ring-mode
;;     bubbles-mode
;;     bzr-annotate-mode
;;     calc-mode
;;     cfw:calendar-mode
;;     completion-list-mode
;;     Custom-mode
;;     custom-theme-choose-mode
;;     debugger-mode
;;     delicious-search-mode
;;     desktop-menu-blist-mode
;;     desktop-menu-mode
;;     doc-view-mode
;;     dun-mode
;;     dvc-bookmarks-mode
;;     dvc-diff-mode
;;     dvc-info-buffer-mode
;;     dvc-log-buffer-mode
;;     dvc-revlist-mode
;;     dvc-revlog-mode
;;     dvc-status-mode
;;     dvc-tips-mode
;;     ediff-mode
;;     ediff-meta-mode
;;     efs-mode
;;     Electric-buffer-menu-mode
;;     emms-browser-mode
;;     emms-mark-mode
;;     emms-metaplaylist-mode
;;     emms-playlist-mode
;;     ess-help-mode
;;     etags-select-mode
;;     fj-mode
;;     gc-issues-mode
;;     gdb-breakpoints-mode
;;     gdb-disassembly-mode
;;     gdb-frames-mode
;;     gdb-locals-mode
;;     gdb-memory-mode
;;     gdb-registers-mode
;;     gdb-threads-mode
;;     gist-list-mode
;;     git-rebase-mode
;;     gnus-article-mode
;;     gnus-browse-mode
;;     gnus-group-mode
;;     gnus-server-mode
;;     gnus-summary-mode
;;     gomoku-mode
;;     google-maps-static-mode
;;     ibuffer-mode
;;     jde-javadoc-checker-report-mode
;;     magit-cherry-mode
;;     magit-diff-mode
;;     magit-log-mode
;;     magit-log-select-mode
;;     magit-popup-mode
;;     magit-popup-sequence-mode
;;     magit-process-mode
;;     magit-reflog-mode
;;     magit-refs-mode
;;     magit-revision-mode
;;     magit-stash-mode
;;     magit-stashes-mode
;;     magit-status-mode
;;     mh-folder-mode
;;     monky-mode
;;     mpuz-mode
;;     mu4e-main-mode
;;     mu4e-headers-mode
;;     mu4e-view-mode
;;     notmuch-hello-mode
;;     notmuch-search-mode
;;     notmuch-show-mode
;;     notmuch-tree-mode
;;     occur-mode
;;     org-agenda-mode
;;     package-menu-mode
;;     pdf-outline-buffer-mode
;;     pdf-view-mode
;;     proced-mode
;;     rcirc-mode
;;     rebase-mode
;;     recentf-dialog-mode
;;     reftex-select-bib-mode
;;     reftex-select-label-mode
;;     reftex-toc-mode
;;     sldb-mode
;;     slime-inspector-mode
;;     slime-thread-control-mode
;;     slime-xref-mode
;;     snake-mode
;;     solitaire-mode
;;     sr-buttons-mode
;;     sr-mode
;;     sr-tree-mode
;;     sr-virtual-mode
;;     tar-mode
;;     tetris-mode
;;     tla-annotate-mode
;;     tla-archive-list-mode
;;     tla-bconfig-mode
;;     tla-bookmarks-mode
;;     tla-branch-list-mode
;;     tla-browse-mode
;;     tla-category-list-mode
;;     tla-changelog-mode
;;     tla-follow-symlinks-mode
;;     tla-inventory-file-mode
;;     tla-inventory-mode
;;     tla-lint-mode
;;     tla-logs-mode
;;     tla-revision-list-mode
;;     tla-revlog-mode
;;     tla-tree-lint-mode
;;     tla-version-list-mode
;;     twittering-mode
;;     urlview-mode
;;     vc-annotate-mode
;;     vc-dir-mode
;;     vc-git-log-view-mode
;;     vc-hg-log-view-mode
;;     vc-svn-log-view-mode
;;     vm-mode
;;     vm-summary-mode
;;     w3m-mode
;;     wab-compilation-mode
;;     xgit-annotate-mode
;;     xgit-changelog-mode
;;     xgit-diff-mode
;;     xgit-revlog-mode
;;     xhg-annotate-mode
;;     xhg-log-mode
;;     xhg-mode
;;     xhg-mq-mode
;;     xhg-mq-sub-mode
;;     xhg-status-extra-mode)
;;   "Modes that should come up in Emacs state."
;;   :type  '(repeat symbol)
;;   :group 'nsevil)

;; (defcustom nsevil-insert-state-modes
;;   '(comint-mode
;;     erc-mode
;;     eshell-mode
;;     geiser-repl-mode
;;     gud-mode
;;     inferior-apl-mode
;;     inferior-caml-mode
;;     inferior-emacs-lisp-mode
;;     inferior-j-mode
;;     inferior-python-mode
;;     inferior-scheme-mode
;;     inferior-sml-mode
;;     internal-ange-ftp-mode
;;     prolog-inferior-mode
;;     reb-mode
;;     shell-mode
;;     slime-repl-mode
;;     term-mode
;;     wdired-mode)
;;   "Modes that should come up in Insert state."
;;   :type  '(repeat symbol)
;;   :group 'nsevil)

;; (defcustom nsevil-motion-state-modes
;;   '(apropos-mode
;;     Buffer-menu-mode
;;     calendar-mode
;;     color-theme-mode
;;     command-history-mode
;;     compilation-mode
;;     dictionary-mode
;;     ert-results-mode
;;     help-mode
;;     Info-mode
;;     Man-mode
;;     speedbar-mode
;;     undo-tree-visualizer-mode
;;     woman-mode)
;;   "Modes that should come up in Motion state."
;;   :type  '(repeat symbol)
;;   :group 'nsevil)

;; (defvar nsevil-pending-overriding-maps nil
;;   "An alist of pending overriding maps.")

;; (defvar nsevil-pending-intercept-maps nil
;;   "An alist of pending intercept maps.")

;; (defcustom nsevil-overriding-maps
;;   '((Buffer-menu-mode-map . nil)
;;     (color-theme-mode-map . nil)
;;     (comint-mode-map . nil)
;;     (compilation-mode-map . nil)
;;     (grep-mode-map . nil)
;;     (dictionary-mode-map . nil)
;;     (ert-results-mode-map . motion)
;;     (Info-mode-map . motion)
;;     (speedbar-key-map . nil)
;;     (speedbar-file-key-map . nil)
;;     (speedbar-buffers-key-map . nil))
;;   "Keymaps that should override Nsevil maps.
;; Entries have the form (MAP-VAR . STATE), where MAP-VAR is
;; a keymap variable and STATE is the state whose bindings
;; should be overridden. If STATE is nil, all states are
;; overridden."
;;   :type '(alist :key-type symbol :value-type symbol)
;;   :group 'nsevil
;;   :set #'(lambda (var values)
;;            (set-default var values)
;;            (nsevil-set-custom-state-maps 'nsevil-overriding-maps
;;                                        'nsevil-pending-overriding-maps
;;                                        'override-state
;;                                        'nsevil-make-overriding-map
;;                                        values))
;;   :initialize 'nsevil-custom-initialize-pending-reset)

;; (add-hook 'after-load-functions #'nsevil-update-pending-maps)

;; (defcustom nsevil-intercept-maps
;;   '((edebug-mode-map . nil))
;;   "Keymaps that should intercept Nsevil maps.
;; Entries have the form (MAP-VAR . STATE), where MAP-VAR is
;; a keymap variable and STATE is the state whose bindings
;; should be intercepted. If STATE is nil, all states are
;; intercepted."
;;   :type '(alist :key-type symbol :value-type symbol)
;;   :group 'nsevil
;;   :set #'(lambda (var values)
;;            (set-default var values)
;;            (nsevil-set-custom-state-maps 'nsevil-intercept-maps
;;                                        'nsevil-pending-intercept-maps
;;                                        'intercept-state
;;                                        'nsevil-make-intercept-map
;;                                        values))
;;   :initialize 'nsevil-custom-initialize-pending-reset)

;; (defcustom nsevil-motions
;;   '(back-to-indentation
;;     backward-char
;;     backward-list
;;     backward-paragraph
;;     backward-sentence
;;     backward-sexp
;;     backward-up-list
;;     backward-word
;;     beginning-of-buffer
;;     beginning-of-defun
;;     beginning-of-line
;;     beginning-of-visual-line
;;     c-beginning-of-defun
;;     c-end-of-defun
;;     diff-file-next
;;     diff-file-prev
;;     diff-hunk-next
;;     diff-hunk-prev
;;     down-list
;;     end-of-buffer
;;     end-of-defun
;;     end-of-line
;;     end-of-visual-line
;;     exchange-point-and-mark
;;     forward-char
;;     forward-list
;;     forward-paragraph
;;     forward-sentence
;;     forward-sexp
;;     forward-word
;;     goto-last-change
;;     ibuffer-backward-line
;;     ibuffer-forward-line
;;     isearch-abort
;;     isearch-cancel
;;     isearch-complete
;;     isearch-del-char
;;     isearch-delete-char
;;     isearch-edit-string
;;     isearch-exit
;;     isearch-highlight-regexp
;;     isearch-occur
;;     isearch-other-control-char
;;     isearch-other-meta-char
;;     isearch-printing-char
;;     isearch-query-replace
;;     isearch-query-replace-regexp
;;     isearch-quote-char
;;     isearch-repeat-backward
;;     isearch-repeat-forward
;;     isearch-ring-advance
;;     isearch-ring-retreat
;;     isearch-toggle-case-fold
;;     isearch-toggle-input-method
;;     isearch-toggle-regexp
;;     isearch-toggle-specified-input-method
;;     isearch-toggle-word
;;     isearch-yank-char
;;     isearch-yank-kill
;;     isearch-yank-line
;;     isearch-yank-word-or-char
;;     keyboard-quit
;;     left-char
;;     left-word
;;     mouse-drag-region
;;     mouse-save-then-kill
;;     mouse-set-point
;;     mouse-set-region
;;     mwheel-scroll
;;     move-beginning-of-line
;;     move-end-of-line
;;     next-error
;;     next-line
;;     paredit-backward
;;     paredit-backward-down
;;     paredit-backward-up
;;     paredit-forward
;;     paredit-forward-down
;;     paredit-forward-up
;;     pop-global-mark
;;     pop-tag-mark
;;     pop-to-mark-command
;;     previous-error
;;     previous-line
;;     right-char
;;     right-word
;;     scroll-down
;;     scroll-down-command
;;     scroll-up
;;     scroll-up-command
;;     sgml-skip-tag-backward
;;     sgml-skip-tag-forward
;;     up-list)
;;   "Non-Nsevil commands to initialize to motions."
;;   :type  '(repeat symbol)
;;   :group 'nsevil
;;   :set 'nsevil-set-custom-motions
;;   :initialize 'nsevil-custom-initialize-pending-reset)

;; (defcustom nsevil-visual-newline-commands
;;   '(LaTeX-section
;;     TeX-font)
;;   "Commands excluding the trailing newline of a Visual Line selection.
;; These commands work better without this newline."
;;   :type  '(repeat symbol)
;;   :group 'nsevil
;;   :set 'nsevil-set-visual-newline-commands
;;   :initialize 'nsevil-custom-initialize-pending-reset)

;; (defcustom nsevil-want-visual-char-semi-exclusive nil
;;   "Visual character selection to beginning/end of line is exclusive.
;; If non nil then an inclusive visual character selection which
;; ends at the beginning or end of a line is turned into an
;; exclusive selection. Thus if the selected (inclusive) range ends
;; at the beginning of a line it is changed to not include the first
;; character of that line, and if the selected range ends at the end
;; of a line it is changed to not include the newline character of
;; that line."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-text-object-change-visual-type t
;;   "Text objects change the current visual state type.
;; If non-nil then a text-object changes the type of the visual state to
;; its default selection type (e.g. a word object always changes to
;; charwise visual state). Otherwise the current visual state type is
;; preserved."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defgroup nsevil-cjk nil
;;   "CJK support"
;;   :prefix "nsevil-cjk-"
;;   :group 'nsevil)

;; (defcustom nsevil-cjk-emacs-word-boundary nil
;;   "Determine word boundary exactly the same way as Emacs does."
;;   :type 'boolean
;;   :group 'nsevil-cjk)

;; (defcustom nsevil-cjk-word-separating-categories
;;   '(;; Kanji
;;     (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
;;     ;; Hiragana
;;     (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
;;     ;; Katakana
;;     (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
;;     ;; half-width Katakana
;;     (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
;;     ;; full-width alphanumeric
;;     (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
;;     ;; full-width Greek
;;     (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
;;     )
;;   "List of pair (cons) of categories to determine word boundary
;; used in `nsevil-cjk-word-boundary-p'. See the documentation of
;; `word-separating-categories'. Use `describe-categories' to see
;; the list of categories."
;;   :type '(alist :key-type (choice character (const nil))
;;                 :value-type (choice character (const nil)))
;;   :group 'nsevil-cjk)

;; (defcustom nsevil-cjk-word-combining-categories
;;   '(;; default value in word-combining-categories
;;     (nil . ?^) (?^ . nil)
;;     ;; Roman
;;     (?r . ?k) (?r . ?A) (?r . ?G)
;;     ;; half-width Katakana
;;     (?k . ?r) (?k . ?A) (?k . ?G)
;;     ;; full-width alphanumeric
;;     (?A . ?r) (?A . ?k) (?A . ?G)
;;     ;; full-width Greek
;;     (?G . ?r) (?G . ?k) (?G . ?A)
;;     )
;;   "List of pair (cons) of categories to determine word boundary
;; used in `nsevil-cjk-word-boundary-p'. See the documentation of
;; `word-combining-categories'. Use `describe-categories' to see the
;; list of categories."
;;   :type '(alist :key-type (choice character (const nil))
;;                 :value-type (choice character (const nil)))
;;   :group 'nsevil-cjk)

;; (defcustom nsevil-ex-complete-emacs-commands 'in-turn
;;   "TAB-completion for Emacs commands in ex command line.
;; This variable determines when Emacs commands are considered for
;; completion, always, never, or only if no Nsevil ex command is
;; available for completion."
;;   :group 'nsevil
;;   :type '(radio (const :tag "Only if no ex-command." :value in-turn)
;;                 (const :tag "Never" :value nil)
;;                 (const :tag "Always" :value t)))

;; (defface nsevil-ex-commands '(( nil
;;                               :underline t
;;                               :slant italic))
;;   "Face for the Nsevil command in completion in ex mode."
;;   :group 'nsevil)

;; (defface nsevil-ex-info '(( ((supports :slant))
;;                           :slant italic
;;                           :foreground "red"))
;;   "Face for the info message in ex mode."
;;   :group 'nsevil)

;; (defcustom nsevil-ex-visual-char-range nil
;;   "Type of default ex range in visual char state.
;; If non-nil the default range when starting an ex command from
;; character visual state is `<,`> otherwise it is '<,'>. In the
;; first case the ex command will be passed a region covering only
;; the visual selection. In the second case the passed region will
;; be extended to contain full lines."
;;   :group 'nsevil
;;   :type 'boolean)

;; ;; Searching
;; (defcustom nsevil-symbol-word-search nil
;;   "If nil then * and # search for words otherwise for symbols."
;;   :group 'nsevil
;;   :type 'boolean)
;; (make-variable-buffer-local 'nsevil-symbol-word-search)

;; (defcustom nsevil-magic t
;;   "Meaning which characters in a pattern are magic.
;; The meaning of those values is the same as in Vim. Note that it
;; only has influence if the Nsevil search module is chosen in
;; `nsevil-search-module'."
;;   :group 'nsevil
;;   :type '(radio (const :tag "Very magic." :value very-magic)
;;                 (const :tag "Magic" :value t)
;;                 (const :tag "Nomagic" :value nil)
;;                 (const :tag "Very nomagic" :value very-nomagic)))

;; (defcustom nsevil-ex-search-vim-style-regexp nil
;;   "If non-nil Vim-style backslash codes are supported in search patterns.
;; See `nsevil-transform-vim-style-regexp' for the supported backslash
;; codes.  Note that this only affects the search command if
;; `nsevil-search-module' is set to 'nsevil-search. The isearch module
;; always uses plain Emacs regular expressions."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-interactive-search-highlight 'all-windows
;;   "Determine in which windows the interactive highlighting should be shown."
;;   :type '(radio (const :tag "All windows." all-windows)
;;                 (const :tag "Selected window." selected-window)
;;                 (const :tag "Disable highlighting." nil))
;;   :group 'nsevil)

;; (defcustom nsevil-ex-search-persistent-highlight t
;;   "If non-nil matches remain highlighted when the search ends."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-search-case 'smart
;;   "The case behaviour of the search command.
;; Smart case means that the pattern is case sensitive if and only
;; if it contains an upper case letter, otherwise it is case
;; insensitive."
;;   :type '(radio (const :tag "Case sensitive." sensitive)
;;                 (const :tag "Case insensitive." insensitive)
;;                 (const :tag "Smart case." smart))
;;   :group 'nsevil)

;; (defcustom nsevil-ex-substitute-case nil
;;   "The case behaviour of the search command.
;; Smart case means that the pattern is case sensitive if and only
;; if it contains an upper case letter, otherwise it is case
;; insensitive. If nil then the setting of `nsevil-ex-search-case' is
;; used."
;;   :type '(radio (const :tag "Same as interactive search." nil)
;;                 (const :tag "Case sensitive." sensitive)
;;                 (const :tag "Case insensitive." insensitive)
;;                 (const :tag "Smart case." smart))
;;   :group 'nsevil)

;; (defcustom nsevil-ex-search-interactive t
;;   "If t search is interactive."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-search-highlight-all t
;;   "If t and interactive search is enabled, all matches are
;; highlighted."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-substitute-highlight-all t
;;   "If t all matches for the substitute pattern are highlighted."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-substitute-interactive-replace t
;;   "If t and substitute patterns are highlighted,
;; the replacement is shown interactively."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-ex-substitute-global nil
;;   "If non-nil substitute patterns are global by default.
;; Usually (if this variable is nil) a substitution works only on
;; the first match of a pattern in a line unless the 'g' flag is
;; given, in which case the substitution happens on all matches in a
;; line. If this option is non-nil, this behaviour is reversed: the
;; substitution works on all matches unless the 'g' pattern is
;; specified, then is works only on the first match."
;;   :type  'boolean
;;   :group 'nsevil)

;; (defface nsevil-ex-search '((t :inherit isearch))
;;   "Face for interactive search."
;;   :group 'nsevil)

;; (defface nsevil-ex-lazy-highlight '((t :inherit lazy-highlight))
;;   "Face for highlighting all matches in interactive search."
;;   :group 'nsevil)

;; (defface nsevil-ex-substitute-matches '((t :inherit lazy-highlight))
;;   "Face for interactive substitute matches."
;;   :group 'nsevil)

;; (defface nsevil-ex-substitute-replacement '((((supports :underline))
;;                                            :underline t
;;                                            :foreground "red"))
;;   "Face for interactive replacement text."
;;   :group 'nsevil)

;; (defcustom nsevil-command-window-height 8
;;   "Height (in lines) of the command line window.
;; Set to 0 to use the default height for `split-window'."
;;   :type 'integer
;;   :group 'nsevil)

;; (defcustom nsevil-display-shell-error-in-message nil
;;   "Show error output of a shell command in the error buffer.
;; If this variable is non-nil the error output of a shell command
;; goes to the messages buffer instead of being mixed with the
;; regular output. This happens only if the exit status of the
;; command is non-zero."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-want-abbrev-expand-on-insert-exit t
;;   "If non-nil abbrevs will be expanded when leaving insert state
;; like in Vim, if `abbrev-mode' is on."
;;   :type 'boolean
;;   :group 'nsevil)

;; ;;; Variables

(defmacro nsevil-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

;; (nsevil-define-local-var nsevil-scroll-count 0
;;   "Holds last used prefix for `nsevil-scroll-up'
;; and `nsevil-scroll-down'.
;; Determines how many lines should be scrolled.
;; Default value is 0 - scroll half the screen.")

(nsevil-define-local-var nsevil-state nil
  "The current Nsevil state.
To change the state, use `nsevil-change-state'
or call the state function (e.g., `nsevil-normal-state').")

;; ;; these may be used inside `nsevil-define-state'
(nsevil-define-local-var nsevil-next-state nil
  "The Nsevil state being switched to.")

(nsevil-define-local-var nsevil-previous-state-alist nil
  "For Each nsevil state the Nsevil state being switched from.")

(nsevil-define-local-var nsevil-previous-state nil
  "The Nsevil state being switched from.")

;; (defvar nsevil-execute-in-emacs-state-buffer nil
;;   "The buffer of the latest `nsevil-execute-in-emacs-state'.
;; When this command is being executed the current buffer is stored
;; in this variable. This is necessary in case the Emacs-command to
;; be called changes the current buffer.")

(nsevil-define-local-var nsevil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'nsevil-mode-line-tag 'risky-local-variable t)

(defvar nsevil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar nsevil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar nsevil-minor-mode-keymaps-alist nil
  "Association list of Nsevil states to minor-mode keymap alists.
Entries have the form (STATE . MODE-MAP-ALIST), where
MODE-MAP-ALIST is an alist taking the form of
`minor-mode-map-alist'.")

(defvar nsevil-state-properties nil
  "Specifications made by `nsevil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `nsevil-state-property'.")

(nsevil-define-local-var nsevil-mode-map-alist nil
  "Association list of keymaps to use for Nsevil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar nsevil-command-properties nil
  "Specifications made by `nsevil-define-command'.")

;; (defvar nsevil-change-commands '(nsevil-change)
;;   "Commands that wrap or replace `nsevil-change'.
;; This list exists to apply an inconsistency with vim's change command
;; to commands that wrap or redefine it. See emacs-nsevil/nsevil#916.")

;; (defvar nsevil-transient-vars '(cua-mode transient-mark-mode select-active-regions)
;;   "List of variables pertaining to Transient Mark mode.")

;; (defvar nsevil-transient-vals nil
;;   "Association list of old values for Transient Mark mode variables.
;; Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
;; whether the variable was previously buffer-local.")

(nsevil-define-local-var nsevil-no-display nil
  "If non-nil, various Nsevil displays are inhibited.
Use the macro `nsevil-without-display' to set this variable.")

;; (defvar nsevil-type-properties nil
;;   "Specifications made by `nsevil-define-type'.
;; Entries have the form (TYPE . PLIST), where PLIST is a property
;; list specifying functions for handling the type: expanding it,
;; describing it, etc.")

;; (defvar nsevil-interactive-alist nil
;;   "Association list of Nsevil-specific interactive codes.")

;; (nsevil-define-local-var nsevil-motion-marker nil
;;   "Marker for storing the starting position of a motion.")

(nsevil-define-local-var nsevil-this-type nil
  "Current motion type.")

(nsevil-define-local-var nsevil-this-type-modified nil
  "Non-nil iff current motion type has been modified by the user.
If the type has been modified, this variable contains the new
type.")

;; (nsevil-define-local-var nsevil-this-register nil
;;   "Current register.")

;; (defvar nsevil-this-macro nil
;;   "Current macro register.")

;; (nsevil-define-local-var nsevil-this-operator nil
;;   "Current operator.")

(nsevil-define-local-var nsevil-this-motion nil
  "Current motion.")

;; (nsevil-define-local-var nsevil-this-motion-count nil
;;   "Current motion count.")

;; (defvar nsevil-last-register nil
;;   "The last executed register.")

;; (defvar nsevil-inhibit-operator nil
;;   "Inhibit current operator.
;; If an operator calls a motion and the motion sets this variable
;; to t, the operator code is not executed.")

;; (defvar nsevil-inhibit-operator-value nil
;;   "This variable is used to transfer the value
;; of `nsevil-inhibit-operator' from one local scope to another.")

;; ;; used by `nsevil-define-operator'
;; (defvar nsevil-operator-range-beginning nil
;;   "Beginning of `nsevil-operator-range'.")

;; (defvar nsevil-operator-range-end nil
;;   "End of `nsevil-operator-range'.")

;; (defvar nsevil-operator-range-type nil
;;   "Type of `nsevil-operator-range'.")

;; (defvar nsevil-operator-range-motion nil
;;   "Motion of `nsevil-operator-range'.")

(defvar nsevil-restriction-stack nil
  "List of previous restrictions.
Using `nsevil-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

;; (nsevil-define-local-var nsevil-markers-alist
;;   '((?\( . nsevil-backward-sentence)
;;     (?\) . nsevil-forward-sentence)
;;     (?{ . nsevil-backward-paragraph)
;;     (?} . nsevil-forward-paragraph)
;;     (?' . nsevil-jump-backward-swap)
;;     (?` . nsevil-jump-backward-swap)
;;     (?< . nsevil-visual-beginning)
;;     (?> . nsevil-visual-goto-end)
;;     (?. . (lambda ()
;;             (let (last-command)
;;               (goto-last-change nil)))))
;;   "Association list for markers.
;; Entries have the form (CHAR . DATA), where CHAR is the marker's
;; name and DATA is either a marker object as returned by `make-marker',
;; a variable, a movement function, or a cons cell (STRING NUMBER),
;; where STRING is a file path and NUMBER is a buffer position.
;; The global value of this variable holds markers available from
;; every buffer, while the buffer-local value holds markers available
;; only in the current buffer.")

(defconst nsevil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap nsevil-suppress-map t)

(defvar nsevil-read-key-map (make-sparse-keymap)
  "Keymap active during `nsevil-read-key'.
This keymap can be used to bind some commands during the
execution of `nsevil-read-key' which is usually used to read a
character argument for some commands, e.g. `nsevil-replace'.")

;; ;; TODO: customize size of ring
;; (defvar nsevil-repeat-ring (make-ring 10)
;;   "A ring of repeat-informations to repeat the last command.")

;; (defvar nsevil-repeat-types
;;   '((t . nsevil-repeat-keystrokes)
;;     (change . nsevil-repeat-changes)
;;     (motion . nsevil-repeat-motion)
;;     (insert-at-point . nsevil-repeat-insert-at-point)
;;     (ignore . nil))
;;   "An alist of defined repeat-types.")

;; (defvar nsevil-recording-repeat nil
;;   "Whether we are recording a repeat.")

;; (defvar nsevil-recording-current-command nil
;;   "Whether we are recording the current command for repeat.")

;; (defvar nsevil-repeat-changes nil
;;   "Accumulated buffer changes for changed-based commands.")

;; (defvar nsevil-repeat-info nil
;;   "Information accumulated during current repeat.")

;; (defvar nsevil-repeat-buffer nil
;;   "The buffer in which the repeat started.
;; If the buffer is changed, the repeat is cancelled.")

;; (defvar nsevil-repeat-pos nil
;;   "The position of point at the beginning of an change-tracking
;;   editing command.")

;; (defvar nsevil-repeat-keys nil
;;   "The keys that invoked the current command.")

;; (defvar nsevil-last-repeat nil
;;   "Information about the latest repeat command.
;; This is a list of three elements (POINT COUNT UNDO-POINTER),
;; where POINT is the position of point before the latest repeat,
;; COUNT the count-argument of the latest repeat command and
;; UNDO-POINTER the head of the undo-list before the last command
;; has been repeated.")

;; (defvar nsevil-repeat-count nil
;;   "The explicit count when repeating a command.")

;; (defvar nsevil-maybe-remove-spaces nil
;;   "Flag to determine if newly inserted spaces should be removed.
;; See the function `nsevil-maybe-remove-spaces'.")

;; (nsevil-define-local-var nsevil-insert-count nil
;;   "The explicit count passed to an command starting Insert state.")

;; (nsevil-define-local-var nsevil-insert-vcount nil
;;   "The information about the number of following lines the
;; insertion should be repeated. This is list (LINE COLUMN COUNT)
;; where LINE is the line-number where the original insertion
;; started and COLUMN is either a number or function determining the
;; column where the repeated insertions should take place. COUNT is
;; number of repeats (including the original insertion).")

;; (defvar nsevil-insert-skip-empty-lines nil
;;   "Non-nil of the current insertion should not take place on
;;   lines at which the insertion point is behind the end of the
;;   line.")

;; (nsevil-define-local-var nsevil-insert-lines nil
;;   "Non-nil if the current insertion command is a line-insertion
;; command o or O.")

;; (nsevil-define-local-var nsevil-insert-repeat-info nil
;;   "Repeat information accumulated during an insertion.")

(nsevil-define-local-var nsevil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(nsevil-define-local-var nsevil-echo-area-message nil
  "Previous value of `current-message'.")

(defvar nsevil-write-echo-area nil
  "If set to t inside `nsevil-save-echo-area', then the echo area
is not restored.")

;; (defvar nsevil-last-find nil
;;   "A pair (FUNCTION . CHAR) describing the lastest character
;;   search command.")

;; (defvar nsevil-last-paste nil
;;   "Information about the latest paste.
;; This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
;; CMD is the last paste-command (`nsevil-paste-before',
;; `nsevil-paste-after' or `nsevil-visual-paste'), COUNT is the repeat
;; count of the paste, POINT is the position of point before the
;; paste, BEG end END are the region of the inserted
;; text. FIRSTVISUAL is t if and only if the previous command was
;; the first visual paste (i.e. before any paste-pop).")

;; (nsevil-define-local-var nsevil-last-undo-entry nil
;;   "Information about the latest undo entry in the buffer.
;; This should be a pair (OBJ . CONS) where OBJ is the entry as an
;; object, and CONS is a copy of the entry.")

(nsevil-define-local-var nsevil-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

;; (defvar nsevil-last-insertion nil
;;   "The last piece of inserted text.")

;; (defvar nsevil-last-small-deletion nil
;;   "The last piece of deleted text.
;; The text should be less than a line.")

;; (defvar nsevil-was-yanked-without-register t
;;   "Whether text being saved to the numbered-register ring was
;; not deleted and not yanked to a specific register.")

;; (defvar nsevil-paste-count nil
;;   "The count argument of the current paste command.")

;; (defvar nsevil-temporary-undo nil
;;   "When undo is disabled in current buffer.
;; Certain commands depending on undo use this variable
;; instead of `buffer-undo-list'.")

;; (nsevil-define-local-var nsevil-undo-list-pointer nil
;;   "Everything up to this mark is united in the undo-list.")

;; (defvar nsevil-in-single-undo nil
;;   "Set to non-nil if the current undo steps are connected.")

;; (defvar nsevil-flash-timer nil
;;   "Timer for flashing search results.")

;; (defvar nsevil-search-prompt nil
;;   "String to use for search prompt.")

;; (defvar nsevil-search-forward-history nil
;;   "History of forward searches.")

;; (defvar nsevil-search-backward-history nil
;;   "History of backward searches.")

;; (defvar nsevil-inner-text-objects-map (make-sparse-keymap)
;;   "Keymap for inner text objects.")

;; (defvar nsevil-outer-text-objects-map (make-sparse-keymap)
;;   "Keymap for outer text objects.")

;; (defvar nsevil-window-map (make-sparse-keymap)
;;   "Keymap for window-related commands.")

(nsevil-define-local-var nsevil-input-method nil
  "Input method used in Insert state and Emacs state.")

;; ;;; Visual state

;; (nsevil-define-local-var nsevil-visual-beginning nil
;;   "The beginning of the Visual selection, a marker.")

;; (nsevil-define-local-var nsevil-visual-end nil
;;   "The end of the Visual selection, a marker.")

;; (nsevil-define-local-var nsevil-visual-point nil
;;   "The position of point in Visual state, a marker.")

;; (nsevil-define-local-var nsevil-visual-mark nil
;;   "The position of mark in Visual state, a marker.")

;; (nsevil-define-local-var nsevil-visual-previous-mark nil
;;   "The position of mark before Visual state, a marker.")

;; (nsevil-define-local-var nsevil-visual-selection nil
;;   "The kind of Visual selection.
;; This is a selection as defined by `nsevil-define-visual-selection'.")

;; ;; we could infer the direction by comparing `nsevil-visual-mark'
;; ;; and `nsevil-visual-point', but destructive operations may
;; ;; displace the markers
;; (nsevil-define-local-var nsevil-visual-direction 0
;;   "Whether point follows mark in Visual state.
;; Negative if point precedes mark, otherwise positive.
;; See also the function `nsevil-visual-direction'.")

;; (nsevil-define-local-var nsevil-visual-properties nil
;;   "Property list of miscellaneous Visual properties.")

;; (nsevil-define-local-var nsevil-visual-region-expanded nil
;;   "Whether the region matches the Visual selection.
;; That is, whether the positions of point and mark have been
;; expanded to coincide with the selection's boundaries.
;; This makes the selection available to functions acting
;; on Emacs' region.")

;; (nsevil-define-local-var nsevil-visual-overlay nil
;;   "Overlay for highlighting the Visual selection.
;; Not used for blockwise selections, in which case
;; see `nsevil-visual-block-overlays'.")

;; (nsevil-define-local-var nsevil-visual-block-overlays nil
;;   "Overlays for Visual Block selection, one for each line.
;; They are reused to minimize flicker.")

(defvar nsevil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

;; (nsevil-define-local-var nsevil-visual-x-select-timer nil
;;   "Timer for updating the X selection in visual state.")

;; (defvar nsevil-visual-x-select-timeout 0.1
;;   "Time in seconds for the update of the X selection.")

;; (declare-function origami-open-all-nodes "ext:origami.el")
;; (declare-function origami-close-all-nodes "ext:origami.el")
;; (declare-function origami-toggle-node "ext:origami.el")
;; (declare-function origami-open-node "ext:origami.el")
;; (declare-function origami-open-node-recursively "ext:origami.el")
;; (declare-function origami-close-node "ext:origami.el")

;; (defvar nsevil-fold-list
;;   `(((vdiff-mode)
;;      :open-all   vdiff-open-all-folds
;;      :close-all  vdiff-close-all-folds
;;      :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
;;      :open       ,(lambda () (call-interactively 'vdiff-open-fold))
;;      :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
;;      :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
;;     ((vdiff-3way-mode)
;;      :open-all   vdiff-open-all-folds
;;      :close-all  vdiff-close-all-folds
;;      :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
;;      :open       ,(lambda () (call-interactively 'vdiff-open-fold))
;;      :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
;;      :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
;;     ((hs-minor-mode)
;;      :open-all   hs-show-all
;;      :close-all  hs-hide-all
;;      :toggle     hs-toggle-hiding
;;      :open       hs-show-block
;;      :open-rec   nil
;;      :close      hs-hide-block)
;;     ((hide-ifdef-mode)
;;      :open-all   show-ifdefs
;;      :close-all  hide-ifdefs
;;      :toggle     nil
;;      :open       show-ifdef-block
;;      :open-rec   nil
;;      :close      hide-ifdef-block)
;;     ((outline-mode
;;       outline-minor-mode
;;       org-mode
;;       markdown-mode)
;;      :open-all   show-all
;;      :close-all  ,(lambda ()
;;                     (with-no-warnings (hide-sublevels 1)))
;;      :toggle     outline-toggle-children
;;      :open       ,(lambda ()
;;                     (with-no-warnings
;;                       (show-entry)
;;                       (show-children)))
;;      :open-rec   show-subtree
;;      :close      hide-subtree)
;;     ((origami-mode)
;;      :open-all   ,(lambda () (origami-open-all-nodes (current-buffer)))
;;      :close-all  ,(lambda () (origami-close-all-nodes (current-buffer)))
;;      :toggle     ,(lambda () (origami-toggle-node (current-buffer) (point)))
;;      :open       ,(lambda () (origami-open-node (current-buffer) (point)))
;;      :open-rec   ,(lambda () (origami-open-node-recursively (current-buffer) (point)))
;;      :close      ,(lambda () (origami-close-node (current-buffer) (point)))))
;;   "Actions to be performed for various folding operations.

;; The value should be a list of fold handlers, were a fold handler has
;; the format:

;;   ((MODES) PROPERTIES)

;; MODES acts as a predicate, containing the symbols of all major or
;; minor modes for which the handler should match.  For example:

;;   '((outline-minor-mode org-mode) ...)

;; would match for either outline-minor-mode or org-mode, even though the
;; former is a minor mode and the latter is a major.

;; PROPERTIES specifies possible folding actions and the functions to be
;; applied in the event of a match on one (or more) of the MODES; the
;; supported properties are:

;;   - `:open-all'
;;     Open all folds.
;;   - `:close-all'
;;     Close all folds.
;;   - `:toggle'
;;     Toggle the display of the fold at point.
;;   - `:open'
;;     Open the fold at point.
;;   - `:open-rec'
;;     Open the fold at point recursively.
;;   - `:close'
;;     Close the fold at point.

;; Each value must be a function.  A value of `nil' will cause the action
;; to be ignored for that respective handler.  For example:

;;   `((org-mode)
;;      :close-all  nil
;;      :open       ,(lambda ()
;;                     (show-entry)
;;                     (show-children))
;;      :close      hide-subtree)

;; would ignore `:close-all' actions and invoke the provided functions on
;; `:open' or `:close'.")

;; ;;; Ex

;; (defvar nsevil-ex-map (make-sparse-keymap)
;;   "Keymap for Ex.
;; Key sequences bound in this map are immediately executed.")

;; (defvar nsevil-ex-completion-map (make-sparse-keymap)
;;   "Completion keymap for Ex.")

;; (defvar nsevil-ex-initial-input nil
;;   "Additional initial content of the ex command line.
;; This content of this variable is appended to the ex command line
;; if ex is started interactively.")

;; (defvar nsevil-ex-shell-argument-initialized nil
;;   "This variable is set to t if shell command completion has been initialized.
;; See `nsevil-ex-init-shell-argument-completion'.")

;; (defvar nsevil-ex-commands nil
;;   "Association list of command bindings and functions.")

;; (defvar nsevil-ex-history nil
;;   "History of Ex commands.")

;; (defvar nsevil-ex-current-buffer nil
;;   "The buffer from which Ex was started.")

;; (defvar nsevil-ex-expression nil
;;   "The evaluation tree.")

;; (defvar nsevil-ex-tree nil
;;   "The syntax tree.")

;; (defvar nsevil-ex-command nil
;;   "The current Ex command.")

;; (defvar nsevil-ex-previous-command nil
;;   "The previously executed Ex command.")

;; (defvar nsevil-ex-cmd nil
;;   "The current Ex command string.")

;; (defvar nsevil-ex-point nil
;;   "The position of `point' when the ex command has been called.")

;; (defvar nsevil-ex-range nil
;;   "The current range of the Ex command.")

;; (defvar nsevil-ex-bang nil
;;   "The \"!\" argument of the current Ex command.")

;; (defvar nsevil-ex-argument nil
;;   "The current argument of the Ex command.")

;; (defvar nsevil-ex-argument-handler nil
;;   "The argument handler for the current Ex command.")

;; (defvar nsevil-ex-argument-types nil
;;   "Association list of argument handlers.")

;; (defvar nsevil-previous-shell-command nil
;;   "The last shell command.")

;; ;; Searching
;; (defvar nsevil-ex-search-history nil
;;   "The history for the search command.")

;; (defvar nsevil-ex-search-direction nil
;;   "The direction of the current search, either 'forward or 'backward.")

;; (defvar nsevil-ex-search-count nil
;;   "The count if the current search.")

;; (defvar nsevil-ex-search-start-point nil
;;   "The point where the search started.")

;; (defvar nsevil-ex-search-overlay nil
;;   "The overlay for the current search result.")

;; (defvar nsevil-ex-search-pattern nil
;;   "The last search pattern.")

;; (defvar nsevil-ex-search-offset nil
;;   "The last search offset.")

;; (defvar nsevil-ex-search-match-beg nil
;;   "The beginning position of the last match.")

;; (defvar nsevil-ex-search-match-end nil
;;   "The end position of the last match.")

;; (defvar nsevil-ex-substitute-pattern nil
;;   "The last substitute pattern.")

;; (defvar nsevil-ex-substitute-replacement nil
;;   "The last substitute replacement.")

;; (defvar nsevil-ex-substitute-flags nil
;;   "The last substitute flags.")

;; (defvar nsevil-ex-substitute-current-replacement nil
;;   "The actual replacement.")

;; (defvar nsevil-ex-last-was-search nil
;;   "Non-nil if the previous was a search.
;; Otherwise the previous command is assumed as substitute.")

;; ;;; Command line window

;; (defvar nsevil-command-window-current-buffer nil
;;   "The buffer from which the command line window was called.")

;; (nsevil-define-local-var nsevil-command-window-execute-fn nil
;;   "The command to execute when exiting the command line window.")

;; (nsevil-define-local-var nsevil-command-window-cmd-key nil
;;   "The key for the command that opened the command line window (:, /, or ?).")

;; ;; The lazy-highlighting framework.
;; (nsevil-define-local-var nsevil-ex-active-highlights-alist nil
;;   "An alist of currently active highlights.")

;; (nsevil-define-local-var nsevil-ex-hl-update-timer nil
;;   "Time used for updating highlights.")

;; (defvar nsevil-ex-search-keymap (make-sparse-keymap)
;;   "Keymap used in ex-search-mode.")
;; (define-key nsevil-ex-search-keymap [escape] 'abort-recursive-edit)
;; (set-keymap-parent nsevil-ex-search-keymap minibuffer-local-map)

;; (defconst nsevil-version
;;   (eval-when-compile
;;     (with-temp-buffer
;;       (let ((dir (file-name-directory (or load-file-name
;;                                           byte-compile-current-file))))
;;         ;; git repository
;;         (if (and (file-exists-p (concat dir "/.git"))
;;                  (ignore-errors
;;                    (zerop (call-process "git" nil '(t nil) nil
;;                                         "rev-parse"
;;                                         "--short" "HEAD"))))
;;             (progn
;;               (goto-char (point-min))
;;               (concat "nsevil-git-"
;;                       (buffer-substring (point-min)
;;                                         (line-end-position))))
;;           ;; no repo, use plain version
;;           "1.14.0"))))
;;   "The current version of Nsevil")

;; (defcustom nsevil-want-integration t
;;   "Whether to load nsevil-integration.el.
;; This variable must be set before Nsevil is loaded."
;;   :type 'boolean
;;   :group 'nsevil)

;; (defcustom nsevil-want-keybinding t
;;   "Whether to load nsevil-keybindings.el.

;; This loads a set of keybindings for nsevil in other modes as well as
;; setting the initial nsevil state in those modes.

;; This variable must be set before nsevil is loaded."
;;   :type 'boolean
;;   :group 'nsevil)

(defcustom nsevil-want-minibuffer nil
  "Whether to enable Nsevil in minibuffer(s)."
  :type 'boolean
  :group 'nsevil
  :set #'(lambda (sym value)
           (set-default sym value)
           (if value
               (add-hook 'minibuffer-setup-hook 'nsevil-initialize)
             (remove-hook 'minibuffer-setup-hook 'nsevil-initialize))))

;; (defun nsevil--redo-placeholder (_count)
;;   (user-error "Customize `nsevil-undo-system' for redo functionality."))

;; (defvar nsevil-undo-function 'undo
;;   "Function to be used by `nsevil-undo'.
;; Customized via `nsevil-undo-system'.")

;; (defvar nsevil-redo-function 'nsevil--redo-placeholder
;;   "Function to be used by 'nsevil-redo'.
;; Customized via `nsevil-undo-system'.")

;; (defun nsevil-set-undo-system (system)
;;   "Set `nsevil-undo-function' and `nsevil-redo-function` by SYSTEM."
;;   (cond
;;    ((not system)
;;     (setq nsevil-undo-function 'undo
;;           nsevil-redo-function 'nsevil--redo-placeholder))
;;    ((eq system 'undo-redo)
;;     (setq nsevil-undo-function 'undo-only
;;           nsevil-redo-function 'undo-redo))
;;    ((eq system 'undo-tree)
;;     (setq nsevil-undo-function 'undo-tree-undo
;;           nsevil-redo-function 'undo-tree-redo))
;;    ((eq system 'undo-fu)
;;     (setq nsevil-undo-function 'undo-fu-only-undo
;;           nsevil-redo-function 'undo-fu-only-redo))
;;    (t
;;     (error "Unknown undo system %s" system))))

;; (defcustom nsevil-undo-system nil
;;   "Undo system Nsevil should use.  If equal to `undo-tree' or
;; `undo-fu', those packages must be installed.  If equal to
;; `undo-tree', `undo-tree-mode' must also be activated.  If equal
;; to `undo-redo', Nsevil uses commands natively available in Emacs 28."
;;   :type '(choice (const :tag "Vanilla undo" nil)
;;                  (const undo-redo)
;;                  (const undo-tree)
;;                  (const undo-fu))
;;   :group 'nsevil
;;   :set #'(lambda (sym value)
;;            (nsevil-set-undo-system value)
;;            (set-default sym value)))

;; (defun nsevil-version ()
;;   (interactive)
;;   (message "Nsevil version %s" nsevil-version))

(provide 'nsevil-vars)

;; ;;; nsevil-vars.el ends here
