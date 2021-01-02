;;; nsevil-common.el --- Common functions and utilities -*- lexical-binding: t -*-
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

(require 'nsevil-vars)
;; (require 'nsevil-digraphs)
;; (require 'rect)
(require 'thingatpt)
(require 'cl-lib)

;;; Code:

(declare-function nsevil-visual-state-p "nsevil-states")
(declare-function nsevil-visual-restore "nsevil-states")
(declare-function nsevil-motion-state "nsevil-states")

(condition-case nil
    (require 'windmove)
  (error
   (message "nsevil: Could not load `windmove', \
window commands not available.")
   nil))

;;; Compatibility with different Emacs versions

;; x-set-selection and x-get-selection have been deprecated since 25.1
;; by gui-set-selection and gui-get-selection
(defalias 'nsevil-get-selection
  (if (fboundp 'gui-get-selection) 'gui-get-selection 'x-get-selection))
(defalias 'nsevil-set-selection
  (if (fboundp 'gui-set-selection) 'gui-set-selection 'x-set-selection))

(defmacro nsevil-called-interactively-p ()
  "Wrapper for `called-interactively-p'.
In older versions of Emacs, `called-interactively-p' takes
no arguments.  In Emacs 23.2 and newer, it takes one argument."
  (called-interactively-p 'any))
(make-obsolete 'nsevil-called-interactively-p
               "please use (called-interactively-p 'any) instead."
               "Git commit 222b791")

;; macro helper
(eval-and-compile
  (defun nsevil-unquote (exp)
    "Return EXP unquoted."
    (while (eq (car-safe exp) 'quote)
      (setq exp (cadr exp)))
    exp))

(defun nsevil-delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "nsevil-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))
(put 'nsevil-delay 'lisp-indent-function 2)

;;; List functions

(defmacro nsevil--add-to-alist (list-var &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list.

\(fn LIST-VAR KEY VAL &rest ELEMENTS)"
  (when (eq (car-safe list-var) 'quote)
    (setq list-var (cadr list-var)))
  `(progn
     ,@(if (version< emacs-version "26")
           ;; TODO: Remove this path when support for Emacs 25 is dropped
           (cl-loop for (key val) on elements by #'cddr
                    collect `(let* ((key ,key)
                                    (val ,val)
                                    (cell (assoc key ,list-var)))
                               (if cell
                                   (setcdr cell val)
                                 (push (cons key val) ,list-var))))
         (cl-loop for (key val) on elements by #'cddr
                  collect `(setf (alist-get ,key ,list-var nil nil #'equal) ,val)))
     ,list-var))

(defun nsevil-add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (set list-var (append (symbol-value list-var)
                            (list (cons key val)))))
    (if elements
        (with-no-warnings
          (apply #'nsevil-add-to-alist list-var elements))
      (symbol-value list-var))))

(make-obsolete 'nsevil-add-to-alist
               "use `nsevil--add-to-alist' instead. You may need to recompile code with nsevil macros."
               "1.13.1")

;; custom version of `delete-if'
(defun nsevil-filter-list (predicate list &optional pointer)
  "Delete by side-effect all items satisfying PREDICATE in LIST.
Stop when reaching POINTER.  If the first item satisfies PREDICATE,
there is no way to remove it by side-effect; therefore, write
\(setq foo (nsevil-filter-list 'predicate foo)) to be sure of
changing the value of `foo'."
  (let ((tail list) elt head)
    (while (and tail (not (eq tail pointer)))
      (setq elt (car tail))
      (cond
       ((funcall predicate elt)
        (setq tail (cdr tail))
        (if head
            (setcdr head tail)
          (setq list tail)))
       (t
        (setq head tail
              tail (cdr tail)))))
    list))

(defun nsevil-member-if (predicate list &optional pointer)
  "Find the first item satisfying PREDICATE in LIST.
Stop when reaching POINTER, which should point at a link
in the list."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun nsevil-member-recursive-if (predicate tree)
  "Find the first item satisfying PREDICATE in TREE."
  (cond
   ((funcall predicate tree)
    tree)
   ((listp tree)
    (catch 'done
      (dolist (elt tree)
        (when (setq elt (nsevil-member-recursive-if predicate elt))
          (throw 'done elt)))))))

(defun nsevil-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
Elements are compared with `eq'."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (push elt result)))
    (nreverse (cl-remove-duplicates result :test #'eq))))

(defun nsevil-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
An alist is a list of cons cells (KEY . VALUE) where each key
may occur only once. Later values overwrite earlier values."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (push elt result)))
    (nreverse result)))

(defun nsevil-concat-plists (&rest sequences)
  "Concatenate property lists, removing duplicates.
A property list is a list (:KEYWORD1 VALUE1 :KEYWORD2 VALUE2...)
where each keyword may occur only once. Later values overwrite
earlier values."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun nsevil-concat-keymap-alists (&rest sequences)
  "Concatenate keymap association lists, removing duplicates.
A keymap alist is a list of cons cells (VAR . MAP) where each keymap
may occur only once, but where the variables may be repeated
\(e.g., (VAR . MAP1) (VAR . MAP2) is allowed). The order matters,
with the highest priority keymaps being listed first."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (unless (rassq (cdr-safe elt) result)
          (push elt result))))
    (nreverse result)))

(defun nsevil-plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (nsevil-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defun nsevil-get-property (alist key &optional prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list.
If PROP is nil, return all properties for KEY.
If KEY is t, return an association list of keys
and their PROP values."
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun nsevil-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (set alist-var
       (let* ((alist (symbol-value alist-var))
              (plist (cdr (assq key alist))))
         (setq plist (plist-put plist prop val))
         (when properties
           (setq plist (nsevil-concat-plists plist properties)
                 val (car (last properties))))
         (setq alist (assq-delete-all key alist))
         (push (cons key plist) alist)))
  val)

(defun nsevil-state-property (state prop &optional value)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `nsevil-define-state'.
STATE is the state's symbolic name.
If VALUE is non-nil and the value is a variable,
return the value of that variable."
  (let ((val (nsevil-get-property nsevil-state-properties state prop)))
    (if (and value (symbolp val) (boundp val))
        (symbol-value val)
      val)))

(defmacro nsevil-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (nsevil-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((nsevil-swap ,that ,@vars)))))

(defmacro nsevil-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  (let ((sorted (make-symbol "sortvar")))
    `(let ((,sorted (sort (list ,min ,max ,@vars) '<)))
       (setq ,min (pop ,sorted)
             ,max (pop ,sorted)
             ,@(apply #'append
                      (mapcar #'(lambda (var)
                                  (list var `(pop ,sorted)))
                              vars))))))

(defun nsevil-vector-to-string (vector)
  "Turns vector into a string, changing <escape> to '\\e'"
  (mapconcat (lambda (c)
               (if (equal c 'escape)
                   "\e"
                 (make-string 1 c)))
             vector
             ""))

;;; Command properties

(defmacro nsevil-define-command (command &rest body)
  "Define a command COMMAND.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((interactive '(interactive))
        arg args doc doc-form key keys)
    ;; collect arguments
    (when (listp (car-safe body))
      (setq args (pop body)))
    ;; collect docstring
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    ;; collect keywords
    (setq keys (plist-put keys :repeat t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (unless nil ; TODO: add keyword check
        (setq keys (plist-put keys key arg))))
    ;; collect `interactive' form
    (when (and body (consp (car body))
               (eq (car (car body)) 'interactive))
      (let* ((iform (pop body))
             (result (apply #'nsevil-interactive-form (cdr iform)))
             (form (car result))
             (attrs (cdr result)))
        (setq interactive `(interactive ,form)
              keys (nsevil-concat-plists keys attrs))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,interactive
             (ignore ,@(cl-set-difference args '(&optional &rest)))
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args
                            ,interactive
                            ,@body)
                       command)))
         (apply #'nsevil-set-command-properties func ',keys)
         func))))

;; If no Nsevil properties are defined for the command, several parts of
;; Nsevil apply certain default rules; e.g., the repeat system decides
;; whether the command is repeatable by monitoring buffer changes.
(defun nsevil-has-command-property-p (command property)
  "Whether COMMAND has Nsevil PROPERTY.
See also `nsevil-has-command-properties-p'."
  (plist-member (nsevil-get-command-properties command) property))

(defun nsevil-has-command-properties-p (command)
  "Whether Nsevil properties are defined for COMMAND.
See also `nsevil-has-command-property-p'."
  (and (nsevil-get-command-properties command) t))

(defun nsevil-get-command-property (command property &optional default)
  "Return the value of Nsevil PROPERTY of COMMAND.
If the command does not have the property, return DEFAULT.
See also `nsevil-get-command-properties'."
  (if (nsevil-has-command-property-p command property)
      (nsevil-get-property nsevil-command-properties command property)
    default))

(defun nsevil-get-command-properties (command)
  "Return all Nsevil properties of COMMAND.
See also `nsevil-get-command-property'."
  (nsevil-get-property nsevil-command-properties command))

(defun nsevil-set-command-property (command property value)
  "Set PROPERTY to VALUE for COMMAND.
To set multiple properties at once, see
`nsevil-set-command-properties' and `nsevil-add-command-properties'."
  (nsevil-put-property 'nsevil-command-properties command property value))
(defalias 'nsevil-put-command-property 'nsevil-set-command-property)

(defun nsevil-add-command-properties (command &rest properties)
  "Add PROPERTIES to COMMAND.
PROPERTIES should be a property list.
To replace all properties at once, use `nsevil-set-command-properties'."
  (apply #'nsevil-put-property
         'nsevil-command-properties command properties))

(defun nsevil-set-command-properties (command &rest properties)
  "Replace all of COMMAND's properties with PROPERTIES.
PROPERTIES should be a property list.
This erases all previous properties; to only add properties,
use `nsevil-set-command-property'."
  (setq nsevil-command-properties
        (assq-delete-all command nsevil-command-properties))
  (when properties
    (apply #'nsevil-add-command-properties command properties)))

(defun nsevil-remove-command-properties (command &rest properties)
  "Remove PROPERTIES from COMMAND.
PROPERTIES should be a list of properties (:PROP1 :PROP2 ...).
If PROPERTIES is the empty list, all properties are removed."
  (let (plist)
    (when properties
      (setq plist (nsevil-get-command-properties command))
      (dolist (property properties)
        (setq plist (nsevil-plist-delete property plist))))
    (apply #'nsevil-set-command-properties command plist)))

(defun nsevil-yank-handler (&optional motion)
  "Return the yank handler for MOTION.
MOTION defaults to the current motion."
  (setq motion (or motion nsevil-this-motion))
  (nsevil-get-command-property motion :yank-handler))

(defun nsevil-declare-motion (command)
  "Declare COMMAND to be a movement function.
This ensures that it behaves correctly in visual state."
  (nsevil-add-command-properties command :keep-visual t :repeat 'motion))

(defun nsevil-declare-repeat (command)
  "Declare COMMAND to be repeatable."
  (nsevil-add-command-properties command :repeat t))

(defun nsevil-declare-not-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (nsevil-add-command-properties command :repeat nil))

(defun nsevil-declare-ignore-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (nsevil-add-command-properties command :repeat 'ignore))

(defun nsevil-declare-change-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes rather than
keystrokes."
  (nsevil-add-command-properties command :repeat 'change))

(defun nsevil-declare-insert-at-point-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes."
  (nsevil-add-command-properties command :repeat 'insert-at-point))

(defun nsevil-declare-abort-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (nsevil-add-command-properties command :repeat 'abort))

(defun nsevil-delimited-arguments (string &optional num)
  "Parse STRING as a sequence of delimited arguments.
Returns a list of NUM strings, or as many arguments as
the string contains. The first non-blank character is
taken to be the delimiter. If some arguments are missing
from STRING, the resulting list is padded with nil values.
Two delimiters following directly after each other gives
an empty string."
  (save-match-data
    (let ((string (or string ""))
          (count (or num -1)) (idx 0)
          argument delim match result)
      (when (string-match "^[[:space:]]*\\([^[:space:]]\\)" string)
        (setq delim (match-string 1 string)
              argument (format "%s\\(\\(?:[\\].\\|[^%s]\\)*\\)"
                               (regexp-quote delim)
                               delim))
        (while (and (/= count 0) (string-match argument string idx))
          (setq match (match-string 1 string)
                idx (match-end 1)
                count (1- count))
          (when (= count 0)
            (unless (save-match-data
                      (string-match
                       (format "%s[[:space:]]*$" delim) string idx))
              (setq match (substring string (match-beginning 1)))))
          (unless (and (zerop (length match))
                       (zerop (length (substring string idx))))
            (push match result))))
      (when (and num (< (length result) num))
        (dotimes (_ (- num (length result)))
          (push nil result)))
      (nreverse result))))

(defun nsevil-concat-charsets (&rest sets)
  "Concatenate character sets.
A character set is the part between [ and ] in a regular expression.
If any character set is complemented, the result is also complemented."
  (let ((bracket "") (complement "") (hyphen "") result)
    (save-match-data
      (dolist (set sets)
        (when (string-match-p "^\\^" set)
          (setq set (substring set 1)
                complement "^"))
        (when (string-match-p "^]" set)
          (setq set (substring set 1)
                bracket "]"))
        (when (string-match-p "^-" set)
          (setq set (substring set 1)
                hyphen "-"))
        (setq result (concat result set)))
      (format "%s%s%s%s" complement bracket hyphen result))))

;;; Key sequences

(defun nsevil-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let (count negative)
    (when input (setq unread-command-events (append input unread-command-events)))
    (catch 'done
      (while t
        (let ((seq (read-key-sequence "")))
          (when seq
            (let ((cmd (key-binding seq)))
              (cond
               ((null cmd) (throw 'done (list nil nil)))
               ((arrayp cmd) ; keyboard macro, recursive call
                (let ((cmd (nsevil-keypress-parser cmd)))
                  (throw 'done
                         (list (car cmd)
                               (if (or count (cadr cmd))
                                   (list (car cmd) (* (or count 1)
                                                      (or (cadr cmd) 1))))))))
               ((or (eq cmd #'digit-argument)
                    (and (eq cmd 'nsevil-digit-argument-or-nsevil-beginning-of-line)
                         count))
                (let* ((event (aref seq (- (length seq) 1)))
                       (char (or (when (characterp event) event)
                                 (when (symbolp event)
                                   (get event 'ascii-character))))
                       (digit (if (or (characterp char) (integerp char))
                                  (- (logand char ?\177) ?0))))
                  (setq count (+ (* 10 (or count 0)) digit))))
               ((eq cmd #'negative-argument)
                (setq negative (not negative)))
               (t
                (throw 'done (list cmd
                                   (and count
                                        (* count
                                           (if negative -1 1))))))))))))))

(defun nsevil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map nil)
        (overriding-local-map nsevil-read-key-map)
        seq char cmd)
    (unwind-protect
        (condition-case nil
            (progn
              (define-key new-global-map [menu-bar]
                (lookup-key global-map [menu-bar]))
              (define-key new-global-map [tab-bar]
                (lookup-key global-map [tab-bar]))
              (define-key new-global-map [tool-bar]
                (lookup-key global-map [tool-bar]))
              (setq new-global-map
                    (append new-global-map
                            (list (make-char-table 'display-table
                                                   'self-insert-command))))
              (use-global-map new-global-map)
              (setq seq (read-key-sequence prompt nil t)
                    char (aref seq 0)
                    cmd (key-binding seq))
              (while (arrayp cmd)
                (setq char (aref cmd 0)
                      cmd (key-binding cmd)))
              (cond
               ((eq cmd 'self-insert-command)
                char)
               (cmd
                (call-interactively cmd))
               (t
                (user-error "No replacement character typed"))))
          (quit
           (when (fboundp 'nsevil-repeat-abort)
             (nsevil-repeat-abort))
           (signal 'quit nil)))
      (use-global-map old-global-map))))

(defun nsevil-read-quoted-char ()
  "Command that calls `read-quoted-char'.
This command can be used wherever `read-quoted-char' is required
as a command. Its main use is in the `nsevil-read-key-map'."
  (interactive)
  (read-quoted-char))

(defun nsevil-read-digraph-char (&optional hide-chars)
  "Read two keys from keyboard forming a digraph.
This function creates an overlay at (point), hiding the next
HIDE-CHARS characters. HIDE-CHARS defaults to 1."
  (interactive)
  (let (char1 char2 string overlay)
    (unwind-protect
        (progn
          (setq overlay (make-overlay (point)
                                      (min (point-max)
                                           (+ (or hide-chars 1)
                                              (point)))))
          (overlay-put overlay 'invisible t)
          ;; create overlay prompt
          (setq string "?")
          (put-text-property 0 1 'face 'minibuffer-prompt string)
          ;; put cursor at (i.e., right before) the prompt
          (put-text-property 0 1 'cursor t string)
          (overlay-put overlay 'after-string string)
          (setq char1 (read-key))
          (setq string (string char1))
          (put-text-property 0 1 'face 'minibuffer-prompt string)
          (put-text-property 0 1 'cursor t string)
          (overlay-put overlay 'after-string string)
          (setq char2 (read-key)))
      (delete-overlay overlay))
    (or (nsevil-digraph (list char1 char2))
        ;; use the last character if undefined
        char2)))

(defun nsevil-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `nsevil-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let (command prefix)
    (setq nsevil-this-type-modified nil)
    (unless motion
      (while (progn
               (setq command (nsevil-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (if count
                     (setq count (string-to-number
                                  (concat (number-to-string count)
                                          (number-to-string prefix))))
                   (setq count prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion nsevil-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion nsevil-visual-alist))))))))
    (when modifier
      (setq type (or type (nsevil-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier)))
      (setq nsevil-this-type-modified type))
    (list motion count type)))

(defun nsevil-mouse-events-p (keys)
  "Returns non-nil iff KEYS contains a mouse event."
  (catch 'done
    (dotimes (i (length keys))
      (when (or (and (fboundp 'mouse-event-p)
                     (mouse-event-p (aref keys i)))
                (mouse-movement-p (aref keys i)))
        (throw 'done t)))
    nil))

(defun nsevil-extract-count (keys)
  "Splits the key-sequence KEYS into prefix-argument and the rest.
Returns the list (PREFIX CMD SEQ REST), where PREFIX is the
prefix count, CMD the command to be executed, SEQ the subsequence
calling CMD, and REST is all remaining events in the
key-sequence. PREFIX and REST may be nil if they do not exist.
If a command is bound to some keyboard macro, it is expanded
recursively."
  (catch 'done
    (let* ((len (length keys))
           (beg 0)
           (end 1)
           (found-prefix nil))
      (while (and (<= end len))
        (let ((cmd (key-binding (substring keys beg end))))
          (cond
           ((memq cmd '(undefined nil))
            (user-error "No command bound to %s" (substring keys beg end)))
           ((arrayp cmd) ; keyboard macro, replace command with macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (nsevil-get-command-property
                          cmd :digit-argument-redirection)))
                ;; skip those commands
                (setq found-prefix t ; found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done
                     (list (unless (zerop beg)
                             (string-to-number
                              (concat (substring keys 0 beg))))
                           cmd
                           (substring keys beg end)
                           (when (< end len)
                             (substring keys end))))))
           (t ; append a further event
            (setq end (1+ end))))))
      (user-error "Key sequence contains no complete binding"))))

(defmacro nsevil-redirect-digit-argument (map keys target)
  "Bind a wrapper function calling TARGET or `digit-argument'.
MAP is a keymap for binding KEYS to the wrapper for TARGET.
The wrapper only calls `digit-argument' if a prefix-argument
has already been started; otherwise TARGET is called."
  (let* ((target (eval target))
         (wrapper (intern (format "nsevil-digit-argument-or-%s"
                                  target))))
    `(progn
       (define-key ,map ,keys ',wrapper)
       (nsevil-define-command ,wrapper ()
         :digit-argument-redirection ,target
         :keep-visual t
         :repeat nil
         (interactive)
         (cond
          (current-prefix-arg
           (setq this-command #'digit-argument)
           (call-interactively #'digit-argument))
          (t
           (let ((target (or (command-remapping #',target)
                             #',target)))
             (setq this-command target)
             (call-interactively target))))))))

(defun nsevil-extract-append (file-or-append)
  "Return an (APPEND . FILENAME) pair based on FILE-OR-APPEND.
FILE-OR-APPEND should either be a filename or a \">> FILE\"
directive.  APPEND will be t if FILE-OR-APPEND is an append
directive and nil otherwise.  FILENAME will be the extracted
filename."
  (if (and (stringp file-or-append)
           (string-match "\\(>> *\\)" file-or-append))
      (cons t (substring file-or-append(match-end 1)))
    (cons nil file-or-append)))

(defun nsevil-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(defun nsevil-lookup-key (map key)
  "Returns non-nil value if KEY is bound in MAP."
  (let ((definition (lookup-key map key)))
    (if (numberp definition) ; in-band error
        nil
      definition)))

;;; Display

(defun nsevil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (not (functionp specs))
               (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (nsevil-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

(defun nsevil-set-cursor-color (color)
  "Set the cursor color to COLOR."
  (unless (equal (frame-parameter nil 'cursor-color) color)
    ;; `set-cursor-color' forces a redisplay, so only
    ;; call it when the color actually changes
    (set-cursor-color color)))

(defun nsevil-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
BUFFER defaults to the current buffer.  If STATE is nil the
cursor type is either `nsevil-force-cursor' or the current state."
  (when (and (boundp 'nsevil-local-mode) nsevil-local-mode)
    (let* ((state (or state nsevil-force-cursor nsevil-state 'normal))
           (default (or nsevil-default-cursor t))
           (cursor (nsevil-state-property state :cursor t))
           (color (or (and (stringp cursor) cursor)
                      (and (listp cursor)
                           (nsevil-member-if #'stringp cursor))
                      (frame-parameter nil 'cursor-color))))
      (with-current-buffer (or buffer (current-buffer))
        ;; if both STATE and `nsevil-default-cursor'
        ;; specify a color, don't set it twice
        (when (and color (listp default))
          (setq default (nsevil-filter-list #'stringp default)))
        (nsevil-set-cursor default)
        (nsevil-set-cursor cursor)))))

(defmacro nsevil-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun)
           (debug t))
  `(let ((cursor cursor-type)
         (color (frame-parameter (selected-frame) 'cursor-color))
         (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (nsevil-set-cursor cursor)
       (nsevil-set-cursor color))))

(defun nsevil-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (unless nsevil-no-display
    (let (message-log-max)
      (apply #'message string args))))

(defun nsevil-echo-area-save ()
  "Save the current echo area in `nsevil-echo-area-message'."
  (setq nsevil-echo-area-message (current-message)))

(defun nsevil-echo-area-restore ()
  "Restore the echo area from `nsevil-echo-area-message'.
Does not restore if `nsevil-write-echo-area' is non-nil."
  (unless nsevil-write-echo-area
    (if nsevil-echo-area-message
        (message "%s" nsevil-echo-area-message)
      (message nil)))
  (setq nsevil-echo-area-message nil
        nsevil-write-echo-area nil))

;; toggleable version of `with-temp-message'
(defmacro nsevil-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         nsevil-echo-area-message
         nsevil-write-echo-area)
     (unwind-protect
         (progn
           (nsevil-echo-area-save)
           ,@body)
       (nsevil-echo-area-restore))))

;;; Movement

(defun nsevil-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

(defmacro nsevil-save-goal-column (&rest body)
  "Restores the goal column after execution of BODY.
See also `nsevil-save-column'."
  (declare (indent defun)
           (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defmacro nsevil-save-column (&rest body)
  "Restores the column after execution of BODY.
See also `nsevil-save-goal-column'."
  (declare (indent defun)
           (debug t))
  `(let ((col (current-column)))
     (nsevil-save-goal-column
       ,@body
       (move-to-column col))))

(defun nsevil-narrow (beg end)
  "Restrict the buffer to BEG and END.
BEG or END may be nil, specifying a one-sided restriction including
`point-min' or `point-max'. See also `nsevil-with-restriction.'"
  (setq beg (or (nsevil-normalize-position beg) (point-min)))
  (setq end (or (nsevil-normalize-position end) (point-max)))
  (narrow-to-region beg end))

(defmacro nsevil-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil as passed to `nsevil-narrow'; this creates
a one-sided restriction."
  (declare (indent 2)
           (debug t))
  `(save-restriction
     (let ((nsevil-restriction-stack
            (cons (cons (point-min) (point-max)) nsevil-restriction-stack)))
       (nsevil-narrow ,beg ,end)
       ,@body)))

(defmacro nsevil-without-restriction (&rest body)
  "Execute BODY with the top-most narrowing removed.
This works only if the previous narrowing has been generated by
`nsevil-with-restriction'."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (widen)
     (narrow-to-region (car (car nsevil-restriction-stack))
                       (cdr (car nsevil-restriction-stack)))
     (let ((nsevil-restriction-stack (cdr nsevil-restriction-stack)))
       ,@body)))

(defmacro nsevil-narrow-to-field (&rest body)
  "Narrow to the current field."
  (declare (indent defun)
           (debug t))
  `(nsevil-with-restriction (field-beginning) (field-end)
     ,@body))

(defmacro nsevil-loop (spec &rest body)
  "Loop with countdown variable.
Evaluate BODY with VAR counting down from COUNT to 0.
COUNT can be negative, in which case VAR counts up instead.
The return value is the value of VAR when the loop
terminates, which is 0 if the loop completes successfully.
RESULT specifies a variable for storing this value.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug dolist))
  (let* ((i (make-symbol "loopvar"))
         (var (pop spec))
         (count (pop spec))
         (result (pop spec)))
    (setq var (or (unless (eq var result) var) i)
          result (or result var))
    `(let ((,var ,count))
       (setq ,result ,var)
       (while (/= ,var 0)
         ,@body
         (if (> ,var 0)
             (setq ,var (1- ,var))
           (setq ,var (1+ ,var)))
         (setq ,result ,var))
       ,var)))

;;; Motions

(defmacro nsevil-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. RESULT, if specified, holds
the number of unsuccessful iterations, which is 0 if the loop
completes successfully. This is also the return value.

Each iteration must move point; if point does not change,
the loop immediately quits. See also `nsevil-loop'.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (countval (or (pop spec) 0))
         (result (pop spec))
         (i (make-symbol "loopvar"))
         (count (make-symbol "countvar"))
         (done (make-symbol "donevar"))
         (orig (make-symbol "origvar")))
    `(let* ((,count ,countval)
            (,var (if (< ,count 0) -1 1)))
       (catch ',done
         (nsevil-loop (,i ,count ,result)
           (let ((,orig (point)))
             ,@body
             (when (= (point) ,orig)
               (throw ',done ,i))))))))

(defmacro nsevil-signal-without-movement (&rest body)
  "Catches errors provided point moves within this scope."
  (declare (indent defun)
           (debug t))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defun nsevil-signal-at-bob-or-eob (&optional count)
  "Signals error if `point' is at boundaries.
If `point' is at bob and COUNT is negative this function signal
'beginning-of-buffer. If `point' is at eob and COUNT is positive
this function singal 'end-of-buffer. This function should be used
in motions. COUNT defaults to 1."
  (setq count (or count 1))
  (cond
   ((< count 0) (nsevil-signal-at-bob))
   ((> count 0) (nsevil-signal-at-eob))))

(defun nsevil-signal-at-bob ()
  "Signals 'beginning-of-buffer if `point' is at bob.
This function should be used in backward motions. If `point' is at
bob so that no further backward motion is possible the error
'beginning-of-buffer is raised."
  (when (bobp) (signal 'beginning-of-buffer nil)))

(defun nsevil-signal-at-eob ()
  "Signals 'end-of-buffer if `point' is at eob.
This function should be used in forward motions. If `point' is close
to eob so that no further forward motion is possible the error
'end-of-buffer is raised. This is the case if `point' is at
`point-max' or if is one position before `point-max',
`nsevil-move-beyond-eol' is nil and `point' is not at the end
of a line. The latter is necessary because `point' cannot be
moved to `point-max' if `nsevil-move-beyond-eol' is nil and
the last line in the buffer is not empty."
  (when (or (eobp)
            (and (not (eolp))
                 (not nsevil-move-beyond-eol)
                 (save-excursion (forward-char) (eobp))))
    (signal 'end-of-buffer nil)))

;;; Markers

;; (defun nsevil-global-marker-p (char)
;;   "Whether CHAR denotes a global marker."
;;   (or (and (>= char ?A) (<= char ?Z))
;;       (assq char (default-value 'nsevil-markers-alist))))

;; (defun nsevil-set-marker (char &optional pos advance)
;;   "Set the marker denoted by CHAR to position POS.
;; POS defaults to the current position of point.
;; If ADVANCE is t, the marker advances when inserting text at it;
;; otherwise, it stays behind."
;;   (interactive (list (read-char)))
;;   (catch 'done
;;     (let ((marker (nsevil-get-marker char t)) alist)
;;       (unless (markerp marker)
;;         (cond
;;          ((and marker (symbolp marker) (boundp marker))
;;           (set marker (or (symbol-value marker) (make-marker)))
;;           (setq marker (symbol-value marker)))
;;          ((eq marker 'nsevil-jump-backward-swap)
;;           (nsevil-set-jump)
;;           (throw 'done nil))
;;          ((functionp marker)
;;           (user-error "Cannot set special marker `%c'" char))
;;          ((nsevil-global-marker-p char)
;;           (setq alist (default-value 'nsevil-markers-alist)
;;                 marker (make-marker))
;;           (nsevil--add-to-alist 'alist char marker)
;;           (setq-default nsevil-markers-alist alist))
;;          (t
;;           (setq marker (make-marker))
;;           (nsevil--add-to-alist 'nsevil-markers-alist char marker))))
;;       (add-hook 'kill-buffer-hook #'nsevil-swap-out-markers nil t)
;;       (set-marker-insertion-type marker advance)
;;       (set-marker marker (or pos (point))))))

;; (defun nsevil-get-marker (char &optional raw)
;;   "Return the marker denoted by CHAR.
;; This is either a marker object as returned by `make-marker',
;; a number, a cons cell (FILE . POS) with FILE being a string
;; and POS a number, or nil. If RAW is non-nil, then the
;; return value may also be a variable, a movement function,
;; or a marker object pointing nowhere."
;;   (let ((marker (if (nsevil-global-marker-p char)
;;                     (cdr-safe (assq char (default-value
;;                                            'nsevil-markers-alist)))
;;                   (cdr-safe (assq char nsevil-markers-alist)))))
;;     (save-excursion
;;       (if raw
;;           marker
;;         (when (and (symbolp marker) (boundp marker))
;;           (setq marker (symbol-value marker)))
;;         (when (functionp marker)
;;           (save-window-excursion
;;             (funcall marker)
;;             (setq marker (move-marker (make-marker) (point)))))
;;         (when (markerp marker)
;;           (if (eq (marker-buffer marker) (current-buffer))
;;               (setq marker (marker-position marker))
;;             (setq marker (and (marker-buffer marker) marker))))
;;         (when (or (numberp marker)
;;                   (markerp marker)
;;                   (and (consp marker)
;;                        (stringp (car marker))
;;                        (numberp (cdr marker))))
;;           marker)))))

;; (defun nsevil-swap-out-markers ()
;;   "Turn markers into file references when the buffer is killed."
;;   (and buffer-file-name
;;        (dolist (entry nsevil-markers-alist)
;;          (and (markerp (cdr entry))
;;               (eq (marker-buffer (cdr entry)) (current-buffer))
;;               (setcdr entry (cons buffer-file-name
;;                                   (marker-position (cdr entry))))))))
;; (put 'nsevil-swap-out-markers 'permanent-local-hook t)

(defun nsevil-get-register (register &optional _noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

The following special registers are supported.
  \"  the unnamed register
  *  the clipboard contents
  +  the clipboard contents
  <C-w> the word at point (ex mode only)
  <C-a> the WORD at point (ex mode only)
  <C-o> the symbol at point (ex mode only)
  <C-f> the current file at point (ex mode only)
  %  the current file name (read only)
  #  the alternate file name (read only)
  /  the last search pattern (read only)
  :  the last command line (read only)
  .  the last inserted text (read only)
  -  the last small (less than a line) delete
  _  the black hole register
  =  the expression register (read only)"
  (condition-case err
      (when (characterp register)
        (or (cond
             ((eq register ?\")
              (current-kill 0))
             ((and (<= ?1 register) (<= register ?9))
              (let ((reg (- register ?1)))
                (and (< reg (length kill-ring))
                     (current-kill reg t))))
             ((memq register '(?* ?+))
              ;; the following code is modified from
              ;; `x-selection-value-internal'
              (let ((what (if (eq register ?*) 'PRIMARY 'CLIPBOARD))
                    (request-type (or (and (boundp 'x-select-request-type)
                                           x-select-request-type)
                                      '(UTF8_STRING COMPOUND_TEXT STRING)))
                    text)
                (unless (consp request-type)
                  (setq request-type (list request-type)))
                (while (and request-type (not text))
                  (condition-case nil
                      (setq text (nsevil-get-selection what (pop request-type)))
                    (error nil)))
                (when text
                  (remove-text-properties 0 (length text) '(foreign-selection nil) text))
                text))
             ((eq register ?\C-W)
              (unless (nsevil-ex-p)
                (user-error "Register <C-w> only available in ex state"))
              (with-current-buffer nsevil-ex-current-buffer
                (thing-at-point 'nsevil-word)))
             ((eq register ?\C-A)
              (unless (nsevil-ex-p)
                (user-error "Register <C-a> only available in ex state"))
              (with-current-buffer nsevil-ex-current-buffer
                (thing-at-point 'nsevil-WORD)))
             ((eq register ?\C-O)
              (unless (nsevil-ex-p)
                (user-error "Register <C-o> only available in ex state"))
              (with-current-buffer nsevil-ex-current-buffer
                (thing-at-point 'nsevil-symbol)))
             ((eq register ?\C-F)
              (unless (nsevil-ex-p)
                (user-error "Register <C-f> only available in ex state"))
              (with-current-buffer nsevil-ex-current-buffer
                (thing-at-point 'filename)))
             ((eq register ?%)
              (or (buffer-file-name (and (nsevil-ex-p)
                                         (minibufferp)
                                         nsevil-ex-current-buffer))
                  (user-error "No file name")))
             ((= register ?#)
              (or (with-current-buffer (other-buffer) (buffer-file-name))
                  (user-error "No file name")))
             ((eq register ?/)
              (or (car-safe
                   (or (and (boundp 'nsevil-search-module)
                            (eq nsevil-search-module 'nsevil-search)
                            nsevil-ex-search-history)
                       (and isearch-regexp regexp-search-ring)
                       search-ring))
                  (user-error "No previous regular expression")))
             ((eq register ?:)
              (or (car-safe nsevil-ex-history)
                  (user-error "No previous command line")))
             ((eq register ?.)
              nsevil-last-insertion)
             ((eq register ?-)
              nsevil-last-small-deletion)
             ((eq register ?=)
              (let* ((enable-recursive-minibuffers t)
                     (result (eval (car (read-from-string (read-string "="))))))
                (cond
                 ((or (stringp result)
                      (numberp result)
                      (symbolp result))
                  (prin1-to-string result))
                 ((sequencep result)
                  (mapconcat #'prin1-to-string result "\n"))
                 (t (user-error "Using %s as a string" (type-of result))))))
             ((eq register ?_) ; the black hole register
              "")
             (t
              (setq register (downcase register))
              (get-register register)))
            (user-error "Register `%c' is empty" register)))
    (error (unless err (signal (car err) (cdr err))))))

(defun nsevil-append-register (register text)
  "Append TEXT to the contents of register REGISTER."
  (let ((content (get-register register)))
    (cond
     ((not content)
      (set-register register text))
     ((or (text-property-not-all 0 (length content)
                                 'yank-handler nil
                                 content)
          (text-property-not-all 0 (length text)
                                 'yank-handler nil
                                 text))
      ;; some non-trivial yank-handler -> always switch to line handler
      ;; ensure complete lines
      (when (and (> (length content) 0)
                 (/= (aref content (1- (length content))) ?\n))
        (setq content (concat content "\n")))
      (when (and (> (length text) 0)
                 (/= (aref text (1- (length text))) ?\n))
        (setq text (concat text "\n")))
      (setq text (concat content text))
      (remove-list-of-text-properties 0 (length text) '(yank-handler) text)
      (setq text (propertize text 'yank-handler '(nsevil-yank-line-handler)))
      (set-register register text))
     (t
      (set-register register (concat content text))))))

(defun nsevil-set-register (register text)
  "Set the contents of register REGISTER to TEXT.
If REGISTER is an upcase character then text is appended to that
register instead of replacing its content."
  (cond
   ((not (characterp register))
    (user-error "Invalid register"))
   ;; don't allow modification of read-only registers
   ((member register '(?: ?. ?%))
    (user-error "Can't modify read-only register"))
   ((eq register ?\")
    (kill-new text))
   ((and (<= ?1 register) (<= register ?9))
    (if (null kill-ring)
        (kill-new text)
      (let ((kill-ring-yank-pointer kill-ring-yank-pointer)
            interprogram-paste-function
            interprogram-cut-function)
        (current-kill (- register ?1))
        (setcar kill-ring-yank-pointer text))))
   ((eq register ?*)
    (nsevil-set-selection 'PRIMARY text))
   ((eq register ?+)
    (nsevil-set-selection 'CLIPBOARD text))
   ((eq register ?-)
    (setq nsevil-last-small-deletion text))
   ((eq register ?_) ; the black hole register
    nil)
   ((and (<= ?A register) (<= register ?Z))
    (nsevil-append-register (downcase register) text))
   (t
    (set-register register text))))

(defun nsevil-register-list ()
  "Returns an alist of all registers, but only those named
with number or character. Registers with symbol or string in names are ignored
to keep Vim compatibility with register jumps."
  (sort (append (mapcar #'(lambda (reg)
                            (cons reg (nsevil-get-register reg t)))
                        '(?\" ?* ?+ ?% ?# ?/ ?: ?. ?-
                              ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                (cl-remove-if-not (lambda (reg) (number-or-marker-p (car reg))) register-alist)
                nil)
        #'(lambda (reg1 reg2) (< (car reg1) (car reg2)))))

(defsubst nsevil-kbd-macro-suppress-motion-error ()
  "Returns non-nil if a motion error should be suppressed.
Whether the motion error should be suppressed depends on the
variable `nsevil-kbd-macro-suppress-motion-error'."
  (or (and defining-kbd-macro
           (memq nsevil-kbd-macro-suppress-motion-error '(t record)))
      (and executing-kbd-macro
           (memq nsevil-kbd-macro-suppress-motion-error '(t replay)))))

;;; Region

;; `set-mark' does too much at once
(defun nsevil-move-mark (pos)
  "Set buffer's mark to POS.
If POS is nil, delete the mark."
  (when pos
    (setq pos (nsevil-normalize-position pos)))
  (set-marker (mark-marker) pos))

(defun nsevil-save-transient-mark-mode ()
  "Save Transient Mark mode and make it buffer-local.
Any changes to Transient Mark mode are now local to the current
buffer, until `nsevil-restore-transient-mark-mode' is called.

Variables pertaining to Transient Mark mode are listed in
`nsevil-transient-vars', and their values are stored in
`nsevil-transient-vals'."
  (dolist (var nsevil-transient-vars)
    (when (and (boundp var)
               (not (assq var nsevil-transient-vals)))
      (push (list var (symbol-value var)
                  (local-variable-p var))
            nsevil-transient-vals)
      (make-variable-buffer-local var)
      (put var 'permanent-local t))))

(defun nsevil-restore-transient-mark-mode ()
  "Restore Transient Mark mode.
This presupposes that `nsevil-save-transient-mark-mode' has been
called earlier. If Transient Mark mode was disabled before but
enabled in the meantime, this function disables it; if it was
enabled before but disabled in the meantime, this function
enables it.

The earlier settings of Transient Mark mode are stored in
`nsevil-transient-vals'."
  (let (entry local var val)
    (while (setq entry (pop nsevil-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if val 1 -1))
          (setq var val))))))

(defun nsevil-save-mark ()
  "Save the current mark, including whether it is transient.
See also `nsevil-restore-mark'."
  (unless nsevil-visual-previous-mark
    (setq nsevil-visual-previous-mark (mark t))
    (nsevil-save-transient-mark-mode)))

(defun nsevil-restore-mark ()
  "Restore the mark, including whether it was transient.
See also `nsevil-save-mark'."
  (when nsevil-visual-previous-mark
    (nsevil-restore-transient-mark-mode)
    (nsevil-move-mark nsevil-visual-previous-mark)
    (setq nsevil-visual-previous-mark nil)))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun nsevil-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (nsevil-active-region -1)
    ;; Transient Mark mode cannot be disabled
    ;; while CUA mode is enabled
    (when (fboundp 'cua-mode)
      (cua-mode -1))
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (nsevil-active-region -1)
      (transient-mark-mode 1)))))

(defun nsevil-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (when (boundp 'cua--explicit-region-start)
        (setq cua--explicit-region-start nil))
      (run-hooks 'deactivate-mark-hook)))
   (t
    (nsevil-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (nsevil-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t)))
    (when (boundp 'cua--explicit-region-start)
      (setq cua--explicit-region-start t)))))

(defmacro nsevil-with-transient-mark-mode (&rest body)
  "Execute BODY with Transient Mark mode.
Then restore Transient Mark mode to its previous setting."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         nsevil-transient-vals)
     (unwind-protect
         (progn
           (nsevil-save-transient-mark-mode)
           (nsevil-transient-mark 1)
           ,@body)
       (nsevil-restore-transient-mark-mode))))

(defmacro nsevil-with-active-region (beg end &rest body)
  "Execute BODY with an active region from BEG to END."
  (declare (indent 2)
           (debug t))
  `(let ((beg ,beg) (end ,end)
         nsevil-transient-vals)
     (nsevil-with-transient-mark-mode
       (save-excursion
         (nsevil-active-region 1)
         (nsevil-move-mark beg)
         (goto-char end)
         ,@body))))

(defun nsevil-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun nsevil-apply-on-block (func beg end pass-columns &rest args)
  "Call FUNC for each line of a block selection.
The selection is specified by the region BEG and END.  FUNC must
take at least two arguments, the beginning and end of each
line. If PASS-COLUMNS is non-nil, these values are the columns,
otherwise tey are buffer positions. Extra arguments to FUNC may
be passed via ARGS."
  (let ((eol-col (and (memq last-command '(next-line previous-line))
                      (numberp temporary-goal-column)
                      temporary-goal-column))
        startcol startpt endcol endpt)
    (save-excursion
      (goto-char beg)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (nsevil-sort startcol endcol)
      ;; maybe find maximal column
      (when eol-col
        (setq eol-col 0)
        (goto-char startpt)
        (while (< (point) endpt)
          (setq eol-col (max eol-col
                             (nsevil-column (line-end-position))))
          (forward-line 1))
        (setq endcol (max endcol
                          (min eol-col
                               (1+ (min (1- most-positive-fixnum)
                                        (truncate temporary-goal-column)))))))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
        (if pass-columns
            (apply func startcol endcol args)
          (apply func
                 (save-excursion (nsevil-move-to-column startcol))
                 (save-excursion (nsevil-move-to-column endcol t))
                 args))
        (forward-line 1)))))

(defun nsevil-apply-on-rectangle (function start end &rest args)
  "Like `apply-on-rectangle' but maybe extends to eol.
If `temporary-goal-column' is set to a big number, then the
region of each line is extended to the end of each line. The end
column is set to the maximal column in all covered lines."
  (apply #'nsevil-apply-on-block function start end t args))

;;; Insertion

(defun nsevil-concat-ranges (ranges)
  "Concatenate RANGES.
RANGES must be a list of ranges.  They must be ordered so that
successive ranges share their boundaries.  The return value is a
single range of disjoint union of the ranges or nil if the
disjoint union is not a single range."
  (let ((range (car-safe ranges)) (ranges (cdr ranges)) r)
    (while (and range (setq r (car-safe ranges)))
      (setq range
            (cond ((and (= (cdr r) (car range))) (cons (car r) (cdr range)))
                  ((and (= (cdr range) (car r))) (cons (car range) (cdr r)))))
      (setq ranges (cdr ranges)))
    range))

(defun nsevil-track-last-insertion (beg end len)
  "Track the last insertion range and its text.
The insertion range is stored as a pair of buffer positions in
`nsevil-current-insertion'. If a subsequent change is compatible,
then the current range is modified, otherwise it is replaced by a
new range. Compatible changes are changes that do not create a
disjoin range."
  ;; deletion
  (when (> len 0)
    (if (and nsevil-current-insertion
             (>= beg (car nsevil-current-insertion))
             (<= (+ beg len) (cdr nsevil-current-insertion)))
        (setcdr nsevil-current-insertion
                (- (cdr nsevil-current-insertion) len))
      (setq nsevil-current-insertion nil)))
  ;; insertion
  (if (and nsevil-current-insertion
           (>= beg (car nsevil-current-insertion))
           (<= beg (cdr nsevil-current-insertion)))
      (setcdr nsevil-current-insertion
              (+ (- end beg)
                 (cdr nsevil-current-insertion)))
    (setq nsevil-current-insertion (cons beg end))))
(put 'nsevil-track-last-insertion 'permanent-local-hook t)

(defun nsevil-start-track-last-insertion ()
  "Start tracking the last insertion."
  (setq nsevil-current-insertion nil)
  (add-hook 'after-change-functions #'nsevil-track-last-insertion nil t))

(defun nsevil-stop-track-last-insertion ()
  "Stop tracking the last insertion.
The tracked insertion is set to `nsevil-last-insertion'."
  (setq nsevil-last-insertion
        (and nsevil-current-insertion
             ;; Check whether the insertion range is a valid buffer
             ;; range.  If a buffer modification is done from within
             ;; another change hook or modification-hook (yasnippet
             ;; does this using overlay modification-hooks), then the
             ;; insertion information may be invalid. There is no way
             ;; to detect this situation, but at least we should
             ;; ensure that no error occurs (see bug #272).
             (>= (car nsevil-current-insertion) (point-min))
             (<= (cdr nsevil-current-insertion) (point-max))
             (buffer-substring-no-properties (car nsevil-current-insertion)
                                             (cdr nsevil-current-insertion))))
  (remove-hook 'after-change-functions #'nsevil-track-last-insertion t))

;;; Paste

(defun nsevil-yank-characters (beg end &optional register yank-handler)
  "Saves the characters defined by the region BEG and END in the kill-ring."
  (let ((text (filter-buffer-substring beg end)))
    (when yank-handler
      (setq text (propertize text 'yank-handler (list yank-handler))))
    (when register
      (nsevil-set-register register text))
    (when nsevil-was-yanked-without-register
      (nsevil-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun nsevil-yank-lines (beg end &optional register yank-handler)
  "Saves the lines in the region BEG and END into the kill-ring."
  (let* ((text (filter-buffer-substring beg end))
         (yank-handler (list (or yank-handler
                                 #'nsevil-yank-line-handler)
                             nil
                             t)))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (setq text (propertize text 'yank-handler yank-handler))
    (when register
      (nsevil-set-register register text))
    (when nsevil-was-yanked-without-register
      (nsevil-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun nsevil-yank-rectangle (beg end &optional register yank-handler)
  "Saves the rectangle defined by region BEG and END into the kill-ring."
  (let ((lines (list nil)))
    (nsevil-apply-on-rectangle #'extract-rectangle-line beg end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `text' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let* ((yank-handler (list (or yank-handler
                                   #'nsevil-yank-block-handler)
                               lines
                               t
                               'nsevil-delete-yanked-rectangle))
           (text (propertize (mapconcat #'identity lines "\n")
                             'yank-handler yank-handler)))
      (when register
        (nsevil-set-register register text))
      (when nsevil-was-yanked-without-register
        (nsevil-set-register ?0 text)) ; "0 register contains last yanked text
      (unless (eq register ?_)
        (kill-new text)))))

(defun nsevil-remove-yank-excluded-properties (text)
  "Removes `yank-excluded-properties' from TEXT."
  (if (eq yank-excluded-properties t)
      (set-text-properties 0 (length text) nil text)
    (remove-list-of-text-properties 0 (length text)
                                    yank-excluded-properties text)))

;;; Interactive forms

(defun nsevil-match-interactive-code (interactive &optional pos)
  "Match an interactive code at position POS in string INTERACTIVE.
Returns the first matching entry in `nsevil-interactive-alist', or nil."
  (let ((length (length interactive))
        (pos (or pos 0)))
    (catch 'done
      (dolist (entry nsevil-interactive-alist)
        (let* ((string (car entry))
               (end (+ (length string) pos)))
          (when (and (<= end length)
                     (string= string
                              (substring interactive pos end)))
            (throw 'done entry)))))))

(defun nsevil-concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions FORMS.
Returns a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t
          (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result))
        (car result))
       (t
        `(append ,@result))))))

(defun nsevil-interactive-string (string)
  "Evaluate the interactive string STRING.
The string may contain extended interactive syntax.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `nsevil-define-command'."
  (let ((length (length string))
        (pos 0)
        code expr forms match plist prompt properties)
    (while (< pos length)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (setq match (nsevil-match-interactive-code string pos))
        (if (null match)
            (user-error "Unknown interactive code: `%s'"
                        (substring string pos))
          (setq code (car match)
                expr (car (cdr match))
                plist (cdr (cdr match))
                pos (+ pos (length code)))
          (when (functionp expr)
            (setq prompt
                  (substring string pos
                             (or (string-match "\n" string pos)
                                 length))
                  pos (+ pos (length prompt))
                  expr `(funcall ,expr ,prompt)))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun nsevil-interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `nsevil-define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (nsevil-interactive-string arg)
              forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply #'nsevil-concatenate-interactive-forms forms)
          properties)))

;;; Types

(defun nsevil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((nsevil-range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ((commandp object)
      (setq type (nsevil-get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (nsevil-type-p type) type)))

(defun nsevil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (nsevil-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((nsevil-range-p object)
    (nsevil-set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (nsevil-set-command-property object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun nsevil-type-property (type prop)
  "Return property PROP for TYPE."
  (nsevil-get-property nsevil-type-properties type prop))

(defun nsevil-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym nsevil-type-properties))

(defun nsevil-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'nsevil-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun nsevil-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'nsevil-transform :contract beg end type properties))

(defun nsevil-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'nsevil-transform :normalize beg end type properties))

(defun nsevil-transform (transform beg end type &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged."
  (let* ((type (or type (nsevil-type properties)))
         (transform (when (and type transform)
                      (nsevil-type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply #'nsevil-range beg end type properties))))

(defun nsevil-describe (beg end type &rest properties)
  "Return description of BEG and END with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (nsevil-type properties)))
         (properties (plist-put properties :type type))
         (describe (nsevil-type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

;;; Ranges

(defun nsevil-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `nsevil-type-p', and PROPERTIES is
a property list."
  (let ((beg (nsevil-normalize-position beg))
        (end (nsevil-normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (append (list (min beg end) (max beg end))
              (when (nsevil-type-p type)
                (list type))
              properties))))

(defun nsevil-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (>= (length object) 2)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun nsevil-range-beginning (range)
  "Return beginning of RANGE."
  (when (nsevil-range-p range)
    (let ((beg (nsevil-normalize-position (nth 0 range)))
          (end (nsevil-normalize-position (nth 1 range))))
      (min beg end))))

(defun nsevil-range-end (range)
  "Return end of RANGE."
  (when (nsevil-range-p range)
    (let ((beg (nsevil-normalize-position (nth 0 range)))
          (end (nsevil-normalize-position (nth 1 range))))
      (max beg end))))

(defun nsevil-range-properties (range)
  "Return properties of RANGE."
  (when (nsevil-range-p range)
    (if (nsevil-type range)
        (nthcdr 3 range)
      (nthcdr 2 range))))

(defun nsevil-copy-range (range)
  "Return a copy of RANGE."
  (copy-sequence range))

(defun nsevil-set-range (range &optional beg end type &rest properties)
  "Set RANGE to have beginning BEG and end END.
The TYPE and additional PROPERTIES may also be specified.
If an argument is nil, it's not used; the previous value is retained.
See also `nsevil-set-range-beginning', `nsevil-set-range-end',
`nsevil-set-range-type' and `nsevil-set-range-properties'."
  (when (nsevil-range-p range)
    (let ((beg (or (nsevil-normalize-position beg)
                   (nsevil-range-beginning range)))
          (end (or (nsevil-normalize-position end)
                   (nsevil-range-end range)))
          (type (or type (nsevil-type range)))
          (plist (nsevil-range-properties range)))
      (nsevil-sort beg end)
      (setq plist (nsevil-concat-plists plist properties))
      (nsevil-set-range-beginning range beg)
      (nsevil-set-range-end range end)
      (nsevil-set-range-type range type)
      (nsevil-set-range-properties range plist)
      range)))

(defun nsevil-set-range-beginning (range beg &optional copy)
  "Set RANGE's beginning to BEG.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (nsevil-copy-range range)))
  (setcar range beg)
  range)

(defun nsevil-set-range-end (range end &optional copy)
  "Set RANGE's end to END.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (nsevil-copy-range range)))
  (setcar (cdr range) end)
  range)

(defun nsevil-set-range-type (range type &optional copy)
  "Set RANGE's type to TYPE.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (nsevil-copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (nsevil-range-properties range)))
    (setcdr (cdr range) (nsevil-range-properties range)))
  range)

(defun nsevil-set-range-properties (range properties &optional copy)
  "Set RANGE's properties to PROPERTIES.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (nsevil-copy-range range)))
  (if (nsevil-type range)
      (setcdr (cdr (cdr range)) properties)
    (setcdr (cdr range) properties))
  range)

(defun nsevil-range-union (range1 range2 &optional type)
  "Return the union of the ranges RANGE1 and RANGE2.
If the ranges have conflicting types, use RANGE1's type.
This can be overridden with TYPE."
  (when (and (nsevil-range-p range1)
             (nsevil-range-p range2))
    (nsevil-range (min (nsevil-range-beginning range1)
                     (nsevil-range-beginning range2))
                (max (nsevil-range-end range1)
                     (nsevil-range-end range2))
                (or type
                    (nsevil-type range1)
                    (nsevil-type range2)))))

(defun nsevil-subrange-p (range1 range2)
  "Whether RANGE1 is contained within RANGE2."
  (and (nsevil-range-p range1)
       (nsevil-range-p range2)
       (<= (nsevil-range-beginning range2)
           (nsevil-range-beginning range1))
       (>= (nsevil-range-end range2)
           (nsevil-range-end range1))))

(defun nsevil-select-inner-object (thing beg end type &optional count line)
  "Return an inner text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((count (or count 1))
         (bnd (or (let ((b (bounds-of-thing-at-point thing)))
                    (and b (< (point) (cdr b)) b))
                  (nsevil-bounds-of-not-thing-at-point thing))))
    ;; check if current object is selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (setq count (if (> count 0) (1- count) (1+ count))))
    (goto-char (if (< count 0) beg end))
    (nsevil-forward-nearest count
                          #'(lambda (cnt) (forward-thing thing cnt))
                          #'(lambda (cnt) (nsevil-forward-not-thing thing cnt)))
    (nsevil-range (if (>= count 0) beg (point))
                (if (< count 0) end (point))
                (if line 'line type)
                :expanded t)))

(defun nsevil-select-an-object (thing beg end type count &optional line)
  "Return an outer text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (objbnd (let ((b (bounds-of-thing-at-point thing)))
                   (and b (< (point) (cdr b)) b)))
         (bnd (or objbnd (nsevil-bounds-of-not-thing-at-point thing)))
         addcurrent other)
    ;; check if current object is not selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      ;; if not, enlarge selection
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (if objbnd (setq addcurrent t)))
    ;; make other and (point) reflect the selection
    (cond
     ((> dir 0) (goto-char end) (setq other beg))
     (t (goto-char beg) (setq other end)))
    (cond
     ;; do nothing more than only current is selected
     ((not (and (= beg (car bnd)) (= end (cdr bnd)))))
     ;; current match is thing, add whitespace
     (objbnd
      (let ((wsend (nsevil-with-restriction
                       ;; restrict to current line if we do non-line selection
                       (and (not line) (line-beginning-position))
                       (and (not line) (line-end-position))
                     (nsevil-bounds-of-not-thing-at-point thing dir))))
        (cond
         (wsend
          ;; add whitespace at end
          (goto-char wsend)
          (setq addcurrent t))
         (t
          ;; no whitespace at end, try beginning
          (save-excursion
            (goto-char other)
            (setq wsend
                  (nsevil-with-restriction
                      ;; restrict to current line if we do non-line selection
                      (and (not line) (line-beginning-position))
                      (and (not line) (line-end-position))
                    (nsevil-bounds-of-not-thing-at-point thing (- dir))))
            (when wsend (setq other wsend addcurrent t)))))))
     ;; current match is whitespace, add thing
     (t
      (forward-thing thing dir)
      (setq addcurrent t)))
    ;; possibly count current object as selection
    (if addcurrent (setq count (1- count)))
    ;; move
    (dotimes (_ count)
      (let ((wsend (nsevil-bounds-of-not-thing-at-point thing dir)))
        (if (and wsend (/= wsend (point)))
            ;; start with whitespace
            (forward-thing thing dir)
          ;; start with thing
          (forward-thing thing dir)
          (setq wsend (nsevil-bounds-of-not-thing-at-point thing dir))
          (when wsend (goto-char wsend)))))
    ;; return range
    (nsevil-range (if (> dir 0) other (point))
                (if (< dir 0) other (point))
                (if line 'line type)
                :expanded t)))

(defun nsevil--get-block-range (op cl selection-type)
  "Return the exclusive range of a visual selection.
OP and CL are pairs of buffer positions for the opening and
closing delimiter of a range. SELECTION-TYPE is the desired type
of selection.  It is a symbol that determines which parts of the
block are selected.  If it is 'inclusive or t the returned range
is \(cons (car OP) (cdr CL)). If it is 'exclusive or nil the
returned range is (cons (cdr OP) (car CL)).  If it is
'exclusive-line the returned range will skip whitespace at the
end of the line of OP and at the beginning of the line of CL."
  (cond
   ((memq selection-type '(inclusive t)) (cons (car op) (cdr cl)))
   ((memq selection-type '(exclusive nil)) (cons (cdr op) (car cl)))
   ((eq selection-type 'exclusive-line)
    (let ((beg (cdr op))
          (end (car cl)))
      (save-excursion
        (goto-char beg)
        (when (and (eolp) (not (eobp)))
          (setq beg (line-beginning-position 2)))
        (goto-char end)
        (skip-chars-backward " \t")
        (when (bolp)
          (setq end (point))
          (goto-char beg)
          (when (and (not (bolp)) (< beg end))
            (setq end (1- end)))))
      (cons beg end)))
   (t
    (user-error "Unknown selection-type %s" selection-type))))

(defun nsevil-select-block (thing beg end type count
                                &optional
                                selection-type
                                countcurrent
                                fixedscan)
  "Return a range (BEG END) of COUNT delimited text objects.
BEG END TYPE are the currently selected (visual) range.  The
delimited object must be given by THING-up function (see
`nsevil-up-block').

SELECTION-TYPE is symbol that determines which parts of the block
are selected.  If it is 'inclusive or t OPEN and CLOSE are
included in the range. If it is 'exclusive or nil the delimiters
are not contained. If it is 'exclusive-line the delimiters are
not included as well as adjacent whitespace until the beginning
of the next line or the end of the previous line. If the
resulting selection consists of complete lines only and visual
state is not active, the returned selection is linewise.

If COUNTCURRENT is non-nil an objected is counted if the current
selection matches that object exactly.

Usually scanning for the surrounding block starts at (1+ beg)
and (1- end). If this might fail due to the behavior of THING
then FIXEDSCAN can be set to t. In this case the scan starts at
BEG and END. One example where this might fail is if BEG and END
are the delimiters of a string or comment."
  (save-excursion
    (save-match-data
      (let* ((orig-beg beg)
             (orig-end end)
             (beg (or beg (point)))
             (end (or end (point)))
             (count (abs (or count 1)))
             op cl op-end cl-end)
        ;; We always assume at least one selected character.
        (if (= beg end) (setq end (1+ end)))
        ;; We scan twice: starting at (1+ beg) forward and at (1- end)
        ;; backward. The resulting selection is the smaller one.
        (goto-char (if fixedscan beg (1+ beg)))
        (when (and (zerop (funcall thing +1)) (match-beginning 0))
          (setq cl (cons (match-beginning 0) (match-end 0)))
          (goto-char (car cl))
          (when (and (zerop (funcall thing -1)) (match-beginning 0))
            (setq op (cons (match-beginning 0) (match-end 0)))))
        ;; start scanning from end
        (goto-char (if fixedscan end (1- end)))
        (when (and (zerop (funcall thing -1)) (match-beginning 0))
          (setq op-end (cons (match-beginning 0) (match-end 0)))
          (goto-char (cdr op-end))
          (when (and (zerop (funcall thing +1)) (match-beginning 0))
            (setq cl-end (cons (match-beginning 0) (match-end 0)))))
        ;; Bug #607: use the tightest selection that contains the
        ;; original selection. If non selection contains the original,
        ;; use the larger one.
        (cond
         ((and (not op) (not cl-end))
          (error "No surrounding delimiters found"))
         ((or (not op) ; first not found
              (and cl-end ; second found
                   (>= (car op-end) (car op)) ; second smaller
                   (<= (cdr cl-end) (cdr cl))
                   (<= (car op-end) beg)      ; second contains orig
                   (>= (cdr cl-end) end)))
          (setq op op-end cl cl-end)))
        (setq op-end op cl-end cl) ; store copy
        ;; if the current selection contains the surrounding
        ;; delimiters, they do not count as new selection
        (let ((cnt (if (and orig-beg orig-end (not countcurrent))
                       (let ((sel (nsevil--get-block-range op cl selection-type)))
                         (if (and (<= orig-beg (car sel))
                                  (>= orig-end (cdr sel)))
                             count
                           (1- count)))
                     (1- count))))
          ;; starting from the innermost surrounding delimiters
          ;; increase selection
          (when (> cnt 0)
            (setq op (progn
                       (goto-char (car op-end))
                       (funcall thing (- cnt))
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         op))
                  cl (progn
                       (goto-char (cdr cl-end))
                       (funcall thing cnt)
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         cl)))))
        (let ((sel (nsevil--get-block-range op cl selection-type)))
          (setq op (car sel)
                cl (cdr sel)))
        (cond
         ((and (equal op orig-beg) (equal cl orig-end)
               (or (not countcurrent)
                   (and countcurrent (/= count 1))))
          (error "No surrounding delimiters found"))
         ((save-excursion
            (and (not (nsevil-visual-state-p))
                 (eq type 'inclusive)
                 (progn (goto-char op) (bolp))
                 (progn (goto-char cl) (bolp))))
          (nsevil-range op cl 'line :expanded t))
         (t
          (nsevil-range op cl type :expanded t)))))))

(defun nsevil-select-paren (open close beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN and CLOSE specify the opening and closing delimiter,
respectively. BEG END TYPE are the currently selected (visual)
range.  If INCLUSIVE is non-nil, OPEN and CLOSE are included in
the range; otherwise they are excluded.

The types of OPEN and CLOSE specify which kind of THING is used
for parsing with `nsevil-select-block'. If OPEN and CLOSE are
characters `nsevil-up-paren' is used. Otherwise OPEN and CLOSE
must be regular expressions and `nsevil-up-block' is used.

If the selection is exclusive, whitespace at the end or at the
beginning of the selection until the end-of-line or beginning-of-line
is ignored."
  ;; we need special linewise exclusive selection
  (unless inclusive (setq inclusive 'exclusive-line))
  (cond
   ((and (characterp open) (characterp close))
    (let ((thing #'(lambda (&optional cnt)
                     (nsevil-up-paren open close cnt)))
          (bnd (or (bounds-of-thing-at-point 'nsevil-string)
                   (bounds-of-thing-at-point 'nsevil-comment)
                   ;; If point is at the opening quote of a string,
                   ;; this must be handled as if point is within the
                   ;; string, i.e. the selection must be extended
                   ;; around the string. Otherwise
                   ;; `nsevil-select-block' might do the wrong thing
                   ;; because it accidentally moves point inside the
                   ;; string (for inclusive selection) when looking
                   ;; for the current surrounding block. (re #364)
                   (and (= (point) (or beg (point)))
                        (save-excursion
                          (goto-char (1+ (or beg (point))))
                          (or (bounds-of-thing-at-point 'nsevil-string)
                              (bounds-of-thing-at-point 'nsevil-comment)))))))
      (if (not bnd)
          (nsevil-select-block thing beg end type count inclusive)
        (or (nsevil-with-restriction (car bnd) (cdr bnd)
              (condition-case nil
                  (nsevil-select-block thing beg end type count inclusive)
                (error nil)))
            (save-excursion
              (setq beg (or beg (point))
                    end (or end (point)))
              (goto-char (car bnd))
              (let ((extbeg (min beg (car bnd)))
                    (extend (max end (cdr bnd))))
                (nsevil-select-block thing
                                   extbeg extend
                                   type
                                   count
                                   inclusive
                                   (or (< extbeg beg) (> extend end))
                                   t)))))))
   (t
    (nsevil-select-block #'(lambda (&optional cnt)
                           (nsevil-up-block open close cnt))
                       beg end type count inclusive))))

(defun nsevil-select-quote-thing (thing beg end _type count &optional inclusive)
  "Selection THING as if it described a quoted object.
THING is typically either 'nsevil-quote or 'nsevil-chars. This
function is called from `nsevil-select-quote'."
  (save-excursion
    (let* ((count (or count 1))
           (dir (if (> count 0) 1 -1))
           (bnd (let ((b (bounds-of-thing-at-point thing)))
                  (and b (< (point) (cdr b)) b)))
           addcurrent
           wsboth)
      (if inclusive (setq inclusive t)
        (when (= (abs count) 2)
          (setq count dir)
          (setq inclusive 'quote-only))
        ;; never extend with exclusive selection
        (setq beg nil end nil))
      ;; check if the previously selected range does not contain a
      ;; string
      (unless (and beg end
                   (save-excursion
                     (goto-char (if (> dir 0) beg end))
                     (forward-thing thing dir)
                     (and (<= beg (point)) (< (point) end))))
        ;; if so forget the range
        (setq beg nil end nil))
      ;; check if there is a current object, if not fetch one
      (when (not bnd)
        (unless (and (zerop (forward-thing thing dir))
                     (setq bnd (bounds-of-thing-at-point thing)))
          (error "No quoted string found"))
        (if (> dir 0)
            (setq end (point))
          (setq beg (point)))
        (setq addcurrent t))
      ;; check if current object is not selected
      (when (or (not beg) (not end) (> beg (car bnd)) (< end (cdr bnd)))
        ;; if not, enlarge selection
        (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
        (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
        (setq addcurrent t wsboth t))
      ;; maybe count current element
      (when addcurrent
        (setq count (if (> dir 0) (1- count) (1+ count))))
      ;; enlarge selection
      (goto-char (if (> dir 0) end beg))
      (when (and (not addcurrent)
                 (= count (forward-thing thing count)))
        (error "No quoted string found"))
      (if (> dir 0) (setq end (point)) (setq beg (point)))
      ;; add whitespace
      (cond
       ((not inclusive) (setq beg (1+ beg) end (1- end)))
       ((not (eq inclusive 'quote-only))
        ;; try to add whitespace in forward direction
        (goto-char (if (> dir 0) end beg))
        (if (setq bnd (bounds-of-thing-at-point 'nsevil-space))
            (if (> dir 0) (setq end (cdr bnd)) (setq beg (car bnd)))
          ;; if not found try backward direction
          (goto-char (if (> dir 0) beg end))
          (if (and wsboth (setq bnd (bounds-of-thing-at-point 'nsevil-space)))
              (if (> dir 0) (setq beg (car bnd)) (setq end (cdr bnd)))))))
      (nsevil-range beg end
                  ;; HACK: fixes #583
                  ;; When not in visual state, an empty range is
                  ;; possible. However, this cannot be achieved with
                  ;; inclusive ranges, hence we use exclusive ranges
                  ;; in this case. In visual state the range must be
                  ;; inclusive because otherwise the selection would
                  ;; be wrong.
                  (if (nsevil-visual-state-p) 'inclusive 'exclusive)
                  :expanded t))))

(defun nsevil-select-quote (quote beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT quoted text objects.
QUOTE specifies the quotation delimiter. BEG END TYPE are the
currently selected (visual) range.

If INCLUSIVE is nil the previous selection is ignore. If there is
quoted string at point this object will be selected, otherwise
the following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)) is selected. If (/= (abs COUNT) 2) the delimiting quotes are not
contained in the range, otherwise they are contained in the range.

If INCLUSIVE is non-nil the selection depends on the previous
selection. If the currently selection contains at least one
character that is contained in a quoted string then the selection
is extended, otherwise it is thrown away. If there is a
non-selected object at point then this object is added to the
selection. Otherwise the selection is extended to the
following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)). Any whitespace following (or preceeding if (< COUNT 0)) the
new selection is added to the selection. If no such whitespace
exists and the selection contains only one quoted string then the
preceeding (or following) whitespace is added to the range. "
  (let ((nsevil-forward-quote-char quote))
    (or (let ((bnd (or (bounds-of-thing-at-point 'nsevil-comment)
                       (bounds-of-thing-at-point 'nsevil-string))))
          (when (and bnd (< (point) (cdr bnd))
                     (/= (char-after (car bnd)) quote)
                     (/= (char-before (cdr bnd)) quote))
            (nsevil-with-restriction (car bnd) (cdr bnd)
              (condition-case nil
                  (nsevil-select-quote-thing 'nsevil-quote-simple
                                           beg end type
                                           count
                                           inclusive)
                (error nil)))))
        (let ((nsevil-forward-quote-char quote))
          (nsevil-select-quote-thing 'nsevil-quote
                                   beg end type
                                   count
                                   inclusive)))))

(defun nsevil-select-xml-tag (beg end type &optional count inclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If INCLUSIVE is non-nil, the tags themselves are included
from the range."
  (cond
   ((and (not inclusive) (= (abs (or count 1)) 1))
    (let ((rng (nsevil-select-block #'nsevil-up-xml-tag beg end type count nil t)))
      (if (or (and beg (= beg (nsevil-range-beginning rng))
                   end (= end (nsevil-range-end rng)))
              (= (nsevil-range-beginning rng) (nsevil-range-end rng)))
          (nsevil-select-block #'nsevil-up-xml-tag beg end type count t)
        rng)))
   (t
    (nsevil-select-block #'nsevil-up-xml-tag beg end type count inclusive))))

(defun nsevil-expand-range (range &optional copy)
  "Expand RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (nsevil-copy-range range)))
  (unless (plist-get (nsevil-range-properties range) :expanded)
    (setq range (nsevil-transform-range :expand range)))
  range)

(defun nsevil-contract-range (range &optional copy)
  "Contract RANGE according to its type.
Return a new range if COPY is non-nil."
  (nsevil-transform-range :contract range copy))

(defun nsevil-normalize-range (range &optional copy)
  "Normalize RANGE according to its type.
Return a new range if COPY is non-nil."
  (nsevil-transform-range :normalize range copy))

(defun nsevil-transform-range (transform range &optional copy)
  "Apply TRANSFORM to RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (nsevil-copy-range range)))
  (when (nsevil-type range)
    (apply #'nsevil-set-range range
           (apply #'nsevil-transform transform range)))
  range)

(defun nsevil-describe-range (range)
  "Return description of RANGE.
If no description is available, return the empty string."
  (apply #'nsevil-describe range))

;;; Undo

(defun nsevil-start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If CONTINUE is non-nil, preceding modifications
are included. The step is terminated with `nsevil-end-undo-step'."
  (when (and (listp buffer-undo-list)
             (not nsevil-in-single-undo))
    (if nsevil-undo-list-pointer
        (nsevil-refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq nsevil-undo-list-pointer (or buffer-undo-list t)))))

(defun nsevil-end-undo-step (&optional continue)
  "End a undo step started with `nsevil-start-undo-step'.
Adds an undo boundary unless CONTINUE is specified."
  (when (and (listp buffer-undo-list)
             nsevil-undo-list-pointer
             (not nsevil-in-single-undo))
    (nsevil-refresh-undo-step)
    (unless (or continue (null (car-safe buffer-undo-list)))
      (undo-boundary))
    (setq nsevil-undo-list-pointer nil)))

(defun nsevil-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `nsevil-undo-list-pointer' are removed to
make the entries undoable as a single action. See
`nsevil-start-undo-step'."
  (when nsevil-undo-list-pointer
    (setq buffer-undo-list
          (nsevil-filter-list #'null buffer-undo-list nsevil-undo-list-pointer))
    (setq nsevil-undo-list-pointer (or buffer-undo-list t))))

(defmacro nsevil-with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `nsevil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         (unwind-protect
             (progn ,@body)
           (setq nsevil-temporary-undo buffer-undo-list)
           ;; ensure nsevil-temporary-undo starts with exactly one undo
           ;; boundary marker, i.e. nil
           (unless (null (car-safe nsevil-temporary-undo))
             (push nil nsevil-temporary-undo))))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list
             ;; prepend new undos (if there are any)
             (if (cdr nsevil-temporary-undo)
                 (nconc nsevil-temporary-undo buffer-undo-list)
               buffer-undo-list)
             nsevil-temporary-undo nil))))

(defmacro nsevil-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent defun)
           (debug t))
  `(let (nsevil-undo-list-pointer)
     (nsevil-with-undo
       (unwind-protect
           (progn
             (nsevil-start-undo-step)
             (let ((nsevil-in-single-undo t))
               ,@body))
         (nsevil-end-undo-step)))))

(defun nsevil-undo-pop ()
  "Undo the last buffer change.
Removes the last undo information from `buffer-undo-list'.
If undo is disabled in the current buffer, use the information
in `nsevil-temporary-undo' instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         nsevil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (user-error "Can't undo previous change"))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (nsevil-save-echo-area
          (undo)))
      (if (eq buffer-undo-list t)
          (setq nsevil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

;;; Search
(defun nsevil-transform-regexp (regexp replacements-alist)
  (replace-regexp-in-string
   "\\\\+[^\\\\]"
   #'(lambda (txt)
       (let* ((b (match-beginning 0))
              (e (match-end 0))
              (ch (aref txt (1- e)))
              (repl (assoc ch replacements-alist)))
         (if (and repl (zerop (mod (length txt) 2)))
             (concat (substring txt b (- e 2))
                     (cdr repl))
           txt)))
   regexp nil t))

(defun nsevil-transform-magic (str magic quote transform &optional _start)
  "Transforms STR with magic characters.
MAGIC is a regexp that matches all potential magic
characters. Each occurence of CHAR as magic character within str
is replaced by the result of calling the associated TRANSFORM
function. TRANSFORM is a function taking two arguments, the
character to be transformed and the rest of string after the
character. The function should return a triple (REPLACEMENT REST
. STOP) where REPLACEMENT is the replacement and REST is the rest
of the string that has not been transformed. If STOP is non-nil
then the substitution stops immediately.  The replacement starts
at position START, everything before that position is returned
literally.  The result is a pair (RESULT . REST). RESULT is a
list containing the transformed parts in order. If two
subsequents parts are both strings, they are concatenated. REST
is the untransformed rest string (usually \"\" but may be more if
TRANSFORM stopped the substitution). Which characters are
considered as magic characters (i.e. the transformation happens
if the character is NOT preceeded by a backslash) is determined
by `nsevil-magic'. The special tokens \\v, \\V, \\m and \\M have
always a special meaning (like in Vim) and should not be
contained in TRANSFORMS, otherwise their meaning is overwritten.

The parameter QUOTE is a quoting function applied to literal
transformations, usually `regexp-quote' or `replace-quote'."
  (save-match-data
    (let ((regexp (concat "\\(?:\\`\\|[^\\]\\)\\(\\\\\\(?:\\(" magic "\\)\\|\\(.\\)\\)\\|\\(" magic "\\)\\)"))
          (magic-chars (nsevil-get-magic nsevil-magic))
          (nsevil-magic nsevil-magic)
          (quote (or quote #'identity))
          result stop)
      (while (and (not stop) str (string-match regexp str))
        (unless (zerop (match-beginning 1))
          (push (substring str 0 (match-beginning 1)) result))
        (let ((char (or (match-string 2 str)
                        (match-string 3 str)
                        (match-string 4 str)))
              (rest (substring str (match-end 0))))
          (cond
           ((match-beginning 4)
            ;; magic character without backslash
            (if (string-match magic-chars char)
                ;; magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; non-magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((match-beginning 2)
            ;; magic character with backslash
            (if (not (string-match magic-chars char))
                ;; non-magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((memq (aref char 0) '(?m ?M ?v ?V))
            (setq nsevil-magic (cdr (assq (aref char 0)
                                        '((?m . t)
                                          (?M . nil)
                                          (?v . very-magic)
                                          (?V . very-nomagic)))))
            (setq magic-chars (nsevil-get-magic nsevil-magic))
            (setq str rest))
           (t
            ;; non-magic char with backslash, literal transformation
            (push (funcall quote char) result)
            (setq str rest)))))
      (cond
       ((and str (not stop))
        (push str result)
        (setq str ""))
       ((not str)
        (setq str "")))
      ;; concatenate subsequent strings
      ;; note that result is in reverse order
      (let (repl)
        (while result
          (cond
           ((and (stringp (car result))
                 (zerop (length (car result))))
            (pop result))
           ((and (stringp (car result))
                 (stringp (cadr result)))
            (setq result (cons (concat (cadr result)
                                       (car result))
                               (nthcdr 2 result))))
           (t
            (push (pop result) repl))))
        (cons repl str)))))

(defconst nsevil-vim-regexp-replacements
  '((?n  . "\n")           (?r  . "\r")
    (?t  . "\t")           (?b  . "\b")
    (?s  . "[[:space:]]")  (?S  . "[^[:space:]]")
    (?d  . "[[:digit:]]")  (?D  . "[^[:digit:]]")
    (?x  . "[[:xdigit:]]") (?X  . "[^[:xdigit:]]")
    (?o  . "[0-7]")        (?O  . "[^0-7]")
    (?a  . "[[:alpha:]]")  (?A  . "[^[:alpha:]]")
    (?l  . "[a-z]")        (?L  . "[^a-z]")
    (?u  . "[A-Z]")        (?U  . "[^A-Z]")
    (?y  . "\\s")          (?Y  . "\\S")
    (?\( . "\\(")          (?\) . "\\)")
    (?{  . "\\{")          (?}  . "\\}")
    (?\[ . "[")            (?\] . "]")
    (?<  . "\\<")          (?>  . "\\>")
    (?_  . "\\_")
    (?*  . "*")            (?+  . "+")
    (??  . "?")            (?=  . "?")
    (?.  . ".")
    (?`  . "`")            (?^  . "^")
    (?$  . "$")            (?|  . "\\|")))

(defconst nsevil-regexp-magic "[][(){}<>_dDsSxXoOaAlLuUwWyY.*+?=^$`|nrtb]")

(defun nsevil-transform-vim-style-regexp (regexp)
  "Transforms vim-style backslash codes to Emacs regexp.
This includes the backslash codes \\d, \\D, \\s, \\S, \\x, \\X,
\\o, \\O, \\a, \\A, \\l, \\L, \\u, \\U and \\w, \\W. The new
codes \\y and \\Y can be used instead of the Emacs code \\s and
\\S which have a different meaning in Vim-style."
  (car
   (car
    (nsevil-transform-magic
     regexp nsevil-regexp-magic #'regexp-quote
     #'(lambda (char rest)
         (let ((repl (assoc char nsevil-vim-regexp-replacements)))
           (if repl
               (list (cdr repl) rest)
             (list (concat "\\" (char-to-string char)) rest))))))))

;;; Substitute

(defun nsevil-downcase-first (str)
  "Return STR with the first letter downcased."
  (if (zerop (length str))
      str
    (concat (downcase (substring str 0 1))
            (substring str 1))))

(defun nsevil-upcase-first (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

(defun nsevil-get-magic (magic)
  "Returns a regexp matching the magic characters according to MAGIC.
Depending on the value of MAGIC the following characters are
considered magic.
  t             [][{}*+?.&~$^
  nil           [][{}*+?$^
  'very-magic   not 0-9A-Za-z_
  'very-nomagic empty."
  (cond
   ((eq magic t) "[][}{*+?.&~$^]")
   ((eq magic 'very-magic) "[^0-9A-Za-z_]")
   ((eq magic 'very-nomagic) "\\\\")
   (t "[][}{*+?$^]")))

;; TODO: support magic characters in patterns
(defconst nsevil-replacement-magic "[eElLuU0-9&#,rnbt=]"
  "All magic characters in a replacement string")

(defun nsevil-compile-subreplacement (to &optional start)
  "Convert a regexp replacement TO to Lisp from START until \\e or \\E.
Returns a pair (RESULT . REST). RESULT is a list suitable for
`perform-replace' if necessary, the original string if not.
REST is the unparsed remainder of TO."
  (let ((result
         (nsevil-transform-magic
          to nsevil-replacement-magic #'replace-quote
          #'(lambda (char rest)
              (cond
               ((eq char ?#)
                (list '(number-to-string replace-count) rest))
               ((eq char ?r) (list "\r" rest))
               ((eq char ?n) (list "\n" rest))
               ((eq char ?b) (list "\b" rest))
               ((eq char ?t) (list "\t" rest))
               ((memq char '(?e ?E))
                `("" ,rest . t))
               ((memq char '(?l ?L ?u ?U))
                (let ((result (nsevil-compile-subreplacement rest))
                      (func (cdr (assoc char
                                        '((?l . nsevil-downcase-first)
                                          (?L . downcase)
                                          (?u . nsevil-upcase-first)
                                          (?U . upcase))))))
                  (list `(,func
                          (replace-quote
                           (nsevil-match-substitute-replacement
                            ,(car result)
                            (not case-replace))))
                        (cdr result))))
               ((eq char ?=)
                (when (or (zerop (length rest))
                          (not (eq (aref rest 0) ?@)))
                  (user-error "Expected @ after \\="))
                (when (< (length rest) 2)
                  (user-error "Expected register after \\=@"))
                (list (nsevil-get-register (aref rest 1))
                      (substring rest 2)))
               ((eq char ?,)
                (let* ((obj (read-from-string rest))
                       (result `(replace-quote ,(car obj)))
                       (end
                        ;; swallow a space after a symbol
                        (if (and (or (symbolp (car obj))
                                     ;; swallow a space after 'foo,
                                     ;; but not after (quote foo)
                                     (and (eq (car-safe (car obj)) 'quote)
                                          (not (= ?\( (aref rest 0)))))
                                 (eq (string-match " " rest (cdr obj))
                                     (cdr obj)))
                            (1+ (cdr obj))
                          (cdr obj))))
                  (list result (substring rest end))))
               ((eq char ?0)
                (list "\\&" rest))
               (t
                (list (concat "\\" (char-to-string char)) rest))))
          start)))
    (let ((rest (cdr result))
          (result (car result)))
      (replace-match-string-symbols result)
      (cons (if (cdr result)
                (cons 'concat result)
              (or (car result) ""))
            rest))))

(defun nsevil-compile-replacement (to)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary, the
original string if not. Currently the following magic characters
in replacements are supported: 0-9&#lLuUrnbt,
The magic character , (comma) start an Emacs-lisp expression."
  (when (stringp to)
    (save-match-data
      (cons 'replace-eval-replacement
            (car (nsevil-compile-subreplacement to))))))

(defun nsevil-replace-match (replacement &optional fixedcase string)
  "Replace text match by last search with REPLACEMENT.
If REPLACEMENT is an expression it will be evaluated to compute
the replacement text, otherwise the function behaves as
`replace-match'."
  (if (stringp replacement)
      (replace-match replacement fixedcase nil string)
    (replace-match (funcall (car replacement)
                            (cdr replacement)
                            0)
                   fixedcase nil string)))

(defun nsevil-match-substitute-replacement (replacement &optional fixedcase string)
  "Return REPLACEMENT as it will be inserted by `nsevil-replace-match'."
  (if (stringp replacement)
      (match-substitute-replacement replacement fixedcase nil string)
    (match-substitute-replacement (funcall (car replacement)
                                           (cdr replacement)
                                           0)
                                  fixedcase nil string)))

;;; Alignment

(defun nsevil-justify-lines (beg end justify position)
  "Justifes all lines in a range.
BEG and END specify the range of those lines to be
justified. JUSTIFY is either 'left, 'right or 'center according
to the justification type. POSITION is the maximal text width for
right and center justification or the column at which the lines
should be left-aligned for left justification."
  (let ((fill-column position)
        adaptive-fill-mode fill-prefix)
    (nsevil-with-restriction
        (save-excursion
          (goto-char beg)
          (line-beginning-position))
        (save-excursion
          (goto-char end)
          (if (bolp)
              (line-end-position 0)
            (line-end-position)))
      (goto-char (point-min))
      (while (progn
               (if (eq justify 'left)
                   (indent-line-to position)
                 (when (re-search-forward "^[[:space:]]*" nil t)
                   (delete-region (match-beginning 0)
                                  (match-end 0)))
                 (justify-current-line justify nil t))
               (and (zerop (forward-line)) (bolp))))
      (goto-char (point-min))
      (back-to-indentation))))

;;; View helper

(defvar-local nsevil-list-view-select-action nil)
(put 'nsevil-list-view-select-action 'permanent-local t)

(define-derived-mode nsevil-list-view-mode tabulated-list-mode
  "Nsevil List View"
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun nsevil-list-view-goto-entry ()
  (interactive)
  (when (and nsevil-list-view-select-action
             (not (eobp)))
    (let* ((line (line-number-at-pos (point)))
           (entry (elt tabulated-list-entries (1- line))))
      (funcall nsevil-list-view-select-action (nth 1 entry)))))

(defun nsevil-list-view-quit ()
  (interactive)
  (quit-window 'kill))

(define-key nsevil-list-view-mode-map (kbd "q") #'nsevil-list-view-quit)
(define-key nsevil-list-view-mode-map [follow-link] nil) ;; allows mouse-1 to be activated
(define-key nsevil-list-view-mode-map [mouse-1] #'nsevil-list-view-goto-entry)
(define-key nsevil-list-view-mode-map [return] #'nsevil-list-view-goto-entry)

(defmacro nsevil-with-view-list (&rest properties)
  "Opens new list view buffer.

PROPERTIES is a property-list which supports the following properties:

:name           (required)   The name of the buffer.
:mode-name      (required)   The name for the mode line.
:format         (required)   The value for `tabulated-list-format'.
:entries        (required)   The value for `tabulated-list-entries'.
:select-action  (optional)   A function for row selection.
                             It takes in a single parameter, which is the selected row's
                             vector value that is passed into `:entries'.
"
  (declare (indent defun) (debug t))
  `(let ((bufname (concat "*" ,(plist-get properties :name) "*"))
         (inhibit-read-only t))
     (and (get-buffer bufname)
          (kill-buffer bufname))
     (let ((buf (get-buffer-create bufname)))
       (with-current-buffer buf
         (setq tabulated-list-format ,(plist-get properties :format))
         (setq tabulated-list-entries ,(plist-get properties :entries))
         (setq nsevil-list-view-select-action ,(plist-get properties :select-action))
         (nsevil-list-view-mode)
         (setq mode-name ,(plist-get properties :mode-name))
         (nsevil-motion-state))
       (switch-to-buffer-other-window buf))))

(provide 'nsevil-common)

;;; nsevil-common.el ends here
