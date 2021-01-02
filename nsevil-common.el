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

;; (defsubst nsevil-kbd-macro-suppress-motion-error ()
;;   "Returns non-nil if a motion error should be suppressed.
;; Whether the motion error should be suppressed depends on the
;; variable `nsevil-kbd-macro-suppress-motion-error'."
;;   (or (and defining-kbd-macro
;;            (memq nsevil-kbd-macro-suppress-motion-error '(t record)))
;;       (and executing-kbd-macro
;;            (memq nsevil-kbd-macro-suppress-motion-error '(t replay)))))

(provide 'nsevil-common)

;;; nsevil-common.el ends here
