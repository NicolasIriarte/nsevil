;;; nsevil-maps.el --- Default keymaps -*- lexical-binding: t -*-

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

(require 'nsevil-states)
;; (require 'nsevil-ex)
(require 'nsevil-commands)
;; (require 'nsevil-command-window)
(require 'nsevil-common)

;;; Code:

;;; Normal state

;; (define-key nsevil-normal-state-map "a" 'nsevil-append)
;; (define-key nsevil-normal-state-map "A" 'nsevil-append-line)
;; (define-key nsevil-normal-state-map "c" 'nsevil-change)
;; (define-key nsevil-normal-state-map "C" 'nsevil-change-line)
;; (define-key nsevil-normal-state-map "d" 'nsevil-delete)
;; (define-key nsevil-normal-state-map "D" 'nsevil-delete-line)
(define-key nsevil-normal-state-map "i" 'nsevil-insert)
;; (define-key nsevil-normal-state-map (kbd "<insert>") 'nsevil-insert)
;; (define-key nsevil-normal-state-map (kbd "<insertchar>") 'nsevil-insert)
;; (define-key nsevil-normal-state-map "I" 'nsevil-insert-line)
;; (define-key nsevil-normal-state-map "J" 'nsevil-join)
;; (define-key nsevil-normal-state-map "m" 'nsevil-set-marker)
;; (define-key nsevil-normal-state-map "o" 'nsevil-open-below)
;; (define-key nsevil-normal-state-map "O" 'nsevil-open-above)
;; (define-key nsevil-normal-state-map "p" 'nsevil-paste-after)
;; (define-key nsevil-normal-state-map "P" 'nsevil-paste-before)
;; (define-key nsevil-normal-state-map "q" 'nsevil-record-macro)
;; (define-key nsevil-normal-state-map "r" 'nsevil-replace)
;; (define-key nsevil-normal-state-map "R" 'nsevil-replace-state)
;; (define-key nsevil-normal-state-map "s" 'nsevil-substitute)
;; (define-key nsevil-normal-state-map "S" 'nsevil-change-whole-line)
;; (define-key nsevil-normal-state-map "x" 'nsevil-delete-char)
;; (define-key nsevil-normal-state-map "X" 'nsevil-delete-backward-char)
;; (define-key nsevil-normal-state-map [deletechar] 'nsevil-delete-char)
;; (define-key nsevil-normal-state-map "y" 'nsevil-yank)
;; (define-key nsevil-normal-state-map "Y" 'nsevil-yank-line)
;; (define-key nsevil-normal-state-map "&" 'nsevil-ex-repeat-substitute)
;; (define-key nsevil-normal-state-map "g&" 'nsevil-ex-repeat-global-substitute)
;; (define-key nsevil-normal-state-map "g8" 'what-cursor-position)
;; (define-key nsevil-normal-state-map "ga" 'what-cursor-position)
;; (define-key nsevil-normal-state-map "gi" 'nsevil-insert-resume)
;; (define-key nsevil-normal-state-map "gJ" 'nsevil-join-whitespace)
;; (define-key nsevil-normal-state-map "gq" 'nsevil-fill-and-move)
;; (define-key nsevil-normal-state-map "gw" 'nsevil-fill)
;; (define-key nsevil-normal-state-map "gu" 'nsevil-downcase)
;; (define-key nsevil-normal-state-map "gU" 'nsevil-upcase)
;; (define-key nsevil-normal-state-map "gf" 'find-file-at-point)
;; (define-key nsevil-normal-state-map "gF" 'nsevil-find-file-at-point-with-line)
;; (define-key nsevil-normal-state-map "gx" 'browse-url-at-point)
;; (define-key nsevil-normal-state-map "g?" 'nsevil-rot13)
;; (define-key nsevil-normal-state-map "g~" 'nsevil-invert-case)
;; (define-key nsevil-normal-state-map "zo" 'nsevil-open-fold)
;; (define-key nsevil-normal-state-map "zO" 'nsevil-open-fold-rec)
;; (define-key nsevil-normal-state-map "zc" 'nsevil-close-fold)
;; (define-key nsevil-normal-state-map "za" 'nsevil-toggle-fold)
;; (define-key nsevil-normal-state-map "zr" 'nsevil-open-folds)
;; (define-key nsevil-normal-state-map "zm" 'nsevil-close-folds)
;; (define-key nsevil-normal-state-map "z=" 'ispell-word)
;; (define-key nsevil-normal-state-map "\C-n" 'nsevil-paste-pop-next)
;; (define-key nsevil-normal-state-map "\C-p" 'nsevil-paste-pop)
;; (define-key nsevil-normal-state-map "\C-t" 'pop-tag-mark)
;; (define-key nsevil-normal-state-map (kbd "C-.") 'nsevil-repeat-pop)
;; (define-key nsevil-normal-state-map (kbd "M-.") 'nsevil-repeat-pop-next)
;; (define-key nsevil-normal-state-map "." 'nsevil-repeat)
;; (define-key nsevil-normal-state-map "@" 'nsevil-execute-macro)
;; (define-key nsevil-normal-state-map "\"" 'nsevil-use-register)
;; (define-key nsevil-normal-state-map "~" 'nsevil-invert-char)
;; (define-key nsevil-normal-state-map "=" 'nsevil-indent)
;; (define-key nsevil-normal-state-map "<" 'nsevil-shift-left)
;; (define-key nsevil-normal-state-map ">" 'nsevil-shift-right)
;; (define-key nsevil-normal-state-map "ZZ" 'nsevil-save-modified-and-close)
;; (define-key nsevil-normal-state-map "ZQ" 'nsevil-quit)
;; (define-key nsevil-normal-state-map (kbd "DEL") 'nsevil-backward-char)
;; (define-key nsevil-normal-state-map [escape] 'nsevil-force-normal-state)
;; (define-key nsevil-normal-state-map [remap cua-paste-pop] 'nsevil-paste-pop)
;; (define-key nsevil-normal-state-map [remap yank-pop] 'nsevil-paste-pop)

;; (when (featurep 'tab-bar)
;;   (define-key nsevil-normal-state-map "gt" 'tab-bar-switch-to-next-tab)
;;   (define-key nsevil-normal-state-map "gT" 'tab-bar-switch-to-prev-tab))

;; ;; go to last change
;; (define-key nsevil-normal-state-map "g;" 'goto-last-change)
;; (define-key nsevil-normal-state-map "g," 'goto-last-change-reverse)

;; ;; undo
;; (define-key nsevil-normal-state-map "u" 'nsevil-undo)
;; (define-key nsevil-normal-state-map "\C-r" 'nsevil-redo)

;; ;; window commands
;; (define-prefix-command 'nsevil-window-map)
;; (define-key nsevil-window-map "b" 'nsevil-window-bottom-right)
;; (define-key nsevil-window-map "c" 'nsevil-window-delete)
;; (define-key nsevil-window-map "h" 'nsevil-window-left)
;; (define-key nsevil-window-map "H" 'nsevil-window-move-far-left)
;; (define-key nsevil-window-map "j" 'nsevil-window-down)
;; (define-key nsevil-window-map "J" 'nsevil-window-move-very-bottom)
;; (define-key nsevil-window-map "k" 'nsevil-window-up)
;; (define-key nsevil-window-map "K" 'nsevil-window-move-very-top)
;; (define-key nsevil-window-map "l" 'nsevil-window-right)
;; (define-key nsevil-window-map "L" 'nsevil-window-move-far-right)
;; (define-key nsevil-window-map "n" 'nsevil-window-new)
;; (define-key nsevil-window-map "o" 'delete-other-windows)
;; (define-key nsevil-window-map "p" 'nsevil-window-mru)
;; (define-key nsevil-window-map "q" 'nsevil-quit)
;; (define-key nsevil-window-map "r" 'nsevil-window-rotate-downwards)
;; (define-key nsevil-window-map "R" 'nsevil-window-rotate-upwards)
;; (define-key nsevil-window-map "s" 'nsevil-window-split)
;; (define-key nsevil-window-map "S" 'nsevil-window-split)
;; (define-key nsevil-window-map "t" 'nsevil-window-top-left)
;; (define-key nsevil-window-map "v" 'nsevil-window-vsplit)
;; (define-key nsevil-window-map "w" 'nsevil-window-next)
;; (define-key nsevil-window-map "W" 'nsevil-window-prev)
;; (define-key nsevil-window-map "+" 'nsevil-window-increase-height)
;; (define-key nsevil-window-map "-" 'nsevil-window-decrease-height)
;; (define-key nsevil-window-map "_" 'nsevil-window-set-height)
;; (define-key nsevil-window-map "<" 'nsevil-window-decrease-width)
;; (define-key nsevil-window-map ">" 'nsevil-window-increase-width)
;; (define-key nsevil-window-map "=" 'balance-windows)
;; (define-key nsevil-window-map "|" 'nsevil-window-set-width)
;; (define-key nsevil-window-map "\C-b" 'nsevil-window-bottom-right)
;; (define-key nsevil-window-map "\C-c" 'nsevil-window-delete)
;; (define-key nsevil-window-map (kbd "C-S-h") 'nsevil-window-move-far-left)
;; (define-key nsevil-window-map (kbd "C-S-j") 'nsevil-window-move-very-bottom)
;; (define-key nsevil-window-map (kbd "C-S-k") 'nsevil-window-move-very-top)
;; (define-key nsevil-window-map (kbd "C-S-l") 'nsevil-window-move-far-right)
;; (define-key nsevil-window-map "\C-n" 'nsevil-window-new)
;; (define-key nsevil-window-map "\C-o" 'delete-other-windows)
;; (define-key nsevil-window-map "\C-p" 'nsevil-window-mru)
;; (define-key nsevil-window-map "\C-r" 'nsevil-window-rotate-downwards)
;; (define-key nsevil-window-map (kbd "C-S-r") 'nsevil-window-rotate-upwards)
;; (define-key nsevil-window-map "\C-s" 'nsevil-window-split)
;; (define-key nsevil-window-map (kbd "C-S-s") 'nsevil-window-split)
;; (define-key nsevil-window-map "\C-t" 'nsevil-window-top-left)
;; (define-key nsevil-window-map "\C-v" 'nsevil-window-vsplit)
;; (define-key nsevil-window-map "\C-w" 'nsevil-window-next)
;; (define-key nsevil-window-map (kbd "C-S-W") 'nsevil-window-prev)
;; (define-key nsevil-window-map "\C-_" 'nsevil-window-set-height)
;; (define-key nsevil-window-map "\C-f" 'ffap-other-window)

;; ;;; Motion state

;; ;; "0" is a special command when called first
;; (nsevil-redirect-digit-argument nsevil-motion-state-map "0" 'nsevil-beginning-of-line)
;; (define-key nsevil-motion-state-map "1" 'digit-argument)
;; (define-key nsevil-motion-state-map "2" 'digit-argument)
;; (define-key nsevil-motion-state-map "3" 'digit-argument)
;; (define-key nsevil-motion-state-map "4" 'digit-argument)
;; (define-key nsevil-motion-state-map "5" 'digit-argument)
;; (define-key nsevil-motion-state-map "6" 'digit-argument)
;; (define-key nsevil-motion-state-map "7" 'digit-argument)
;; (define-key nsevil-motion-state-map "8" 'digit-argument)
;; (define-key nsevil-motion-state-map "9" 'digit-argument)
;; (define-key nsevil-motion-state-map "b" 'nsevil-backward-word-begin)
;; (define-key nsevil-motion-state-map "B" 'nsevil-backward-WORD-begin)
;; (define-key nsevil-motion-state-map "e" 'nsevil-forward-word-end)
;; (define-key nsevil-motion-state-map "E" 'nsevil-forward-WORD-end)
;; (define-key nsevil-motion-state-map "f" 'nsevil-find-char)
;; (define-key nsevil-motion-state-map "F" 'nsevil-find-char-backward)
;; (define-key nsevil-motion-state-map "G" 'nsevil-goto-line)
;; (define-key nsevil-motion-state-map "h" 'nsevil-backward-char)
;; (define-key nsevil-motion-state-map "H" 'nsevil-window-top)
;; (define-key nsevil-motion-state-map "j" 'nsevil-next-line)
;; (define-key nsevil-motion-state-map "k" 'nsevil-previous-line)
;; (define-key nsevil-motion-state-map "l" 'nsevil-forward-char)
;; (define-key nsevil-motion-state-map " " 'nsevil-forward-char)
;; (define-key nsevil-motion-state-map "K" 'nsevil-lookup)
;; (define-key nsevil-motion-state-map "L" 'nsevil-window-bottom)
;; (define-key nsevil-motion-state-map "M" 'nsevil-window-middle)
;; (define-key nsevil-motion-state-map "n" 'nsevil-search-next)
;; (define-key nsevil-motion-state-map "N" 'nsevil-search-previous)
;; (define-key nsevil-motion-state-map "t" 'nsevil-find-char-to)
;; (define-key nsevil-motion-state-map "T" 'nsevil-find-char-to-backward)
;; (define-key nsevil-motion-state-map "w" 'nsevil-forward-word-begin)
;; (define-key nsevil-motion-state-map "W" 'nsevil-forward-WORD-begin)
;; (define-key nsevil-motion-state-map "y" 'nsevil-yank)
;; (define-key nsevil-motion-state-map "Y" 'nsevil-yank-line)
;; (define-key nsevil-motion-state-map "gd" 'nsevil-goto-definition)
;; (define-key nsevil-motion-state-map "ge" 'nsevil-backward-word-end)
;; (define-key nsevil-motion-state-map "gE" 'nsevil-backward-WORD-end)
;; (define-key nsevil-motion-state-map "gg" 'nsevil-goto-first-line)
;; (define-key nsevil-motion-state-map "gj" 'nsevil-next-visual-line)
;; (define-key nsevil-motion-state-map "gk" 'nsevil-previous-visual-line)
;; (define-key nsevil-motion-state-map "g0" 'nsevil-beginning-of-visual-line)
;; (define-key nsevil-motion-state-map "g_" 'nsevil-last-non-blank)
;; (define-key nsevil-motion-state-map "g^" 'nsevil-first-non-blank-of-visual-line)
;; (define-key nsevil-motion-state-map "gm" 'nsevil-middle-of-visual-line)
;; (define-key nsevil-motion-state-map "g$" 'nsevil-end-of-visual-line)
;; (define-key nsevil-motion-state-map "g\C-]" 'nsevil-jump-to-tag)
;; (define-key nsevil-motion-state-map "{" 'nsevil-backward-paragraph)
;; (define-key nsevil-motion-state-map "}" 'nsevil-forward-paragraph)
;; (define-key nsevil-motion-state-map "#" 'nsevil-search-word-backward)
;; (define-key nsevil-motion-state-map "g#" 'nsevil-search-unbounded-word-backward)
;; (define-key nsevil-motion-state-map "$" 'nsevil-end-of-line)
;; (define-key nsevil-motion-state-map "%" 'nsevil-jump-item)
;; (define-key nsevil-motion-state-map "`" 'nsevil-goto-mark)
;; (define-key nsevil-motion-state-map "'" 'nsevil-goto-mark-line)
;; (define-key nsevil-motion-state-map "(" 'nsevil-backward-sentence-begin)
;; (define-key nsevil-motion-state-map ")" 'nsevil-forward-sentence-begin)
;; (define-key nsevil-motion-state-map "]]" 'nsevil-forward-section-begin)
;; (define-key nsevil-motion-state-map "][" 'nsevil-forward-section-end)
;; (define-key nsevil-motion-state-map "[[" 'nsevil-backward-section-begin)
;; (define-key nsevil-motion-state-map "[]" 'nsevil-backward-section-end)
;; (define-key nsevil-motion-state-map "[(" 'nsevil-previous-open-paren)
;; (define-key nsevil-motion-state-map "])" 'nsevil-next-close-paren)
;; (define-key nsevil-motion-state-map "[{" 'nsevil-previous-open-brace)
;; (define-key nsevil-motion-state-map "]}" 'nsevil-next-close-brace)
;; (define-key nsevil-motion-state-map "]s" 'nsevil-next-flyspell-error)
;; (define-key nsevil-motion-state-map "[s" 'nsevil-prev-flyspell-error)
;; (define-key nsevil-motion-state-map "*" 'nsevil-search-word-forward)
;; (define-key nsevil-motion-state-map "g*" 'nsevil-search-unbounded-word-forward)
;; (define-key nsevil-motion-state-map "," 'nsevil-repeat-find-char-reverse)
;; (define-key nsevil-motion-state-map "/" 'nsevil-search-forward)
;; (define-key nsevil-motion-state-map ";" 'nsevil-repeat-find-char)
;; (define-key nsevil-motion-state-map "?" 'nsevil-search-backward)
;; (define-key nsevil-motion-state-map "|" 'nsevil-goto-column)
;; (define-key nsevil-motion-state-map "^" 'nsevil-first-non-blank)
;; (define-key nsevil-motion-state-map "+" 'nsevil-next-line-first-non-blank)
;; (define-key nsevil-motion-state-map "_" 'nsevil-next-line-1-first-non-blank)
;; (define-key nsevil-motion-state-map "-" 'nsevil-previous-line-first-non-blank)
;; (define-key nsevil-motion-state-map "\C-w" 'nsevil-window-map)
;; (define-key nsevil-motion-state-map (kbd "C-6") 'nsevil-switch-to-windows-last-buffer)
;; (define-key nsevil-motion-state-map "\C-]" 'nsevil-jump-to-tag)
;; (define-key nsevil-motion-state-map (kbd "C-b") 'nsevil-scroll-page-up)
;; (define-key nsevil-motion-state-map (kbd "C-e") 'nsevil-scroll-line-down)
;; (define-key nsevil-motion-state-map (kbd "C-f") 'nsevil-scroll-page-down)
;; (define-key nsevil-motion-state-map (kbd "C-o") 'nsevil-jump-backward)
;; (define-key nsevil-motion-state-map (kbd "C-y") 'nsevil-scroll-line-up)
;; (define-key nsevil-motion-state-map (kbd "RET") 'nsevil-ret)
;; (define-key nsevil-motion-state-map "\\" 'nsevil-execute-in-emacs-state)
;; (define-key nsevil-motion-state-map "z^" 'nsevil-scroll-top-line-to-bottom)
;; (define-key nsevil-motion-state-map "z+" 'nsevil-scroll-bottom-line-to-top)
;; (define-key nsevil-motion-state-map "zt" 'nsevil-scroll-line-to-top)
;; ;; TODO: z RET has an advanced form taking an count before the RET
;; ;; but this requires again a special state with a single command
;; ;; bound to RET
;; (define-key nsevil-motion-state-map (vconcat "z" [return]) "zt^")
;; (define-key nsevil-motion-state-map (kbd "z RET") (vconcat "z" [return]))
;; (define-key nsevil-motion-state-map "zz" 'nsevil-scroll-line-to-center)
;; (define-key nsevil-motion-state-map "z." "zz^")
;; (define-key nsevil-motion-state-map "zb" 'nsevil-scroll-line-to-bottom)
;; (define-key nsevil-motion-state-map "z-" "zb^")
;; (define-key nsevil-motion-state-map "v" 'nsevil-visual-char)
;; (define-key nsevil-motion-state-map "V" 'nsevil-visual-line)
;; (define-key nsevil-motion-state-map "\C-v" 'nsevil-visual-block)
;; (define-key nsevil-motion-state-map "gv" 'nsevil-visual-restore)
;; (define-key nsevil-motion-state-map (kbd "C-^") 'nsevil-buffer)
;; (define-key nsevil-motion-state-map [left] 'nsevil-backward-char)
;; (define-key nsevil-motion-state-map [right] 'nsevil-forward-char)
;; (define-key nsevil-motion-state-map [up] 'nsevil-previous-line)
;; (define-key nsevil-motion-state-map [down] 'nsevil-next-line)
;; (define-key nsevil-motion-state-map "zl" 'nsevil-scroll-column-right)
;; (define-key nsevil-motion-state-map [?z right] "zl")
;; (define-key nsevil-motion-state-map "zh" 'nsevil-scroll-column-left)
;; (define-key nsevil-motion-state-map [?z left] "zh")
;; (define-key nsevil-motion-state-map "zL" 'nsevil-scroll-right)
;; (define-key nsevil-motion-state-map "zH" 'nsevil-scroll-left)
;; (define-key nsevil-motion-state-map
;;   (read-kbd-macro nsevil-toggle-key) 'nsevil-emacs-state)

;; ;; text objects
;; (define-key nsevil-outer-text-objects-map "w" 'nsevil-a-word)
;; (define-key nsevil-outer-text-objects-map "W" 'nsevil-a-WORD)
;; (define-key nsevil-outer-text-objects-map "s" 'nsevil-a-sentence)
;; (define-key nsevil-outer-text-objects-map "p" 'nsevil-a-paragraph)
;; (define-key nsevil-outer-text-objects-map "b" 'nsevil-a-paren)
;; (define-key nsevil-outer-text-objects-map "(" 'nsevil-a-paren)
;; (define-key nsevil-outer-text-objects-map ")" 'nsevil-a-paren)
;; (define-key nsevil-outer-text-objects-map "[" 'nsevil-a-bracket)
;; (define-key nsevil-outer-text-objects-map "]" 'nsevil-a-bracket)
;; (define-key nsevil-outer-text-objects-map "B" 'nsevil-a-curly)
;; (define-key nsevil-outer-text-objects-map "{" 'nsevil-a-curly)
;; (define-key nsevil-outer-text-objects-map "}" 'nsevil-a-curly)
;; (define-key nsevil-outer-text-objects-map "<" 'nsevil-an-angle)
;; (define-key nsevil-outer-text-objects-map ">" 'nsevil-an-angle)
;; (define-key nsevil-outer-text-objects-map "'" 'nsevil-a-single-quote)
;; (define-key nsevil-outer-text-objects-map "\"" 'nsevil-a-double-quote)
;; (define-key nsevil-outer-text-objects-map "`" 'nsevil-a-back-quote)
;; (define-key nsevil-outer-text-objects-map "t" 'nsevil-a-tag)
;; (define-key nsevil-outer-text-objects-map "o" 'nsevil-a-symbol)
;; (define-key nsevil-inner-text-objects-map "w" 'nsevil-inner-word)
;; (define-key nsevil-inner-text-objects-map "W" 'nsevil-inner-WORD)
;; (define-key nsevil-inner-text-objects-map "s" 'nsevil-inner-sentence)
;; (define-key nsevil-inner-text-objects-map "p" 'nsevil-inner-paragraph)
;; (define-key nsevil-inner-text-objects-map "b" 'nsevil-inner-paren)
;; (define-key nsevil-inner-text-objects-map "(" 'nsevil-inner-paren)
;; (define-key nsevil-inner-text-objects-map ")" 'nsevil-inner-paren)
;; (define-key nsevil-inner-text-objects-map "[" 'nsevil-inner-bracket)
;; (define-key nsevil-inner-text-objects-map "]" 'nsevil-inner-bracket)
;; (define-key nsevil-inner-text-objects-map "B" 'nsevil-inner-curly)
;; (define-key nsevil-inner-text-objects-map "{" 'nsevil-inner-curly)
;; (define-key nsevil-inner-text-objects-map "}" 'nsevil-inner-curly)
;; (define-key nsevil-inner-text-objects-map "<" 'nsevil-inner-angle)
;; (define-key nsevil-inner-text-objects-map ">" 'nsevil-inner-angle)
;; (define-key nsevil-inner-text-objects-map "'" 'nsevil-inner-single-quote)
;; (define-key nsevil-inner-text-objects-map "\"" 'nsevil-inner-double-quote)
;; (define-key nsevil-inner-text-objects-map "`" 'nsevil-inner-back-quote)
;; (define-key nsevil-inner-text-objects-map "t" 'nsevil-inner-tag)
;; (define-key nsevil-inner-text-objects-map "o" 'nsevil-inner-symbol)
;; (define-key nsevil-motion-state-map "gn" 'nsevil-next-match)
;; (define-key nsevil-motion-state-map "gN" 'nsevil-previous-match)

;; (when nsevil-want-C-i-jump
;;   (define-key nsevil-motion-state-map (kbd "C-i") 'nsevil-jump-forward))

;; (when nsevil-want-C-u-scroll
;;   (define-key nsevil-motion-state-map (kbd "C-u") 'nsevil-scroll-up))

;; (when nsevil-want-C-d-scroll
;;   (define-key nsevil-motion-state-map (kbd "C-d") 'nsevil-scroll-down))

;; (when nsevil-want-C-g-bindings
;;   (define-key nsevil-motion-state-map "g\C-g" 'count-words))

;; ;;; Visual state

;; (define-key nsevil-visual-state-map "A" 'nsevil-append)
;; (define-key nsevil-visual-state-map "I" 'nsevil-insert)
;; (define-key nsevil-visual-state-map "o" 'exchange-point-and-mark)
;; (define-key nsevil-visual-state-map "O" 'nsevil-visual-exchange-corners)
;; (define-key nsevil-visual-state-map "R" 'nsevil-change)
;; (define-key nsevil-visual-state-map "u" 'nsevil-downcase)
;; (define-key nsevil-visual-state-map "U" 'nsevil-upcase)
;; (define-key nsevil-visual-state-map "z=" 'ispell-word)
;; (define-key nsevil-visual-state-map "a" nsevil-outer-text-objects-map)
;; (define-key nsevil-visual-state-map "i" nsevil-inner-text-objects-map)
;; (define-key nsevil-visual-state-map (kbd "<insert>") 'undefined)
;; (define-key nsevil-visual-state-map (kbd "<insertchar>") 'undefined)
;; (define-key nsevil-visual-state-map [remap nsevil-repeat] 'undefined)
;; (define-key nsevil-visual-state-map [escape] 'nsevil-exit-visual-state)

;; ;;; Operator-Pending state

;; (define-key nsevil-operator-state-map "a" nsevil-outer-text-objects-map)
;; (define-key nsevil-operator-state-map "i" nsevil-inner-text-objects-map)
;; ;; (define-key nsevil-operator-state-map [escape] 'keyboard-quit)

;; ;;; Insert state

;; (defvar nsevil-insert-state-bindings
;;   `(("\C-v" . quoted-insert)
;;     ("\C-k" . nsevil-insert-digraph)
;;     ("\C-o" . nsevil-execute-in-normal-state)
;;     ("\C-r" . nsevil-paste-from-register)
;;     ("\C-y" . nsevil-copy-from-above)
;;     ("\C-e" . nsevil-copy-from-below)
;;     ("\C-n" . nsevil-complete-next)
;;     ("\C-p" . nsevil-complete-previous)
;;     ("\C-x\C-n" . nsevil-complete-next-line)
;;     ("\C-x\C-p" . nsevil-complete-previous-line)
;;     ("\C-t" . nsevil-shift-right-line)
;;     ("\C-d" . nsevil-shift-left-line)
;;     ("\C-a" . nsevil-paste-last-insertion)
;;     ([remap delete-backward-char] . nsevil-delete-backward-char-and-join)
;;     ,(if nsevil-want-C-w-delete
;;          '("\C-w" . nsevil-delete-backward-word)
;;        '("\C-w" . nsevil-window-map))
;;     ,@(when nsevil-want-C-u-delete
;;         '(("\C-u" . nsevil-delete-back-to-indentation)))
;;     ([mouse-2] . mouse-yank-primary))
;;   "Nsevil's bindings for insert state (for
;; `nsevil-insert-state-map'), excluding <delete>, <escape>, and
;; `nsevil-toggle-key'.")

;; (defun nsevil-update-insert-state-bindings (&optional _option-name remove force)
;;   "Update bindings in `nsevil-insert-state-map'.
;; If no arguments are given add the bindings specified in
;; `nsevil-insert-state-bindings'. If REMOVE is non nil, remove only
;; these bindings. Unless FORCE is non nil, this will not
;; overwriting existing bindings, which means bindings will not be
;; added if one already exists for a key and only default bindings
;; are removed.

;; Note that <delete>, <escape> and `nsevil-toggle-key' are not
;; included in `nsevil-insert-state-bindings' by default."
;;   (interactive)
;;   (dolist (binding nsevil-insert-state-bindings)
;;     (cond
;;      ((and remove
;;            (or force
;;                ;; Only remove if the default binding has not changed
;;                (eq (nsevil-lookup-key nsevil-insert-state-map (car binding))
;;                    (cdr binding))))
;;       (define-key nsevil-insert-state-map (car binding) nil))
;;      ((and (null remove)
;;            (or force
;;                ;; Check to see that nothing is bound here before adding
;;                (not (nsevil-lookup-key nsevil-insert-state-map (car binding)))))
;;       (define-key nsevil-insert-state-map (car binding) (cdr binding))))))

;; (define-key nsevil-insert-state-map [delete] 'delete-char)
(define-key nsevil-insert-state-map [escape] 'nsevil-normal-state)
;; (define-key nsevil-insert-state-map
;;   (read-kbd-macro nsevil-toggle-key) 'nsevil-emacs-state)

;; ;;; Replace state

;; (define-key nsevil-replace-state-map (kbd "DEL") 'nsevil-replace-backspace)
;; (define-key nsevil-replace-state-map [escape] 'nsevil-normal-state)

;; ;;; Emacs state

;; (define-key nsevil-emacs-state-map
;;   (read-kbd-macro nsevil-toggle-key) 'nsevil-exit-emacs-state)

;; (when nsevil-want-C-w-in-emacs-state
;;   (define-key nsevil-emacs-state-map "\C-w" 'nsevil-window-map))

;; ;;; Mouse
;; (define-key nsevil-motion-state-map [down-mouse-1] 'nsevil-mouse-drag-region)
;; (define-key nsevil-visual-state-map [mouse-2] 'nsevil-exit-visual-and-repeat)
;; (define-key nsevil-normal-state-map [mouse-2] 'mouse-yank-primary)

;; ;; Ex
;; (define-key nsevil-motion-state-map ":" 'nsevil-ex)
;; (define-key nsevil-motion-state-map "!" 'nsevil-shell-command)

;; (nsevil-ex-define-cmd "e[dit]" 'nsevil-edit)
;; (nsevil-ex-define-cmd "w[rite]" 'nsevil-write)
;; (nsevil-ex-define-cmd "wa[ll]" 'nsevil-write-all)
;; (nsevil-ex-define-cmd "sav[eas]" 'nsevil-save)
;; (nsevil-ex-define-cmd "r[ead]" 'nsevil-read)
;; (nsevil-ex-define-cmd "b[uffer]" 'nsevil-buffer)
;; (nsevil-ex-define-cmd "bn[ext]" 'nsevil-next-buffer)
;; (nsevil-ex-define-cmd "bp[revious]" 'nsevil-prev-buffer)
;; (nsevil-ex-define-cmd "bN[ext]" "bprevious")
;; (nsevil-ex-define-cmd "sb[uffer]" 'nsevil-split-buffer)
;; (nsevil-ex-define-cmd "sbn[ext]" 'nsevil-split-next-buffer)
;; (nsevil-ex-define-cmd "sbp[revious]" 'nsevil-split-prev-buffer)
;; (nsevil-ex-define-cmd "sbN[ext]" "sbprevious")
;; (nsevil-ex-define-cmd "buffers" 'buffer-menu)
;; (nsevil-ex-define-cmd "files" 'nsevil-show-files)
;; (nsevil-ex-define-cmd "ls" "buffers")

;; (nsevil-ex-define-cmd "c[hange]" 'nsevil-change)
;; (nsevil-ex-define-cmd "co[py]" 'nsevil-copy)
;; (nsevil-ex-define-cmd "t" "copy")
;; (nsevil-ex-define-cmd "m[ove]" 'nsevil-move)
;; (nsevil-ex-define-cmd "d[elete]" 'nsevil-ex-delete)
;; (nsevil-ex-define-cmd "y[ank]" 'nsevil-ex-yank)
;; (nsevil-ex-define-cmd "go[to]" 'nsevil-goto-char)
;; (nsevil-ex-define-cmd "j[oin]" 'nsevil-ex-join)
;; (nsevil-ex-define-cmd "le[ft]" 'nsevil-align-left)
;; (nsevil-ex-define-cmd "ri[ght]" 'nsevil-align-right)
;; (nsevil-ex-define-cmd "ce[nter]" 'nsevil-align-center)
;; (nsevil-ex-define-cmd "sp[lit]" 'nsevil-window-split)
;; (nsevil-ex-define-cmd "vs[plit]" 'nsevil-window-vsplit)
;; (nsevil-ex-define-cmd "new" 'nsevil-window-new)
;; (nsevil-ex-define-cmd "ene[w]" 'nsevil-buffer-new)
;; (nsevil-ex-define-cmd "vne[w]" 'nsevil-window-vnew)
;; (nsevil-ex-define-cmd "clo[se]" 'nsevil-window-delete)
;; (nsevil-ex-define-cmd "on[ly]" 'delete-other-windows)
;; (nsevil-ex-define-cmd "q[uit]" 'nsevil-quit)
;; (nsevil-ex-define-cmd "wq" 'nsevil-save-and-close)
;; (nsevil-ex-define-cmd "quita[ll]" 'nsevil-quit-all)
;; (nsevil-ex-define-cmd "qa[ll]" "quitall")
;; (nsevil-ex-define-cmd "cq[uit]" 'nsevil-quit-all-with-error-code)
;; (nsevil-ex-define-cmd "wqa[ll]" 'nsevil-save-and-quit)
;; (nsevil-ex-define-cmd "xa[ll]" "wqall")
;; (nsevil-ex-define-cmd "x[it]" 'nsevil-save-modified-and-close)
;; (nsevil-ex-define-cmd "exi[t]" 'nsevil-save-modified-and-close)
;; (nsevil-ex-define-cmd "bd[elete]" 'nsevil-delete-buffer)
;; (nsevil-ex-define-cmd "bw[ipeout]" 'nsevil-delete-buffer)
;; (nsevil-ex-define-cmd "g[lobal]" 'nsevil-ex-global)
;; (nsevil-ex-define-cmd "v[global]" 'nsevil-ex-global-inverted)
;; (nsevil-ex-define-cmd "norm[al]" 'nsevil-ex-normal)
;; (nsevil-ex-define-cmd "s[ubstitute]" 'nsevil-ex-substitute)
;; (nsevil-ex-define-cmd "&" 'nsevil-ex-repeat-substitute)
;; (nsevil-ex-define-cmd "&&" 'nsevil-ex-repeat-substitute-with-flags)
;; (nsevil-ex-define-cmd "~" 'nsevil-ex-repeat-substitute-with-search)
;; (nsevil-ex-define-cmd "~&" 'nsevil-ex-repeat-substitute-with-search-and-flags)
;; (nsevil-ex-define-cmd "registers" 'nsevil-show-registers)
;; (nsevil-ex-define-cmd "marks" 'nsevil-show-marks)
;; (nsevil-ex-define-cmd "delm[arks]" 'nsevil-delete-marks)
;; (nsevil-ex-define-cmd "ju[mps]" 'nsevil-show-jumps)
;; (nsevil-ex-define-cmd "noh[lsearch]" 'nsevil-ex-nohighlight)
;; (nsevil-ex-define-cmd "f[ile]" 'nsevil-show-file-info)
;; (nsevil-ex-define-cmd "<" 'nsevil-shift-left)
;; (nsevil-ex-define-cmd ">" 'nsevil-shift-right)
;; (nsevil-ex-define-cmd "=" 'nsevil-ex-line-number)
;; (nsevil-ex-define-cmd "!" 'nsevil-shell-command)
;; (nsevil-ex-define-cmd "@:" 'nsevil-ex-repeat)
;; (nsevil-ex-define-cmd "mak[e]" 'nsevil-make)
;; (nsevil-ex-define-cmd "cc" 'nsevil-goto-error)
;; (nsevil-ex-define-cmd "cfir[st]" 'first-error)
;; (nsevil-ex-define-cmd "cr[ewind]" 'first-error)
;; (nsevil-ex-define-cmd "cn[ext]" 'next-error)
;; (nsevil-ex-define-cmd "cp[revious]" 'previous-error)
;; (nsevil-ex-define-cmd "set-initial-state" 'nsevil-ex-set-initial-state)
;; (nsevil-ex-define-cmd "show-digraphs" 'nsevil-ex-show-digraphs)
;; (nsevil-ex-define-cmd "sor[t]" 'nsevil-ex-sort)
;; (nsevil-ex-define-cmd "res[ize]" 'nsevil-ex-resize)
;; (nsevil-ex-define-cmd "u[ndo]" 'nsevil-undo)
;; (nsevil-ex-define-cmd "red[o]" 'nsevil-redo)

;; (when (featurep 'tab-bar)
;;   (nsevil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
;;   (nsevil-ex-define-cmd "tabn[ext]" 'tab-bar-switch-to-next-tab)
;;   (nsevil-ex-define-cmd "tabp[revious]" 'tab-bar-switch-to-prev-tab))

;; ;; search command line
;; (define-key nsevil-ex-search-keymap "\d" #'nsevil-ex-delete-backward-char)
;; (define-key nsevil-ex-search-keymap "\C-f" 'nsevil-ex-search-command-window)
;; (define-key nsevil-ex-search-keymap "\C-r" 'nsevil-paste-from-register)
;; (define-key nsevil-ex-search-keymap "\C-n" 'next-history-element)
;; (define-key nsevil-ex-search-keymap "\C-p" 'previous-history-element)

;; ;; ex command line
;; (define-key nsevil-ex-completion-map "\d" #'nsevil-ex-delete-backward-char)
;; (define-key nsevil-ex-completion-map "\t" #'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map [tab] #'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map [remap completion-at-point] #'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map "\C-a" 'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map "\C-b" 'move-beginning-of-line)
;; (define-key nsevil-ex-completion-map "\C-c" 'abort-recursive-edit)
;; (define-key nsevil-ex-completion-map "\C-d" 'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map "\C-f" 'nsevil-ex-command-window)
;; (define-key nsevil-ex-completion-map "\C-g" 'abort-recursive-edit)
;; (define-key nsevil-ex-completion-map "\C-k" 'nsevil-insert-digraph)
;; (define-key nsevil-ex-completion-map "\C-l" 'nsevil-ex-completion)
;; (define-key nsevil-ex-completion-map "\C-p" #'previous-complete-history-element)
;; (define-key nsevil-ex-completion-map "\C-r" 'nsevil-paste-from-register)
;; (define-key nsevil-ex-completion-map "\C-n" #'next-complete-history-element)
;; (define-key nsevil-ex-completion-map "\C-u" 'nsevil-delete-whole-line)
;; (define-key nsevil-ex-completion-map "\C-v" #'quoted-insert)
;; (define-key nsevil-ex-completion-map "\C-w" 'backward-kill-word)
;; (define-key nsevil-ex-completion-map [escape] 'abort-recursive-edit)
;; (define-key nsevil-ex-completion-map [S-left] 'backward-word)
;; (define-key nsevil-ex-completion-map [S-right] 'forward-word)
;; (define-key nsevil-ex-completion-map [up] 'previous-complete-history-element)
;; (define-key nsevil-ex-completion-map [down] 'next-complete-history-element)
;; (define-key nsevil-ex-completion-map [prior] 'previous-history-element)
;; (define-key nsevil-ex-completion-map [next] 'next-history-element)
;; (define-key nsevil-ex-completion-map [return] 'exit-minibuffer)
;; (define-key nsevil-ex-completion-map (kbd "RET") 'exit-minibuffer)

;; ;; nsevil-read-key
;; (define-key nsevil-read-key-map (kbd "ESC") #'keyboard-quit)
;; (define-key nsevil-read-key-map (kbd "C-]") #'keyboard-quit)
;; (define-key nsevil-read-key-map (kbd "C-g") #'keyboard-quit)
;; (define-key nsevil-read-key-map (kbd "C-q") #'nsevil-read-quoted-char)
;; (define-key nsevil-read-key-map (kbd "C-v") #'nsevil-read-quoted-char)
;; (define-key nsevil-read-key-map (kbd "C-k") #'nsevil-read-digraph-char)
;; (define-key nsevil-read-key-map "\r" "\n")

;; ;; command line window
;; (nsevil-define-key 'normal
;;   nsevil-command-window-mode-map (kbd "RET") 'nsevil-command-window-execute)
;; (nsevil-define-key 'insert
;;   nsevil-command-window-mode-map (kbd "RET") 'nsevil-command-window-execute)

(provide 'nsevil-maps)

;;; nsevil-maps.el ends here
