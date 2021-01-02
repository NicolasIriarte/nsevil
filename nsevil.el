;;; nsevil.el --- Nsevil (Not so evil) is a fork of emacs evil with multiples major changes and a little inspiration on Modalka -*- lexical-binding: t -*-

;; The following list of authors was kept up to date until the beginning of
;; 2017, when nsevil moved under new maintainers. For authors since then, please
;; consult the git logs.

;;      Alessandro Piras <laynor at gmail.com>
;;      Alexander Baier <alexander.baier at mailbox.org>
;;      Antono Vasiljev <antono.vasiljev at gmail.com>
;;      Bailey Ling <bling at live.ca>
;;      Barry O'Reilly <gundaetiapo at gmail.com>
;;      Christoph Lange <langec at web.de>
;;      Daniel Reiter <danieltreiter at gmail.com>
;;      Eivind Fonn <evfonn at gmail.com>
;;      Emanuel Evans <emanuel.evans at gmail.com>
;;      Eric Siegel <siegel.eric at gmail.com>
;;      Eugene Yaremenko <w3techplayground at gmail.com>
;;      Frank Fischer <frank-fischer at shadow-soft.de>
;;      Frank Terbeck <ft at bewatermyfriend.org>
;;      Gordon Gustafson <gordon3.14 at gmail.com>
;;      Herbert Jones <jones.herbert at gmail.com>
;;      Jonas Bernoulli <jonas at bernoul.li>
;;      Jonathan Claggett <jclaggett at lonocloud.com>
;;      José A. Romero L. <escherdragon at gmail.com>
;;      Justin Burkett <justin at burkett.cc>
;;      Lars Andersen <expez at expez.com>
;;      Lintaro Ina <tarao.gnn at gmail.com>
;;      Lukasz Wrzosek <wrzoski at mail.com>
;;      Marian Schubert <maio at netsafe.cz>
;;      Matthew Malcomson <>
;;      Michael Markert <markert.michael at googlemail.com>
;;      Mike Gerwitz <mikegerwitz at gnu.org>
;;      Nikolai Weibull <now at bitwi.se>
;;      phaebz <phaebz at gmail.com>
;;      ralesi <scio62 at gmail.com>
;;      Rodrigo Setti <rodrigosetti at gmail.com>
;;      Sanel Zukan <sanelz at gmail.com>
;;      Sarah Brofeldt <sarah at thinkmonster.(none)>
;;      Simon Hafner <hafnersimon at gmail.com>
;;      Stefan Wehr <mail at stefanwehr.de>
;;      Sune Simonsen <sune.simonsen at jayway.com>
;;      Thomas Hisch <thomas at opentech.at>
;;      Tim Harper <timcharper at gmail.com>
;;      Tom Willemse <tom at ryuslash.org>
;;      Trevor Murphy <trevor.m.murphy at gmail.com>
;;      Ulrich Müller <ulm at gentoo.org>
;;      Vasilij Schneidermann <v.schneidermann at gmail.com>
;;      Vegard Øye <vegard_oye at hotmail.com>
;;      Winfred Lu <winfred.lu at gmail.com>
;;      Wolfgang Jenkner <wjenkner at inode.at>
;;      Xiao Hanyu <xiaohanyu1988 at gmail.com>
;;      York Zhao <yzhao at telecor.com>

;; Maintainers: The emacs-nsevil team. <https://github.com/orgs/emacs-nsevil/people>
;;      To get in touch, please use the bug tracker or the
;;      mailing list (see below).
;; Created: 2011-03-01
;; Version: 1.14.0
;; Keywords: emulation, vim
;; URL: https://github.com/emacs-nsevil/nsevil
;;      Repository: https://github.com/emacs-nsevil/nsevil.git
;;      EmacsWiki: http://www.emacswiki.org/emacs/Nsevil
;; Bug tracker: https://github.com/emacs-nsevil/nsevil/issues
;;      If you have bug reports, suggestions or patches, please
;;      create an issue at the bug tracker (open for everyone).
;;      Other discussions (tips, extensions) go to the mailing list.
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;      You don't have to subscribe to post; we usually reply
;;      within a few days and CC our replies back to you.
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

;;; Commentary:

;; Nsevil is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.
;;
;; Nsevil lives in a Git repository. To obtain Nsevil, do
;;
;;      git clone git://github.com/emacs-nsevil/nsevil.git
;;
;; Move Nsevil to ~/.emacs.d/nsevil (or somewhere else in the `load-path').
;; Then add the following lines to ~/.emacs:
;;
;;      (add-to-list 'load-path "~/.emacs.d/nsevil")
;;      (require 'nsevil)
;;      (nsevil-mode 1)
;;
;; Nsevil requires undo-redo (Emacs 28), undo-fu or undo-tree for redo
;; functionality.  Otherwise, Nsevil uses regular Emacs undo.
;;
;; Nsevil requires `goto-last-change' and `goto-last-change-reverse'
;; function for the corresponding motions g; g, as well as the
;; last-change-register `.'. One package providing these functions is
;; goto-chg.el:
;;
;;     http://www.emacswiki.org/emacs/GotoChg
;;
;; Without this package the corresponding motions will raise an error.

;;; Code:

(require 'nsevil-vars)
(require 'nsevil-common)
(require 'nsevil-core)
(require 'nsevil-states)
;; (require 'nsevil-repeat)
;; (require 'nsevil-macros)
;; (require 'nsevil-search)
;; (require 'nsevil-ex)
;; (require 'nsevil-digraphs)
;; (require 'nsevil-types)
;; (require 'nsevil-commands)
;; (require 'nsevil-jumps)
(require 'nsevil-maps)

;; (when nsevil-want-integration
;;   (require 'nsevil-integration))

;; (when nsevil-want-keybinding
;;   (require 'nsevil-keybindings))

;; (run-hooks 'nsevil-after-load-hook)

(provide 'nsevil)

;;; nsevil.el ends here
