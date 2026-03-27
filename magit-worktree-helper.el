;;; magit-worktree-helper.el --- Copy untracked files to new worktrees -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ngoc

;; Author: Ngoc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0.0"))
;; Keywords: vc tools
;; URL: https://github.com/ngoc/magit-worktree-helper

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically copy untracked files (e.g. .env, IDE configs) to new
;; git worktrees created via Magit.
;;
;; Enable `magit-worktree-helper-mode' globally, then configure which
;; files to copy.  Include and exclude lists work together:
;;
;;   - `magit-worktree-helper-copy-all': when non-nil, all untracked
;;     files are candidates.
;;   - `magit-worktree-helper-include': patterns for files to copy
;;     (used when `copy-all' is nil).
;;   - `magit-worktree-helper-exclude': patterns for files to never
;;     copy (always applied last, overrides include).
;;
;; Each pattern matches against the file's relative path via prefix
;; match, or against its base name via exact match.  All options can
;; be overridden per-project via .dir-locals.el.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'seq)

;;; Options

(defgroup magit-worktree-helper nil
  "Copy untracked files to new git worktrees."
  :group 'magit
  :prefix "magit-worktree-helper-")

(defcustom magit-worktree-helper-copy-all t
  "When non-nil, all untracked files are candidates for copying.
When nil, only files matching `magit-worktree-helper-include' are copied.
In both cases, `magit-worktree-helper-exclude' is applied last."
  :type 'boolean)

(defcustom magit-worktree-helper-include '(".env")
  "Patterns of files to copy when `magit-worktree-helper-copy-all' is nil.
Each pattern matches if the file's relative path starts with it,
or the file's base name equals it."
  :type '(repeat string))

(defcustom magit-worktree-helper-exclude '("node_modules" ".venv")
  "Patterns of files to never copy.  Applied after include."
  :type '(repeat string))

(defcustom magit-worktree-helper-include-ignored t
  "When non-nil, gitignored files are also candidates for copying.
Files like .env are typically gitignored but often need to be
copied to new worktrees."
  :type 'boolean)

(defcustom magit-worktree-helper-confirm nil
  "Whether to prompt before copying files to a new worktree."
  :type 'boolean)

(put 'magit-worktree-helper-copy-all 'safe-local-variable #'booleanp)
(put 'magit-worktree-helper-include 'safe-local-variable #'listp)
(put 'magit-worktree-helper-exclude 'safe-local-variable #'listp)
(put 'magit-worktree-helper-include-ignored 'safe-local-variable #'booleanp)
(put 'magit-worktree-helper-confirm 'safe-local-variable #'booleanp)

;;; Internal

(defun magit-worktree-helper--match-p (file patterns)
  "Return non-nil if FILE matches any entry in PATTERNS.
FILE is a path relative to the repo root."
  (cl-some (lambda (pat)
             (or (string-prefix-p pat file)
                 (string= pat (file-name-nondirectory file))))
           patterns))

(defun magit-worktree-helper--select-files (files)
  "Return FILES filtered by include/exclude settings."
  (let ((candidates (if magit-worktree-helper-copy-all
                        files
                      (seq-filter
                       (lambda (f)
                         (magit-worktree-helper--match-p
                          f magit-worktree-helper-include))
                       files))))
    (if magit-worktree-helper-exclude
        (seq-remove
         (lambda (f)
           (magit-worktree-helper--match-p
            f magit-worktree-helper-exclude))
         candidates)
      candidates)))

(defun magit-worktree-helper--copy-files (source-root target-root files)
  "Copy FILES from SOURCE-ROOT to TARGET-ROOT preserving structure."
  (dolist (file files)
    (let* ((src (expand-file-name file source-root))
           (dst (expand-file-name file target-root))
           (dst-dir (file-name-directory dst)))
      (when (file-exists-p src)
        (make-directory dst-dir t)
        (if (file-directory-p src)
            (copy-directory src dst nil t t)
          (copy-file src dst t))))))

(defun magit-worktree-helper--do-copy (source-root target-root)
  "Copy selected untracked files from SOURCE-ROOT to TARGET-ROOT."
  (let* ((default-directory source-root)
         (untracked (magit-untracked-files magit-worktree-helper-include-ignored))
         (selected (magit-worktree-helper--select-files untracked)))
    (when (and selected
               (or (not magit-worktree-helper-confirm)
                   (y-or-n-p (format "Copy %d untracked file(s) to new worktree? "
                                     (length selected)))))
      (magit-worktree-helper--copy-files source-root target-root selected)
      (message "magit-worktree-helper: Copied %d file(s) to %s"
               (length selected)
               (abbreviate-file-name target-root)))))

(defun magit-worktree-helper--around-create (orig-fn &rest args)
  "Advice around worktree creation to copy untracked files.
Captures source toplevel before ORIG-FN changes `default-directory'."
  (let ((source-root (magit-toplevel)))
    (apply orig-fn args)
    (let ((target-root (expand-file-name (car args))))
      (when (and source-root (file-directory-p target-root))
        (magit-worktree-helper--do-copy source-root target-root)))))

;;; Mode

;;;###autoload
(define-minor-mode magit-worktree-helper-mode
  "Automatically copy untracked files to new git worktrees."
  :global t
  :group 'magit-worktree-helper
  (if magit-worktree-helper-mode
      (progn
        (advice-add 'magit-worktree-checkout :around
                    #'magit-worktree-helper--around-create)
        (advice-add 'magit-worktree-branch :around
                    #'magit-worktree-helper--around-create))
    (advice-remove 'magit-worktree-checkout
                   #'magit-worktree-helper--around-create)
    (advice-remove 'magit-worktree-branch
                   #'magit-worktree-helper--around-create)))

(provide 'magit-worktree-helper)
;;; magit-worktree-helper.el ends here
