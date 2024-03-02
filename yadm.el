;;; yadm.el --- Emacs integration for yadm dotfiles -*- lexical-binding: t -*-

;; Copyright (C) 2024 Sean Farley

;; Author: Sean Farley
;; URL: https://github.com/seanfarley/yadm.el
;; Version: 0.1
;; Created: 2024-02-22
;; Package-Requires: ((emacs "26.1") (magit "3.0") (projectile "2.0"))
;; Keywords: extensions dotfiles yadm projectile magit

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates yadm, a git-based dotfiles managers, as best it can
;; into the Emacs ecosystem. The idea is to advise the minimum number of
;; functions to tweak and replace the usage of git with yadm.
;;
;; We achieve this by determining the working directory (usually
;; `defualt-directory') and comparing that with the home directory.

;;; Code:
(require 'cl-lib)
(require 'magit)
(require 'projectile)

(defvar yadm-projectile-directory-cache nil
  "A list used to cache yadm directories.

This is only used to speed up testing for a file's membership
inside a yadm project.")

(defcustom yadm-executable
  (or (executable-find "yadm")
      "yadm")
  "The yadm executable."
  :package-version '(yadm . "0.1")
  :group 'yadm
  :type 'string)

(defcustom yadm-files-command "ls-files -zc --exclude-standard"
  "Command used by yadm to get the files in a project."
  :group 'yadm
  :type 'string
  :package-version '(yadm . "0.1"))

;;;###autoload
(cl-defsubst yadm-files-command ()
  "Convenience method for creating the ls-files command."
  (concat yadm-executable " " yadm-files-command))



;;;###autoload
(defun yadm-projectile-root-marked (dir)
  "Identify a yadm root in DIR by search for `.local/share/yadm/repo.git`."
  ;; TODO here is where we will get a list of directories from yadm and test for
  ;; membership here
  ;; TODO explain why it's the best we can do
  ;; TODO add ignored directories variable (list)

  ;; REVIEW should we include parent-children directories? e.g. yadm could only
  ;; have ~/.local/share/foo/ but the user expects ~/.local or ~/.local/share
  ;; to be there
  (unless yadm-projectile-directory-cache
    (let ((default-directory (expand-file-name "~")))
      (setq yadm-projectile-directory-cache
            (delete-dups
             (mapcar
              (lambda (filename)
                (file-name-directory (expand-file-name filename)))
              (split-string
               (shell-command-to-string (yadm-files-command)) "\0" t))))))
  ;; test equality with `file-equal-p' so that we don't have to worry about
  ;; trailing slashes
  (when (cl-member dir yadm-projectile-directory-cache :test #'file-equal-p)
    (expand-file-name "~/")))

;;;###autoload
(defun yadm-projectile-project-vcs (orig-fn &optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
the variable `projectile-project-root'.

Run ORIG-FN if yadm isn't found."
  (or project-root (setq project-root (projectile-acquire-root)))
  (cond
   ;; first we check for a VCS marker in the project root itself
   ((projectile-file-exists-p
     (expand-file-name ".local/share/yadm/repo.git" project-root)) 'yadm)
   (t (funcall orig-fn project-root))))

;;;###autoload
(defun yadm-projectile-get-ext-command (orig-fn vcs)
  "Determine which external command to invoke based on the project's VCS.
Fallback to a generic command when not in a VCS-controlled project.

Run ORIG-FN if VCS isn't yadm."
  (pcase vcs
    ('yadm (yadm-files-command))
    (_ (funcall orig-fn vcs))))



;;;###autoload
(defun yadm-magit-process-git (orig-fn destination &rest args)
  "Call yadm synchronously in a separate process, returning its exit code.

If `default-directory' is inside a yadm repo then manually call
`magit-process-file', otherwise call ORIG-FN with DESTINATION and
ARGS.

Note: we can't override the function `magit-git-executable' since
it is a defined using `defsubst' which is put directly into byte
code for performance."
  (if (yadm-projectile-root-marked default-directory)
      (apply #'magit-process-file
             yadm-executable
             nil destination nil
             (magit-process-git-arguments args))
    (apply orig-fn destination args)))

;;;###autoload
(defun yadm-magit-call-process (orig-fn program &rest args)
  "Call `magit-call-process' with yadm as PROGRAM or git.

The argument PROGRAM is changed depending on whether
`default-directory' is the user's home directory. If not, then
pass through to ORIG-FN with ARGS."
  (let ((program (if (yadm-projectile-root-marked default-directory)
                     yadm-executable
                   program)))
    (apply orig-fn program args)))

;;;###autoload
(defun yadm-magit-start-process (orig-fn program &optional input &rest args)
  "Call `magit-start-process' with yadm as PROGRAM or git.

The argument PROGRAM is changed depending on whether
`default-directory' is the user's home directory. If not, then
pass through to ORIG-FN with INPUT and ARGS."
  (let ((program (if (yadm-projectile-root-marked default-directory)
                     yadm-executable
                   program)))
    (apply orig-fn program input args)))

;;;###autoload
(defun yadm-magit-toplevel (orig-fn &optional directory)
  "Return the absolute path to the top level of the yadm repository.

If DIRECTORY is inside a yadm repo then return the home
directory, otherwise fallback to calling ORIG-FN."
  (if-let ((yadm-dir (yadm-projectile-root-marked default-directory)))
      yadm-dir
    (funcall orig-fn directory)))

(defun yadm--magit-assert-usable-git (orig-fn)
  "Wrap `magit--assert-usable-git' to allow use of yadm.

Test `default-directory' to determine whether to use yadm,
otherwise fallback to calling ORIG-FN."
  (unless (yadm-projectile-root-marked default-directory)
    (apply orig-fn nil)))



;;;###autoload
(cl-defsubst yadm-register-projectile ()
  "Register various projectile related settings."

  ;; advise functions which is a game of whack-a-mole
  (advice-add #'projectile-project-vcs
              :around #'yadm-projectile-project-vcs)
  (advice-add #'projectile-get-ext-command
              :around #'yadm-projectile-get-ext-command)

  (add-to-list 'projectile-project-root-functions
               #'yadm-projectile-root-marked))

;;;###autoload
(cl-defsubst yadm-register-magit ()
  "Register various magit related settings."

  ;; advise functions which is a game of whack-a-mole
  (advice-add #'magit-call-process
              :around #'yadm-magit-call-process)
  (advice-add #'magit-process-git
              :around #'yadm-magit-process-git)
  (advice-add #'magit-start-process
              :around #'yadm-magit-start-process)
  (advice-add #'magit-toplevel
              :around #'yadm-magit-toplevel))

;;;###autoload
(cl-defsubst yadm-register ()
  "Convenience method of registering everything."
  (yadm-register-projectile)
  (yadm-register-magit))

;;;###autoload (with-eval-after-load 'projectile (yadm-register-projectile))
;;;###autoload (with-eval-after-load 'magit (yadm-register-magit))

(provide 'yadm)
;;; yadm.el ends here
