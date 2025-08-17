;;; project-cmake.el --- A cmake backend for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Lucius Martius

;; Author: Lucius Martius <lucius.martius@mailbox.org>
;; Keywords: tools, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/lucius-martius/project-cmake

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds a cmake backend for Emacs' project.el framwork.
;;
;; The added value comes from cmake's reliance on external build directories
;; which breaks `project-compile', as well as some convenience functions for
;; configuring projects and running unit tests.
;;
;; For more information see the README.org on the github page of the project.
;;
;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup project-cmake nil
  "A project.el backend for cmake."
  :version "0.1"
  :group 'tools)

(defcustom project-cmake-cmake-program "cmake"
  "The binary that gets run for cmake.

This can be set to a command, an absolute or relative path.
As a command it requires it to be in $PATH.
As a relative path it will be called relative to the build directory.

You can override this variable in your project's .dir-locals.el."
  :group 'project-cmake
  :type 'string)

;;;###autoload
(put 'project-cmake-cmake-program 'safe-local-variable 'stringp)

(defcustom project-cmake-ctest-program "ctest"
  "The binary that gets run for ctest.

This can be set to a command, an absolute or relative path.
As a command it requires it to be in $PATH.
As a relative path it will be called relative to the build directory.

You can override this variable in your project's .dir-locals.el."
  :group 'project-cmake
  :type 'string)

;;;###autoload
(put 'project-cmake-ctest-program 'safe-local-variable 'stringp)

(defcustom project-cmake-ctest-arguments '("--output-on-failure")
  "A list of command-line arguments passed to the ctest command by default."
  :group 'project-cmake
  :type '(list string))

(defcustom project-cmake-build-directory "build"
  "Determines the build directory for CMake.

This can either be a string, or a function.

When a string is given it will be interpreted either as a relative path under
the source directory, or an absolute path directly to the build directory.
Absolute paths mostly make sense as overrides in `.dir-locals.el'.

You can also set this to a function that takes the source directory as an
argument and returns a relative or absolute path to the build directory.  This
function needs to return the same path for all files in the project and it can
not make use of defmethods that require the project object, i.e `project-name'
etc.

If set via `.dir-locals.el' only paths are accepted as safe, not functions."
  :group 'project-cmake
  :type '(choice string function))

;;;###autoload
(put 'project-cmake-build-directory 'safe-local-variable 'stringp)

(defcustom project-cmake-default-cmake-options '("CMAKE_BUILD_TYPE:STRING=Release"
                                                 "CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON")
  "An initial list of options, value types and values to pass to cmake.

Note that once a project has been configured for the first time any changes to
these values will no longer be taken into account until the variable is unset
manually or the entire project is reconfigured.  See the docstring of
`project-cmake-run-cmake' on how to do that."
  :group 'project-cmake
  :type '(list string))

;;;###autoload
(put 'project-cmake-default-cmake-options 'safe-local-variable 'list-of-strings-p)

(defun project-cmake--get-options-from-cache (project)
  "Read CMakeCache.txt to get a list of options for PROJECT."
  (let* ((default-directory (cdr (assq 'build project))))
    (with-temp-buffer
      (insert-file-contents-literally (expand-file-name "CMakeCache.txt"))
      (goto-char (point-min))
      (let (docstring ret)
        (while (not (eq (point) (point-max)))
          (cond ((looking-at (rx "#")))
                ((looking-at (rx "//" (group (+? anychar)) eol))
                 (setq docstring (concat docstring (match-string 1))))
                ((looking-at (rx (group (+? (or alnum "_" "-")))
                                 ":" (group (or "BOOL" "PATH" "FILEPATH" "STRING"
                                                "INTERNAL" "STATIC" "UNINITIALIZED"))
                                 "=" (group (*? anychar) eol)))
                 (let ((name (match-string 1))
                       (type (match-string 2))
                       (value (match-string 3)))
                   (when (member type '("BOOL" "PATH" "FILEPATH" "STRING"))
                     (setf (alist-get name ret nil nil 'equal)
                           (list (cons 'type type)
                                 (cons 'docstring docstring)
                                 (cons 'value value)))))
                 (setq docstring "")))
          (forward-line))
        ret))))

(defun project-cmake--get-tests-from-ctest (project)
  "Retrieves a list of test names in PROJECT."
  (declare-function json-read "json")
  (require 'json)
  (let ((default-directory (cdr (assq 'build project)))
        (source (cdr (assq 'source project))))
    (with-temp-buffer
      (let ((ctest-program (or (project--value-in-dir 'project-cmake-ctest-program
                                                      (file-name-as-directory source))
                               project-cmake-ctest-program)))
        (process-file ctest-program nil (current-buffer) nil "--show-only=json-v1"))
      (goto-char (point-min))
      (let* ((json (json-read)))
        (mapcar (lambda (test) (cdr (assq 'name test)))
                (cdr (assq 'tests json)))))))

(defun project-cmake--run-cmake-with-options (project options &optional fresh)
  "Run cmake for PROJECT with the given OPTIONS.

OPTIONS is a list of cons cells where the car is the string of the cmake option
and the cdr is a string of the option's value.

If FRESH is non-nil, run cmake with the '--fresh' option, to reconfigure from
scratch."
  (let* ((default-directory (cdr (assq 'build project)))
         (source (cdr (assq 'source project)))
         (cmake-program (or (project--value-in-dir 'project-cmake-cmake-program
                                                   (file-name-as-directory source))
                            project-cmake-cmake-program))
         (compile-command (apply 'concat cmake-program " " (if fresh "--fresh ")
                                 `(,@(mapcar (lambda (option)
                                               (concat "-D" option " "))
                                             options)
                                   ,(cdr (assq 'source project)))))
         (compilation-buffer-name-function (lambda (_mode) "" "*cmake*")))
    (unless (executable-find "cmake" t)
      (error "Can not locate cmake executable"))
    (make-directory default-directory t)
    (compile compile-command)))

(defun project-cmake--run-ctest (&optional pattern)
  "Run ctest in the project's build directory.

PATTERN is a regex pattern as accepted by the '-R' option of ctest (see the man
page).  Unless PATTERN is given, all tests are run (i.e. ctest is run without
arguments)."
  (let* ((project (project-current))
         (default-directory (cdr (assq 'build project)))
         (source (cdr (assq 'source project)))
         (ctest-program (or (project--value-in-dir 'project-cmake-ctest-program
                                                   (file-name-as-directory source))
                            project-cmake-ctest-program))
         (compile-command (apply 'concat ctest-program
                                 (nconc (mapcar (lambda (arg)
                                                  (concat " " arg))
                                                project-cmake-ctest-arguments)
                                        (if pattern (list " -R '" pattern "'")))))
         (compilation-buffer-name-function (lambda (_mode) "" "*ctest*")))
    (compile compile-command)))

(defun project-cmake--get-build-path (source)
  "Return a full path for the expected build directory for SOURCE."
  (let* ((source (file-name-as-directory source))
         (dir (let ((value (project--value-in-dir 'project-cmake-build-directory source)))
                (cond ((functionp value)
                       (funcall value source))
                      ((stringp value)
                       value)
                      (t (error "Invalid value for project-cmake-build-directory"))))))
    (if (file-name-absolute-p dir) dir
      (expand-file-name dir source))))

(defun project-cmake--cmake-project-p (_symbol buffer)
  "Return non-nil if BUFFER is part of a cmake project."
  (with-current-buffer buffer
    (eq (car (project-current)) 'cmake)))

(defun project-cmake--read-cmake-option (&optional project)
  "Read a cmake option of the given PROJECT from the user."
    (let* ((project (or project (project-current)))
           (options (project-cmake--get-options-from-cache project))
           (widths (let ((name 0)  (type 0) (value nil) (docstr nil))
                     (dolist (option options)
                       (setq name (max name (length (car option))))
                       (setq type (max type (length (cdr (assq 'type option)))))
                       (push (length (cdr (assq 'value option))) value)
                       (push (length (cdr (assq 'docstring option))) docstr))
                     (list name type
                           (nth (/ (* (length value) 95) 100) (sort value))
                           (nth (/ (* (length docstr) 75) 100) (sort docstr))))))
      (cl-labels ((pad-or-trunc (str width)
                    (cond ((= (length str) width) str)
                          ((< (length str) width)
                           (concat str (make-string (- width (length str)) ?\s)))
                          (t (substring str 0 width))))
                  (annotation-function (name)
                    (let ((option (cdr (assoc name options))))
                      (let ((value (cdr (assq 'value option)))
                            (type (cdr (assq 'type option)))
                            (docstring (cdr (assq 'docstring option))))
                        (concat (make-string (- (car widths) (length name)) ?\s)
                                (propertize " : " 'display '(space-width 0.3))
                                (propertize (string-pad type (nth 1 widths))
                                            'face 'font-lock-type-face)
                                (propertize " = " 'display '(space-width 0.3))
                                (propertize (pad-or-trunc value (nth 2 widths))
                                            'face (if (equal type "BOOL")
                                                      'font-lock-number-face
                                                    'font-lock-string-face))
                                (propertize " // " 'display '(space-width 0.3)
                                            'face 'font-lock-doc-face)
                                (propertize (pad-or-trunc docstring (nth 3 widths))
                                            'face 'font-lock-doc-face)))))
                  (collection-function (str pred act)
                    (if (eq act 'metadata)
                        `(metadata ,(cons 'annotation-function #'annotation-function)
                                   (category . cmake-option))
                      (complete-with-action act options str pred))))
        (let* ((completion-ignore-case t))
          (assoc (completing-read "Select Option: " #'collection-function) options)))))

(defun project-cmake--read-cmake-test (&optional project)
  "Read the name of a test for the current or given PROJECT."
  (let* ((project (or project (project-current)))
         (tests (project-cmake--get-tests-from-ctest project))
         (completion-ignore-case t))
    (completing-read "Select Test: " tests)))

;;;###autoload
(defun project-cmake-find-root (dir)
  "Find the root of a potential Cmake project under DIR."
  (cl-labels ((recursive-locate (base)
                (when (setq base (locate-dominating-file base "CMakeLists.txt"))
                  (or (recursive-locate (file-name-parent-directory base)) base)))
              (locate-from-build (build)
                (with-temp-buffer
                  (insert-file-contents-literally (expand-file-name "CMakeCache.txt" build))
                  (goto-char (point-min))
                  (unless (search-forward-regexp (rx bol "CMAKE_HOME_DIRECTORY:INTERNAL="
                                                     (group (*? anychar)) eol))
                    (error "Could not find variable CMAKE_HOME_DIRECTORY in CMakeCache.txt"))
                  (let ((source (match-string 1)))
                    (unless (file-name-absolute-p source)
                      (error "CMAKE_HOME_DIRECTORY not an absolute path"))
                    (if-let ((remote (file-remote-p build)))
                        (expand-file-name (file-relative-name source "/") remote)
                      source)))))
    (let* ((dir (file-truename dir))
           (build (locate-dominating-file dir "CMakeCache.txt")))
      (when-let* ((source (or (and build (locate-from-build build))
                              (recursive-locate dir)))
                  (build (let ((configured-build (project-cmake--get-build-path source)))
                           (when (and build (not (file-equal-p build configured-build)))
                             (error "CMake build dir mismatch: %s found and %s configured"
                                    (file-truename build) configured-build))
                           (when (not (equal (file-remote-p source)
                                             (file-remote-p configured-build)))
                             (error "Source (%s) and build (%s) dirs not on the same system"
                                    source configured-build))
                           configured-build)))
        (list 'cmake
              (cons 'source (file-truename source))
              (cons 'build (file-truename build)))))))

(cl-defmethod project-root ((project (head cmake)))
  "Get the project root of the given cmake PROJECT."
  (cdr (assq 'source project)))

(cl-defmethod project-external-roots ((project (head cmake)))
  "Return the build directory of a cmake PROJECT."
  (let ((source (cdr (assq 'source project)))
        (build (cdr (assq 'build project))))
    (unless (string-prefix-p source build)
      (list build))))

(cl-defmethod project-ignores ((project (head cmake)) dir)
  "Return the ignored files in DIR of a cmake PROJECT."
  (when-let* ((source (cdr (assq 'source project)))
              (build (cdr (assq 'build project)))
              (relative-build (file-relative-name build dir))
              (cmake-ignore (unless (string-match-p (rx bos ?. (? ?.) ?/) relative-build)
                              (list relative-build)))
              (vc-prj (project-try-vc source))
              (other-ignore (if vc-prj (project-ignores vc-prj dir)
                              (project-ignores (cons 'transient source)))))
    (append other-ignore cmake-ignore)))

(defun project-cmake-run-cmake (&optional reset)
  "Run cmake for the current project.
when RESET is non-nil, run cmake with '--fresh' to configure
the build from scratch."
  (interactive current-prefix-arg)
  (if-let* ((project (project-current))
            (build (and (eq (car project) 'cmake)
                        (cdr (assq 'build project)))))
      (let* ((cache-exists (file-readable-p (expand-file-name "CMakeCache.txt" build)))
             (options (when (or reset (not cache-exists))
                        project-cmake-default-cmake-options)))
        (project-cmake--run-cmake-with-options project options reset))
    (error "This buffer is not part of a cmake project")))

(put #'project-cmake-run-cmake 'completion-predicate #'project-cmake--cmake-project-p)

(defun project-cmake-test (&optional test)
  "Run all test units of the current project.
If the prefix arg is given, let the user select a TEST to run interactively.
Since the selected string is passed to ´ctest -R´ you can also specify a regex
that matches a set of tests."
  (interactive (when current-prefix-arg
                 (list (project-cmake--read-cmake-test))))
  (project-cmake--run-ctest test))

(put #'project-cmake-test 'completion-predicate #'project-cmake--cmake-project-p)

(defun project-cmake-set-option (option)
  "Set the cmake cache OPTION to the given value.
OPTION here is a cons cell in the form (name . value)."
  (interactive (let* ((alist (project-cmake--read-cmake-option))
                      (name (car alist))
                      (type (cdr (assq 'type alist)))
                      (old-value (cdr (assq 'value alist)))
                      (value (pcase type
                               ("BOOL" (when (y-or-n-p (format "Toggle %s (Currently %s)?"
                                                               name old-value))
                                         (pcase old-value
                                           ("ON" "OFF")
                                           ("OFF" "ON")
                                           ("YES" "NO")
                                           ("NO" "YES")
                                           ("TRUE" "FALSE")
                                           ("FALSE" "TRUE"))))
                               ("PATH" (read-directory-name "Set path: "))
                               ("FILEPATH" (read-file-name "Set file-name: "))
                               ("STRING" (read-string "Set string value: ")))))
                 (list (concat name ":" type "=" value))))
  (project-cmake--run-cmake-with-options (project-current) (list option)))

(put #'project-cmake-set-option 'completion-predicate #'project-cmake--cmake-project-p)

(define-advice project-compile (:around (orig-fn) cmake-build)
  "Trick `project-compile' to run `compile' in the build directory."
  (cl-letf* ((real-project-root (symbol-function 'project-root))
             ((symbol-function 'project-root)
              (lambda (project)
                (if (eq (car project) 'cmake)
                    (let ((build (cdr (assq 'build project))))
                      (unless (file-readable-p (expand-file-name "CMakeCache.txt" build))
                        (error "No CMakeCache.txt: Run `project-cmake-run-cmake' first"))
                      build)
                  (funcall real-project-root project)))))
    (funcall orig-fn)))

(provide 'project-cmake)
;;; project-cmake.el ends here
