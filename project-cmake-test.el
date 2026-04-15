;;; project-cmake-test.el --- Test suite for project-cmake.el -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2026 Lucius Martius

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
;; Run tests to confirm everything is working.

;;; Code:

(require 'ert)
(require 'project-cmake)

(defconst load-file-dir (file-name-directory load-file-name)
  "The directory where this file is located.")

(defun print-stdout (string &rest args)
  "Format STRING with ARGS and print to stdout and flush."
  (princ (apply #'format string args))
  (flush-standard-output))

(defun wait-for-process (proc &optional timeout)
  "Wait for process PROC to exit.
If a TIMEOUT in seconds is given as an optional argument, if the process
does not exit before the end of that timeout, it will be killed."
  (let ((start (current-time)))
    (while (not (eq (process-status proc) 'exit))
      (if (and timeout
               (time-less-p (time-add start (seconds-to-time timeout)) (current-time)))
          (progn
            (kill-process proc)
            (error "Process killed after timeout"))
        (sleep-for 0.01)))))

(defun eval-in-subprocess (expr &optional timeout)
  "Eval EXPR in subprocess and return its output evaluated as a s-expression.
The process will be killed if it does not return within TIMEOUT (in seconds).
If timeout is not given, the default value of 5s will be used."
  (with-temp-buffer
    (let* ((proc-name (concat "test" (number-to-string (random 10000))))
           (proc (start-process proc-name (current-buffer) "emacs" "-Q"
                                (concat "--fg-daemon=" proc-name)
                                "-L" load-file-dir "--eval"
                                (prin1-to-string
                                 `(progn
                                    (require 'project-cmake)
                                    (defvar inhibit-kill-emacs nil)
                                    (setq standard-output
                                          #'external-debugging-output
                                          inhibit-message t)
                                    ,expr
                                    (run-with-timer 0.05 0.05
                                                    (lambda ()
                                                      (unless inhibit-kill-emacs
                                                        (kill-emacs)))))))))
      (wait-for-process proc (or timeout 5))
      (goto-char (point-min))
      (re-search-forward (rx "Starting Emacs daemon."
                             (group (*? (or any "\n")))
                             "Process test"))
      (let ((expr (string-trim (match-string 1))))
        (unless (string-empty-p expr)
          (read expr))))))

(ert-deftest project-cmake-test-current-project-simple ()
  "Test whether `project-current' correctly detects the project directories."
  (with-temp-buffer
    (add-hook 'project-find-functions #'project-cmake-find-root -1 t)
    (dolist (project-cmake-build-directory (list "build"
                                                 (make-temp-file "build" t)))
      (let* ((inhibit-message t)
             (expected-source (expand-file-name "test/simple"))
             (expected-build (expand-file-name project-cmake-build-directory
                                               expected-source))
             (default-directory expected-source))
        (unwind-protect
            (progn
              (should (zerop (call-process "cmake" nil nil nil
                                           "-B" expected-build
                                           "-S" default-directory)))
              (dolist (default-directory (list default-directory
                                               (expand-file-name "subdir")
                                               (expand-file-name project-cmake-build-directory)))
                (let* ((project (project-current)))
                  (should (eq 'cmake (car project)))
                  (let ((source (should (cdr (assq 'source project))))
                        (build (should (cdr (assq 'build project)))))
                    (should (file-name-absolute-p source))
                    (should (file-equal-p source expected-source))
                    (should (file-name-absolute-p build))
                    (should (string= build expected-build))))))
          (delete-directory expected-build t))))))

(ert-deftest project-cmake-test-default-cmake-options ()
  "Test whether global cmake options are always applied correctly."
  (let ((project-cmake-default-cmake-options '("CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON"
                                               "CMAKE_CXX_FLAGS:STRING=\"-O2\""))
        (default-directory (expand-file-name "test/simple")))
    (unwind-protect
        (cl-letf (((symbol-function #'message) (symbol-function #'ignore)))
          (add-hook 'project-find-functions #'project-cmake-find-root -1)
          (let* ((proc (get-buffer-process (project-cmake-run-cmake t))))
            (wait-for-process proc))
          (let* ((options (project-cmake--get-options-from-cache (project-current)))
                 (cecc (alist-get "CMAKE_EXPORT_COMPILE_COMMANDS" options nil nil 'equal))
                 (ccf (alist-get "CMAKE_CXX_FLAGS" options nil nil 'equal)))
            (should cecc)
            (should (string-equal (alist-get 'value cecc) "ON"))
            (should ccf)
            (should (string-equal (alist-get 'value ccf) "-O2"))))
      (delete-directory "build" t)
      (remove-hook 'project-find-functions #'project-cmake-find-root))))

(ert-deftest project-cmake-test-set-option ()
  "Test `project-cmake-set-option' correctly setting options."
  (with-temp-buffer
    (let ((project-cmake-build-directory (make-temp-file "build" t))
          (default-directory (expand-file-name "test/simple")))
      (unwind-protect
          (cl-letf (((symbol-function #'message) (symbol-function #'ignore)))
            (add-hook 'project-find-functions #'project-cmake-find-root -1)
            (let* ((proc (get-buffer-process (project-cmake-run-cmake))))
              (wait-for-process proc))
            (let* ((options (project-cmake--get-options-from-cache (project-current)))
                   (cecc (alist-get "CMAKE_EXPORT_COMPILE_COMMANDS" options nil nil 'equal)))
              (should cecc)
              (should (string-empty-p (alist-get 'value cecc))))
            (let* ((buf (project-cmake-set-option "CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=ON"))
                   (proc (get-buffer-process buf)))
              (wait-for-process proc))
            (let* ((options (project-cmake--get-options-from-cache (project-current)))
                   (cecc (alist-get "CMAKE_EXPORT_COMPILE_COMMANDS" options nil nil 'equal)))
              (should cecc)
              (should (string-equal (alist-get 'value cecc) "ON"))))
        (delete-directory project-cmake-build-directory t)
        (remove-hook 'project-find-functions #'project-cmake-find-root)))))

(ert-deftest project-cmake-test-project-compile ()
  "Test whether `project-compile' chooses the right build directory."
  (let* ((default-directory (expand-file-name "test/simple")))
    (delete-directory "build" t)
    (unwind-protect
        (progn
          (should (eval-in-subprocess
                   `(cl-letf (((symbol-function #'message) (symbol-function #'ignore)))
                      (setq inhibit-kill-emacs t)
                      (add-hook 'project-find-functions #'project-cmake-find-root -1)
                      (let* ((buf (project-compile)))
                        (with-current-buffer buf
                          (add-hook 'compilation-finish-functions
                                    (lambda (_ result)
                                      (if (string= result "finished\n")
                                          (let ((buf (get-buffer "*compilation*")))
                                            (with-current-buffer buf
                                              (add-hook 'compilation-finish-functions
                                                        (lambda (_ result)
                                                          (prin1 (string= result "finished\n"))
                                                          (setq inhibit-kill-emacs nil))
                                                        100 t)))
                                        (prin1 nil)
                                        (setq inhibit-kill-emacs nil)))
                                    100 t))))))
          (should (file-exists-p (expand-file-name "build/CMakeCache.txt")))
          (should (file-executable-p (expand-file-name "build/test"))))
      (delete-directory "build" t))))

(provide 'project-cmake-test)
;;; project-cmake-test.el ends here.
