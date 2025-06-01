;; -*- coding: utf-8; lexical-binding: t; -*-

(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
       (file (file-truename (file-name-concat dir "../lisp/eslint.el"))))
  (unless (featurep 'eslint) (load file)))

(defun lint-file (file)
  (let ((buffer (find-file-noselect file)))
    (emacs-eslint-lint-file
     file buffer
     (lambda (r) (message "file: %s\nresult: %s" file r))
     (lambda (e) (message "file: %s\nerror: %s" file e))
     )
    nil))

(let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
      (file (file-truename (file-name-concat dir "../test.cjs"))))
  (lint-file file))
