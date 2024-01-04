;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar emacs-flymake-eslint--process nil
  "Linter process.
All buffers use the same process.")

(defun emacs-flymake-eslint--kill-process ()
  (when (process-live-p emacs-flymake-eslint--process)
    (kill-process emacs-flymake-eslint--process))

  (let ((proc-buffer
         (when (processp emacs-flymake-eslint--process)
           (process-buffer emacs-flymake-eslint--process))))
    (when proc-buffer (kill-buffer proc-buffer)))

  (setq emacs-flymake-eslint--process nil))

(defun emacs-flymake-eslint--detect-node-cmd ()
  (locate-file "node" exec-path))

(defun emacs-flymake-eslint--sentinel (process event)
  (when (eq 'exit (process-status process))
    (emacs-flymake-eslint--kill-process)))

(defun emacs-flymake-eslint--filter (process str)
  (let* ((obj (json-parse-string str :object-type 'plist))
         (filename (plist-get obj :filename)))
    (message "filename: %s" filename)))

(defun emacs-flymake-eslint--create-process ()
  (let ((node (emacs-flymake-eslint--detect-node-cmd))
        (js-file (expand-file-name
                  "emacs-flymake-eslint.cjs"
                  (file-name-directory (or load-file-name buffer-file-name)))))
    (when node
      (setq emacs-flymake-eslint--process
            (make-process
             :name "emacs-flymake-eslint"
             :connection-type 'pipe
             :noquery t
             :buffer (generate-new-buffer " *emacs-flymake-eslint*")
             :command `("node" ,js-file)
             :sentinel #'emacs-flymake-eslint--sentinel
             :filter #'emacs-flymake-eslint--filter
             )))))

(defun emacs-flymake-eslint--init-process ()
  (emacs-flymake-eslint--kill-process)
  (setq emacs-flymake-eslint--process (emacs-flymake-eslint--create-process)))

(emacs-flymake-eslint--init-process)
(process-live-p emacs-flymake-eslint--process)
(process-send-string emacs-flymake-eslint--process (json-serialize `(:filename ,(buffer-file-name))))
(process-send-eof emacs-flymake-eslint--process)


(provide 'emacs-flymake-eslint)
