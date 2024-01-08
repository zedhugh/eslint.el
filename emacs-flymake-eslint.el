;; -*- coding: utf-8; lexical-binding: t; -*-

(defconst emacs-flymake-eslint--home
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory which `emacs-flymake-eslint' installed.")

(defvar emacs-flymake-eslint--process nil
  "Linter process.
All buffers use the same process.")

(defvar emacs-flymake-eslint--report-fn-map (make-hash-table :test #'equal)
  "File path and flymake report function map.")

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
    (emacs-flymake-eslint--kill-process))
  (when (hash-table-p emacs-flymake-eslint--report-fn-map)
    (clrhash emacs-flymake-eslint--report-fn-map)))

(defun emacs-flymake-eslint--parse-message (msg buffer)
  (let* ((ruleId (plist-get msg :ruleId))
         (severity (plist-get msg :severity))
         (message (plist-get msg :message))
         (line (plist-get msg :line))
         (column (plist-get msg :column))
         (endLine (plist-get msg :endLine))
         (endColumn (plist-get msg :endColumn))
         (start-region (flymake-diag-region buffer line column))
         (end-region (when (numberp endLine)
                       (flymake-diag-region buffer endLine endColumn)))
         begin end msg-text type type-symbol)
    (if end-region
        (setq begin (car start-region)
              end (car end-region))
      (setq begin (car start-region)
            end (cdr start-region)))
    (if (equal severity 1)
        (setq type "warning"
              type-symbol :warning)
      (setq type "error"
            type-symbol :error))
    (setq msg-text (format "%s: %s [%s]" type message ruleId))
    (flymake-make-diagnostic buffer begin end type-symbol msg-text
                             (list :rule-name ruleId))))

(defun emacs-flymake-eslint--filter (process str)
  (let* ((obj (json-parse-string str :object-type 'plist))
         (filepath  (plist-get obj :filename))
         (buffer (find-buffer-visiting filepath))
         cost messages report-fn diags)
    (when (and filepath buffer)
      (setq cost        (plist-get obj :cost)
            messages    (plist-get obj :messages)
            report-fn   (gethash filepath emacs-flymake-eslint--report-fn-map))

      (setq diags (mapcar (lambda (msg)
                            (emacs-flymake-eslint--parse-message msg buffer))
                          messages))
      (funcall report-fn diags)
      (remhash filepath emacs-flymake-eslint--report-fn-map))))

(defun emacs-flymake-eslint--create-process ()
  (let ((node (emacs-flymake-eslint--detect-node-cmd))
        (js-file (expand-file-name
                  "emacs-flymake-eslint.cjs"
                  emacs-flymake-eslint--home)))
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

(defun emacs-flymake-eslint--get-process ()
  (unless (process-live-p emacs-flymake-eslint--process)
    (emacs-flymake-eslint--init-process))
  emacs-flymake-eslint--process)

(defun emacs-flymake-eslint-lint-file (filepath)
  (let ((process (emacs-flymake-eslint--get-process)))
    (when (process-live-p process)
      (process-send-string process (json-serialize `(:filename ,filepath))))))

(defun emacs-flymake-eslint--checker (report-fn &rest _ignore)
  (let ((filepath (buffer-file-name)))
    (when filepath
      (emacs-flymake-eslint-lint-file filepath)
      (puthash filepath report-fn emacs-flymake-eslint--report-fn-map))))

(defun emacs-flymake-eslint-enable ()
  (when (emacs-flymake-eslint--detect-node-cmd)
    (unless (bound-and-true-p flymake-mode) (flymake-mode 1))
    (add-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker nil t)))


(provide 'emacs-flymake-eslint)
