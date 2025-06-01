;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'eslint)
(require 'flymake)

(defun eslint-flymake-parse-message (msg buffer)
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

(defun eslint-flymake-backend (report-fn &rest _ignore)
  (let ((filepath (buffer-file-name))
        (buffer (current-buffer))
        diags)
    (when filepath
      (eslint-lint-file
       filepath buffer
       (lambda (result)
         (setq diags (mapcar
                      (lambda (message)
                        (eslint-flymake-parse-message message buffer))
                      result))
         (funcall report-fn diags))
       ))))

(defun eslint-flymake-enable ()
  "Enable `eslint-flymake' in current buffer."
  (interactive)
  (when (eslint-detect-node-cmd)
    (unless (bound-and-true-p flymake-mode) (flymake-mode 1))
    (add-hook 'flymake-diagnostic-functions #'eslint-flymake-backend nil t)))

(provide 'eslint-flymake)
