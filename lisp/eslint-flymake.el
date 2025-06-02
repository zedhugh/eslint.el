;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'eslint)
(require 'flymake)
(require 'cl-lib)

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

(defvar-local eslint-flymake--versioned-identifier 0
  "eslint flymake buffer content version. Bumped on `after-change-functions'")
(defvar-local eslint-flymake--diagnostics nil
  "A cons (DIAGNOSTICS . VERSION) for current-buffer.
DIAGNOSTICS is a list of Flymake diagnostics objects.  VERSION is the
`eslint-flymake--versioned-identifier'.")
(defvar-local eslint-flymake--current-report-fn nil
  "Current flymake report function for this buffer.")

(defun eslint-flymake--after-change (beg end pre-change-length)
  (cl-incf eslint-flymake--versioned-identifier))

(defun eslint-flymake-backend (report-fn &rest _ignore)
  (let ((filepath (buffer-file-name))
        (buffer (current-buffer))
        (version eslint-flymake--versioned-identifier)
        (diag-version (cdr eslint-flymake--diagnostics))
        code)
    (if filepath
        (if (and (numberp diag-version) (= diag-version version))
            (funcall report-fn (car eslint-flymake--diagnostics))
          (setq eslint-flymake--current-report-fn report-fn)
          (save-restriction (widen) (setq code (buffer-string)))
          (eslint-lint-file
           filepath code
           (lambda (result) (eslint-flymake--success result buffer version)))
          )
      (funcall report-fn nil))))

(defun eslint-flymake--success (result buffer version)
  (with-current-buffer buffer
    (let ((diags (mapcar (lambda (message)
                           (eslint-flymake-parse-message message (current-buffer)))
                         result)))
      (if (< version eslint-flymake--versioned-identifier)
          (eslint-flymake--report (car eslint-flymake--diagnostics)
                                  (cdr eslint-flymake--diagnostics))
        (eslint-flymake--report diags version)))))

(defun eslint-flymake--report (diags version)
  (save-restriction
    (widen)
    (if (or (null version) (= version eslint-flymake--versioned-identifier))
        (funcall eslint-flymake--current-report-fn diags
                 :region (cons (point-min) (point-max)))
      (funcall eslint-flymake--current-report-fn nil
               :region (cons (point-min) (point-max))))
    (setq eslint-flymake--diagnostics (cons diags version))))

(defun eslint-flymake--kill-buffer-hook ()
  (eslint-close-file (buffer-file-name)))

(defun eslint-flymake-enable ()
  "Enable `eslint-flymake' in current buffer."
  (interactive)
  (when (eslint-detect-node-cmd)
    (unless (bound-and-true-p flymake-mode) (flymake-mode 1))
    (add-hook 'flymake-diagnostic-functions #'eslint-flymake-backend nil t)
    (add-hook 'after-change-functions #'eslint-flymake--after-change nil t)
    (add-hook 'kill-buffer-hook #'eslint-flymake--kill-buffer-hook nil t)
    (flymake-start)))

(defun eslint-flymake-disable ()
  "Disable `eslint-flymake' in current buffer."
  (interactive)
  (when (functionp eslint-flymake--current-report-fn)
    (eslint-flymake--report nil nil))
  (remove-hook 'flymake-diagnostic-functions #'eslint-flymake-backend t)
  (remove-hook 'after-change-functions #'eslint-flymake--after-change t)
  (remove-hook 'kill-buffer-hook #'eslint-flymake--kill-buffer-hook t))

(provide 'eslint-flymake)
