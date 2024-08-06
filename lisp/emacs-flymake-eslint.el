;; -*- coding: utf-8; lexical-binding: t; -*-

(defconst emacs-flymake-eslint--home
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory which `emacs-flymake-eslint' installed.")
(defconst emacs-flymake-eslint--js-file
  (expand-file-name "../js/index.mjs" emacs-flymake-eslint--home)
  "Node program entry of `emacs-flymake-eslint'.")
(defconst emacs-flymake-eslint--stdout-name " *emacs-flymake-eslint output*"
  "Standard output buffer name of `emacs-flymake-eslint'.")
(defconst emacs-flymake-eslint--stderr-name " *emacs-flymake-eslint stderr*"
  "Standard error buffer name of `emacs-flymake-eslint'.")

(defvar emacs-flymake-eslint--process nil
  "Linter process.
All buffers use the same process.")

(defvar emacs-flymake-eslint--report-fn-map (make-hash-table :test #'equal)
  "File path and flymake report function map.")

(defun emacs-flymake-eslint--buffer (buffer-name)
  (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))
(defun emacs-flymake-eslint--stdout-buffer ()
  (emacs-flymake-eslint--buffer emacs-flymake-eslint--stdout-name))
(defun emacs-flymake-eslint--stderr-buffer ()
  (emacs-flymake-eslint--buffer emacs-flymake-eslint--stderr-name))

(defun emacs-flymake-eslint--log-process-exit (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert "====================== process exit ======================\n")))

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

(defun emacs-flymake-eslint--filter (stdout-output stdout-buffer stderr-buffer)
  (condition-case err
      (let* ((obj (json-parse-string stdout-output :object-type 'plist))
             (filepath  (plist-get obj :file))
             (buffer (find-buffer-visiting filepath))
             cost messages report-fn diags)
        (when (and filepath buffer
                   (hash-table-p emacs-flymake-eslint--report-fn-map))
          (setq cost        (plist-get obj :cost)
                messages    (plist-get obj :messages)
                report-fn   (gethash filepath emacs-flymake-eslint--report-fn-map))
          (with-current-buffer stdout-buffer
            (goto-char (point-max))
            (when (> (point) 1)
              (insert "-----------------------------------------\n"))
            (insert (format "file: %s\ncost: %sms\n" filepath cost)))
          (setq diags (mapcar (lambda (msg)
                                (emacs-flymake-eslint--parse-message msg buffer))
                              messages))
          (when (functionp report-fn) (funcall report-fn diags))
          (remhash filepath emacs-flymake-eslint--report-fn-map)))
    (t (with-current-buffer stderr-buffer
         (goto-char (point-max))
         (insert (format "\nerror: %s\ntype: %s, origin: %s"
                         err (type-of stdout-output) stdout-output)))
       (message "emacs-flymake-eslint error: %s" err))))

(defun emacs-flymake-eslint--create-process ()
  (let ((node (emacs-flymake-eslint--detect-node-cmd))
        buffer stderr)
    (when (and node (file-exists-p emacs-flymake-eslint--js-file))
      (setq buffer (emacs-flymake-eslint--stdout-buffer)
            stderr (emacs-flymake-eslint--stderr-buffer))
      (setq emacs-flymake-eslint--process
            (make-process
             :name "emacs-flymake-eslint"
             :connection-type 'pipe
             :noquery t
             :buffer buffer
             :stderr stderr
             :command (list "node" emacs-flymake-eslint--js-file)
             :filter (lambda (process output)
                       (emacs-flymake-eslint--filter output buffer stderr))
             :sentinel (lambda (process event)
                         (when (eq 'exit (process-status process))
                           (when (bufferp buffer) (kill-buffer buffer))
                           (when (bufferp stderr)
                             (emacs-flymake-eslint--log-process-exit stderr)))
                         (when (hash-table-p emacs-flymake-eslint--report-fn-map)
                           (clrhash emacs-flymake-eslint--report-fn-map))
                         )
             )))))

(defun emacs-flymake-eslint--init-process ()
  (emacs-flymake-eslint--kill-process)
  (setq emacs-flymake-eslint--process (emacs-flymake-eslint--create-process)))

(defun emacs-flymake-eslint--get-process ()
  (unless (process-live-p emacs-flymake-eslint--process)
    (emacs-flymake-eslint--init-process))
  emacs-flymake-eslint--process)

(defun emacs-flymake-eslint-lint-file (filepath &optional buffer)
  "Lint file by a node process which run eslint instance."
  (let ((process (emacs-flymake-eslint--get-process))
        (code (when (bufferp buffer)
                (with-current-buffer buffer (buffer-string)))))
    (when (and (stringp code)
               (process-live-p process))
      (process-send-string
       process
       (json-serialize (list :cmd "lint" :file filepath :code code))))))

(defun emacs-flymake-eslint-kill-buffer-hook ()
  "Hook function run after buffer killed."
  (let ((filepath (buffer-file-name))
        (process (when (process-live-p emacs-flymake-eslint--process)
                   emacs-flymake-eslint--process)))
    (when (and process filepath
               (bound-and-true-p flymake-mode)
               (member 'emacs-flymake-eslint--checker
                       flymake-diagnostic-functions))
      (process-send-string
       process
       (json-serialize (list :cmd "close" :file filepath))))))

(defun emacs-flymake-eslint--checker (report-fn &rest _ignore)
  (let ((filepath (buffer-file-name)))
    (when filepath
      (emacs-flymake-eslint-lint-file filepath (current-buffer))
      (puthash filepath report-fn emacs-flymake-eslint--report-fn-map))))

(defun emacs-flymake-eslint-enable ()
  "Enable `emacs-flymake-eslint' in current buffer."
  (interactive)
  (when (emacs-flymake-eslint--detect-node-cmd)
    (unless (bound-and-true-p flymake-mode) (flymake-mode 1))
    (add-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker nil t)
    (add-hook 'kill-buffer-hook #'emacs-flymake-eslint-kill-buffer-hook nil t)
    (add-hook 'kill-buffer-hook #'emacs-flymake-eslint-kill-buffer-hook)))

(defun emacs-flymake-eslint-disable ()
  "Disable `emacs-flymake-eslint' in current buffer."
  (interactive)
  (when (and (bound-and-true-p flymake-mode)
             flymake-mode
             (emacs-flymake-eslint--detect-node-cmd))
    (remove-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker)
    (remove-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker t)))

(defun emacs-flymake-eslint-stop ()
  "Kill node process of `emacs-flymake-eslint' and disable in current buffer."
  (interactive)
  (when (process-live-p emacs-flymake-eslint--process)
    (process-send-string
     emacs-flymake-eslint--process
     (json-serialize (list :cmd "exit"))))
  (let ((buffer (get-buffer emacs-flymake-eslint--stderr-name)))
    (when (bufferp buffer) (kill-buffer buffer)))
  (remove-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker)
  (remove-hook 'flymake-diagnostic-functions #'emacs-flymake-eslint--checker t)
  (remove-hook 'kill-buffer-hook #'emacs-flymake-eslint-kill-buffer-hook)
  (remove-hook 'kill-buffer-hook #'emacs-flymake-eslint-kill-buffer-hook t))

(defun emacs-flymake-eslint-log ()
  "Log current info of node process."
  (interactive)
  (when (process-live-p emacs-flymake-eslint--process)
    (process-send-string emacs-flymake-eslint--process
                         (json-serialize (list :cmd "log")))))


(provide 'emacs-flymake-eslint)
