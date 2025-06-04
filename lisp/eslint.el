;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'jsonrpc)

(defconst eslint--home
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory which `eslint' installed.")
(defconst eslint--js-file
  (expand-file-name "../js/index.mjs" eslint--home)
  "Node program entry of `eslint'.")

(defvar eslint--connection nil
  "JSONRPC connection of eslint.")


;;; User customization
(defgroup eslint nil
  "ESLint Client for Emacs."
  :prefix "eslint-"
  :group 'tools)

(defcustom eslint-events-buffer-size 2000000
  "Configure the eslint events buffer.

0 disables, nil means infinite."
  :type '(choice (integer :tag "Number of characters")
                 (const :tag "No limit" nil)))


;;; Private function
(defun eslint--new-connection ()
  (let ((node (eslint-detect-node-cmd)))
    (when (and node (file-exists-p eslint--js-file))
      (make-instance
       'jsonrpc-process-connection
       :name "eslint"
       :on-shutdown (lambda (conn) (setq eslint--connection nil))
       :events-buffer-config
       (list :size eslint-events-buffer-size :format 'full)
       :process
       (make-process
        :name "eslint node process"
        :connection-type 'pipe
        :noquery t
        :command (list node eslint--js-file))))))

(defun eslint--init-connection ()
  (unless (jsonrpc-process-connection-p eslint--connection)
    (setq eslint--connection (eslint--new-connection))))


;;; public function
(defun eslint-detect-node-cmd ()
  (locate-file "node" exec-path))

(defun eslint-lint-file (file code &optional success-fn error-fn)
  "Lint the file named FILE by a node process which run eslint instance.

CODE is content of FILE.

SUCCESS-FN or ERROR-FN will be called with a
JSONRPC `:result' or `:error' object respectively."
  (eslint--init-connection)
  (when (and file
             (stringp file)
             (stringp code)
             (jsonrpc-process-connection-p eslint--connection))
    (jsonrpc-async-request
     eslint--connection "lint"
     (list :file file :code code)
     :success-fn success-fn :error-fn error-fn)
    ))

(defun eslint-close-file (file)
  "Notify node process FILE closed."
  (eslint--init-connection)
  (when (and (stringp file)
             (jsonrpc-process-connection-p eslint--connection))
    (jsonrpc-notify eslint--connection "close" (list :file file))))

(defun eslint--show-cached-info ()
  "Show data of configWorkerMap and configFilesMap in node process."
  (eslint--init-connection)
  (when (jsonrpc-process-connection-p eslint--connection)
    (jsonrpc-request eslint--connection "debug" nil)))


(provide 'eslint)
