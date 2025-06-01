;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'jsonrpc)

(defvar rpc--connection nil
  "RPC connection of jsonrpc.")
(defconst rpc--node-script-path
  (file-name-concat
   (file-name-directory (or load-file-name buffer-file-name))
   "rpc.mjs"))

(defun rpc--start-node-process ()
  (make-process
   :name "rpc node process"
   :connection-type 'pipe
   :noquery t
   :buffer "*rpc-node-stdout*"
   :sentinel (lambda (process event) (message "node-rpc status: %s" event))
   :command (list "node" rpc--node-script-path)))

(defun rpc-init-connection ()
  (unless (jsonrpc-process-connection-p rpc--connection)
    (setq rpc--connection
          (make-instance
           'jsonrpc-process-connection
           :name "rpc connection"
           :process (rpc--start-node-process)
           :on-shutdown (lambda (conn) (setq rpc--connection nil))
           :events-buffer-config '(:size nil :format 'lisp)
           ))))

(defun rpc--test-concurrent (count)
  "测试 jsonrpc 并发请求"
  (let ((l [])
        (i 0))
    (rpc-init-connection)
    (while (< i count)
      (setq l (vector i (1+ i)))
      (setq i (1+ i))
      (jsonrpc-async-request
       rpc--connection "sum" l
       :error-fn (lambda (e) (message "`sum' error: %s" e))
       :success-fn (lambda (r) (message "`sum' result: %s" r)))
      )))

(defun rpc--async-send (method params)
  (rpc-init-connection)
  (jsonrpc-async-request
   rpc--connection method params
   :error-fn (lambda (e) (message "%s error: %s" method e))
   :success-fn (lambda (r) (message "%s result: %s" method r))
   ))

(defun rpc--notify (method params)
  (rpc-init-connection)
  (jsonrpc-notify rpc--connection method params))


;; 关闭现有 jsonrpc 连接
(when (jsonrpc-process-connection-p rpc--connection)
  (jsonrpc-shutdown rpc--connection))

;; (rpc--test-concurrent 10)
(rpc--async-send "sum" [1 2 3 4 "2"])
(rpc--notify "hel" [1 2 3])
(rpc--notify "notify" [4 5 6])
