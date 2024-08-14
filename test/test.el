;; -*- lexical-binding: t; -*-

(defvar temp-process nil
  "Temperary process.")

(defconst js-file
  (expand-file-name
   "./fs.mjs"
   (file-name-directory (or load-file-name buffer-file-name))))
(defconst pac-file-name "/home/zedhugh/.mozilla/autoproxy.pac")
(defconst stdout-name " *temp-process-stdout*")
(defconst stderr-name " *temp-process-stderr*")


(defun create-temp-buffer (buffer-name)
  (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))

(defun create-process ()
  (unless (and (processp temp-process) (process-live-p temp-process))
    (let ((stdout (create-temp-buffer stdout-name))
          (stderr (create-temp-buffer stderr-name)))
      (setq temp-process
            (make-process
             :name "temp-process"
             :connection-type 'pipe
             :noquery t
             :buffer stdout
             :stderr stderr
             :command (list "node" js-file)
             :sentinel (lambda (process event)
                         (when (eq 'exit (process-status process))
                           (when (bufferp stdout) (kill-buffer stdout))
                           (when (bufferp stderr) (kill-buffer stderr))
                           (setq temp-process nil)))
             )))))


(defun _prepare ()
  (create-process)
  (find-file-noselect pac-file-name t t))

(defun lint-pac-file (&optional id switch)
  (_prepare)
  (process-send-string
   temp-process
   (json-serialize
    (list :cmd "lint"
          :id (or id :null)
          :file pac-file-name
          :code (with-current-buffer
                    (file-name-nondirectory pac-file-name)
                  (buffer-string)))))
  (when switch (switch-to-buffer-other-window stderr-name)))

(defun lint-pac-file-twice ()
  (lint-pac-file 1)
  (lint-pac-file 2 t))

(defun kill-temp-process ()
  (when (and (processp temp-process)
             (process-live-p temp-process))
    (process-send-eof temp-process)))


(provide 'test)
