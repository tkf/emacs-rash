;;; rash.el --- RASH advances shell history, even in Emacs!

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; rash.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; rash.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with rash.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl)
                   (require 'compile))
(require 's)
(require 'deferred nil t)

(defgroup rash nil
  "RASH Emacs plugin."
  :group 'convenience
  :prefix "rash:")

(defvar rash:session-id nil)

(defcustom rash:command (list "rash")
  "Base command to invoke RASH CLI."
  :group 'rash)

(defvar rash:record-args nil)
(make-variable-buffer-local 'rash:record-args)

(defun rash:-construct-command (base &rest args)
  (apply #'append base
         (mapcar (lambda (opt) (when (cadr opt) (list (car opt) (cadr opt))))
                 args)))

(defun* rash:record
    (&key command
          (type "command")
          (cwd default-directory)
          exit-code
          start
          stop
          pipestatus
          print-session-id)
  (let ((default-directory "/"))
    (apply #'deferred:process
           (rash:-construct-command (append rash:command
                                            (list "record")
                                            (when print-session-id
                                              (list "--print-session-id")))
                                    (list "--session-id" rash:session-id)
                                    (list "--command" command)
                                    (list "--record-type" type)
                                    (list "--cwd" cwd)
                                    (list "--exit-code" exit-code)
                                    (list "--start" start)
                                    (list "--stop" stop)
                                    (list "--pipestatus" pipestatus)))))


(defun rash:record-compilation-start-handler (process)
  (setq rash:record-args
        (list
         :command (car compilation-arguments)
         :start (format-time-string "%s"))))

(defun rash:record-compilation-finish-handler (buffer msg)
  (with-current-buffer buffer
    (apply #'rash:record
           :exit-code (rash:-decode-process-status msg)
           rash:record-args)))

(defun rash:-decode-process-status (msg)
  (cond
   ((equal msg "finished\n") "0")
   ((string-match
     "exited abnormally with code \\([0-9]+\\)\n"
     msg)
    (match-string 1 msg))))

(defmacro rash:-add-remove-hooks (mode &rest hooks-and-callbacks)
  (declare (indent 1))
  `(if ,mode
       (progn
         ,@(loop for (hook cb append local) in hooks-and-callbacks
                 collect `(add-hook ',hook ',cb ,append ,local)))
     ,@(loop for (hook cb _ local) in hooks-and-callbacks
             collect `(remove-hook ',hook ',cb ,local))))

;;;###autoload
(define-minor-mode rash-mode
  "RASH mode -- command logger.

\\{rash-mode-map}"
  ;; :keymap rash-mode-map
  :global t
  :group 'rash
  (if rash-mode
      (deferred:$
        (rash:record :type "init" :print-session-id t)
        (deferred:nextc it
          (lambda (session-id)
            (setq rash:session-id (s-trim session-id)))))
    (when rash:session-id
      (rash:record :type "exit")
      (setq rash:session-id nil)))
  (rash:-add-remove-hooks
      rash-mode
    (compilation-finish-functions rash:record-compilation-finish-handler)
    (compilation-start-hook rash:record-compilation-start-handler)))


(provide 'rash)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; rash.el ends here
