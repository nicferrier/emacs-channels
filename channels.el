;; channels.el --- async channels in EmacsLisp -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'noflet)


(defun channel/add-to-buffer (proc data)
  "Add the DATA to the buffer for PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert data))))

(defun channel/pop-line (proc)
  "Remove the top most line from the buffer for PROC.

If there is no line in the buffer it calls `$/escape' which
throws an error."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (prog1
            (if (re-search-forward "\n" nil t)
                (prog1
                    (cons :line (buffer-substring (point-min) (point)))
                  (delete-region (point-min) (point)))
                (cons :error :no-line)))))))

(defun channel/pop-char (proc)
  "Remove the top most char from the buffer for PROC.

If there is no char to read it calls `$/escape' which throws an
error."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (if (> (point-max) (point-min))
          (prog1
              (cons :char (buffer-substring (point-min) (+ (point-min) 1)))
            (delete-char 1))
          ;; Else return no char
          (cons :error :no-char)))))


(defun channel/rqpop (proc)
  (let ((rq (process-get proc :channel-queue)))
    (let ((v (car rq)))
      (process-put proc :channel-queue (cdr rq))
      v)))

(defun channel/rqpush (proc r)
  (process-put proc :channel-queue (cons r (process-get proc :channel-queue))))

(defun channel/complete (proc)
  "Try to complete the futures attached to PROC.

PROC must be a channel process."
  (noflet ((channel/complete-1 (receiver proc)
             (when receiver
               (let ((to-send
                      (case (kva :satisfy receiver)
                        (:line (channel/pop-line proc))
                        (:char (channel/pop-char proc)))))
                 (if (memq (cdr to-send) '(:no-char :no-line))
                     (channel/rqpush proc receiver)
                     ;; Else
                     (apply (kva :future receiver)
                            (case (car to-send)
                              (:error (list nil (cdr to-send)))
                              (t (list (cdr to-send) nil))))
                     (channel/complete-1 (channel/rqpop proc) proc))))))
    (channel/complete-1 (channel/rqpop proc) proc)))

(defun channel/filter (proc data)
  "The filter for a channel process."
  (channel/add-to-buffer proc data)
  (channel/complete proc))

(defun channel/make-receiver (satisfy receiver-proc)
  "Make a receiver expecting SATISFY and wrapping RECEIVER-PROC."
  (list (cons :id (random))
        (cons :satisfy satisfy)
        (cons :future receiver-proc)))

(defun channel-readline (channel receiver-proc)
  "Read a line from CHANNEL and pass it to RECEIVER-PROC.

In fact this just expects a line from CHANNEL, the line is read
asynchronously, only when it arrives."
  (let ((proc (kva :process channel)))
    (let ((q (process-get proc :channel-queue)))
      (process-put
       proc :channel-queue 
       (cons (channel/make-receiver :line receiver-proc) q)))))

(defmacro channel--readline (channel &rest form)
  "Macro form of `channel-readline'.

FORM has `data' and `err' bound while it runs."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(channel-readline
    channel
    (lambda (data err)
      ,@form)))

(defun channel-readchar (channel receiver-proc)
  "Read a char from CHANNEL and pass it to RECEIVER-PROC.

In fact this just expects a char from CHANNEL, the char is read
asynchronously, only when it arrives."
  (let ((proc (kva :process channel)))
    (let ((q (process-get proc :channel-queue)))
      (process-put
       proc :channel-queue 
       (cons (channel/make-receiver :char receiver-proc) q)))))

(defun channel/make (proc)
  "Make a channel around PROC.

A channel is an a-list with a `:process' key being the process.

The process has a plist key `:channel-queue' which describes the
expectations (the current list of read lines and read chars) on
the channel's buffer."
  ;; This constructor should probably be private since we don't want
  ;; channels to be constructed free form around procs (because it
  ;; might encourage the proc to be read in another way)
  (process-put proc :channel-queue nil)
  (process-put proc :channel 
               (list (cons :process proc)))
  (process-get proc :channel))

;;; channels.el ends here
