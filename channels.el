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

(defvar $/err nil
  "Global binding of error symbol for $/with-escape.")

(defun $/escape (data)
  "Escape to the nearest `$/with-escape' handler.")

(defmacro $/with-escape (form on-throw)
  (declare
   (debug (form form))
   (indent 0))
  (let ((catch-sym (make-symbol "catch-sym"))
        (escape-sym (make-symbol "escape-sym")))
    `(let ((form-res
            (catch (quote ,catch-sym)
              (noflet (($/escape (data)
                         (throw (quote ,catch-sym)
                           (cons (quote ,escape-sym) data))))
                ,form))))
       (if (and (consp form-res)
                (eq (car form-res) (quote ,escape-sym)))
           (let (($/err (cdr form-res)))
             (apply ,on-throw (list $/err)))))))


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
         (buffer-substring
          (point-min)
          (or (re-search-forward "\n" nil t)
              ($/escape :no-line)))
         (delete-region (point-min) (point)))))))

(defun channel/pop-char (proc)
  "Remove the top most char from the buffer for PROC.

If there is no char to read it calls `$/escape' which throws an
error."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (if (> (point-max) (point-min))
          (buffer-substring
           (point-min)
           (+ (point-min) 1))
          ;; Else throw no-char
          ($/escape :no-char)))))

(defun channel/filter (proc data)
  "The filter for a channel process."
  ;;  check the futures waiting on the channel
  (let ((receiverq (process-get proc :channel-queue)))
    (channel/add-to-buffer proc data)
    (when receiverq
      (let ((receiver (car receiverq)))
        ($/with-escape
          (let ((to-send
                 (case (kva :satisfy receiver)
                   (:line (channel/pop-line proc))
                   (:char (channel/pop-char proc)))))
            ;; Remove the proc from the queue
            (process-put proc :channel-queue (cdr receiverq))
            ;; Send result
            (funcall (kva :future receiver) to-send nil))
          ;; Escape handle
          (lambda (err)
            (cond
              ((memq err '(:no-char :no-line)) t)
              (t
               ;; Remove the proc from the queue
               (process-put proc :channel-queue (cdr receiverq))
               ;; Send error
               (funcall (kva :future receiver) nil err)))))))))

(defun channel/make-receiver (satisfy receiver-proc)
  "Make a receiver expecting SATISFY and wrapping RECEIVER-PROC."
  (list (cons :satisfy satisfy)
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
  (list (cons :process proc)))


;;; channels.el ends here
