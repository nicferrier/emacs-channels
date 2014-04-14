;;; channels-tests.el --- test the channels -*- lexical-binding: t -*-



(require 'fakir)
(require 'kva)

(defmacro $/when (expr &rest body)
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let (($ ,expr))
     (when $ ,@body)))

;; Mock the process for calling the future
(ert-deftest channel/filter$check-buffer ()
  (fakir-mock-process :proc ()
    (channel/filter :proc "some data")
    (channel/filter :proc " on a line\nand then more data")
    (equal 
     (with-current-buffer (process-buffer :proc)
       (buffer-string))
     "some data on a line\nand then more data")))

(ert-deftest channel/filter$read-line-before ()
  (fakir-mock-process :proc ()
    (let (received-line
          (channel (channel/make :proc)))
      (channel--readline channel
        (unless err
          (setq received-line data)))
      (channel/filter :proc "some data")
      (channel/filter :proc " on a line\nand then more data")
      (list
       (equal received-line "some data on a line\n")
       (equal ;; What's left in the buffer
        (with-current-buffer (process-buffer :proc) (buffer-string))
        "and then more data")))))

(ert-deftest channel/filter$read-2-lines-before ()
  (fakir-mock-process :proc ()
    (let (received-line
          (channel (channel/make :proc)))
      (channel--readline channel
        (when data
          (channel--readline channel
            (when data
              (setq received-line data)))))
      (channel/filter :proc "some data")
      (channel/filter :proc " on a line\nand then more data")
      (channel/filter :proc "\n")
      (list
       received-line
       (with-current-buffer (process-buffer :proc) (buffer-string))))))

(provide 'channels-tests)

;;; channels-tests.el ends here
