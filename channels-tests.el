;;; channels-tests.el --- test the channels -*- lexical-binding: t -*-

(require 'fakir)
(require 'kv)

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
      ;; The test code
      (channel--readline channel
        (unless err
          (setq received-line data)))
      ;; Now feed things into the channel
      (channel/filter :proc "some data")
      (channel/filter :proc " on a line\nand then more data")
      (list
       (equal received-line "some data on a line\n")
       (equal ;; What's left in the buffer
        (with-current-buffer (process-buffer :proc) (buffer-string))
        "and then more data")))))

(ert-deftest channel/filter$read-2-lines-before ()
  (fakir-mock-process :proc ()
    (let (first-line received-line)
      (let ((channel (channel/make :proc)))
        ;; The program
        (channel--readline channel
          (when data
            (channel--readline channel
              (when data (setq received-line data)))))
        ;; The mocking of data
        (channel/filter :proc "some data")
        (channel/filter :proc " on a line\nand then more data")
        (channel/filter :proc "\n")
        (list
         (equal received-line "and then more data\n")
         (equal
          (with-current-buffer (process-buffer :proc) (buffer-string))
          ""))))))

(ert-deftest channel/filter$read-2-lines-after ()
  (fakir-mock-process :proc ()
    (let (received-line
          (channel (channel/make :proc)))
      ;; First feed some data
      (channel/filter :proc "some data")
      (channel/filter :proc " on a line\nand then more data")
      (channel/filter :proc "\n")

      ;; Now the program
      (channel--readline channel
        (when data
          (channel--readline channel
            (when data
              (setq received-line data)))))

      ;; We need to have something we call at the end which will
      ;; trigger the process of reconciliation of the input queue and
      ;; the expectation queue
      (channel/complete :proc)

      (list
       (equal received-line "and then more data\n")
       (equal
        (with-current-buffer (process-buffer :proc) (buffer-string))
        "")))))

(provide 'channels-tests)

;;; channels-tests.el ends here
