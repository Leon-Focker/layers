(multiple-value-bind
      (items end-time)
    (make-reaper-items1 (get-sndfiles
                         (concatenate 'string
                                      cl-user::+slippery-chicken-home-dir+
                                      "tests/test-sndfiles-dir-2"))
                        '(w+w (w) (q) h.+h+e (h) (e) h (q.) w (w) (w) (e))
                        :input-start '(0 .1 .2 .3)
                        :tempo 60
                        :play-rate '(1 1.02 1 .98 1.01 1 1.02)
                        :preserve-pitch t)
  (let ((rf (make-reaper-file 'otest items :cursor end-time)))
    (write-reaper-file rf :file "/tmp/reaper-test.rpp")
    (assoc-list-p (tracks rf))))
