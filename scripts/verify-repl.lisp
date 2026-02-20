(handler-case
    (progn
      (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
        (when (probe-file quicklisp-init)
          (load quicklisp-init)))
      (require :asdf)
      (push (truename "src/zed-cl-repl-impl/") asdf:*central-registry*)
      (asdf:load-system :zed-cl/master-repl)
      (funcall (intern "EVAL-FORMS-FROM-CODE" :zed-cl.master-repl) "(+ 1 2)" nil nil nil)
      (sb-ext:exit :code 0))
  (error (e)
    (format *error-output* "✗ REPL Lisp verification failed: ~A~%" e)
    (sb-ext:exit :code 1)))
