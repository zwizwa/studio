(defun studio-start ()
  (interactive)
  (make-comint "studio-dev" "~/studio/studio.sh")
  (switch-to-buffer "*studio-dev*")
  )

(defun studio-build ()
  (interactive)
  (setq compilation-finish-functions
        (list (lambda (buf info) (studio-start))))
  (compile "make -C ~/studio"))





    
