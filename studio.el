(defun studio-start ()
  (interactive)
  (make-comint "studio-dev" "~/exo/erl.sh")
  (switch-to-buffer "*studio-dev*")
  )

(defun studio-dev ()
  (interactive)
  (studio-start)
  (compile "make -C ~/studio"))




    
