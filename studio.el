(defun studio-start ()
  (interactive)
  (make-comint "studio-dev" "~/studio/studio.sh")
  (switch-to-buffer "*studio-dev*")
  )

;; Switch dev setup to studio.
(defun dev-studio ()
  (interactive)
  (setq compilation-finish-functions
        (list (lambda (buf info) (studio-start))))
  (compile "make -C ~/studio"))





    
