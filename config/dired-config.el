(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer)
					(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
						(kill-buffer buffer)))
				(buffer-list)))
(eval-after-load "dired-aux"
	'(add-to-list 'dired-compress-file-suffixes
								'("\\.zip\\'" ".zip" "unzip")))
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(provide 'dired-config.el)
;;; dired-config.el ends here
