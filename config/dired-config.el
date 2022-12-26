(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer)
					(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
						(kill-buffer buffer)))
				(buffer-list)))

(eval-after-load "dired-aux"
	'(add-to-list 'dired-compress-file-suffixes
								'("\\.zip\\'" ".zip" "unzip")))
;; (eval-after-load "dired"
;; 	'(define-key dired-mode-map "z" 'dired-zip-files))

;; (defun dired-zip-files (zip-file)
;; 	"Create an archive containing the marked files."
;; 	(interactive "sEnter name of zip file: ")
;; 	;; create the zip file
;; 	(let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
;; 		(shell-command
;; 		 (concat "zip "
;; 						 zip-file
;; 						 " "
;; 						 (concat-string-list
;; 							(mapcar
;; 							 '(lambda (filename)
;; 									(file-name-nondirectory filename))
;; 							 (dired-get-marked-files))))))

;; 	(revert-buffer))

	;; remove the mark on all the files  "*" to " "
	;; (dired-change-marks 42 ?\040)
	;; mark zip file
	;; (dired-mark-files-regexp (filename-to-regexp zip-file))

(provide 'dired-config.el)
;;; dired-config.el ends here
