(require 'dired)
(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer)
					(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
						(kill-buffer buffer)))
				(buffer-list)))
(eval-after-load "dired-aux"
	'(add-to-list 'dired-compress-file-suffixes
								'("\\.zip\\'" ".zip" "unzip")))

(defun jakub/dired-narrow ()
	(interactive)
	(dired-mark-files-regexp (read-string "Search: "))
	(dired-toggle-marks)
	(dired-do-kill-lines))

;; (setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)

;; (defun jakub/dired-really-delete ()
;; 	(interactive)
;; 	(let ((delete-by-moving-to-trash nil))
;; 		(dired-do-delete)))

(setq dired-listing-switches "-alFh")

(provide 'dired-config.el)
;;; dired-config.el ends here
