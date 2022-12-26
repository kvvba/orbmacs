(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
(setq eshell-prompt-function
			(lambda nil
				(concat
				 (if (string= (eshell/pwd) (getenv "HOME"))
						 (propertize "~" 'face `(:foreground "#268bd2"))
					 (replace-regexp-in-string
						(getenv "HOME")
						(propertize "~" 'face `(:foreground "#268bd2"))
						(propertize (eshell/pwd) 'face `(:foreground "#268bd2"))))
				 (if (= (user-uid) 0)
						 (propertize " α " 'face `(:foreground "#d33682"))
					 (propertize " λ " 'face `(:foreground "#d33682"))))))

(setq eshell-highlight-prompt nil)

;; Aliases
(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

(defun eshell/sudo-open (filename)
	"Open a file (FILENAME) as root in Eshell."
	(let ((qual-filename (if (string-match "^/" filename)
													 filename
												 (concat (expand-file-name (eshell/pwd)) "/" filename))))
		(switch-to-buffer
		 (find-file-noselect
			(concat "/sudo::" qual-filename)))))

(defun eshell-other-window ()
	"Create or visit an eshell buffer."
	(interactive)
	(if (not (get-buffer "*eshell*"))
			(progn
				(split-window-sensibly (selected-window))
				(other-window 1)
				(eshell))
		(switch-to-buffer-other-window "*eshell*")))

(provide 'eshell)
;;; eshell.el ends here
