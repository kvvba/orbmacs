(defun org-refile-book ()
	(interactive)
	(let ((org-refile-targets '(("books.org" :maxlevel . 1))))
		(org-refile)))


(provide 'org-books.el)
;;; org-books.el ends here
