(define-key global-map (kbd "C-c r") 'org-refile)
(setq org-refile-targets
      '(("corkboard.org" :maxlevel . 1)
				("agenda.org" :maxlevel . 1)))

(provide 'org-books.el)
;;; org-books.el ends here
