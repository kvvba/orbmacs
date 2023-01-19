;; (setq sentence-end-double-space nil)
(setq org-pretty-entities t)

(defun org-capture-mail ()
	(interactive)
	(call-interactively 'org-store-link)
	(org-capture nil "@"))

(setq org-capture-templates '(("i" "Inbox" entry
															 (file+headline "~/org/gtd/inbox.org" "Work")
															 "* TODO %i%?\nEntered on: %U")
															("p" "Personal inbox" entry
															 (file+headline "~/org/gtd/inbox.org" "Personal")
															 "* TODO %i%?\nEntered on: %U")
															("@" "Inbox [mu4e]" entry
															 (file+headline "~/org/gtd/inbox.org" "Mail")
															 "* TODO Process \"%a\" %?\nEntered on: %U")
															("r" "Reminder" entry
															 (file+headline "~/org/gtd/reminders.org" "Reminders")
															 "* %i%?\nEntered on: %U")
															;; ("m" "Meeting minutes" entry
															;;  (file+headline "~/org/meetings.org" "Meeting notes")
															;;  "* Meeting title: %(read-string \"Meeting title: \")\nAttending: %(read-string \"Attendees: \")\nTime: %U\n\n%i%?")
															))

(setq org-agenda-files '("~/org/gtd/inbox.org"
												 ;; "~/org/gtd/corkboard.org"
												 "~/org/gtd/reminders.org"
												 "~/org/timetable.org"))
;; (setq recentf-exclude '("\\.org\\"))
(setq org-todo-keywords
			'((sequence "TODO" "|" "DONE" )))
;; (setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")

(defun org-summary-todo (n-done n-not-done)
	(let (org-log-done org-log-states)   ; turn off logging
		(org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(setq org-refile-targets '(("~/org/gtd/reminders.org" :maxlevel . 2)
													 ("~/org/gtd/someday.org" :level . 1)
													 ("~/org/gtd/corkboard.org" :maxlevel . 3)))
(add-to-list 'org-entities-user
						 '("oint","\\oint{}" t "&#8750" "..." "..." "∮"))
(with-eval-after-load 'ox-latex
	(add-to-list 'org-latex-classes
							 '("elsarticle"
								 "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
								 ("\\section{%s}" . "\\section*{%s}")
								 ("\\subsection{%s}" . "\\subsection*{%s}")
								 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
								 ("\\paragraph{%s}" . "\\paragraph*{%s}")
								 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-startup-with-inline-images t)

(provide 'org-config.el)
;;; org-config.el ends here
