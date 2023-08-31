(require 'org)

;; Files
(setq org-directory "~/Documents/org")
(setq org-agenda-files 
      (mapcar 'file-truename 
	      (file-expand-wildcards "~/Documents/org/gtd/*.org")))

;; Capture

(setq org-capture-templates '(("i" "inbox" entry
															 (file "gtd/inbox.org")
															 "* TODO %i%?\nEntered on: %U")
															("m" "meeting" entry  (file+headline "gtd/agenda.org" "Future")
															 "* %? :meeting:\nEntered on: %U")
															("n" "note" entry  (file "gtd/notes.org")
															 "* Note (%a)\nEntered on: %U\n%?")
															("e" "event" entry
															 (file+headline "gtd/agenda.org" "Future")
															 "* %? :event:\nEntered on: %U")
															("d" "deadline" entry
															 (file+headline "gtd/agenda.org" "Future")
															 "* NEXT %? :deadline:\nEntered on: %U")
															("r" "reminder" entry
															 (file+headline "gtd/reminders.org" "Future")
															 "* TODO %? :reminder:\nEntered on: %U")))

;; Keybinds
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c r") 'org-refile)

;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("corkboard.org" :maxlevel . 1)
				("agenda.org" :maxlevel . 1)))

;; (setq org-agenda-hide-tags-regexp ".")

(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
			 (when (member (buffer-file-name) org-agenda-files) 
			   t)))
  (message "Saving org-agenda-files buffers... done"))

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

;; TODO
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-log-done 'time)

;; Agenda view
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 14)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(define-key global-map (kbd "C-c r") 'org-refile)
(setq org-refile-targets
      '(("corkboard.org" :maxlevel . 1)
				("agenda.org" :maxlevel . 1)))

;; mu4e
;; (defun org-capture-mail ()
;; 	(interactive)
;; 	(call-interactively 'org-store-link)
;; 	(org-capture nil "@"))

(provide 'gtd.el)
;;; gtd.el ends here
