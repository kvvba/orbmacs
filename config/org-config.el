;; (setq sentence-end-double-space nil)
(setq org-pretty-entities t)

;; (setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")

;; (defun org-summary-todo (n-done n-not-done)
;; 	(let (org-log-done org-log-states)   ; turn off logging
;; 		(org-todo (if (= n-not-done 0) "DONE" "TODO"))))

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

(with-eval-after-load 'ox-latex
	(add-to-list 'org-latex-classes
							 '("extarticle"
								 "\\documentclass{extarticle}"
								 ("\\section{%s}" . "\\section*{%s}")
								 ("\\subsection{%s}" . "\\subsection*{%s}")
								 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
								 ("\\paragraph{%s}" . "\\paragraph*{%s}")
								 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; (setq org-startup-with-inline-images t)

(provide 'org-config.el)
;;; org-config.el ends here
