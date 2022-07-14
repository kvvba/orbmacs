;;;; orbmacs --- Emacs configuration for org mode

;; Copyright (C) 2022  jakub@posteo.net

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Orbmacs is an Emacs configuration with a focus on org mode and "living in Emacs"

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (straight-use-package 'blackout)
	(straight-use-package 'hydra)
  (leaf-keywords-init))

(leaf emacs
  :blackout auto-revert-mode
  :blackout (emacs-lisp-mode . "λ")
  :blackout abbrev-mode
  :init
  ;; Make emacs start faster
  (setq startup/gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum)
  (defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))

  (defvar startup/file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (defun startup/revert-file-name-handler-alist ()
		(setq file-name-handler-alist startup/file-name-handler-alist))

  ;; Show parent brackets
  (show-paren-mode 1)

  ;; Diable GUI elements
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Copy paste outside of emacs
  (setq select-enable-clipboard t)

  ;; Disable automatic backup files
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;; Enable conservative scrolling
  (setq scroll-conservatively 100)

  ;; Diable ring-bell
  ;; (setq ring-bell-function 'ignore)

  ;; Indentation
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq c-basic-offset tab-width)
  (setq-default electric-indent-inhibit t)
  (setq-default indent-tabs-mode t)
  (setq backward-delete-char-untabify-method 'nil)

  ;; Enable prettify symbols
  ;; (global-prettify-symbols-mode t)

  ;; Enable bracket pair-matching
  (setq electric-pair-pairs '(
															(?\{ . ?\})
															(?\( . ?\))
															(?\[ . ?\])
															(?\" . ?\")
															))
  (electric-pair-mode t)
  (setq insert-pair-alist '((40 41) (91 93) (123 125) (34 34) (39 39) (96 39)))

	(global-display-line-numbers-mode)

  ;; Cursor follows new window
  (defun split-and-follow-horizontally ()
		(interactive)
		(split-window-below)
		(balance-windows)
		(other-window 1))
  (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

  (defun split-and-follow-vertically ()
		(interactive)
		(split-window-right)
		(balance-windows)
		(other-window 1))
  (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

  ;; Turn yes-or-no to y-or-n
  (setq use-short-answers t)

  ;; Rebind keys for resizing
  (global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "s-C-<down>") 'shrink-window)
  (global-set-key (kbd "s-C-<up>") 'enlarge-window)

  ;; Highlight current line
  (global-hl-line-mode t)

  (set-face-attribute 'default nil
											:family "Source Code Pro Regular"
											;; :height 100
											;; :weight 'normal
											;; :width 'normal
											)

  (defun concat-string-list (list)
		"Return a string which is a concatenation of all elements of the list separated by spaces"
		(mapconcat '(lambda (obj) (format "%s" obj)) list " "))

  (require 'cl-lib)

  (setq inferior-lisp-program "sbcl")

  (setq enable-recursive-minibuffers t)
  :bind
  ;; Hitting suspend frame by accident is annoying me
  ("C-z" . nil)
  ("C-x C-z" . nil)
  ("H-b" . consult-buffer)
  ("H-B" . consult-buffer-other-window)
  ("H-k" . kill-buffer)
	("H-0" . delete-window)
  :hook
  (emacs-startup-hook . startup/revert-file-name-handler-alist))

(leaf octave
  :leaf-defer t
  :defer-config
  (setq octave-comment-char ?%)
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(leaf eshell
  :leaf-defer t
  :defer-config
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
  :bind
  ("<s-C-return>" . eshell-other-window)
  ("C-c e" . eshell))

(leaf dired
  :defer-config
  (defun kill-dired-buffers ()
		(interactive)
		(mapc (lambda (buffer)
						(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
							(kill-buffer buffer)))
					(buffer-list)))

  (eval-after-load "dired-aux"
		'(add-to-list 'dired-compress-file-suffixes
									'("\\.zip\\'" ".zip" "unzip")))
  (eval-after-load "dired"
		'(define-key dired-mode-map "z" 'dired-zip-files))

  (defun dired-zip-files (zip-file)
		"Create an archive containing the marked files."
		(interactive "sEnter name of zip file: ")
		;; create the zip file
		(let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
			(shell-command
			 (concat "zip "
							 zip-file
							 " "
							 (concat-string-list
								(mapcar
								 '(lambda (filename)
										(file-name-nondirectory filename))
								 (dired-get-marked-files))))))

		(revert-buffer)

		;; remove the mark on all the files  "*" to " "
		;; (dired-change-marks 42 ?\040)
		;; mark zip file
		;; (dired-mark-files-regexp (filename-to-regexp zip-file))
		)
  :bind
  ("H-d" . dired)
  (dired-mode-map
   ("K" . dired-kill-subdir)
	 ("H-n" . dired-next-subdir)
	 ("H-p . dired-prev-subdir")))

;; (leaf dired-subtree
;; 	:straight t
;; 	:after dired)

(leaf dired-narrow
  :straight t
  :after dired
  :bind ((dired-mode-map
					:package dired
					("/" . dired-narrow))))

(leaf meow
  :straight t
  :init
  (load-file "~/.emacs.d/meow.el")
  (meow-setup)
  (meow-global-mode t)
  (meow-define-keys
      'normal
    '("P" . consult-yank-pop)
    '("Q" . avy-goto-line)
		'("%" . meow-query-replace)))

(leaf org
  :straight t
  :config
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
																 "* TODO %i%?\nEntered on: %U")
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
				'((sequence "TODO" "WAIT" "|" "DONE" "CANC" )))
  (setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")

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
  :blackout ((org-indent-mode)
						 (flyspell-mode)
						 (visual-line-mode))
  :bind
  ("H-c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c b" . org-cite-insert)
	("C-c t" . hydra-timer/body)
  :hook
  (org-mode-hook . flyspell-mode)
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . visual-line-mode)
  (org-after-todo-statistics-hook . org-summary-todo))
;; :hydra ((hydra-timer
;; 				"Org timer"
;; 				("s" org-timer-set-timer "Set timer")
;; 				("p" org-timer-pause-or-continue "Pause/resume timer")
;; 				("k" org-timer-stop "Stop timer"))))

(leaf org-modern
	:straight t
	:config
	;; (setq org-modern-tag nil)
	(setq org-modern-list ; I swap the defaults for + and *
        '((?+ . "•")
          (?- . "–")
          (?* . "◦")))
	(setq org-modern-radio-target nil)
	(global-org-modern-mode))

;; (leaf svg-tag-mode
;; 	:straight t
;; 	:after org
;; 	:config
;; 	(setq svg-tag-tags
;;       '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO"))))
;; 				(":HELLO:" .  ((lambda (tag) (svg-tag-make "HELLO"))
;;                        (lambda () (interactive) (message "Hello world!"))
;;                        "Print a greeting message"))
;; 				(":TODO:" . ((lambda (tag) (svg-tag-make tag))))
;; 				(":TODO:" . ((lambda (tag)
;;                        (svg-tag-make tag :beg 1 :end -1))))
;; 				("\\(:[A-Z]+:\\)" . ((lambda (tag)
;;                                (svg-tag-make tag :beg 1 :end -1))))
;; 				("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
;;                                            (svg-tag-make tag :beg 1 :inverse t
;;                                                           :margin 0 :crop-right t))))
;;         (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
;;                                            (svg-tag-make tag :beg 1 :end -1
;;                                                          :margin 0 :crop-left t))))
;; 				("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
;;                                      (svg-tag-make tag :beg 2))))
;;         ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
;;                                        (svg-tag-make tag :beg 2 :end -1))))))
;; 	;; (global-svg-tag-mode)
;; 	)

(leaf auctex
  :straight t)

(leaf leaf
  :straight (org-pandoc-import
						 :type git
						 :host github
						 :repo "tecosaur/org-pandoc-import"
						 :files ("*.el" "filters" "preprocessors")))

(leaf olivetti
  :straight t
  :config
  (setq olivetti-style 'fancy)
  :bind
  ("C-c O" . olivetti-mode)
  :hook
  (olivetti-mode-hook . (lambda ()(setq olivetti-body-width 0.5))))

(leaf page-break-lines
  :straight t
  :blackout t
  :init
  (global-page-break-lines-mode))

(leaf citar
  :straight t
  :after org
  :custom
  (org-cite-global-bibliography . '("~/org/papers/references.bib"))
  (org-cite-insert-processor . 'citar)
  (org-cite-follow-processor . 'citar)
  (org-cite-activate-processor . 'citar)
  (citar-bibliography . org-cite-global-bibliography)
  :config
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (setq citar-at-point-function 'embark-act))

(leaf mu4e
  :leaf-defer t
  :bind
  ("C-c z" . mu4e)
  ("C-c Z" . mu4e-other-window)
  (mu4e-main-mode-map
   ("e" . kill-current-buffer))
  (mu4e-headers-mode-map
   ("C-c c" . org-capture-mail))
  (mu4e-view-mode-map
   ("C-c c" . org-capture-mail))
  :defer-config
  (load "~/.emacs.d/mail.el"))

(leaf mu4e-alert
  :straight t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(leaf elfeed
  :straight t
  :leaf-defer t
  :defer-config
  (setq elfeed-feeds
				'(
					;; emacs
					("https://masteringemacs.org/feed" emacs)
					("https://rss.sciencedirect.com/publication/science/03019322" fluids multiphase)
					("https://rss.sciencedirect.com/publication/science/13594311" fluids thermal)
					("https://rss.sciencedirect.com/publication/science/0142727X" fluids thermal)
					("https://rss.sciencedirect.com/publication/science/00457930" fluids computation)
					("https://www.mdpi.com/rss/journal/fluids" fluids)
					("https://www.cambridge.org/core/rss/product/id/1F51BCFAA50101CAF5CB9A20F8DEA3E4" fluids mechanics)
					))
  :bind
  ("C-c w" . elfeed)
  (elfeed-search-mode-map
   ("U" . elfeed-update)))

(leaf bongo
  :straight t
  :leaf-defer t
  :defer-config (load-file "~/.emacs.d/music.el")
  :bind
  (("<C-XF86AudioPlay>" . bongo-pause/resume)
   ("<C-XF86AudioNext>" . bongo-next)
   ("<C-XF86AudioPrev>" . bongo-previous)
   ("<M-XF86AudioPlay>" . bongo-show)
   ("<S-XF86AudioNext>" . bongo-seek-forward-10)
   ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
   ("C-c p"             . bongo)
   (bongo-playlist-mode-map
		("n" . bongo-next-object)
		("p" . bongo-previous-object)
		("M-n" . prot/bongo-paylist-section-next)
		("M-p" . prot/bongo-paylist-section-previous)
		("M-h" . prot/bongo-playlist-mark-section)
		("M-d" . prot/bongo-playlist-kill-section)
		("g" . prot/bongo-playlist-reset)
		("D" . prot/bongo-playlist-terminate)
		("r" . prot/bongo-playlist-random-toggle)
		("R" . bongo-rename-line)
		("j" . bongo-dired-line)       ; Jump to dir of file at point
		("J" . dired-jump)             ; Jump to library buffer
		("i" . prot/bongo-playlist-insert-playlist-file)
		("I" . bongo-insert-special))
   (bongo-dired-library-mode-map
		("<C-return>" . prot/bongo-dired-insert)
		("C-c SPC" . prot/bongo-dired-insert)
		("C-c +" . prot/bongo-dired-make-playlist-file))))

(leaf vterm
  :straight t
  :leaf-defer t
  :config
  (setq vterm-timer-delay 0.01)
  (global-set-key (kbd "C-c v") 'vterm))

(leaf slime
  :straight t
  :leaf-defer t)

(leaf slime-company
  :straight t
  :leaf-defer t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(leaf all-the-icons
  :straight t
	:blackout all-the-icons-dired-mode
  :if (display-graphic-p))

(leaf all-the-icons-dired
  :straight t
  :if (display-graphic-p)
  :leaf-defer t
  :commands all-the-icons-dired-mode
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf eglot
  :straight t
  :leaf-defer
  :hook ((c-mode-hook c++-mode-hook python-mode-hook) . eglot-ensure)
  :config
  (add-to-list 'exec-path (expand-file-name "~/.local/bin/"))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("ccls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))


(leaf company
  :straight t
  :blackout t
  :custom
  (company-idle-delay . 0.15) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (global-company-mode . t)
  :config
  (add-to-list 'company-backends 'company-capf)
  :bind (company-active-map
				 ("C-n" . company-select-next)
				 ("C-p" . company-select-previous)
				 ("M-<" . company-select-first)
				 ("M->" . company-select-last)))

(leaf yasnippet
  :straight t
  :blackout yas-minor-mode
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode))

(leaf leaf
  :straight (denote
						 :type git
						 :host github
						 :repo "protesilaos/denote")
  :init
  (setq denote-directory "~/org/notes/")
  (setq denote-dired-directories
				(list denote-directory))
  :config
  (with-eval-after-load 'org-capture
		(require 'denote-org-capture)
		(add-to-list 'org-capture-templates
								 '("n" "New note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  (defun my-denote-journal ()
		"Create an entry tagged 'journal' with the date as its title."
		(interactive)
		(denote
		 (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
		 "journal")) ; multiple keywords are a list of strings: '("one" "two")
  :hook
  (dired-mode-hook . denote-dired-mode-in-directories)
  ;; (find-file-hook . denote-link-buttonize-buffer)
  :bind
  ("C-c n i" . denote-link)
  ("C-c n I" . denote-link-add-links)
  ("C-c n j" . my-denote-journal)
  ("C-c n b" . denote-link-backlinks))

(leaf leaf
  :straight (consult-notes
						 :type git
						 :host github
						 :repo "mclear-tools/consult-notes")
  :init
  (setq consult-notes-sources '(("notes" ?n "~/org/notes/")
																("papers" ?p "~/org/papers/")))
  (defun jakub/consult-notes-ripgrep ()
		(interactive)
		(consult-ripgrep denote-directory))
  :bind
  ("C-c n r" . jakub/consult-notes-ripgrep)
  ("C-c n f" . consult-notes))

(leaf vertico
  :straight t
  :init
  (vertico-mode))

(leaf consult
  :straight t
  :bind
  ("H-s" . consult-line)
  ("M-g g" . consult-goto-line)
  ("C-x r b" . consult-bookmark)
  ("H-r" . consult-ripgrep))

(leaf embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
							 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
								 nil
								 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :straight t
  :after (embark consult)
  :leaf-defer nil
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf savehist
  :straight t
  :init
  (savehist-mode)
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring)))

(leaf marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(leaf orderless
  :straight t
  :after vertico
  :config
  (setq completion-styles '(orderless)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))))

(leaf htmlize
  :straight t)

(leaf which-key
  :straight t
  :blackout t
  :init
  (which-key-mode))

(leaf beacon
  :straight t
  :blackout beacon-mode
  :init
  (beacon-mode 1))

(leaf avy
  :straight t
  :bind
  ("M-s" . avy-goto-char))

(leaf ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("H-o" . ace-window))

(leaf async
  :straight t
  :init
  (dired-async-mode 1))

;; (leaf undo-tree
;; 	:straight t
;;   :blackout undo-tree-mode
;; 	:config
;; 	(global-undo-tree-mode t))

(leaf magit
  :straight t
  :leaf-defer)

(leaf eldoc
  :straight t
  :blackout eldoc-mode
  :config
  (global-eldoc-mode t))

(leaf rainbow-delimiters
  :straight t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;; (leaf leaf
;;   :straight (lambda-line :type git :host github :repo "lambda-emacs/lambda-line")
;;   :custom
;;   (lambda-line-position . 'bottom) ;; Set position of status-line
;;   (lambda-line-abbrev . t) ;; abbreviate major modes
;;   (lambda-line-hspace . "  ")  ;; add some cushion
;;   (lambda-line-prefix . t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding . nil) ;; no extra space for prefix
;;   (lambda-line-status-invert . nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol . " ⨂") ;; symbols
;;   (lambda-line-gui-mod-symbol . " ⬤")
;;   (lambda-line-gui-rw-symbol . " ◯")
;;   (lambda-line-space-top . +.1)  ;; padding on top and bottom of line
;;   (lambda-line-space-bottom . -.1)
;;   (lambda-line-symbol-position . 0.1) ;; adjust the vertical placement of symbol
;;   :config
;; 	(customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
;; 	(customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
;;   ;; activate lambda-line
;;   (lambda-line-mode)
;;   ;; set divider line in footer
;;   (when (eq lambda-line-position 'top)
;;    (setq-default mode-line-format (list "%_"))
;;    (setq mode-line-format (list "%_"))))

(leaf leaf
  :straight (lambda-themes :type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments . t)
  (lambda-themes-set-italic-keywords . t)
  (lambda-themes-set-variable-pitch . t)
  :config
  (setq custom-safe-themes t)
  (load-theme 'lambda-light-faded))

;; (leaf nix-mode
;;   :mode "\\.nix\\'")

(leaf flycheck
  :straight t
  :blackout t
  :init (global-flycheck-mode))

(leaf dashboard
  :straight t
  :leaf-defer nil
  :preface
  (defun create-scratch-buffer ()
		"Create a scratch buffer"
		(interactive)
		(switch-to-buffer (get-buffer-create "*scratch*"))
		(lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq inhibit-startup-message t)
  (setq dashboard-items '((recents . 9)
													(bookmarks . 15)))
  ;; (add-to-list 'dashboard-items '(agenda) t)
  ;; (setq dashboard-week-agenda t)
  (setq dashboard-banner-logo-title "Welcome to Orbmacs.")
  ;; (setq dashboard-startup-banner "~/.emacs.d/media/orb.png")
  (setq dashboard-startup-banner "~/.emacs.d/media/sicp.png")
  ;; (setq dash-board-startup-banner 'official)
  (setq dashboard-center-content t)
  ;; (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)
  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "Emacs loaded in %s"
																		(emacs-init-time))))
(provide 'emacs)
;;; emacs.el ends here
