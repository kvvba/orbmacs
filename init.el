
;; Copyright (C) 2022  kvvba

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
	;; (straight-use-package 'hydra)
  (leaf-keywords-init))

(setq user-full-name "jakub")

(leaf org
	:straight t
	:blackout visual-line-mode
  :init
	(load-file "~/.emacs.d/config/org-config.el")
	(load-file "~/.emacs.d/config/gtd.el")
	(load-file "~/.emacs.d/config/website-config.el")
  :bind
  ("C-c b" . org-cite-insert)
	("C-M-<return>" . org-insert-subheading)
	("C-M-S-<return>" . org-insert-todo-subheading)
  :hook
  (org-mode-hook . (lambda ()
										 (flyspell-mode)
										 (blackout 'flyspell-mode)))
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . (lambda ()
										 (org-indent-mode)
										 (blackout 'org-indent-mode)))
  ;; (org-after-todo-statistics-hook . org-summary-todo)
	)

(leaf org-agenda
	:bind
	(org-agenda-mode-map
	 ("r" . org-agenda-refile)
	 ("g" . org-agenda-redo)))

(leaf emacs
  :blackout auto-revert-mode
  :blackout (emacs-lisp-mode . "λ")
  :blackout abbrev-mode
  :init
	(setq inhibit-startup-screen t)
	;; (find-file "~/Documents/org/gtd/corkboard.org")
	(list-bookmarks)
	(switch-to-buffer "*Bookmark List*")
		
	;; (remove-hook 'find-file-hook 'vc-refresh-state)
	
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

  ;; Indentation
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq c-basic-offset tab-width)
  (setq-default electric-indent-inhibit t)
  (setq-default indent-tabs-mode t)
  (setq backward-delete-char-untabify-method 'nil)


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
	;; (global-linum-mode)

	;; (eshell-command "xmodmap ~/.Xmodmap")

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

  ;; Highlight current line
  (global-hl-line-mode t)

  (set-face-attribute 'default nil
											:family "Source Code Pro"
											;; :height 100
											:weight 'normal
											:width 'normal
											)

  (defun concat-string-list (list)
		"Return a string which is a concatenation of all elements of the list separated by spaces"
		(mapconcat #'(lambda (obj) (format "%s" obj)) list " "))
  (require 'cl-lib)
  (setq inferior-lisp-program "sbcl")
  (setq enable-recursive-minibuffers t)
	(setq log-warning-minimum-level :error)
	(setq sentence-end-double-space nil)
	;; (display-time-mode)

	(setq custom-safe-themes t)
	(setq modus-themes-mode-line '(borderless))
	;; (load-theme 'modus-operandi)

	(setq ediff-split-window-function 'split-window-sensibly)

	:bind
  ;; Hitting suspend frame by accident is annoying me
  ("C-z" . nil)
  ("C-x C-z" . nil)
  ("C-x b" . consult-buffer)
  ("C-x B" . consult-buffer-other-window)
	("C-c o" . consult-outline)
	("M-[" . backward-paragraph)
	("M-]" . forward-paragraph)
	("C-x C-b" . ibuffer)
  :hook
  (emacs-startup-hook . startup/revert-file-name-handler-alist))

(leaf ispell
	:defer-config
	(setq ispell-program-name "hunspell")
	(setq ispell-dictionary "en_GB")
	)

(leaf flypsell
	:defer-config
	(setq flyspell-default-dictionary "en_GB")
	)

(leaf eshell
	:defer-config
	(load-file "~/.emacs.d/config/eshell-config.el")
  :bind
  ("<C-s-return>" . eshell-other-window)
  ("C-c e" . eshell)
	("C-c E" . eshell-command)
	:hook
	(eshell-mode-hook . eat-eshell-visual-command-mode)
	(eshell-mode-hook . eat-eshell-mode))

(leaf exec-path-from-shell
	:straight t
	:init
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize))
	(exec-path-from-shell-copy-envs '("WM_PROJECT" "WM_PROJECT_DIR" "WM_PROJECT_USER_DIR")))

(leaf detached
	:straight t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session)
				 )
	:defer-config
	(defun detached-extra-project-compile ()
    "Run `compile' in the project root."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function))
          (detached-session-origin 'project))
      (call-interactively #'detached-compile)))
  (advice-add 'project-compile
              :override #'detached-extra-project-compile)
  :custom
	(detached-show-output-on-attach . t)
  (detached-terminal-data-command . system-type))

(leaf dired
  :config
	(load-file "~/.emacs.d/config/dired-config.el")
  :bind
  (dired-mode-map
   ("K" . dired-do-kill-lines)
	 ("C-x C-j" . dired-jump)
	 ("W" . wdired-change-to-wdired-mode)
	 ("r" . rgrep)
	 ("/" . jakub/dired-narrow))
	:hook
	(dired-mode-hook . dired-hide-details-mode))

(leaf all-the-icons
	:straight t)

(leaf all-the-icons-dired
	:straight t
	:after dired
	:blackout t
	:hook
	(dired-mode-hook . all-the-icons-dired-mode))

(leaf all-the-icons-completion
	:straight t
	:init (all-the-icons-completion-mode)
	:blackout t
	:hook
	(marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

(leaf meow
  :straight t
  :init
  (load-file "~/.emacs.d/config/meow-config.el")
  (meow-setup)
  (meow-global-mode t)
  (meow-define-keys
      'normal
    '("P" . consult-yank-pop)
    '("Q" . avy-goto-line)
		'("%" . meow-query-replace)
		'("S" . avy-kill-region)
		'("V" . avy-kill-ring-save-region)))

(leaf nov
	:straight t
	:config
	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(leaf yaml-mode
	:straight t
	:leaf-defer t
	:config
	(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(leaf auctex
  :straight t)

(leaf ox-pandoc
	:straight t)

(leaf pandoc
	:straight t)

(leaf page-break-lines
  :straight t
  :blackout t
  :init
  (global-page-break-lines-mode))

(leaf eat
	:straight (eat
						 :type git
						 :host codeberg
						 :repo "akib/emacs-eat"
						 :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
	:bind
	("C-c v" . (lambda() (interactive) (eat "/bin/bash"))))

(leaf slime
  :straight t
  :leaf-defer t)

(leaf slime-company
  :straight t
  :leaf-defer t
  :after (slime company)
  :defer-config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(leaf julia-mode
	:straight t)

(leaf julia-repl
	:straight t)

(leaf openfoam
	:straight (openfoam
						 :type git
						 :host github
						 :repo "kvvba/openfoam.el")
	;; :hook
	;; (c++-mode-hook . openfoam-c++-mode)
	)

(leaf gmsh-mode
	:straight (gmsh-mode
						 :type git
						 :host gitlab
						 :repo "kvvba/gmsh-mode"))

(leaf company
  :straight t
  :blackout t
  :custom
  (company-idle-delay . 0) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (global-company-mode . t)
  :config
  (add-to-list 'company-backends 'company-capf)
	;; (defun complete-or-indent ()
  ;;   (interactive)
  ;;   (if (company-manual-begin)
  ;;       (company-complete-common)
  ;;     (indent-according-to-mode)))
  :bind (company-active-map
				 ("C-n" . company-select-next)
				 ("C-p" . company-select-previous)
				 ("M-<" . company-select-first)
				 ("M->" . company-select-last))
	:hook
	(c++-mode-hook . (lambda () (when (file-remote-p default-directory) (company-mode -1)))))

(leaf yasnippet
  :straight t
  :blackout yas-minor-mode
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode))

(leaf denote
  :straight (denote
						 :type git
						 :host github
						 :repo "protesilaos/denote")
  :init
  (setq denote-directory "~/Documents/org/notes/")
  (setq denote-dired-directories
				(list denote-directory "~/Documents/org/journal/"))
	(denote-rename-buffer-mode 1)
  :config
	(defun denote-meeting-minutes ()
		(interactive)
		(denote
		 (format-time-string "Progress meeting %F")
		 '("meeting")))
	(defun my-denote-journal ()
		"Create an entry tagged 'journal' with the date as its title."
		(interactive)
		(let ((denote-directory "~/Documents/org/journal/"))
			(denote
			 (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
			 '("journal")))) ; multiple keywords are a list of strings: '("one" "two")
	(setq denote-backlinks-show-context t)
	(setq denote-known-keywords nil)
  :hook
  (dired-mode-hook . denote-dired-mode-in-directories)
	;; (denote-dired-mode-hook . dired-hide-details-mode)
	(denote-dired-mode-hook . meow-motion-mode)
  ;; (find-file-hook . denote-link-buttonize-buffer)
	(org-mode-hook . denote-rename-buffer-mode)
  :bind
  ("C-c n i" . denote-link-or-create)
  ("C-c n I" . denote-link-add-links)
  ("C-c n j" . my-denote-journal)
  ("C-c n b" . denote-link-backlinks)
	("C-c n m" . denote-meeting-minutes)
	("C-c n N" . denote)
	("C-c n n" . denote-open-or-create)
	("C-c n d" . (lambda () (interactive) (dired denote-directory))))

(leaf consult-notes
  :straight (consult-notes
						 :type git
						 :host github
						 :repo "mclear-tools/consult-notes")
  :init
  (setq consult-notes-sources '(("notes" ?n "~/Documents/org/notes/")
																("papers" ?p "~/Documents/org/papers/")))
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
	:config
	(setq consult-preview-key "M-.")
  :bind
  ("C-s" . consult-line)
  ("C-x r b" . consult-bookmark)
  ("C-c R" . consult-ripgrep)
	("C-c f" . consult-find))

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

(leaf switch-window
	:straight t
	:config
	(setq switch-window-shortcut-style 'qwerty)
	:bind
	("C-x o" . switch-window))

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

(leaf ef-themes
	:straight t)

;; (leaf doom-themes
;; 	:straight t)

;; (leaf standard-themes
;; 	:straight t
;; 	:config
;; 	(setq standard-themes-italic-constructs t))

(leaf nix-mode
	:straight t
	:leaf-defer
  :mode "\\.nix\\'")

(leaf fireplace
	:straight t)

(leaf markdown-mode
	:straight t)

(leaf Emacs-wgrep
	:straight (Emacs-wgrep
						 :type git
						 :host github
						 :repo "mhayashi1120/Emacs-wgrep")
	:config
	(setq wgrep-auto-save-buffer t)
	(setq wgrep-enable-key "r"))

(leaf eyebrowse
	:straight t
  :config
  (eyebrowse-mode t))

(leaf auto-dark
	:straight t
	:init
	(setq auto-dark-light-theme 'modus-operandi)
	(setq auto-dark-dark-theme 'modus-vivendi)
	(auto-dark-mode 1))

(provide 'init)
;;; init.el ends here
