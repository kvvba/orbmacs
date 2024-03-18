  ;; Make emacs start faster
  ;; (setq startup/gc-cons-threshold gc-cons-threshold)
  ;; (setq gc-cons-threshold most-positive-fixnum)
  ;; (defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))

	;; (remove-hook 'find-file-hook 'vc-refresh-state)

  ;; Enable prettify symbols
  ;; (global-prettify-symbols-mode t)

  ;; Diable ring-bell
  ;; (setq ring-bell-function 'ignore)

;; (leaf dired-hacks-utils
;; 	:straight t
;; 	:after dired)

;; (leaf org-moden-indent
;; 	:straight (org-modern-indent
;; 						 :type git
;; 						 :host github
;; 						 :repo "jdtsmith/org-modern-indent")
;; 	:hook
;; 	(org-indent-mode-hook . org-modern-indent-mode))

;; (leaf org-pandoc-import
;;   :straight (org-pandoc-import
;; 						 :type git
;; 						 :host github
;; 						 :repo "tecosaur/org-pandoc-import"
;; 						 :files ("*.el" "filters" "preprocessors")))

;; (leaf mu4e
;;   :leaf-defer t
;;   :bind
;;   ("C-c z" . mu4e)
;;   ("C-c Z" . mu4e-other-window)
;;   (mu4e-main-mode-map
;;    ("e" . kill-current-buffer))
;;   (mu4e-headers-mode-map
;;    ("C-c c" . org-capture-mail))
;;   (mu4e-view-mode-map
;;    ("C-c c" . org-capture-mail))
;;   :defer-config
;;   (load "~/.emacs.d/config/mail-config.el"))

;; (leaf mu4e-alert
;;   :straight t
;;   :after mu4e
;;   :config
;;   (mu4e-alert-set-default-style 'libnotify)
;;   (mu4e-alert-enable-mode-line-display)
;;   (mu4e-alert-enable-notifications))

;; (leaf elfeed
;; 	:straight t
;; 	:init
;; 	(defun elfeed-other-frame ()
;; 		"Opens elfeed in a new frame."
;; 		(interactive)
;; 		(switch-to-buffer-other-frame "*elfeed-search*")
;; 		(elfeed))
;; 	:config
;; 	(setq elfeed-feeds
;; 				'(("https://rss.sciencedirect.com/publication/science/03019322" work fluids)
;; 					;; ("https://rss.sciencedirect.com/publication/science/13594311" work fluids)
;; 					("https://rss.sciencedirect.com/publication/science/0142727X" work fluids)
;; 					("https://rss.sciencedirect.com/publication/science/00457930" work fluids)
;; 					("https://www.mdpi.com/rss/journal/fluids" work fluids)
;; 					("https://www.cambridge.org/core/rss/product/id/1F51BCFAA50101CAF5CB9A20F8DEA3E4" work fluids)
;; 					("https://www.annualreviews.org/r/fluid_rss" work fluids)
;; 					("https://onlinelibrary.wiley.com/journal/10970363#" work fluids)
;; 					("https://juliacomputing.com/feed.xml" work prog)
;; 					("https://masteringemacs.org/feed" emacs)
;; 					("https://protesilaos.com/master.xml" prot)
;; 					("http://feeds.arstechnica.com/arstechnica/index" news tech)
;; 					("https://www.sciencedaily.com/rss/top/technology.xml" news tech)
;; 					("https://planet.emacslife.com/zh/atom.xml" emacs)))
;; 	:bind
;; 	;; ("C-c w" . elfeed)
;; 	;; ("C-c W" . elfeed-other-frame)
;; 	(elfeed-search-mode-map
;; 	 ("U" . elfeed-update))
;; 	:hook
;; 	(elfeed-show-mode-hook . visual-line-mode))

;; (leaf bongo
;;   :straight t
;;   :leaf-defer t
;;   :defer-config (load-file "~/.emacs.d/config/bongo-config.el")
;;   :bind
;;   (("<C-XF86AudioPlay>" . bongo-pause/resume)
;;    ("<C-XF86AudioNext>" . bongo-next)
;;    ("<C-XF86AudioPrev>" . bongo-previous)
;;    ("<M-XF86AudioPlay>" . bongo-show)
;;    ("<S-XF86AudioNext>" . bongo-seek-forward-10)
;;    ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
;;    ("C-c p"             . bongo)
;;    (bongo-playlist-mode-map
;; 		("n" . bongo-next-object)
;; 		("p" . bongo-previous-object)
;; 		("M-n" . prot/bongo-paylist-section-next)
;; 		("M-p" . prot/bongo-paylist-section-previous)
;; 		("M-h" . prot/bongo-playlist-mark-section)
;; 		("M-d" . prot/bongo-playlist-kill-section)
;; 		("g" . prot/bongo-playlist-reset)
;; 		("D" . prot/bongo-playlist-terminate)
;; 		("r" . prot/bongo-playlist-random-toggle)
;; 		("R" . bongo-rename-line)
;; 		("d" . bongo-dired-line)       ; Jump to dir of file at point
;; 		("J" . dired-jump)             ; Jump to library buffer
;; 		("i" . prot/bongo-playlist-insert-playlist-file)
;; 		("I" . bongo-insert-special))
;;    (bongo-dired-library-mode-map
;; 		("<C-return>" . prot/bongo-dired-insert)
;; 		("C-c SPC" . prot/bongo-dired-insert)
;; 		("C-c +" . prot/bongo-dired-make-playlist-file))))

;; (leaf tree-sitter
;; 	:straight t
;; 	:init (global-tree-sitter-mode))

;; (leaf tree-sitter-langs
;; 	:straight t)


;; (leaf leaf
;; 	:straight (org-jl
;; 						 :type git
;; 						 :host github
;; 						 :repo "tecosaur/Org.jl"))

;; (leaf eglot
;; 	:straight t
;; 	:leaf-defer t
;; 	:hook
;; 	(julia-mode-hook . eglot-ensure)
;; 	;; ((c-mode-hook c++-mode-hook) . eglot-ensure)
;; 	)

;; (leaf eglot-jl
;; 	:straight t
;; 	:init (eglot-jl-init))

;; (leaf denote-menu
;; 	:straight (denote-menu
;; 						 :type git
;; 						 :host github
;; 						 :repo "namilus/denote-menu"))

;; (leaf easy-jekyll
;; 	:straight t
;; 	:defer-config
;; 	(setq easy-jekyll-basedir "~/Documents/org/blog/")
;; 	(setq easy-jekyll-url "https://kvvba.github.io/")
;; 	:bind
;; 	("C-c j" . easy-jekyll))

;; (leaf async
;;   :straight t
;;   :init
;;   (dired-async-mode 1))

;; (leaf undo-tree
;; 	:straight t
;;   :blackout undo-tree-mode
;; 	:config
;; 	(global-undo-tree-mode t))

;; (leaf doom-modeline
;; 	:straight t
;; 	:init (doom-modeline-mode 1))

;; (leaf lambda-themes
;;   :straight (lambda-themes :type git :host github :repo "lambda-emacs/lambda-themes") 
;;   :custom
;;   (lambda-themes-set-italic-comments . t)
;;   (lambda-themes-set-italic-keywords . t)
;;   (lambda-themes-set-variable-pitch . nil) 
;;   :config
  ;; load preferred theme 
  ;; (load-theme 'lambda-light)
	;; )

;; (leaf flycheck
;;   :straight t
;;   :blackout t
;;   :init (global-flycheck-mode))

;; (leaf dashboard
;; 	:straight t
;; 	:config
;; 	(dashboard-setup-startup-hook)
;; 	(setq dashboard-startup-banner "~/.emacs.d/media/sicp.png")
;; 	(setq dashboard-center-content t)
;; 	(setq dashboard-banner-logo-title "Welcome to Orbmacs")
;; 	(setq dashboard-items '((recents  . 5)
;; 													(bookmarks . 15)))
;; 	(setq dashboard-set-footer nil))

;; (leaf olivetti
;; 	:straight t)

