(require 'erc)
(setq erc-server "irc.libera.chat"
			erc-nick "jakvb"
			erc-kill-buffer-on-part t
			erc-track-shorten-start 8
			erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs"))
			erc-auto-query 'bury)

(provide 'erc-config)
;;; erc-config.el ends here
