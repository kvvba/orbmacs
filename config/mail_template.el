(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)

(auth-source-pass-enable)
(setq auth-source-debug t)
(setq auth-source-do-cache nil)
(setq auth-sources '(password-store))
(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-compose-complete-addresses t)
(setq mu4e-compose-context-policy nil)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-compose-keep-self-cc nil)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval 120)
(setq mu4e-headers-auto-update t)
(setq mu4e-index-update-in-background t)
(setq mu4e-headers-date-format "%d-%m-%Y %H:%M")
(setq mu4e-headers-fields '((:human-date . 20)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from . 22)
                            (:subject)))
(setq mu4e-headers-include-related t)
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-view-show-addresses t)
(setq mu4e-view-show-images t)
(setq smtpmail-debug-info t)
(setq smtpmail-stream-type 'starttls)
(setq mm-sign-option 'guided)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; (defun sign-or-encrypt-message ()
;;   (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
;;     (cond
;;      ((string-equal answer "s") (progn
;;                                   (message "Signing message.")
;;                                   (mml-secure-message-sign-pgpmime)))
;;      ((string-equal answer "e") (progn
;;                                   (message "Encrypt and signing message.")
;;                                   (mml-secure-message-encrypt-pgpmime)))
;;      (t (progn
;;           (message "Dont signing or encrypting message.")
;;           nil)))))

;; (add-hook 'message-send-hook 'sign-or-encrypt-message)

(setq mu4e-maildir "~/.mail")

(setq mu4e-contexts
			(list
			 (make-mu4e-context
				:name "outlook"
				:enter-func (lambda () (mu4e-message "Entering outlook context"))
				:leave-func (lambda () (mu4e-message "Leaving outlook context"))
				:match-func
				(lambda (msg)
					(when msg
						(mu4e-message-contact-field-matches msg :to "username@outlook.com")))
				:vars '( ( user-mail-address . "username@outlook.com" )
								 ( user-full-name . "Firstname Lastname" )
								 ( mu4e-sent-folder . "/outlook/Sent" )
								 ( mu4e-drafts-folder . "/outlook/Drafts" )
								 ( mu4e-trash-folder . "/outlook/Deleted" )
								 ( smtpmail-smtp-user . "username@outlook.com" )
								 ( smtpmail-smtp-server . "smtp-mail.outlook.com" )
								 ( smtpmail-smtp-service . "587" )
								 ( smtpmail-stream-type . starttls)
								 ( mu4e-maildir-shortcuts . (("/outlook/Inbox"   . ?i)
																						 ("/outlook/Sent"    . ?s)
																						 ("/outlook/Drafts"  . ?d)
																						 ("/outlook/Deleted" . ?t)
																						 ("/outlook/Junk"    . ?j)
																						 ("/outlook/Archive" . ?a)
																						 ("/outlook/Outbox"  . ?o)) ) ))
			 
			 (make-mu4e-context
				:name "posteo"
				:enter-func (lambda () (mu4e-message "Entering posteo context"))
				:leave-func (lambda () (mu4e-message "Leaving posteo context"))
				:match-func
				(lambda (msg)
					(when msg
						(mu4e-message-contact-field-matches msg :to "username@posteo.net")))
				:vars '( ( user-mail-address . "username@posteo.net" )
								 ( user-full-name . "Firstname Lastname" )
								 ( mu4e-sent-folder . "/posteo/Sent" )
								 ( mu4e-drafts-folder . "/posteo/Drafts" )
								 ( mu4e-trash-folder . "/posteo/Trash" )
								 ( smtpmail-smtp-user . "username@posteo.net" )
								 ( smtpmail-smtp-server . "posteo.de" )
								 ( smtpmail-smtp-service . "587" )
								 ( smtpmail-stream-type . starttls)
								 ( mu4e-maildir-shortcuts . (("/posteo/Inbox"  . ?i)
																						 ("/posteo/Sent"   . ?s)
																						 ("/posteo/Drafts" . ?d)
																						 ("/posteo/Trash"  . ?t)) ) ))))

(defun mu4e-other-window ()
	"Open mu4e in new window."
  (interactive)
	(progn
		(split-window-sensibly (selected-window))
		(other-window 1)
		(mu4e)))

(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c M") 'mu4e-other-window)

(mu4e t)

(provide 'mail)
;;; mail.el ends here
