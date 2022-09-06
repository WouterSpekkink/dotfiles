;;;;;;;;;;;;;;
;; STARTUP  ;;
;;;;;;;;;;;;;;

;; Straight package management
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
(setq package-enable-at-startup nil)

;; Use-package
(straight-use-package 'use-package)

;; Undo
(use-package undo-fu
  :straight t)

;; Which key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer)
  (setq max-mini-window-height 0.5))

;;;;;;;;;;;;;;;
;; VIM STUFF ;;
;;;;;;;;;;;;;;;

;;; Vim Bindings
(use-package evil
  :straight t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search_
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Vim Bindings Everywhere else
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(load "~/.config/emacs/keybinds.el")

;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;
(use-package org
  :straight t
  :config
  (setq org-agenda-files (quote("~/org/")))
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9)
				   ("~/org/org-roam/" :maxlevel . 9))))

  (setq org-refile-use-outline-path t)

  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (defun ws/verify-refile-target ()
    "Eclude todo keywords with a done state"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

  (use-package org-bullets
    :straight t))

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :straight t)

;;;;;;;;;;;
;; Email ;;
;;;;;;;;;;;

(use-package mu4e
  :straight t
  :config
  (setq mu4e-user-mail-address-list '("spekkink@essb.eur.nl"))
  ;; viewing options
  (setq mu4e-view-show-addresses t)
  ;; Do not leave message open after it has been sent
  (setq message-kill-buffer-on-exit t)
  ;; Don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; Don't ask to quit
  (setq mu4e-confirm-quit nil)
  (setq mu4e-root-maildir (expand-file-name "~/.local/share/mail/essb")
	mu4e-get-mail-command "mbsync -a -c \"$XDG_CONFIG_HOME/isync/mbsyncrc\""
	mu4e-index-update-in-background t
	mu4e-use-fancy-chars t
	mu4e-view-show-addresses t
	mu4e-view-show-images t
	mu4e-compose-format-flowed t
	mu4e-compose-signature-auto-include nil
	mu4e-view-use-gnus t
	mu4e-change-filenames-when-moving t
	message-send-mail-function 'smtpmail-send-it
	message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
	message-citation-line-function 'message-insert-formatted-citation-line
	message-kill-buffer-on-exit t
	org-mu4e-convert-to-html t)
  (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
  (add-hook 'mu4e-compose-mode-hook (lambda() (use-hard-newlines -1)))

  ;; org-msg
  (use-package org-msg
    :straight t
    :config
    (setq mail-user-agent 'mu4e-user-agent)
    (setq org-msg-default-alternatives nil)
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	  org-msg-startup "hidestars indent inlineimages"
	  org-msg-recipient-names '(("spekkink@essb.eur.nl" . "Wouter"))
	  org-msg-greeting-name-limit 3
	  org-msg-default-alternatives '((new		. (text html))
					 (reply-to-html	. (text html))
					 (reply-to-text	. (text)))
	  org-msg-convert-citation t)
    (org-msg-mode))

  ;; Setup email account
  (setq mu4e-contexts
	`(
	  ,(make-mu4e-context
	    :name "essb"
	    :match-func (lambda (msg)
			  (when msg
			    (mu4e-message-contact-field-matches
			     msg '(:from :to :cc :bcc) "spekkink@essb.eur.nl")))
	    :vars '((mu4e-sent-folder                 .       "/essb/Sent")
		    (mu4e-drafts-folder               .       "/essb/Drafts")
		    (mu4e-trash-folder                .       "/essb/Trash")
		    (mu4e-refile-folder               .       "/essb/INBOX")
		    (message-send-mail-function       .       smtpmail-send-it)
		    (smtpmail-smtp-user               .       "45995wsp@eur.nl")
		    (smtpmail-smtp-server             .       "localhost")
		    (smtpmail-smtp-service            .       1025)
		    (smtpmail-stream-type             .       nil)
		    (user-mail-address                .       "spekkink@essb.eur.nl")
		    (mu4e-update-interval             .       300))
	    ))))
;; Email alert
;(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)


;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(use-package flyspell-correct-ivy
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

;; Ivy
(use-package ivy
  :straight t
  :config
  (ivy-mode 1))

;; Line numbers
(global-display-line-numbers-mode)
(menu-bar--display-line-numbers-mode-relative)

;; Mode-line
(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode))

;; ibuffer
(use-package ibuffer
  :straight t
  :config
  (add-hook 'ibuffer-mode-hook
	    #'(lambda ()
	       (ibuffer-switch-to-saved-filter-groups "home")))
  (setq ibuffer-saved-filter-groups
	(quote (("home"
		 ("dired" (mode . dired-mode))
		 ("emacs-config" (or (filename . "init.el")
				     (filename . "keybinds.el")))
		 ("Org" (or (mode . org-mode)
			    (filename . "OrgMode")))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
			   (name . "^\\*straight-process\\*$")
			   (name . "^\\*GNU Emacs*\\*$")))
		 ("Help" (or (name . "\*Help\*")
			     (name . "\*Apropos\*")
			     (name . "\*info\*"))))))))		      

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package anaconda-mode
  :straight t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))


;;;;;;;;;;;
;; Looks ;;
;;;;;;;;;;;

;; Set theme
(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t))

;; Set font
(add-to-list 'default-frame-alist '(font . "DejaVuSansMono NF Book 13"))
(set-face-attribute 'default t :font "DejaVuSansMono NF Book 13") 

;; Tilde fringe
(use-package vi-tilde-fringe
  :straight t
  :config
  (global-vi-tilde-fringe-mode))

;; All the icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("735561d82728e28f275802fc875c3a2caf14d06f434604a7516c59d49120b163" default))
 '(package-selected-packages '(ivy dracula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
