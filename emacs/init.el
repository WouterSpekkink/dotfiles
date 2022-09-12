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

;; Starting buffer
(setq initial-buffer-choice t)

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
  (setq evil-want-C-u-scroll t)
  (evil-mode 1))

;; Vim Bindings Everywhere else
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;
(use-package org
  :straight t (:type built-in)
  :config
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9)
				   ("~/org/org-roam/" :maxlevel . 9))))
  (setq org-agenda-files (quote("~/org/"
				"~/org/synced/"
				"~/org/org-roam/"
				"~/org/org-roam/daily/"
				"~/org/org-roam/references/"
				)))
  
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (dolist (hook '(org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
    (add-hook 'org-mode-hook (lambda () (writegood-mode 1))))
  (setq org-ellipsis " ▼ ")
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)

  ;; org keyword related stuff
  (setq org-todo-keywords
	(quote ((sequence
		 "TODO(t)"
		 "PROJ(p)"
		 "LOOP(r)"
		 "STRT(s)"
		 "IDEA(i)"
		 "NEXT(n)"
		 "|"
		 "DONE(d)")
		(sequence
		 "WAIT(w@/!)"
		 "HOLD(h@/!)"
		 "|"
		 "KILL(k@/!)")
		(sequence
		 "[ ](T)"
		 "[-](S)"
		 "[?](W)"
		 "|"
		 "[X](D)"
		 ))))

  (setq org-todo-keyword-faces
	(quote (
		("NEXT" +-lock-constant-face bold))))

  (setq org-todo-state-tags-triggers
	(quote (("KILL" ("KILL" . t))
		("WAIT" ("WAIT" . t))
		("HOLD" ("WAIT") ("HOLD" . t))
		(done ("WAIT") ("HOLD"))
		("TODO" ("WAIT") ("KILL") ("HOLD"))
		("NEXT" ("WAIT") ("KILL") ("HOLD"))
		("DONE" ("WAIT") ("KILL") ("HOLD")))))

  ;; org capture related stuff
  (setq org-capture-templates
	(quote (("r" "respond" entry (file+headline "~/org/refile.org" "Emails")
		 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
		("p" "project" entry (file+headline "~/org/refile.org" "Projects")
		 "* PROJ %?\n%U\n%a\n")
		("t" "todo" entry (file+headline "~/org/refile.org" "Tasks")
		 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n")
		("i" "idea" entry (file+headline "~/org/refile.org" "Ideas")
		 "* IDEA %?\n%U\n%a\n")
		("e" "external" entry (file+headline "~/org/refile.org" "External")
		 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n %(progn (setq kk/delete-frame-after-capture 1) \"\")")
		)))

  ;; Caldav sync
  (setq diary-location "~/.local/share/diary/")

  (setq calendars
	'(("outlook" . "http://localhost:1080/users/45995wsp@eur.nl/calendar/")
	  ))

  (setq org-agenda-include-diary t)
  (setq diary-file "~/.local/share/diary/outlook")

  ;; Kill capture frame
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  (setq org-refile-use-outline-path t)

  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; Set up org-mode export stuff
  (setq org-latex-to-mathml-convert-command
	"java -jar %j -unicode -force -df %o %I"
	org-latex-to-mathml-jar-file
	"/home/wouter/Tools/math2web/mathtoweb.jar"))

;; Add latex classes; needs to be done after loading ox-latex
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("apa6"
		 "\\documentclass{apa6}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("report"
		 "\\documentclass{report}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
	       '("koma-article"
		 "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("memoir"
		 "\\documentclass{memoir}"
		 ("\\book{%s}" . "\\book*{%s}")
		 ("\\part{%s}" . "\\part*{%s}")
		 ("\\chapter{%s} .\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("paper"
		 "\\documentclass{paper}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; org-bullets
(use-package org-bullets
  :straight t
  :after org)

;; Set up org-ref stuff
(use-package org-ref
  :straight t
  :after org
  :custom
  (org-ref-default-bibliography "/home/wouter/Tools/Zotero/bibtex/library.bib")
  (org-ref-default-citation-link "citep")
  (org-ref-insert-link-function 'org-ref-insert-link-hydra/body)
  (org-ref-insert-cite-function 'org-ref-cite-insert-helm)
  (org-ref-insert-label-function 'org-ref-insert-label-link)
  (org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

  (setq org-ref-completion-library 'org-ref-ivy-cite
	org-export-latex-format-toc-function 'org-export-latex-no-toc
	org-ref-get-pdf-filename-function
	(lambda (key) (car (bibtex-completion-find-pdf key)))
	org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
	;; For pdf export engines
	org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
	org-ref-notes-function 'orb-edit-notes))

;; org-noter stuff
(use-package org-noter
  :straight t
  :config
  (setq
   org-noter-notes-search-path "~/org/org-roam/references/"
   org-noter-hide-other nil
   org-noter-separate-notes-from-heading t
   org-noter-always-create-frame t))

(use-package org-roam
  :straight t
  :after org
  :config
  (setq org-roam-directory "~/org/org-roam/")
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)
  ;; (add-hook 'after-init-hook 'org-roam-mode)

  ;; Let's set up some org-roam capture templates
  (setq org-roam-capture-templates
	(quote (("d" "default" plain
		 "%?"
		 :target
		 (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
		 :unnarrowed t)
		("r" "bibliography reference" plain
		 (file "~/org/org-roam/templates/orb-capture")
		 :target
		 (file+head "references/${citekey}.org" "#+title: ${title}\n")))))

  ;; And now we set necessary variables for org-roam-dailies
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* %?"
	   :target
	   (file+head "%<%Y-%m-%d>.org"
		      "#+title: %<%Y-%m-%d>\n")))))

;; For org-roam-ui
(use-package websocket
  :straight t
  :after org-roam)
(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t))

;; org-roam-bibtex stuff
(use-package org-roam-bibtex
  :straight t
  :after org-roam
  :config
  (org-roam-bibtex-mode)
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf")))

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

;; org-msg
(use-package org-msg
  :straight t
  :after mu4e
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

;; Email alert
(use-package mu4e-alert
  :straight t
  :after mu4e
  :config
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(use-package flyspell-correct-ivy
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package langtool
  :straight t
  :config
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-default-language "en-GB"))

(use-package writegood-mode
  :straight t)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-show-numbers            t
	company-minimum-prefix-length   1
	company-idle-delay              0.5
	company-backends
	'((company-files          ; files & directory
	   company-keywords       ; keywords
	   company-capf           ; what is this?
	   company-yasnippet)
	  (company-abbrev company-dabbrev))))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(use-package wc-mode
  :straight t
  :config
  (add-to-list 'global-mode-string '("" wc-buffer-stats)))

(use-package general
  :straight t
  :config
  (load "~/.config/emacs/keybinds.el")) 

(setq delete-by-moving-to-trash t)

;; Get rid of stupid sound
(setq visible-bell 1)

;; Auto-follow symbolic links
(setq vc-follow-symlinks t)

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

;; helm-bibtex
(use-package helm-bibtex
  :straight t
  :custom
  (bibtex-completion-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
  (reftex-default-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
  (bibtex-completion-pdf-field "file")
  :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))

;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; For deft
(use-package deft
  :straight t
  :config
  (setq deft-extensions '("org")
	deft-directory "~/org/org-roam/"
	deft-recursive t
	deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
	deft-use-filename-as-title t))

;; Treemacs
(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package anaconda-mode
  :straight t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME" "~/.config/pyenv/versions"))

;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;

(use-package rustic
  :straight t)

;;;;;;;;;;;;;;;
;; lsp stuff ;;
;;;;;;;;;;;;;;;

;; lsp
(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
	treemacs-space-between-root-nodes nil
	company-idle-delay 0.0
	company-minimum-prefix-length 1
	lsp-idle-delay 0.1)
  (setq lsp-lens-enable nil) ;This resolves extreme cpu use 
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . evil-normalize-keymaps)))

(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package yasnippet
  :straight t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package ccls
  :after lsp-mode
  :straight t
  :config
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
	'(:index (:comments 2) :completion (:detailedLabel t))))

(use-package modern-cpp-font-lock
  :straight t)

(use-package disaster
  :straight t)

;; flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list)

  (setq flycheck-indication-mode nil))

(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

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

;; All the icons in dired
(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;
;; New functions ;;
;;;;;;;;;;;;;;;;;;;
(defun ws/verify-refile-target ()
  "Eclude todo keywords with a done state"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(defun getcal (url file)
  "Download ics file and add it to file"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile file)
    (kill-buffer (car (last (split-string tmpfile "/"))))))

(defun getcals ()
  "Load a set of ICS calendars into Emacs diary files"
  (interactive)
  (mapcar #'(lambda (x)
	      (let ((file (concat diary-location (car x)))
		    (url (cdr x)))
		(message (concat "Loading " url " into " file))
		(find-file file)
		;; (flush-lines "^[& ]") ;; if you import ical as non marking
		(erase-buffer) ;; to avoid duplicating events
		(getcal url file)
		))
	  calendars))


(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
		 (file-exists-p filename))
	(delete-file filename))
      (kill-buffer buffer))))

(defun kk/delete-frame-if-neccessary (&rest r)
  (cond
   ((= kk/delete-frame-after-capture 0) nil)
   ((> kk/delete-frame-after-capture 1)
    (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
   (t
    (setq kk/delete-frame-after-capture 0)
    (delete-frame))))

(defun ws/verify-refile-target ()
  "Eclude todo keywords with a done state"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
	(find-file pdf-file)
      (message "No PDF found for %s" key))))

(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
	    depth)))

(defun org-roam-capture-pdf-active-region ()
  (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
	 (pdf-buf (get-buffer pdf-buf-name)))
    (if (buffer-live-p pdf-buf)
	(with-current-buffer pdf-buf
	  (car (pdf-view-active-region-text)))
      (user-error "Buffer %S not alive" pdf-buf-name))))

;; Workaround for org-roam minibuffer issues
(defun my/org-roam-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
  TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
			 template
			 node
			 (1- (frame-width)))))
    (cons (propertize candidate-main 'node node) node)))
(advice-add 'org-roam-node-read--to-candidate :override #'my/org-roam-node-read--to-candidate)