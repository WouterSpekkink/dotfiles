;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Wouter Spekkink"
      user-mail-address "wahsp@tutanota.com")

;; Font settings
(setq doom-font (font-spec :family "RobotoMono Nerd Font"  :size 18)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 15))

;; Theme
(setq doom-theme 'doom-dracula)

;; Set line-number style
(setq display-line-numbers-type 'relative)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Exit insert mode by pressing j twice quickly
(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)
(setq evil-escape-key-sequence nil)

;; doom-mode-line stuff
(setq doom-modeline-enable-word-count t)

;; mu4e setup
(after! mu4e
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
  (add-hook 'mu4e-compose-mode-hook (lambda() (use-hard-newlines -1))))

;; Email alert
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; Setup email account
(set-email-account! "essb"
                    '((mu4e-sent-folder                 .       "/essb/Sent")
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
                    t)

;; org-msg
(after! org-msg
  (setq org-msg-default-alternatives nil))

;; org-mode related stuff
(after! org
  ;; Set org directories
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-agenda-files (quote("~/org/"
                                "~/org/synced/"
                                "~/org/org-roam/"
                                "~/org/org-roam/daily/"
                                "~/org/org-roam/references/"
                                )))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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

  (setq org-agenda-include-diary t)
  (setq diary-file "~/.local/share/diary/outlook")

  ;; Kill capture frame
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  ;; org refile related stuff
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)
                                   ("~/org/org-roam/" :maxlevel . 9))))

  (setq org-refile-use-outline-path t)

  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (defun ws/verify-refile-target ()
    "Eclude todo keywords with a done state"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  ;; Prevent adding org agenda files
  (map! :map org-mode-map "C-c [" nil)

  ;; helm-bibtex related stuff
  (use-package! helm-bibtex
    :custom
    (bibtex-completion-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
    (reftex-default-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
    (bibtex-completion-pdf-field "file")
    :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))

  ;; Set up org-ref stuff
  (use-package! org-ref
    :custom
    (org-ref-default-bibliography "/home/wouter/Tools/Zotero/bibtex/library.bib")
    (org-ref-default-citation-link "citep")
    (org-ref-insert-link-function 'org-ref-insert-link-hydra/body)
    (org-ref-insert-cite-function 'org-ref-cite-insert-helm)
    (org-ref-insert-label-function 'org-ref-insert-label-link)
    (org-ref-insert-ref-function 'org-ref-insert-ref-link)
    (org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function
        (lambda (key) (car (bibtex-completion-find-pdf key)))
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        ;; For pdf export engines
        org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
        org-ref-notes-function 'orb-edit-notes)

  ;; Set up org-mode export stuff
  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        "/home/wouter/Tools/math2web/mathtoweb.jar")

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
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))

  ;; org-noter stuff
  (after! org-noter
    (setq
     org-noter-notes-search-path "~/org/org-roam/references/"
     org-noter-hide-other nil
     org-noter-separate-notes-from-heading t
     org-noter-always-create-frame t)
    (map!
     :after org-noter
     :map org-noter-notes-mode-map
     :desc "Insert note"
     "C-M-i" #'org-noter-insert-note
     :desc "Insert precise note"
     "C-M-p" #'org-noter-insert-precise-note
     :desc "Go to previous note"
     "C-M-k" #'org-noter-sync-prev-note
     :desc "Go to next note"
     "C-M-j" #'org-noter-sync-next-note
     :desc "Create skeleton"
     "C-M-s" #'org-noter-create-skeleton
     :desc "Kill session"
     "C-M-q" #'org-noter-kill-session
     )
    (map!
     :after org-noter
     :map org-noter-doc-mode-map
     :desc "Insert note"
     "C-M-i" #'org-noter-insert-note
     :desc "Insert precise note"
     "C-M-p" #'org-noter-insert-precise-note
     :desc "Go to previous note"
     "C-M-k" #'org-noter-sync-prev-note
     :desc "Go to next note"
     "C-M-j" #'org-noter-sync-next-note
     :desc "Create skeleton"
     "C-M-s" #'org-noter-create-skeleton
     :desc "Kill session"
     "C-M-q" #'org-noter-kill-session
     )
    )
  (after! org-roam
    (setq org-roam-directory "~/org/org-roam/")

    (add-hook 'after-init-hook 'org-roam-mode)

    ;; org-roam-bibtex stuff
    (use-package! org-roam-bibtex)
    (org-roam-bibtex-mode)

    (setq orb-preformat-keywords
          '("citekey" "title" "url" "author-or-editor" "keywords" "file")
          orb-process-file-keyword t
          orb-file-field-extensions '("pdf"))

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
                        "#+title: %<%Y-%m-%d>\n"))))

    ;; Function to capture quotes from pdf
    (defun org-roam-capture-pdf-active-region ()
      (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
             (pdf-buf (get-buffer pdf-buf-name)))
        (if (buffer-live-p pdf-buf)
            (with-current-buffer pdf-buf
              (car (pdf-view-active-region-text)))
          (user-error "Buffer %S not alive" pdf-buf-name))))

    ;; For org-roam-ui
    (package! org-roam-ui)
    (use-package! websocket)
    (use-package! org-roam-ui
      :config
      (setq org-roam-ui-sync-theme t
            org-roam-ui-follow t
            org-roam-ui-update-on-save t))

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
    ))

;; This is to use pdf-tools instead of doc-viewer
(use-package! pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; For deft
(after! deft
  (setq deft-extensions '("org")
        deft-directory "~/org/org-roam/"
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t))

;; For textklintrc
(after! flycheck
  (setq flycheck-textlint-config "~/.config/textlint/textlintrc.json")
  (setq flycheck-textlint-executable "~/npm-workspace/node_modules/.bin/textlint")
  )

;; For calendar support
(defun my-open-calendar()
  "Opens calendar."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    )))

;; Spelling related
(global-set-key (kbd "C-c N")
                (lambda()(interactive)
                  (ispell-change-dictionary "dutch")
                  (flyspell-buffer)))

;; For inline evaluation of elisp
(eros-mode 1)

;; Setting default dictionary
(setq ispell-dictionary "en_GB")

;; Adding some new global keys
(map! :leader
      :desc "Open calendar"
      "o c" #'my-open-calendar)

(map! :leader
      :desc "Org noter"
      "n p" #'org-noter)

(map! :leader
      :desc "Open literature database"
      "o l" #'helm-bibtex)

(map! :map helm-map
      "C-j" #'helm-next-line
      "C-k" #'helm-previous-line)

