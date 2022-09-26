;; Setup evil
(general-evil-setup)
(evil-collection-init)

(general-define-key
 :states '(normal visual insert motion emacs)
 :keymaps '(override global)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "" '(nil :wk "leader")
 "f" '(:ignore t :wk "file")
 "ff" '(find-file :wk "find file")
 "fd" '(delete-file :wk "delete file")

 "b" '(:ignore t :wk "buffers")
 "bb" '(switch-to-buffer :wk "switch buffer")
 "bk" '(kill-buffer :wk "kill buffer")
 "bl" '(ibuffer :wk "list buffers")

 "w" '(:ignore t :wk "windows")
 "ww" '(evil-window-next :wk "next window")
 "wp" '(evil-window-prev :wk "previous window")
 "w|" '(evil-window-set-width :wk "set window width")
 "wq" '(evil-window-delete :wk "delete window"p)

 "o" '(:ignore t :wk "open")
 "om" '(mu4e :wk "email")
 "ol" '(helm-bibtex :wk "literature")
 "oa" '(org-agenda :wk "agenda")

 "n" '(:ignore t :wk "notes")
 "nd" '(deft :wk "deft")
 "np" '(org-noter :w "noter")
 "nr" '(:ignore t :wk "org-roam")
 "nrf" '(org-roam-node-find :wk "find note")
 "nri" '(org-roam-node-insert :wk "insert note")
 "nrr" '(org-roam-buffer-toggle :wk "buffer")
 
 "X" '(org-capture :wk "org capture")
 "U" '(straight-pull-all :wk "update all packages")
 "u" '(universal-argument :wk "universal argument")

 "i" '(:ignore t :wk "insert")
 "ic" '(insert-char :wk "character")

 "t" '(:ignore t :wk "treemacs")
 "to" '(treemacs :wk "open browser")
 "tw" '(:ignore t :wk "workspaces")
 "tws" '(treemacs-switch-workspace :wk "switch workspace")
 "twe" '(treemacs-edit-workspaces :wk "edit workspaces"))

(general-define-key
 :keymaps 'flyspell-mode-map
 :states 'normal
 "C-;" 'flyspell-correct-wrapper
 "C-'" '+flyspell-add-word)

(global-set-key (kbd "C-c N")
                (lambda()(interactive)
                  (ispell-change-dictionary "nl_NL")
                  (flyspell-buffer)))

(general-define-key
 :keymaps 'org-agenda-mode-map
 :states 'motion
 :prefix "SPC"
 "" '(nil :wk "leader")
 "m" '(:ignore t :wk "localleader")
 "mc" '(:ignore t :wk "clock")
 "mci" '(org-agenda-clock-in :wk "clock in")
 "mco" '(org-agenda-clock-out :wk "clock out")
 "md" '(:ignore t :wk "dates")
 "mds" '(org-agenda-schedule :wk "schedule")
 "mdd" '(org-agenda-deadline :wk "set deadline")
 "ms" '(:ignore t :wk "toggles")
 "msm" '(org-modern-mode :wk "org modern mode")
 "mt" '(:ignore t :wk "todo")
 "mtt" '(org-agenda-todo :wk "todo states")
 "mr" '(org-agenda-refile :wk "refile"))

(eval-after-load "org-mode"
  (general-define-key
   :keymaps 'org-mode-map
   "C-c [" nil
   "C-c ]" 'org-ref-insert-link
   "s-[" 'org-ref-insert-link-hydra/body
   "C-<return>" #'+org--insert-item-below
   "C-S-<return>" #'+org--insert-item-above))
(eval-after-load "org-mode"
  (general-define-key
    :keymaps 'org-mode-map
    :states 'normal
    "RET" 'org-open-at-point))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual insert replace emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "m" '(:ignore t :wk "localleader")
 "me" '(org-export-dispatch :wk "export")
 "mi" '(:ignore t :wk "insert")
 "mir" '(org-ref-insert-cite-link :wk "citation")
 "mt" '(:ignore t :wk "todo")
 "mtt" '(org-todo :wk "todo states")
 "ms" '(:ignore t :wk "toggles")
 "mst" '(visual-line-mode :wk "visual line mode")
 "msm" '(org-modern-mode :wk "org modern mode")
 "msr" '(rainbow-mode :wk "rainbow mode")
 "md" '(:ignore t :wk "dates")
 "mds" '(org-schedule :wk "schedule")
 "mdd" '(org-deadline :wk "set deadline")
 "mc" '(:ignore t :wk "clock")
 "mci" '(org-clock-in :wk "clock in")
 "mco" '(org-clock-out :wk "clock out")
 "mr" '(org-refile :wk "refile"))

(eval-after-load "org-noter"
  (general-define-key
   :keymaps '(org-noter-doc-mode-map org-noter-notes-mode-map)
   "C-M-i" 'org-noter-insert-note
   "C-M-p" 'org-noter-insert-precise-note
   "C-M-k" 'org-noter-sync-prev-note
   "C-M-j" 'org-noter-sync-next-note
   "C-M-s" 'org-noter-create-skeleton
   "C-M-q" 'org-noter-kill-session))

(eval-after-load "ivy"
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line))

(general-define-key
 :states '(normal visual replace emacs)
 :keymaps '(lsp-mode-map c-mode-map)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "l" '(:ignore t :wk "lsp")

 "lw" '(:ignore t :wk "workspaces")
 "lwD" '(lsp-disconnect :wk "disconnect")
 "lwd" '(lsp-describe-session :wk "describe session")
 "lwq" '(lsp-workspace-shutdown :wk "shutdown server")
 "lwr" '(lsp-workspace-restart :wk "restart server")
 "lws" '(lsp :wk "start server")

 "l=" '(:ignore t :wk "format")
 "l==" '(lsp-format-buffer :wk"format buffer")
 "l=r" '(lsp-format-region :wk "format region")

 "lF" '(:ignore t :wk "folders")
 "lFa" '(lsp-workspace-folders-add :wk "add folder")
 "lFb" '(lsp-workspace-blacklist-remove :wk "un-blacklist folder")
 "lFr" '(lsp-workspace-folders-remove :wk "remove folder")

 "lT" '(:ignore t :wk "toggles")
 "lTD" '(lsp-modeline-diagnostics-mode :wk "toggle modeline diagnostics")
 "lTL" '(lsp-toggle-trace-io :wk "toggle log io")
 "lTS" '(lsp-ui-sideline-mode :wk "toggle sideline")
 "lTT" '(lsp-treemacs-sync-mode :wk "toggle treemacs integration")
 "lTa" '(lsp-modeline-code-actions-mode :wk "toggle modeline code actions")
 "lTb" '(lsp-headerline-breadcrumb-mode :wk "toggle breadcrumb")
 "lTd" '(lsp-ui-doc-mode :wk "toggle documentation popup")
 "lTf" '(lsp-toggle-on-type-formatting :wk "toggle on type formatting")
 "lTh" '(lsp-toggle-symbol-highlight :wk "toggle highlighting")
 "lTl" '(lsp-lens-mode :wk "toggle lenses")
 "lTs" '(lsp-toggle-signature-auto-activate :wk "toggle signature")
 
 "lg" '(:ignore t :wk "goto")
 "lga" '(xref-find-apropos :wk "find symbol in workspace")
 "lgd" '(lsp-find-declaration :wk "find declarations")
 "lge" '(lsp-treemacs-errors-list :wk "show errors")
 "lgg" '(lsp-find-definition :wk "find definitions")
 "lgh" '(lsp-treemacs-call-hierarchy :wk "call hierarchy")
 "lgi" '(lsp-find-implementation :wk "find implementations")
 "lgr" '(lsp-find-references :wk "find references")
 "lgt" '(lsp-find-type-definition :wk "find type definition")

 "lh" '(:ignore t :wk "help")
 "lhg" '(lsp-ui-doc-glance :wk "glance symbol")
 "lhh" '(lsp-describe-thing-at-point :wk "describe symbol at point")
 "lhs" '(lsp-signature-activate :wk "signature help")

 "lr" '(:ignore t :wk "refactoring")
 "lro" '(lsp-organize-imports :wk "organize imports")
 "lrr" '(lsp-rename :wk "rename")

 "la" '(:ignore t :wk "actions")
 "laa" '(lsp-execute-code-action :wk "code actions")
 "lah" '(lsp-document-highlight :wk "highlight symbol")
 "lal" '(lsp-avy-lens :wk "lens")

 ;; peeks
 "lG" '(:ignore t :wk "peeks")
 "lGg" '(lsp-ui-peek-find-definitions :wk "peek definitions")
 "lGi" '(lsp-ui-peek-find-implementation :wk "peek implementations")
 "lGr" '(lsp-ui-peek-find-references :wk "peek references")
 "lGs" '(lsp-ui-peek-find-workspace-symbol :wk "peek workspace symbol"))

;; Everything below, so far, was shamelessly copied from doom emacs
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org--insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(defun +org--insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))

(defun +flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
