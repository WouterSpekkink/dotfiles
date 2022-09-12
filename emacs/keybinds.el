(general-define-key
 :states '(normal visual insert replace emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
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
 "wq" '(evil-window-delete :wk "delete window")

 "o" '(:ignore t :wk "open")
 "om" '(mu4e :wk "email")
 "ol" '(helm-bibtex :wk "literature")
 "oa" '(org-agenda :wk "agenda")

 "n" '(:ignore t :wk "notes")
 "nf" '(org-roam-node-find :wk "find note")
 "ni" '(org-roam-node-insert :wk "insert note")
 "nd" '(deft :wk "deft")
 "np" '(org-noter :w "noter")
 
 "X" '(org-capture :wk "org capture")
 "U" '(straight-pull-all :wk "update all packages")
 "u" '(universal-argument :wk "universal argument")

 "i" '(:ignore t :wk "insert")
 "ic" '(insert-char :wk "character")

 "t" '(:ignore t :wk "treemacs")
 "to" '(treemacs :wk "open browser")
 "tw" '(:ignore t :wk "workspaces")
 "tws" '(treemacs-switch-workspace :wk "switch workspace")
 "tes" '(treemacs-edit-workspaces :wk "edit workspaces")
 )

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

(general-define-key
 :prefix "SPC"
 :states '(normal visual insert replace emacs)
 :non-normal-prefix "M-SPC"
 :keymaps 'org-mode-map
 "e" '(org-export-dispatch :wk "export")
 "ir" '(org-ref-insert-cite-link :wk "citation"))

(eval-after-load "org-mode"
  (general-define-key
   :keymaps 'org-mode-map
   "C-c [" nil
   "C-c ]" 'org-ref-insert-link
   "s-[" 'org-ref-insert-link-hydra/body))

(eval-after-load "org-noter"
  (general-define-key
   :keymaps '(org-noter-insert-note org-noter-doc-mode-map)
   "C-M-i" 'org-noter-insert-note
   "C-M-p" 'org-noter-insert-precise-note
   "C-M-k" 'org-noter-sync-prev-note
   "C-M-j" 'org-noter-sync-next-note
   "C-M-s" 'org-noter-create-skeleton
   "C-M-q" 'org-noter-kill-session))
