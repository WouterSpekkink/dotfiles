(general-define-key
 :states '(normal visual)
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

 "o" '(:ignore t :wk "open")
 "om" '(mu4e :wk "email")
 "ol" '(helm-bibtex :wk "literature")
 "oa" '(org-agenda :wk "agenda")
 "ot" '(treemacs :wk "treemacs")

 "n" '(:ignore t :wk "notes")
 "nf" '(org-roam-node-find :wk "find note")
 "ni" '(org-roam-node-insert :wk "insert note")
 "nd" '(deft :wk "deft")
 
 "X" '(org-capture :wk "org capture")
 "U" '(straight-pull-all :wk "update all packages"))

;  (setq lsp-keymap-prefix "C-c l")
 ; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
 ; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(general-define-key
 :prefix "SPC"
 :states '(normal visual)
 :non-normal-prefix "M-SPC"
 :keymaps '(c++-mode-map c-mode-map)
 "d" '(disaster :wk "disaster"))

(general-define-key
 :prefix "SPC"
 :states '(normal visual)
 :non-normal-prefix "M-SPC"
 :keymaps 'org-mode-map
 "e" '(org-export-dispatch :wk "export"))

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
