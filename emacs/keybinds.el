;; Evil-leader
(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
  "f" '("file" . (keymap))
  "ff" '("find file" . find-file)
  "fd" '("delete file" . delete-file)

  "b" '("buffer" . (keymap))
  "bb" '("switch buffer" . switch-to-buffer)
  "bk" '("kill buffer" . kill-buffer)
  "bl" '("buffer list" . ibuffer)

  "w" '("window" . (keymap))
  "ww" '("next window" . evil-window-next)
  "wp" '("previous window" . evil-window-prev)

  "o" '("open" . (keymap))
  "om" '("email" . mu4e)
  ))

