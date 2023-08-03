# Automatic configuration setup
# Inspired by Gavin Freeborn's example:
# https://www.youtube.com/watch?v=aP8eggU2CaU

BASE = $(PWD)
MKDIR = mkdir -p
LN = ln -vsf
SLN = sudo ln -vsf
LNDIR = ln -vs
PKGINSTALL = sudo pacman --noconfirm -S

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

init: ## deploy dotfiles
	$(LN) $(PWD)/zsh/.zshrc $(HOME)/.config/zsh/.zshrc
	$(LN) $(PWD)/nvim/init.vim $(HOME)/.config/nvim/init.vim
	$(LN) $(PWD)/picom/picom.conf $(HOME)/.config/picom/picom.conf
	$(LN) $(PWD)/starship/starship.toml $(HOME)/.config/starship.toml
	$(LN) $(PWD)/textlint/textlintrc.json $(HOME)/.config/textlint/textlintrc.json
	$(LN) $(PWD)/isync/mbsyncrc $(HOME)/.config/isync/mbsyncrc
	$(LN) $(PWD)/dwm/autostart.sh $(HOME)/.local/share/dwm/autostart.sh
	$(LN) $(PWD)/emacs/init.el $(HOME)/.config/emacs/init.el
	$(LN) $(PWD)/emacs/keybinds.el $(HOME)/.config/emacs/keybinds.el
	$(LN) $(PWD)/newsboat/config $(HOME)/.config/newsboat/config
	$(LN) $(PWD)/newsboat/urls $(HOME)/.config/newsboat/urls
	$(SLN) $(PWD)/profile/profile /etc/profile

execs: 
	$(LN) $(PWD)/scripts/gemtoot $(HOME)/.local/bin/gemtoot
	$(LN) $(PWD)/scripts/dmenuconfig $(HOME)/.local/bin/dmenuconfig
	$(LN) $(PWD)/scripts/checkaudio $(HOME)/.local/bin/checkaudio
	$(LN) $(PWD)/scripts/dmenupass $(HOME)/.local/bin/dmenupass
	$(LN) $(PWD)/scripts/welcomemessage $(HOME)/.local/bin/welcomemessage
	$(LN) $(PWD)/scripts/quitcmd $(HOME)/.local/bin/quitcmd
	$(LN) $(PWD)/scripts/remaps $(HOME)/.local/bin/remaps
	$(LN) $(PWD)/scripts/sb-clock $(HOME)/.local/bin/sb-clock
	$(LN) $(PWD)/scripts/sb-battery $(HOME)/.local/bin/sb-battery
	$(LN) $(PWD)/scripts/sb-cpu $(HOME)/.local/bin/sb-cpu
	$(LN) $(PWD)/scripts/sb-memory $(HOME)/.local/bin/sb-memory
	$(LN) $(PWD)/scripts/sb-volume $(HOME)/.local/bin/sb-volume
	$(LN) $(PWD)/scripts/sb-internet $(HOME)/.local/bin/sb-internet
	$(LN) $(PWD)/scripts/sb-forecast $(HOME)/.local/bin/sb-forecast
	$(LN) $(PWD)/scripts/org-capture $(HOME)/.local/bin/org-capture
	$(LN) $(PWD)/scripts/screenshot $(HOME)/.local/bin/screenshot
	$(LN) $(PWD)/scripts/bookmark-this $(HOME)/.local/bin/bookmark-this

install: ## Install arch linux packages
	$(PKGINSTALL) --needed - < $(PWD)/archlinux/pacmanlist
	sudo pkgfile --update

aur: ## Install arch linux AUR packages using yay
	yay -S --needed - < $(PWD)/archlinux/aurlist

backup: ## Backup arch linux packages
	$(MKDIR) $(PWD)/archlinux
	pacman -Qnq > $(PWD)/archlinux/pacmanlist
	pacman -Qqem > $(PWD)/archlinux/aurlist
