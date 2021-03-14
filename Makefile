# Automatic configuration setup

BASE = $(PWD)
MKDIR = mkdir -p
LN = ln -vsf
SLN = sudo ln -vsf
LNDIR = ln -vs
PKGINSTALL = sudo pacman --noconfirm -S

init: ## deploy dotfiles
	$(LN) $(PWD)/zsh/.zshrc $(HOME)/.config/zsh/.zshrc
	$(LN) $(PWD)/doom/init.el $(HOME)/.config/doom/init.el
	$(LN) $(PWD)/doom/config.el $(HOME)/.config/doom/config.el
	$(LN) $(PWD)/doom/packages.el $(HOME)/.config/doom/packages.el
	$(LN) $(PWD)/nvim/init.vim $(HOME)/.config/nvim/init.vim
	$(LN) $(PWD)/picom/picom.conf $(HOME)/.config/picom/picom.conf
	$(LN) $(PWD)/starship/starship.toml $(HOME)/.config/starship.toml
	$(LN) $(PWD)/textlint/textlintrc.json $(HOME)/.config/textlint/textlintrc.json
	$(LN) $(PWD)/exchange2org/exchange2orgconfig.py $(HOME)/.config/exchange2org/exchange2orgconfig.py
	$(SLN) $(PWD)/profile/profile /etc/profile
