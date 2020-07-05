all: mu

mu:
	mu init --my-address=${EMAIL} --maildir ${HOME}/Mail
