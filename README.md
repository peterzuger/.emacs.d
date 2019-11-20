<p align="center"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="emacs"></p>

<h3 align="center">~/.emacs.d</h3>

<div align="center">

  [![Status](https://img.shields.io/badge/status-active-success.svg)]()
  [![GitHub Issues](https://img.shields.io/github/issues/peterzuger/dotfiles.svg)](https://github.com/kylelobo/The-Documentation-Compendium/issues)
  [![GitHub Pull Requests](https://img.shields.io/github/issues-pr/peterzuger/dotfiles.svg)](https://github.com/kylelobo/The-Documentation-Compendium/pulls)
  [![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)

</div>

---

<p align="center"> My Emacs Configuration
    <br>
</p>

## 📝 Table of Contents
- [About](#about)
- [Getting Started](#getting_started)
- [Usage](#usage)
- [Authors](#authors)
- [Acknowledgments](#acknowledgement)

## 🧐 About <a name = "about"></a>
This Repository contains the emacs configuration of all of my computers.
This configuration is common across all my machines.

## 🏁 Getting Started <a name = "getting_started"></a>
These instructions will get you a copy of my emacs setup up and running on your local machine.

### Prerequisites
To use my ```~/.emacs.d``` you will need to install the following packages:

| Emacs Package | Prerequisites                                                                        | Link                                 |
|---------------|--------------------------------------------------------------------------------------|--------------------------------------|
| auctex        | [Texlive](https://www.archlinux.org/groups/x86_64/texlive-most/)                     | https://www.tug.org/texlive/         |
| ggtags        | [GNU global](https://aur.archlinux.org/packages/global/)                             | https://www.gnu.org/software/global/ |
| ggtags        | [ctags](https://www.archlinux.org/packages/extra/x86_64/ctags/)                      | http://ctags.sourceforge.net/        |
| magit         | [git](https://www.archlinux.org/packages/extra/x86_64/git/)                          | https://git-scm.com/                 |
| mu4e          | [offlineimap](https://www.archlinux.org/packages/community/any/offlineimap/)         | http://www.offlineimap.org/          |
| mu4e          | [mu](https://aur.archlinux.org/packages/mu/)                                         | https://www.djcbsoftware.nl/code/mu/ |
| jedi          | [python-virtualenv](https://www.archlinux.org/packages/extra/any/python-virtualenv/) | https://pypi.org/project/virtualenv/ |
| flyspell      | [aspell](https://www.archlinux.org/packages/extra/x86_64/aspell/)                    | http://aspell.net/                   |

To use this emacs configuration, just clone this repo with:

```
git clone --recursive https://gitlab.com/peterzuger/emacsd.git
```

The ```--recursive``` is **required**, otherwise the sub-repository's wont be
downloaded.

And place it in ```~/```.

You can also directly clone it into ```~/.emacs.d```:
```
git clone --recursive https://gitlab.com/peterzuger/emacsd.git ~/.emacs.d
```

### Build Prerequisites
For jedi and irony, a server has to be built during the first startup,
this requires some additional packages, they can be removed once the installation is done:

| Prerequisite                                                      | Description                              | [Arch Packages](https://www.archlinux.org/packages/)              |
|-------------------------------------------------------------------|------------------------------------------|-------------------------------------------------------------------|
| [clang](https://clang.llvm.org/)                                  | C language family frontend for LLVM      | [clang](https://www.archlinux.org/packages/staging/x86_64/clang/) |
| [cmake](https://cmake.org/)                                       | A cross-platform open-source make system | [cmake](https://www.archlinux.org/packages/extra/x86_64/cmake/)   |
| [base-devel](https://www.archlinux.org/groups/x86_64/base-devel/) | Developer utilities                      | [base-devel](https://www.archlinux.org/groups/x86_64/base-devel/) |

### Installing
When starting emacs for the first time with my ```~/.emacs.d```,
emacs will automatically install the following packages:

| Package name            | language | function              |
|-------------------------|:--------:|-----------------------|
| auctex                  | TeX      | language Support      |
| company                 | *        | text-completion       |
| company-emoji           | *        | emoji-completion      |
| company-irony           | C/C++    | auto-completion       |
| company-jedi            | Python   | auto-completion       |
| company-shell           | Shell    | auto-completion       |
| emojify                 | Shell    | auto-completion       |
| fill-column-indicator   | *        | visibility            |
| flycheck                | *        | syntax check          |
| flycheck-clang-analyzer | C/C++    | static analyzer check |
| flycheck-irony          | C/C++    | syntax check          |
| flycheck-pycheckers     | C/C++    | syntax check          |
| ggtags                  | C/C++    | jump to definition    |
| magit                   | *        | git support           |
| markdown-mode           | Markdown | language Support      |
| pdf-tools               | .pdf     | PDF support           |
| pinentry                | passwd   | password entry        |
| ranger                  | dired    | file browsing         |
| smartparens             | *        | helper                |
| whitespace-cleanup-mode | *        | cleanup               |
| yasnippet               | *        | code snippets         |

This should only take a few minutes.

Once that has finished, excecute the following 2 commands, they install the irony and the jedi server.
```
M-x irony-install-server
M-x jedi:install-server
```

## ✍️ Authors <a name = "authors"></a>
- [@peterzuger](https://github.com/peterzuger)


## 🎉 Acknowledgements <a name = "acknowledgement"></a>
- [zenburn](https://github.com/bbatsov/zenburn-emacs)
- [moe](https://github.com/kuanyui/moe-theme.el)
- [material](https://github.com/cpaulik/emacs-material-theme)
- [cyberpunk](https://github.com/n3mo/cyberpunk-theme.el)
- [ample](https://github.com/jordonbiondo/ample-theme)
- [alect](https://github.com/alezost/alect-themes)
- [gotham](https://github.com/wasamasa/gotham-theme)
- [tangotango](https://github.com/juba/color-theme-tangotango)
- [gruber-darker](https://github.com/rexim/gruber-darker-theme)
- [ample-zen](https://github.com/mjwall/ample-zen)
- [noctilux](https://github.com/sjrmanning/noctilux-theme)
- [afternoon](https://github.com/osener/emacs-afternoon-theme)
- [grandshell](https://github.com/steckerhalter/grandshell-theme)
- [@kylelobo](https://github.com/kylelobo) - Documentation template
