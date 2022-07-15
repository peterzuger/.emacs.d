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

## 🧐 About <a name = "about"></a>
This Repository contains my emacs configuration for everything.

The entire configuration is centered around `use-package` and almost everything
is configured using it.

## 🏁 Getting Started <a name = "getting_started"></a>
These instructions will get you a copy of my emacs setup up and running on your
local machine.

### Prerequisites
To use my ```~/.emacs.d``` you will need to install the following packages on
your system these examples are the ArchLinux packages but they should exist in
other repositories:

| Emacs Package | Prerequisites                                                                        | Link                                                |
|---------------|--------------------------------------------------------------------------------------|-----------------------------------------------------|
| auctex        | [Texlive](https://www.archlinux.org/groups/x86_64/texlive-most/)                     | https://www.tug.org/texlive/                        |
| black         | [python-black](https://www.archlinux.org/packages/community/any/python-black/)       | https://github.com/psf/black                        |
| flyspell      | [aspell](https://www.archlinux.org/packages/extra/x86_64/aspell/)                    | http://aspell.net/                                  |
| ggtags        | [GNU global](https://aur.archlinux.org/packages/global/)                             | https://www.gnu.org/software/global/                |
| ggtags        | [ctags](https://www.archlinux.org/packages/extra/x86_64/ctags/)                      | http://ctags.sourceforge.net/                       |
| go-mode       | [go](https://www.archlinux.org/packages/community/x86_64/go/)                        | https://golang.org/                                 |
| go-mode       | [gocode](https://www.github.com/rogpeppe/godef/)                                     | https://www.github.com/rogpeppe/godef/              |
| go-mode       | [godef](https://github.com/mdempsky/gocode/)                                         | https://github.com/mdempsky/gocode/                 |
| irony         | [base-devel](https://www.archlinux.org/groups/x86_64/base-devel/)                    | https://www.archlinux.org/groups/x86_64/base-devel/ |
| irony         | [clang](https://www.archlinux.org/packages/staging/x86_64/clang/)                    | https://clang.llvm.org/                             |
| irony         | [cmake](https://www.archlinux.org/packages/extra/x86_64/cmake/)                      | https://cmake.org/                                  |
| irony         | [llvm-libs](https://www.archlinux.org/packages/extra/x86_64/llvm-libs/)              | https://llvm.org/                                   |
| jedi          | [python-virtualenv](https://www.archlinux.org/packages/extra/any/python-virtualenv/) | https://pypi.org/project/virtualenv/                |
| magit         | [git](https://www.archlinux.org/packages/extra/x86_64/git/)                          | https://git-scm.com/                                |
| mu4e          | [mu](https://aur.archlinux.org/packages/mu/)                                         | https://www.djcbsoftware.nl/code/mu/                |
| mu4e          | [offlineimap](https://www.archlinux.org/packages/community/any/offlineimap/)         | http://www.offlineimap.org/                         |
| pycheckers    | [bandit](https://archlinux.org/packages/community/any/bandit/)                       | https://github.com/PyCQA/bandit                     |
| pycheckers    | [flake8](https://archlinux.org/packages/community/any/flake8/)                       | http://flake8.pycqa.org/                            |
| pycheckers    | [pyflakes](https://archlinux.org/packages/community/any/python-pyflakes/)            | https://pypi.python.org/pypi/pyflakes               |
| pycheckers    | [pylint](https://archlinux.org/packages/community/any/python-pylint/)                | https://pylint.org/                                 |

The go packages can be installed like this:
```
go get -u github.com/mdempsky/gocode
go get -u github.com/rogpeppe/godef
```

#### install ~\/.emacs.d\/
Just clone this repo with:

```
git clone --recursive https://github.com/peterzuger/emacsd.git
```

The ```--recursive``` is **required**, otherwise the sub-repository's wont be
downloaded.

And place it in ```~/```.

You can also directly clone it into ```~/.emacs.d```:
```
git clone --recursive https://github.com/peterzuger/emacsd.git ~/.emacs.d
```

### Package Installation
When starting emacs for the first time with my ```~/.emacs.d```,
emacs will automatically install all packages that are not builtin.

This should only take a few minutes.
This only takes long during the very first startup.

| Package Name                                                                      | Notes                               |
|-----------------------------------------------------------------------------------|-------------------------------------|
| [ace-window](https://github.com/abo-abo/ace-window)                               |                                     |
| [auctex](https://www.gnu.org/software/auctex/)                                    |                                     |
| [avy](https://github.com/abo-abo/avy)                                             |                                     |
| [company](https://github.com/company-mode/company-mode)                           |                                     |
| [company-emoji](https://github.com/dunn/company-emoji)                            |                                     |
| [company-go](https://github.com/emacsmirror/company-go)                           |                                     |
| [company-irony](https://github.com/Sarcasm/company-irony)                         |                                     |
| [company-jedi](https://github.com/emacsorphanage/company-jedi)                    |                                     |
| [company-shell](https://github.com/Alexander-Miller/company-shell)                |                                     |
| [counsel](https://github.com/abo-abo/swiper#counsel)                              |                                     |
| [dockerfile-mode](https://github.com/spotify/dockerfile-mode)                     |                                     |
| [engine-mode](https://github.com/hrs/engine-mode)                                 |                                     |
| [emojify](https://github.com/iqbalansari/emacs-emojify#emojify)                   |                                     |
| [fill-column-indicator](https://github.com/alpaker/fill-column-indicator)         |                                     |
| [flycheck](https://www.flycheck.org/en/latest/)                                   |                                     |
| [flycheck-irony](https://github.com/Sarcasm/flycheck-irony)                       |                                     |
| [flycheck-pycheckers](https://github.com/msherry/flycheck-pycheckers)             |                                     |
| [ggtags](https://github.com/leoliu/ggtags)                                        |                                     |
| [go-mode](https://github.com/dominikh/go-mode.el)                                 |                                     |
| [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides) |                                     |
| [hydra](https://github.com/abo-abo/hydra)                                         |                                     |
| [irony](https://github.com/Sarcasm/irony-mode)                                    |                                     |
| [ivy](https://github.com/abo-abo/swiper#ivy)                                      |                                     |
| [ivy-hydra](https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el)           |                                     |
| [js2-mode](https://github.com/mooz/js2-mode)                                      |                                     |
| [js2-refactor](https://github.com/js-emacs/js2-refactor.el)                       |                                     |
| [json-mode](https://github.com/joshwnj/json-mode)                                 |                                     |
| [magit](https://magit.vc/)                                                        |                                     |
| [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)                             | is only loaded when mu is installed |
| [markdown-mode](https://github.com/jrblevin/markdown-mode)                        |                                     |
| [org](https://orgmode.org/)                                                       |                                     |
| [pdf-tools](https://github.com/politza/pdf-tools)                                 |                                     |
| [pinentry](https://github.com/ueno/pinentry-el)                                   |                                     |
| [python-black](https://github.com/wbolster/emacs-python-black)                    |                                     |
| [ranger](https://github.com/ralesi/ranger.el)                                     |                                     |
| [smartparens](https://github.com/Fuco1/smartparens)                               |                                     |
| [swiper](https://github.com/abo-abo/swiper)                                       |                                     |
| [xref-js2](https://github.com/js-emacs/xref-js2)                                  |                                     |
| [yasnippet](https://joaotavora.github.io/yasnippet/)                              |                                     |


### Builtin Packages
The following packages are builtins that have `use-package` definitions just for
configuration purposes:

| Builtin Package Name | Notes                         |
|----------------------|-------------------------------|
| cc-mode              |                               |
| compile              | creates desktop notifications |
| ibuffer              |                               |
| image                |                               |
| make-mode            |                               |
| minibuffer           |                               |
| sgml-mode            |                               |
| whitespace           |                               |

### Use emacs
And emacs is ready to use !

Some usefull keybinds that are not standard are listed here sorted by their
usefulness:

| Keybinding  | Description                       | Note                    |
|-------------|-----------------------------------|-------------------------|
| `M-o`       | switch window using ace shortcuts |                         |
| `C-c c`     | org capture                       |                         |
| `C-c g`     | magit (just the best)             |                         |
| `C-c C-l`   | compile from anywhere             |                         |
| `<backtab>` | complete any yasnippet            |                         |
| `C-x C-g`   | insert a random string            |                         |
| `C-x C-l`   | smart downcase region/word        |                         |
| `C-x C-u`   | smart upcase region/word          |                         |
| `C-c m`     | mu4e                              | only if mu is installed |
| `C-c a`     | org agenda                        |                         |
| `C-M-c C-l` | switch to compilation window      |                         |
