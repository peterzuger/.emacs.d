<p align="center"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="emacs"></p>

<h3 align="center">~/.emacs.d</h3>

<div align="center">

  [![Status](https://img.shields.io/badge/status-active-success.svg)]()
  [![GitHub Issues](https://img.shields.io/github/issues/peterzuger/.emacs.d.svg)](https://github.com/peterzuger/.emacs.d/issues)
  [![GitHub Pull Requests](https://img.shields.io/github/issues-pr/peterzuger/.emacs.d.svg)](https://github.com/peterzuger/.emacs.d/pulls)
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

| Emacs Package | Prerequisites                                                                          | Link                                             |
|---------------|----------------------------------------------------------------------------------------|--------------------------------------------------|
| auctex        | [Texlive](https://www.archlinux.org/groups/x86_64/texlive-most/)                       | https://www.tug.org/texlive/                     |
| black         | [python-black](https://www.archlinux.org/packages/community/any/python-black/)         | https://github.com/psf/black                     |
| flyspell      | [aspell](https://www.archlinux.org/packages/extra/x86_64/aspell/)                      | http://aspell.net/                               |
| lsp C/C++     | [clang](https://www.archlinux.org/packages/staging/x86_64/clang/)                      | https://clang.llvm.org/                          |
| lsp-jedi      | [python-virtualenv](https://www.archlinux.org/packages/extra/any/python-virtualenv/)   | https://pypi.org/project/virtualenv/             |
| lsp-jedi      | [jedi-language-server](https://archlinux.org/packages/extra/any/jedi-language-server/) | https://github.com/pappasam/jedi-language-server |
| magit         | [git](https://www.archlinux.org/packages/extra/x86_64/git/)                            | https://git-scm.com/                             |
| mu4e          | [mu](https://aur.archlinux.org/packages/mu/)                                           | https://www.djcbsoftware.nl/code/mu/             |
| mu4e          | [offlineimap](https://www.archlinux.org/packages/community/any/offlineimap/)           | http://www.offlineimap.org/                      |
| pycheckers    | [bandit](https://archlinux.org/packages/community/any/bandit/)                         | https://github.com/PyCQA/bandit                  |
| pycheckers    | [flake8](https://archlinux.org/packages/community/any/flake8/)                         | http://flake8.pycqa.org/                         |
| pycheckers    | [pyflakes](https://archlinux.org/packages/community/any/python-pyflakes/)              | https://pypi.python.org/pypi/pyflakes            |
| pycheckers    | [pylint](https://archlinux.org/packages/community/any/python-pylint/)                  | https://pylint.org/                              |


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
| [cmake-mode](https://gitlab.kitware.com/cmake/cmake.git)                          |                                     |
| [company-emoji](https://github.com/dunn/company-emoji)                            |                                     |
| [company-shell](https://github.com/Alexander-Miller/company-shell)                |                                     |
| [company](https://github.com/company-mode/company-mode)                           |                                     |
| [counsel](https://github.com/abo-abo/swiper#counsel)                              |                                     |
| [csv-mode](https://elpa.gnu.org/packages/csv-mode.html)                           |                                     |
| [diminish](https://github.com/myrjola/diminish.el)                                |                                     |
| [dockerfile-mode](https://github.com/spotify/dockerfile-mode)                     |                                     |
| [emojify](https://github.com/iqbalansari/emacs-emojify#emojify)                   |                                     |
| [engine-mode](https://github.com/hrs/engine-mode)                                 |                                     |
| [fill-column-indicator](https://github.com/alpaker/fill-column-indicator)         |                                     |
| [flycheck-pycheckers](https://github.com/msherry/flycheck-pycheckers)             |                                     |
| [flycheck](https://www.flycheck.org/en/latest/)                                   |                                     |
| [haskell-mode](https://github.com/haskell/haskell-mode)                           |                                     |
| [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides) |                                     |
| [hydra](https://github.com/abo-abo/hydra)                                         |                                     |
| [immortal-scratch](https://github.com/peterzuger/immortal-scratch)                |                                     |
| [ivy-hydra](https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el)           |                                     |
| [ivy](https://github.com/abo-abo/swiper#ivy)                                      |                                     |
| [js2-mode](https://github.com/mooz/js2-mode)                                      |                                     |
| [js2-refactor](https://github.com/js-emacs/js2-refactor.el)                       |                                     |
| [json-mode](https://github.com/joshwnj/json-mode)                                 |                                     |
| [lsp-jedi](https://github.com/fredcamps/lsp-jedi)                                 |                                     |
| [lsp-mode](https://github.com/emacs-lsp/lsp-mode/)                                |                                     |
| [magit](https://magit.vc/)                                                        |                                     |
| [markdown-mode](https://github.com/jrblevin/markdown-mode)                        |                                     |
| [mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)                             | is only loaded when mu is installed |
| [multiple-cursors](https://github.com/magnars/multiple-cursors.el)                |                                     |
| [ob-rust](https://github.com/micanzhang/ob-rust)                                  |                                     |
| [org-roam-ui](https://github.com/org-roam/org-roam-ui)                            |                                     |
| [org-roam](https://github.com/org-roam/org-roam)                                  |                                     |
| [org](https://orgmode.org/)                                                       |                                     |
| [orgit](https://github.com/magit/orgit)                                           |                                     |
| [pdf-tools](https://github.com/politza/pdf-tools)                                 |                                     |
| [pinentry](https://github.com/ueno/pinentry-el)                                   |                                     |
| [python-black](https://github.com/wbolster/emacs-python-black)                    |                                     |
| [pyvenv](http://github.com/jorgenschaefer/pyvenv)                                 |                                     |
| [scad-mode](https://github.com/openscad/emacs-scad-mode)                          |                                     |
| [smartparens](https://github.com/Fuco1/smartparens)                               |                                     |
| [swiper](https://github.com/abo-abo/swiper)                                       |                                     |
| [systemd](https://github.com/holomorph/systemd-mode)                              |                                     |
| [xref-js2](https://github.com/js-emacs/xref-js2)                                  |                                     |
| [yaml-mode](https://github.com/yoshiki/yaml-mode)                                 |                                     |
| [yasnippet](https://joaotavora.github.io/yasnippet/)                              |                                     |


### Use emacs
And emacs is ready to use !

Some usefull keybinds that are not standard are listed here sorted by their
usefulness:

| Keybinding | Description                       | Note                    |
|------------|-----------------------------------|-------------------------|
| `M-o`      | switch window using ace shortcuts |                         |
| `C-\|`     | toggle fill-column indicator      |                         |
| `C-<tab>`  | complete any yasnippet            |                         |
| `C-c g`    | magit (just the best)             |                         |
| `C-c C-m`  | mu4e                              | only if mu is installed |
| `C-c e`    | elfeed RSS/Atom reader            |                         |
| `C-c c`    | org capture                       |                         |
| `C-c a`    | org agenda                        |                         |
| `C-c n f`  | org roam node find                |                         |
| `C-c n c`  | org roam node capture             |                         |
| `C-c n g`  | org roam ui mode                  |                         |
| `C-c C-l`  | compile from anywhere             |                         |
| `C-c M-l`  | switch to compilation window      |                         |
| `C-c C-g`  | gptel                             |                         |
| `C-c M-g`  | gptel menu                        |                         |
| `C-x C-g`  | insert a random string            |                         |
| `C-x C-l`  | smart downcase region/word        |                         |
| `C-x C-u`  | smart upcase region/word          |                         |
