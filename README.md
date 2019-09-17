# My Emacs configuration
My Personal Emacs configuration, contains some themes as well.

## init.el
This is the main configuration file loaded by Emacs it should only contains the
configurations that are used in all languages. All language dependent
configurations are separated into different files called: `<language>.el` and
are loaded with: `(load-file "~/.emacs.d/<language>.el")`

## Packages
### package.el
`init.el` adds the melpa archive to the package archives it then auto installs
some packages:

| Package name            | language | function           |
|-------------------------|:--------:|--------------------|
| auctex                  | TeX      | language Support   |
| company                 | *        | text-completion    |
| company-irony           | C/C++    | auto-completion    |
| company-shell           | Shell    | auto-completion    |
| fill-column-indicator   | *        | visibility         |
| flycheck                | *        | syntax check       |
| flycheck-irony          | C/C++    | syntax check       |
| ggtags                  | C/C++    | jump to definition |
| helm                    | C/C++    | navigation         |
| jedi                    | Python   | language Support   |
| magit                   | *        | git support        |
| markdown-mode           | Markdown | language Support   |
| pdf-tools               | .pdf     | PDF support        |
| pinentry                | passwd   | password entry     |
| smartparens             | *        | helper             |
| whitespace-cleanup-mode | *        | cleanup            |
| yasnippet               | *        | code snippets      |

### Outside packages
mu4e is an outside package, it is installed along with mu from the OS package
manager. For this package to work some more things are required, read about
them here: [mu](https://www.djcbsoftware.nl/code/mu/mu4e/index.html)

## Additional requirements
Some of the packages require additional Software outside of Emacs:

| Package | requirements | Link                                 |
|---------|--------------|--------------------------------------|
| auctex  | Texlive      | https://www.tug.org/texlive/         |
| ggtags  | GNU global   | https://www.gnu.org/software/global/ |
| ggtags  | ctags        | http://ctags.sourceforge.net/        |
| magit   | git          | https://git-scm.com/                 |
| mu4e    | offlineimap  | http://www.offlineimap.org/          |
| mu4e    | mu           | https://www.djcbsoftware.nl/code/mu/ |

# Installation
To use this emacs configuration, just clone this repo with:

The ```--recursive``` is **required**, otherwise the sub-repository's wont be
downloaded.

```
git clone --recursive https://gitlab.com/peterzuger/emacsd.git
```

Then you can start emacs with
```
emacs -q --load "<path to this repo>/emacsd/init.el"
```

# Links
## Themes
links to all git repository's used as themes

[zenburn](https://github.com/bbatsov/zenburn-emacs)<br>
[moe](https://github.com/kuanyui/moe-theme.el)<br>
[material](https://github.com/cpaulik/emacs-material-theme)<br>
[cyberpunk](https://github.com/n3mo/cyberpunk-theme.el)<br>
[ample](https://github.com/jordonbiondo/ample-theme)<br>
[alect](https://github.com/alezost/alect-themes)<br>
[gotham](https://github.com/wasamasa/gotham-theme)<br>
[tangotango](https://github.com/juba/color-theme-tangotango)<br>
[gruber-darker](https://github.com/rexim/gruber-darker-theme)<br>
[ample-zen](https://github.com/mjwall/ample-zen)<br>
[noctilux](https://github.com/sjrmanning/noctilux-theme)<br>
[afternoon](https://github.com/osener/emacs-afternoon-theme)<br>
[grandshell](https://github.com/steckerhalter/grandshell-theme)<br>
