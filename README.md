# My Emacs configuration

My Personal Emacs configuration
contains some themes as well

## init.el

this is the main configuration file loaded by Emacs
it only contains the configurations that are used in
all languages

all language dependent configurations are separated into
different files called :

`<language>.el`

and are loaded with :

`(load-file "~/.emacs.d/<language>.el")`

## Packages

`init.el` adds the melpa archive to the package archives
it then auto installs some packages :

| Package name               | language   | function            |
|----------------------------|:----------:|---------------------|
| ac-c-headers               | C/C++      | auto-completion     |
| ac-clang                   | C/C++      | auto-completion     |
| ac-html                    | HTML       | auto-completion     |
| ac-js2                     | Javascript | auto-completion     |
| auctex                     | Tex        | language Support    |
| auto-complete              | *          | auto-completion     |
| auto-complete-auctex       | Tex        | auto-completion     |
| auto-complete-c-headers    | C/C++      | auto-completion     |
| auto-complete-clang        | C/C++      | auto-completion     |
| company                    | *          | text-completion     |
| company-auctex             | Tex        | auto-completion     |
| company-c-headers          | C/C++      | auto-completion     |
| company-irony              | C/C++      | auto-completion     |
| company-irony-c-headers    | C/C++      | auto-completion     |
| company-jedi               | Python     | auto-completion     |
| company-shell              | Shell      | auto-completion     |
| fill-column-indicator      | *          | visibility          |
| flycheck                   | *          | syntax check        |
| flycheck-clang-analyzer    | C/C++      | syntax check        |
| flycheck-clang-tidy        | C/C++      | syntax check        |
| flycheck-clangcheck        | C/C++      | syntax check        |
| flycheck-irony             | C/C++      | syntax check        |
| function-args              | C/C++      | helper              |
| gist                       | *          | *                   |
| highlight-escape-sequences | *          | syntax highlighting |
| highlight-function-calls   | C/C++      | syntax highlighting |
| irony                      | C/C++      | language Support    |
| jedi                       | Python     | language Support    |
| js2-highlight-vars         | Javascript | syntax highlighting |
| js2-refactor               | Javascript | code refractoring   |
| js3-mode                   | Javascript | language Support    |
| json-mode                  | JSON       | language Support    |
| magit                      | *          | *                   |
| markdown-mode              | Markdown   | language Support    |
| python                     | Python     | language Support    |
| smartparens                | *          | helper              |
| string-edit                | *          | helper              |
| whitespace-cleanup-mode    | *          | cleanup             |


## Additional requirements

Some of the packages require additional Software
outside of Emacs

### NOT COMPLETE

| Package       | OS  | requirements  | Link                                        |
|---------------|-----|---------------|---------------------------------------------|
| auctex        | OSX | Mactex        | http://www.tug.org/mactex/                  |
| python        | OSX | Python        | https://www.python.org                      |


for viewing Markdown files on Mac there is also a QuickLook package

https://github.com/toland/qlmarkdown

## Links

### Themes

links to all git repository's used as themes

https://github.com/bbatsov/zenburn-emacs<br>
https://github.com/purcell/color-theme-sanityinc-tomorrow<br>
https://github.com/kuanyui/moe-theme.el<br>
https://github.com/cpaulik/emacs-material-theme<br>
https://github.com/n3mo/cyberpunk-theme.el<br>
https://github.com/m00natic/anti-zenburn-theme<br>
https://github.com/jordonbiondo/ample-theme<br>
https://github.com/alezost/alect-themes<br>
https://github.com/wasamasa/gotham-theme<br>
https://github.com/juba/color-theme-tangotango<br>
https://github.com/rexim/gruber-darker-theme<br>
https://github.com/mjwall/ample-zen<br>
https://github.com/sjrmanning/noctilux-theme<br>
https://github.com/osener/emacs-afternoon-theme<br>
https://github.com/steckerhalter/grandshell-theme<br>
https://github.com/cryon/subatomic<br>
