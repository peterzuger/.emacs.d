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
| auctex                     | Tex        | language Support    |
| company                    | *          | text-completion     |
| company-irony              | C/C++      | auto-completion     |
| company-shell              | Shell      | auto-completion     |
| fill-column-indicator      | *          | visibility          |
| flycheck                   | *          | syntax check        |
| irony                      | C/C++      | language Support    |
| jedi                       | Python     | language Support    |
| markdown-mode              | Markdown   | language Support    |
| smartparens                | *          | helper              |
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
