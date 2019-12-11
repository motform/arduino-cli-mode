# arduino-cli-mode

`arduino-cli-mode` is an Emacs minor mode for using the excellent new [arduino command line interface](https://github.com/arduino/arduino-cli)
in an Emacs-native fashion. The mode covers the full range of
`arduino-cli` features in an Emacs native fashion. It even 
leverages the infinite power the GNU to provide fussy-finding
of libraries and much improved support for handling multiple boards.
The commands that originally require multiple steps (such as first
searching for a library and then separately installing it) have
been folded into one.


## Installation

Until the package is published on melpa, I recommend using [straight.el](https://github.com/raxod502/straight.el).


## Customization

If you want to automatically enable `arduino-cli-mode` on `.ino` files, you have to get [auto-minor-mode](https://github.com/joewreschnig/auto-minor-mode).
Once that is installed, add the following to your init:

```elisp
(add-to-list 'auto-minor-mode-alist '("\\.ino\\'" . arduino-cli-mode))
```

## Keymap

The default keymap prefix is `C-c C-a`.

The following keybindings are provided out of the box.

| Function                | Keymap      |
| ---                     | ---         |
| Compile                 | `C-c C-a c` |
| Upload                  | `C-c C-a u` |
| Compile and Upload      | `C-c C-a b` |
| List Connected Boards   | `C-c C-a l` |
| Create new sketch       | `C-c C-a n` |
| Install a Library       | `C-c C-a i` |
| Uninstall a Library     | `C-c C-a u` |


## Limitations

* Does not support `board attach` commands
* Only tested on macOS (but will probably work on other Unices)
* The fact that I decided against calling it `elduino-mode`


## What it is not

This mode is not an Arduino major mode, it only provides convenient access to arduino-cli.
If you are looking for something like that, check out [arduino-mode](https://github.com/stardiviner/arduino-mode/tree/23ae47c9f28f559e70b790b471f20310e163a39b).
In fact, I think they would complement each other rather well, as `arduino-mode` lacks support 
for `arduino-cli`, simply due to pre-dating it with a decade.

If you want auto-completion for Arduino development, see [company-arduino](https://github.com/yuutayamada/company-arduino/tree/d7e369702b8eee63e6dfdeba645ce28b6dc66fb1).

Depending on your board, you might also enjoy [platform-io-mode](https://github.com/ZachMassia/PlatformIO-Mode),
an excellent wrapper that I took a lot of inspiration from while writing this one.


## Contribute

This is my first real elisp project, so everything from code review to feature implementations are welcome!
The plan is to support (more or less) the entire feature set of arduino-cli, and then go into maintenance mode.
