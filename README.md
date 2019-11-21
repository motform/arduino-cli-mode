# arduino-cli-mode

`arduino-cli-mode` is an Emacs minor mode for using the excellent new [arduino command line interface](https://github.com/arduino/arduino-cli)
in an Emacs-native fashion. The project at a very early stage, and only supports
basic compilation and uploading.


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

The following keybindings are currently available.

| Function                | Keymap      |
| --------                | :------–--: |
| Compile                 | `C-c C-a c` |
| Upload                  | `C-c C-a u` |
| Compile and Upload      | `C-c C-a b` |
| List Connected Boards   | `C-c C-a c` |


## Limitations

* If there are more than one board connected simultaneously, there will be dragons (fixing this is highest on the TODO).
* The only supported commands are `compile`, `upload` and `board list`.
* Only tested on macOS (but will probably work on other Unices)
* The fact that I decided against calling it `elduino-mode`.


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
