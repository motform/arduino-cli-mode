# arduino-cli-mode

`arduino-cli-mode` is an Emacs minor mode for using the excellent new 
[arduino command line interface](https://github.com/arduino/arduino-cli)
in an Emacs-native fashion. The mode covers the full range of
`arduino-cli` features in an Emacs native fashion. It even 
leverages the infinite power the GNU to provide fussy-finding
of libraries and much improved support for handling multiple boards.
The commands that originally require multiple steps (such as first
searching for a library and then separately installing it) have
been folded into one.


## Installation

The recommended way to install `arduino-cli-mode` is through [melpa](http://melpa.org/#/arduino-cli-mode). 
Depending on if you use [arduino-mode](https://melpa.org/#/arduino-mode) 
or not, you might want to load `arduino-cli-mode` either as a hook or as a mode.
A sample configuration with [use-package](https://github.com/jwiegley/use-package) could look like this:

```elisp
(use-package arduino-cli-mode
  :ensure t
  ;; :hook arduino-mode
  ;; :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))
```


## Default boards

By default `arduino-cli-mode` uses the `board list` command from
`arduino-cli` to determine which board to target. This works well most
of the time, but sometimes fails due to connectivity issues. It also
requires the target board to be connected, which might not always be
possible. To cover these use cases you are able to set a default board
(fqbn) and port via `arduino-cli-default-fqbn` and
`arduino-cli-default-port` respectively. These can, of course, be set
globally via your `init`, but you may find them to be an excellent fit
for
[dir](https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html)
and [file local
variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html#File-Local-Variables).
For example, on a Linux system with a NodeMCU v2 board connected to
ttyUSB0: ```cpp // -*- arduino-cli-default-fqbn:
"esp8266:esp8266:nodemcuv2"; arduino-cli-default-port: "/dev/ttyUSB0";
-*- // (The rest of your sketch follows as usual.)  ```


To get the fqbn/port information for a currently connected board, use 
`arduino-cli-board-list`.

Using default board variables should be a bit faster, as it
eliminates the need to shelling out and then parse JSON from `arduino-cli`.


## Customization


You can enable the major flags from `arduino-cli` using similar enumerations. 

| Flag                                 | Values                                       |
| ---                                  | ---                                          |
| `arduino-cli-verify`                 | `nil` (default), `t`                         |
| `arduino-cli-warnings`               | `nil` (default), `'default`, `'more`, `'all` |
| `arduino-cli-verbosity`              | `nil` (default), `'quiet`, `'verbose`        |
| `arduino-cli-compile-only-verbosity` | `nil`, `t` (default)                         |
| `arduino-cli-compile-color`          | `nil`, `t` (default)                         |

If you want to automatically enable `arduino-cli-mode` on `.ino` files, you have to get [auto-minor-mode](https://github.com/joewreschnig/auto-minor-mode).
Once that is installed, add the following to your init:

```elisp
(add-to-list 'auto-minor-mode-alist '("\\.ino\\'" . arduino-cli-mode))
```


## Keymap

The default keymap prefix is `C-c C-a` and can be customized with `arduino-cli-mode-keymap-prefix`.

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
| Kill Arduino Connection | `C-c C-a k` |


## Limitations

* Does not support `board attach` commands
* Only tested on macOS (but will probably work on other Unices)
* Not called `elduino-mode`


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
# arduino-cli-mode

`arduino-cli-mode` is an Emacs minor mode for using the excellent new 
[arduino command line interface](https://github.com/arduino/arduino-cli)
in an Emacs-native fashion. The mode covers the full range of
`arduino-cli` features in an Emacs native fashion. It even 
leverages the infinite power the GNU to provide fussy-finding
of libraries and much improved support for handling multiple boards.
The commands that originally require multiple steps (such as first
searching for a library and then separately installing it) have
been folded into one.


## Installation

The recommended way to install `arduino-cli-mode` is through [melpa](http://melpa.org/#/arduino-cli-mode). 
Depending on if you use [arduino-mode](https://melpa.org/#/arduino-mode) 
or not, you might want to load `arduino-cli-mode` either as a hook or as a mode.
A sample configuration with [use-package](https://github.com/jwiegley/use-package) could look like this:

```elisp
(use-package arduino-cli-mode
  :ensure t
  ;; :hook arduino-mode
  ;; :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))
```


## Default boards

By default `arduino-cli-mode` uses the `board list` command from
`arduino-cli` to determine which board to target. This works well 
if you have a genuine Arduino board (with its unique USB Vendor ID and Product ID)
and it is currently connected, 
but won't work if your board is not plugged in 
or is an unbranded board with generic USB Vendor and Product IDs.

To cover these use cases you are able to set a default board
(fqbn) and port via `arduino-cli-default-fqbn` and
`arduino-cli-default-port` respectively. These can, of course, be set
globally via your `init`, but you may find them to be an excellent fit
for
[dir](https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html)
and [file local variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html#File-Local-Variables).
For example, on a Linux system with an unbranded NodeMCU v2 compatible
board connected to ttyUSB0,
you could use the following line at the top of your sketch: 

```cpp
// -*- arduino-cli-default-fqbn: "esp8266:esp8266:nodemcuv2"; arduino-cli-default-port: "/dev/ttyUSB0"; -*-
// (The rest of your sketch follows as usual.)
```


To get the fqbn/port information for a currently connected board, use 
`arduino-cli-board-list`.

Using default board variables should be a bit faster, as it
eliminates the need to shelling out and then parse JSON from `arduino-cli`.


## Customization


You can enable the major flags from `arduino-cli` using similar enumerations. 

| Flag                                 | Values                                       |
| ---                                  | ---                                          |
| `arduino-cli-verify`                 | `nil` (default), `t`                         |
| `arduino-cli-warnings`               | `nil` (default), `'default`, `'more`, `'all` |
| `arduino-cli-verbosity`              | `nil` (default), `'quiet`, `'verbose`        |
| `arduino-cli-compile-only-verbosity` | `nil`, `t` (default)                         |
| `arduino-cli-compile-color`          | `nil`, `t` (default)                         |

If you want to automatically enable `arduino-cli-mode` on `.ino` files, you have to get [auto-minor-mode](https://github.com/joewreschnig/auto-minor-mode).
Once that is installed, add the following to your init:

```elisp
(add-to-list 'auto-minor-mode-alist '("\\.ino\\'" . arduino-cli-mode))
```


## Keymap

The default keymap prefix is `C-c C-a` and can be customized with `arduino-cli-mode-keymap-prefix`.

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
| Kill Arduino Connection | `C-c C-a k` |


## Limitations

* Does not support `board attach` commands
* Only tested on macOS (but will probably work on other Unices)
* Not called `elduino-mode`


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
