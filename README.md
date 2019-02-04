# Hyperspec
This is **hyperspec**, a small library which includes some utilities to refer [Common Lisp HyperSpec](http://clhs.lisp.se/Front/index.htm) from repl.

# Installation

```
$ ros install koji-kojiro/hyperspec
```

# APIs

#### `function get-url (symbol) => url`
returns URL to the document on CLHS corresponding to `symbol`.

#### `funcion get-html (symbol) => url`
returns HTML source of the document on CLHS corresponding `symbol`.

#### `function show (symbol) => nil`
format and display the document on CLHS corresponding `symbol`.

# clhs
A command line tool named `clhs` is also included in this project.

## Installation

### With Roswell

```
$ ros install koji-kojiro/hyperspec
```

### Build from source (with SBCL)

```
$ git clone https://github.com/koji-kojiro/hyperspec.git
$ cd hyperspec
$ sbcl --load build.lisp
# then place `./clhs` anywhere inlcluded in PATH
```

## Usage

```
$ clhs --help
Usage: clhs [-h|--help] [-u|--url] symbol

Available options:
  -h, --help               Print this help and exit.
  -u, --url                Print url instead of document.
  -n, --nocolor            Do not use ANSI escapes.

```

Here is a screenshot of typical usage with `more` (`clhs format | more`):

<div align="center">
  <img src=image/more.png>
</div>

# Author
[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)

# License
Licecnsed [MIT](./LICENSE).
