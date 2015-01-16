# gotest.el

[![License GPL 3][badge-license]][LICENSE]
[![travis][badge-travis]][travis]
[![drone][badge-drone]][drone]
[![Melpa Status](http://melpa.milkbox.net/packages/gotest-badge.svg)](http://melpa.milkbox.net/#/gotest)
[![MELPA Stable](http://stable.melpa.org/packages/gotest-badge.svg)](http://stable.melpa.org/#/gotest)
[![Coverage Status](https://coveralls.io/repos/nlamirault/gotest.el/badge.png?branch=master)](https://coveralls.io/r/nlamirault/gotest.el?branch=master)

Run [Go](http://golang.org) tests and programs from Emacs (>= 24.3)

## Installation

The recommended way to install ``gotest.el`` is via [MELPA][]:

    M-x package-install gotest.el

or [Cask][]:

	(depends-on "gotest.el")


## Usage

The following interactive commands can be run via <kbd>M-x</kbd> or
bound to a key of your choice.

All `go-test-*` functions can optionally configure the buffer-local
`go-test-args` variable to pass additional arguments.  Or, by using
a prefix command, you will be prompted for arguments.  For example:
<kbd>C-u M-x go-test-current-test</kbd>.

When using the `'_` prefix arg with any of the `go-test-*` or `go-run`
functions, the most recent arguments from history will be used without
prompting.  For example: <kbd>M-- M-x go-run</kbd>.

### go-test-current-test

Launch unit tests for the current test.

### go-test-current-file

Launch unit tests for the current file.

### go-test-current-project

Launch unit tests for the current project.

### go-test-current-coverage

Launch unit tests coverage for the current project.

### go-run

Launch program via `go run`.  Optionally configure the buffer-local
`go-test-args` variable to pass additional arguments.  Or, by using
a prefix command, you will be prompted for arguments.  For example:
<kbd>C-u M-x go-run</kbd>.

Be sure to make use of minibuffer history (<kbd>C-r</kbd>) to recall
recent arguments to `go run`.  And remember that the <kbd>M--</kbd>
prefix can be used in combination with your `go run` key binding to
use the most recent arguments without prompting.  The go file is
included in history, so you can `go-run` from history regardless of
which buffer you are currently visiting.

## Example key bindings

You can create some key bindings with these commands:

```lisp
(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-x x") 'go-run)
```

## Development

### Cask

``gotest.el`` use [Cask][] for dependencies management. Install it and
retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Testing

* Launch unit tests from shell

    $ make clean test

* Using [overseer][] :

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>C-c , t</kbd>   | launch unit tests from buffer
<kbd>C-c , b</kbd>   | launch unit tests
<kbd>C-c , g</kbd>   | launch unit tests with tag (find, regexp, ...)

* Tips:

If you want to launch a single unit test, add a specify tag :

```lisp
(ert-deftest test-foobar ()
  :tags '(current)
  ```

And launch it using : <kbd>C-c , g</kbd> and specify tag : *current*


## Support / Contribute

See [here](CONTRIBUTING.md)


## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>

[gotest]: https://github.com/nlamirault/gotest.el
[badge-license]: https://img.shields.io/badge/license-GPL_2-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/gotest.el/blob/master/LICENSE
[travis]: https://travis-ci.org/nlamirault/gotest.el
[badge-travis]: http://img.shields.io/travis/nlamirault/gotest.el.svg?style=flat
[badge-drone]: https://drone.io/github.com/nlamirault/gotest.el/status.png
[drone]: https://drone.io/github.com/nlamirault/gotest.el/latest
[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: http://melpa.milkbox.net/
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/gotest.el/issues

[overseer]: https://github.com/tonini/overseer.el
