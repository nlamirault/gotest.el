# gotest.el

[![License GPL 3][badge-license]][LICENSE]

* Master : [![MELPA Stable](https://stable.melpa.org/packages/gotest-badge.svg)](https://stable.melpa.org/#/gotest) [![Circle CI](https://circleci.com/gh/nlamirault/gotest.el/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/gotest.el/tree/master) [![Coverage Status](https://coveralls.io/repos/nlamirault/gotest.el/badge.png?branch=master)](https://coveralls.io/r/nlamirault/gotest.el?branch=master)
* Develop: [![Melpa Status](https://melpa.org/packages/gotest-badge.svg)](https://melpa.org/#/gotest) [![Circle CI](https://circleci.com/gh/nlamirault/gotest.el/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/gotest.el/tree/develop) [![Coverage Status](https://coveralls.io/repos/github/nlamirault/gotest.el/badge.svg?branch=develop)](https://coveralls.io/github/nlamirault/gotest.el?branch=develop)

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

### go-test-current-test-cache

Rerun the current test.

### go-test-current-file

Launch unit tests and examples for the current file

### go-test-current-project

Launch unit tests and examples for the current project.

### go-test-current-coverage

Launch unit tests coverage for the current project.

### go-test-current-benchmark

Launch go benchmark on current benchmark

### go-test-current-file-benchmarks

Launch go benchmark on current file

### go-test-current-project-benchmarks

Launch go benchmark on current project

### go-test-rerun-test

Rerun the previous test.

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
(define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
(define-key go-mode-map (kbd "C-x x") 'go-run)
```

## Addons

### Gb

If your project use [gb][], *gotest* will use this tool to launch unit tests.


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

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: https://melpa.org/
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/gotest.el/issues

[overseer]: https://github.com/tonini/overseer.el

[gb]: http://getgb.io/
