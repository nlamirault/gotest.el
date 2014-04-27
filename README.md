# gotest.el

Manage the [GO](http://golang.org) tests from Emacs.

## Deprecated

See [Cerbere](https://github.com/nlamirault/cerbere)

## Installation

The recommended way to install ``gotest.el`` is via [MELPA](http://melpa.milkbox.net/):

    M-x package-install gotest.el

or [Cask](https://github.com/cask/cask):

	(depends-on "gotest.el")


## Usage

### Available commands

2 functions are available :
* `go-test-current-test`: launch unit tests for the current test
* `go-test-current-file`: launch unit tests for the current file

You can create some key bindings with these commands:

```lisp
(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
```

## Development

### Cask

``gotest.el`` use [Cask](https://github.com/cask/cask) for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

Launch unit tests :

    $ make clean test


## Support / Contribute

See [here](CONTRIBUTING.md)



## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
