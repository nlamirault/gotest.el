# Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

APP = gotest

EMACS ?= emacs
EMACSFLAGS = --debug-init -L .
CASK = cask
EVM = evm
VAGRANT = vagrant

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

VERSION=$(shell \
        grep Version gotest.el \
	|awk -F'"' '{print $$2}')


NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR) ==== $(APP) [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)                   : launch unit tests$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- integration-test$(NO_COLOR)       : launch integration tests$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR)                  : clean Scame installation$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- reset$(NO_COLOR)                  : remote Scame dependencies for development$(NO_COLOR)"

.PHONY: build
build :
	@$(CASK) install
	@$(CASK) update

.PHONY: local-test
test : build
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
	$(EMACSFLAGS) \
	-l test/run-tests

.PHONY: integration-test
integration-test: build
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
	$(EMACSFLAGS) \
	-l test/run-global-tests

.PHONY: clean
clean :
	@$(CASK) clean-elc
	rm -fr dist

reset : clean
	@rm -rf .cask # Clean packages installed for development
	@rm -fr test/sandbox

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
	$(EMACSFLAGS) \
	-f batch-byte-compile $<
