# gotest.el ChangeLog

# Version 0.14.0 (08/23/2017)

- #56: add vendoring support to go-test-current-project (Thanks Sergey Kostyaev)
- #55: Make column in compiler message regex optional (Thanks Andreas Fuchs)
- #54: No longer provide a feature in test-helper.el (Thanks Jonas Bernoulli)
- #52: Fixed test profile multiple packages issue (Thanks Junjie Nan)

# Version 0.13.0 (10/17/2016)

- #48: Re-use the test output buffer if it is shown (Thanks Andreas Fuchs)
- #47: Anchor font-lock-keywords at BoL & add subtest support (Thanks Andreas Fuchs)
- #46: Pass . as the package name in current-{test,file} (Thanks Andreas Fuchs)
- #43: Add support for compilation with comint-mode enabled (Thanks Novak Boškov)
- #41: Fix running ExampleFoo that follows TestFoo (Thanks Konstantin Shaposhnikov)
- #39: add-custom-arguments-function (Thanks IvanMalison)

# Version 0.12.0 (14/04/2016)

- #37: for fish shell compatibility, strictly escape $ (thanks Eric Drechsel)

# Version 0.11.0 (02/26/2016)

- #34: Fix gb project detection (thanks dougm)
- `FIX` CircleCI build (Emacs24 as default version)

# Version 0.10.0 (01/25/2016)

- #32: Can't install package in melpa stable
- Refactoring testing current file
- #30: Tests with only tests or examples

# Version 0.9.0 (12/18/2015)

- #30: Remove extra  when no examples in file
- Cleanup removing unused hook

# Version 0.8.0 (11/30/2015)

- #25: use -m flag for suite tests (only use -m when length > 0) (thanks IvanMalison)
- #24: Load cl-lib.el instead of cl.el (thanks syohex)

# Version 0.7.0 (11/09/2015)

- #23: Add support for github.com/stretchr/testify/suite (thanks IvanMalison)
- Add some color output using compilation mode for unit tests

# Version 0.6.0 (10/30/2015)

- Init support for [gb][]
- #20: Add support for running benchmark
- `FIX` #19: Regex for single unit test
- Update documentation

# Version 0.5.0 (10/09/2015)

- #PR15: Specify minimum Emacs version

# Version 0.4.0 (09/17/2015)

- Migrate to CircleCI for continuous integration
- #14: escape file path in go command (thanks ntcong)

# Version 0.3.0 (01/15/2015)

- Support test coverage ([P11][])
- Refactoring unit tests (use [ert-runner][] and [overseer][])
- Code coverage in unit tests

# Version 0.2.0 (10/23/2014)

- Update compilation buffer
- Add TravisCI setup

# Version 0.1.0 (04/10/2014)

- Launch all tests from current file
- Launch unit test for a file


[ert-runner]: https://github.com/rejeep/ert-runner.el
[overseer]: https://github.com/tonini/overseer.el

[gb]: http://getgb.io/
