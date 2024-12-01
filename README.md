# Electric operator mode

[![CI](https://github.com/davidshepherd7/electric-operator/workflows/CI/badge.svg)](https://github.com/davidshepherd7/electric-operator/actions)
[![melpa](http://melpa.org/packages/electric-operator-badge.svg)](http://melpa.org/#/electric-operator)
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[melpa-stable-link]: https://stable.melpa.org/#/electric-operator
[melpa-stable-badge]: https://stable.melpa.org/packages/electric-operator-badge.svg

An emacs minor-mode to automatically add spacing around operators.

For example typing

    a=10*5+2

results in

    a = 10 * 5 + 2

I'm aiming to have electric-operator-mode correctly handle almost every
operator for major modes built in to Emacs (and a few that aren't). If you find a
case where it doesn't space something as expected then consider it a bug, 
and please report it :).


## Setup

The simplest way to install electric-operator-mode is by using package.el
to get it from [MELPA unstable](http://melpa.org/#/getting-started).
Alternatively you can install the dependencies listed in
`electric-operator.el` and add `electric-operator.el` to your load path.

Either way you also need to make sure electric-operator is loaded with
`(require 'electric-operator)` (or you could use 
[`use-package`](https://github.com/jwiegley/use-package) to load packages
and keep your customisations organised).

To temporarily enable electric-operator-mode call `electric-operator-mode`.
To permanently enable it for a major mode add it to the relevant mode hook.
For example for python-mode add the following to your config:

    (add-hook 'python-mode-hook #'electric-operator-mode)

Note that `electric-operator-mode` is not a global minor mode. It must be
enabled separately for each major mode that you wish to use it with.


## Customisation

The spacing around operators is controlled by "rules". The simplest rule is
a pair containing the (unspaced) operator and the correctly spaced operator
as strings. For example `(cons "=" " = ")` is the default rule for `=`.
Alternatively the second element of the pair can be a function which
returns the correctly spaced operator.

Each major mode has its own list of rules for the spacing of operators. The
rule list for a major mode is looked up in the hash table
`electric-operator-mode-rules-table`. Rule lists can be modified using the
function `electric-operator-add-rules-for-mode`, which will automatically
replace any existing rules for the same operator. As an example: to
automatically add spacing around `->` and `=>` in python mode you would use

    (electric-operator-add-rules-for-mode 'python-mode
      (cons "->" " -> ")
      (cons "=>" " => "))

To print a list of the rules in use for a mode use `M-x
electric-operator-pretty-print-rules-for-mode` (note: this displays in the
`*Messages*` buffer).

Rules can be disabled in a similar way by setting the second element of the
rule to nil. For example if you find that the `*` operator in C is not
working reliably enough for pointer types you would use

    (electric-operator-add-rules-for-mode 'c-mode
      (cons "*" nil))

Rules for new modes can be added in exactly the same way. To add all the default
prog-mode rules to a new mode use:

    (apply #'electric-operator-add-rules-for-mode 'my-new-mode
           (electric-operator-get-rules-for-mode 'prog-mode))

Or similarly to add all existing rules for another mode. The default rules for
text modes can be added in the same way using
`(electric-operator-get-rules-for-mode text-mode)`.


Other customisation options are available to tweak behaviour for some
modes. Use `M-x customize-apropos <RET> electric-operator` to see the full
list and set them as normal using `setq`, customize etc. as you prefer.

Note that [inferior
modes](https://stackoverflow.com/questions/25225131/whats-so-inferior-about-inferior-lisp)
are implemented by Emacs as separate modes to their respective programming
modes. So customisations to electric-operator for e.g. `python-mode` (or
`ess-r-mode`) will not automatically apply to `inferior-python-mode`
(respectively `inferior-ess-r-mode`).


## Programming language support

A number of basic operator rules are defined for any major mode, so if your
language is vaguely C-like then a good amount of functionality should just work.

If you use `electric-operator` for a major mode not listed below here
please open an issue to let me know what works.

Good support is implemented for:

* Python
* R
* Javascript (and coffeescript, typescript)
* SQL
* C
* C++
* Java
* Rust
* PHP
* Julia
* CSS
* Fortran
* Perl (except `x`, `%`, `<`, `>`, and `/` operators)

In C-like languages there are some difficulties with distinguishing between `*`
for pointer types and for multiplication. Similiarly for `&` (reference types vs
bitwise and), and `<`, `>` (comparision operators vs angle brackets).


The following languages are supported but not extensively tested, please open an
issue to let me know how it goes if you use them.

* Ruby
* LaTeX

## Electric newlines

Curretly I don't really support adding new lines after operators because emacs
core has support for this in `electric-layout-mode`. But since it's a closely
related concept here's my not-quite-trivial configuration for electric newlines
in python which works well with `electric-operator-mode`:

    (defun ds/python-electric-newline ()
      (let ((paren (ds/enclosing-paren)))
        (if (not (or (eq paren ?\{)
                     (eq paren ?\[)
                     (eq paren ?\()
                     (looking-back "\\blambda\\b.*")))
            'after
        nil)))

    (defun ds/setup-python-electric-layout ()
      (make-local-variable 'electric-layout-rules)
      (add-to-list 'electric-layout-rules (cons ?: #'ds/python-electric-newline)))

    (electric-layout-mode 1)
    (add-hook 'python-mode-hook #'ds/setup-python-electric-layout)




## Contributing

I'm using [Cask](https://github.com/cask/cask.el) to manage dependencies, so to
run the tests you'll need to install cask then run `cask install` to install
dependencies. Then the tests can be run with `make test` (or manually with
`ecukes` and `ert-runner`, but make sure you exclude ecukes tests tagged with
`@known-failure`).

Bug reports are also welcome!


## History

Electric-operator is based on a heavily refactored version of
[electric-spacing](https://github.com/xwl/electric-spacing) by William Xu
with contributions from Nikolaj Schumacher. Electric-spacing is itself
based on [smart-operator](http://www.emacswiki.org/emacs/SmartOperator),
also by William Xu.

Electric-operator uses simpler and more flexible rules to define how
operators should be treated, and also adds a full suite of tests. However
it has some additional dependencies (in particular the excellent
[`dash`](https://github.com/magnars/dash.el)) which were decided
to be too heavy to add to electric-spacing.


## Changelog


### Unstable

* Fix treating comint-based modes (typically called inferior-...) as plain text
  modes by default.
* Apply default python rules to inferior-python-mode, and similiarly for R.
* Add rules pretty-printing function `electric-operator-pretty-print-rules-for-mode`.
* Fix some C/C++ edge cases in declaring r-value references and pointer-to-pointers.
* Add support for postgresql json operators
* Dropped Haskell support because it was essentially unusable and made
  everything more complex.
* Add support for LaTeX math.
* Add Fortran support
* Fix spacing of pseudo classes in CSS/SCSS
* Add perl support
* Improve ruby support
* Add support for treesitter modes like c-ts-mode


### Version 2.0

BREAKING:

* `electric-operator-prog-mode-rules` and `electric-operator-prose-mode-rules`
  have been removed, please use `(get-rules-for-mode 'prog-mode)` to get the
  rules and `(add-rules-for-mode 'prog-mode ...rules... )` to set rules for
  prog-mode. For prose rules use `'text-mode`.

Other changes:

* Major performance improvements!
* Electric operator now uses a trie data structure instead of a list of rules
  internally. This shouldn't have any effect on configs that only use
  non-private functions (i.e. those without `--` in their name).


### Version 1.1.0

* Major performance improvements
* Expansions of operators at the start of a line now won't modify indentation
* Typescript support
* Julia support
* Better comment handling in C-like languages
* Type annotations in Python
* C++ lambda captures
* Some additional python operators
* Fix comma operator in R for current unstable version of ess-mode (July 2018)

### Version 1.0

* Improve SQL support
* Add basic Haskell support (no partially-evaluated operators)
* Add js2-mode support
* Fix running hook for `,` in ess-mode even when electric-operator is disabled

### Version 0.3

* Feature: now fixes existing whitespace after operators as well as before them
* Improve python lambdas
* Add option for R to control default argument spacing
* Improve Javascript comment handling
* Improve Javascript regex handling
* Support Javascript ES6 arrow functions
* Support some additional PHP operators
* Add support for CSS

### Version 0.2

* Disabled `<` and `>` in C++ and Java modes by default.
* Added support for emacs25.
* Added support for Javascript Coffeescript, Rust, PHP.
* Improved support for R, C++, C.


## Edge cases

This section lists places where electric-operator may fail, but I don't think
it's a big deal *and* it would be difficult to add support. Open an issue if you
really want one of these to work.

### Complex default arguments in python lambdas

In python lambda functions can have default arguments with almost arbitrary
initialisation code. If this code contains a `:`, i.e. a dict initialisation or
a list slice, then electric-operator may get confused. This shouldn't be an
issue in practice because:

1. The
  [mutable default argument](http://stackoverflow.com/questions/1132941/least-astonishment-in-python-the-mutable-default-argument)
  problem means you probably shouldn't use dicts or lists as default arguments.

2. If you need default arguments then the function is probably complex enough to
   deserve a name.

It's difficult to solve because we would need to step over matching `{}` and
`[]` while searching for a `lambda` to match the current `:`. I think this
impossible with regexs so I would have to find something more powerful.

### Perl

Perl has some extremely flexible operator behaviours which seem impossible to support:

* Use of `x` as both an operator and a part of a variable name.
* Defining your own operators
* Defining your own regex quote charaters, e.g. `q#foo#b`.

