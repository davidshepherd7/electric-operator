# Electric operator mode

[![travis](https://travis-ci.org/davidshepherd7/electric-operator.svg?branch=master)](https://travis-ci.org/davidshepherd7/electric-operator) [![melpa](http://melpa.org/packages/electric-operator-badge.svg)](http://melpa.org/#/electric-operator) [![GPLv3](http://img.shields.io/badge/license-GNU%20GPLv3-blue.svg)](https://github.com/davidshepherd7/electric-operator/blob/master/LICENSE)

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

Rules can be disable in a similar way by setting the second element of the
rule to nil. For example if you find that the `*` operator in C is not
working reliably enough for pointer types you would use

    (electric-operator-add-rules-for-mode 'c-mode
      (cons "*" nil))

Rules for new modes can be added in exactly the same way. To use the default
rules for a new programming mode use `apply` to add all rules from
`electric-operator-prog-mode-rules`:

    (apply #'electric-operator-add-rules-for-mode 'my-new-mode
           electric-operator-prog-mode-rules)

The default rules for text modes can be added in the same way from the list
`electric-operator-prose-rules`.


Other customisation options are available to tweak behaviour for some
modes. Use `M-x customize-apropos <RET> electric-operator` to see the full
list and set them as normal using `setq`, customize etc. as you prefer.

## Programming language support

A number of basic operator rules are defined for any major mode, so if your
language is "normal" (i.e C-like) then a good amount of functionality
should just work.

If you use `electric-operator` for a major mode not listed below here
please open an issue to let me know what works.

Complete support is implemented for

* Python
* R
* Javascript
* Coffeescript
* PHP
* SQL


Languages with good but imperfect support are

* C
* C++
* Java
* Rust

there are difficulties with distinguishing between `*` for pointer types
and for multiplication. Similiarly for `&` (reference types vs bitwise
and), and `<`, `>` (comparision operators vs angle brackets.


The following languages are supported but not extensively tested (as far as
I know), please open an issue to let me know if you use them.

* Haskell
* Perl
* Ruby

## Contributing

I'm using [Cask](https://github.com/cask/cask.el) to manage dependencies, so to
run the tests you'll need to install cask then run `cask install` to install
dependencies. Then the tests can be run with `./test.sh` (or manually with
`ecukes`, but make sure you exclude tests tagged with `@known-failure`).

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
[`dash`](https://github.com/magnars/dash.el) and
[`names`](https://github.com/Malabarba/names) packages) which were decided
to be too heavy to add to electric-spacing.


## Changelog

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
