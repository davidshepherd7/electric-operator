# Electric operator mode

[![travis](https://travis-ci.org/davidshepherd7/electric-operator.svg?branch=master)](https://travis-ci.org/davidshepherd7/electric-operator) [![melpa](http://melpa.org/packages/electric-operator-badge.svg)](http://melpa.org/#/electric-operator) [![GPLv3](http://img.shields.io/badge/license-GNU%20GPLv3-blue.svg)](https://github.com/davidshepherd7/electric-operator/blob/master/LICENSE)

An emacs minor-mode to automatically add spacing around operators.

For example typing

    a=10*5+2

results in

    a = 10 * 5 + 2

I'm aiming to have electric-operator-mode "correctly" handle almost every
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

To temporarily enable electric-operator-mode simply call
`electric-operator-mode`. To permenantly enable it for a major mode simply
add it to the relevant mode hook. For example for python-mode add the
following to your config:

    (add-hook 'python-mode-hook #'electric-operator-mode)

Note that `electric-operator-mode` is not a global minor mode, and so must
be enabled separately for each major mode.


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
replace any existing rules for the same operator. To disable a rule set the
action part of the rule (the second element) to nil.

As an example: to automatically add spacing around `->` and `=>` in python
mode you would use

    (electric-operator-add-rules-for-mode 'python-mode
      (cons "->" " -> ")
      (cons "=>" " => "))

To use the default rules for a new programming mode use `apply` to add all
rules from `electric-operator-prog-mode-rules`:

    (apply #'electric-operator-add-rules-for-mode 'my-new-mode
           electric-operator-prog-mode-rules)

The default rules for text modes can be added in the same way from the list
`electric-operator-prose-rules`.


## Major mode readiness

A number of basic operator rules are defined for any major mode, so if your
language is "normal" (i.e C-like) then a good amount of functionality
should just work.

* *Python*: I've used `electric-operator` a lot for python. Support is
  extremely good. Specifically we handle edge cases in `*args`, `**kwargs`,
  `:` in dictionaries, kwarg assignment, and slices.

* *`ess-mode`*: At least one person is using `electric-operator` with
  `ess-mode` for R. R's syntax is fairly standard and straightforward, so
  everything *should* work well. Using `ess-mode` for languages other than
  R is not currently supported.

* *C and C++*: these are much more tricky to get right. Most things should
  be working ok but there are still some issues with
  pointer-to-class/struct types (#11) and templates (#8). I'm actively
  working on this.

* *Java*: I've added tweaks based on memory and a
  [syntax guide](http://www.tutorialspoint.com/java/java_quick_guide.htm).
  Since the syntax is largely simlar a simplification of C++ it should work
  well, but I haven't tried it out yet. Generics aren't handled properly
  (#8).

* *Ruby and Perl*: some tweaks for these modes were inherited from
  `electric-spacing`, but I haven't tried them personally. Pull requests
  are welcome!

* *Haskell*: I've added a number of tweaks based on memory and skimming a
  [syntax guide](http://prajitr.github.io/quick-haskell-syntax/), but I
  haven't had a chance to try it out yet. There's probabaly some work to do.

* *Lisps*: I don't think `electric-operator` has much to offer for lisps, so
  it doesn't do anything at the moment. If you can think of any spacing
  rules that would be nice then please submit an issue/pull-request.

* *Text modes*: Again, I'm not sure that we can do much in text modes. At the moment 
  spacing is added after `.` (behaviour inherited from `electric-spacing`) and `,`.

If you use `electric-operator` for a major mode not on this list please
open an issue to let me know how it went.


In general:

* Negative numbers might still be a bit flakey. It seems to be difficult to
reliably distinguish between negative numbers and the minus operator.
Suggestions welcome :)


## Contributing

I'm using [Cask](https://github.com/rejeep/cask.el) to manage dependencies,
so to run the tests you'll need to install cask then run `cask install` to
install dependencies. Then the tests can be run with `make test`.

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
