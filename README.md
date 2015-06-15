# Electric spacing mode

An emacs minor-mode to automatically add spacing around operators.

For example typing

    a=10*5+2

results in

    a = 10 * 5 + 2

I'm aiming to have electric-spacing-mode "correctly" handle every (or
almost every) operator. If you find a case where it doesn't space something
as expected then consider it a bug, and please report it :).


## Setup

The simplest way to install electric-spacing-mode is via package.el.

To temporarily enable electric-spacing-mode simply call
`electric-spacing-mode`. To permenantly enable it for a major mode simply
add it to the relevant mode hook. For example for python-mode add the
following to your config:

    (add-hook 'python-mode-hook #'electric-spacing-mode)

Note that `electric-spacing-mode` is not a global minor mode, and so must
be enabled separately for each major mode.


## Customisation

The spacing around operators is controlled by "rules". The simplest rule is
a pair strings giving the (unspaced) operator and the correctly spaced
operator, e.g. `(cons "=" " = ")` is the default rule for `=`.
Alternatively the second element of the pair can be a function which
returns the correctly spaced operator.

Each major mode has its own list of rules for the spacing of operators. The
rule list for a major mode is looked up in the hash table
`electric-spacing-mode-rules-table`. Rule lists can be modified using the
function `electric-spacing-add-rules-for-mode`, which will automatically
replace any existing rules for the same operator. To disable a rule set the
action part of the rule (the second element) to nil.

As an example: to automatically add spacing around `->` and `=>` in python
mode you would use

    (electric-spacing-add-rules-for-mode 'python-mode
      (cons "->" " -> ")
      (cons "=>" " => "))

placed somewhere in your startup files. 

To use the default rules for a new programming mode use `apply` to add all
rules from `electric-spacing-prog-mode-rules`:

    (apply #'electric-spacing-add-rules-for-mode 'my-new-mode
           electric-spacing-prog-mode-rules)

The default rules for text modes can be added in the same way from the list
`electric-spacing-prose-rules`.


## Caveats

* Negative numbers might still be a bit flakey. It seems to be difficult to
  reliably distinguish between negative numbers and the minus operator.
  Suggestions welcome :)

* No support for lisp modes. I'm not sure what we could do that's
  particularly useful in these cases since there aren't really many/any
  operators (again, suggestions are welcome).


## Contributing

I'm using [Cask](https://github.com/rejeep/cask.el) to manage dependencies,
so to run the tests you'll need to install cask then run `cask install` to
install dependencies. Then the tests can be run with `make test`.

Bug reports are also welcome!
