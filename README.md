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


## Customisation and "rules"

The spacing around operators is controlled by "rules". The simplest rule is
a pair strings for the operator alone and the correctly spaced operator,
e.g. `(cons "=" " = ")` is the default rule for `=`. Alternatively the
second string can be replaced by a function which returns the correctly
spaced operator.

Each major mode has it's own list of rules for the spacing of operators.
The rule list for a major mode is looked up in the hash table
`electric-spacing-mode-rules-table`. To customise the behaviour of
electric-spacing-mode in a major mode you can modify the corresponding
"rule list". Rule lists can be accessed and replaced in the hash table with
the `gethash` and `puthash` functions respectively.

To add rules to a rule-list you should use the function `add-rules` which
will automatically replace any existing rules for the same operator. To
disable a rule set the action part (the second element) of the rule to nil.

As an example: to automatically add spacing around `->` in python mode you
would use

    (puthash 'python-mode
      (electric-spacing-add-rules
        (gethash 'python-mode electric-spacing-mode-rules-table)
        (cons "->" " -> "))
      electric-spacing-mode-rules-table)

This should be placed somewhere in your startup files.


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
