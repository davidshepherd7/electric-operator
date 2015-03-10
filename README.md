Electric spacing mode
============

An emacs minor-mode to automatically add spacing around operators.

For example typing

    a=10*5+2

results in

    a = 10 * 5 + 2


Setup
------

The simplest way to install electric-spacing-mode is via package.el.

To temporarily enable electric-spacing-mode simply call
`electric-spacing-mode`. To permenantly enable it for a major mode simply
add it to the relevant mode hook. For example for python-mode add the
following to your config[1]:

    (add-hook 'python-mode-hook #'electric-spacing-mode)

Note that `electric-spacing-mode` is not a global minor mode, and so must
be enabled separately for each major mode.


[1] For emacs versions < 24 you may need to [define a wrapper function](http://emacs.stackexchange.com/questions/5358/proper-way-to-enable-minor-mode) to enable the mode unconditionally.
