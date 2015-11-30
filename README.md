# command-chain

Word chain game by emacs command names.

Emacsのコマンド名でしりとり

![Screenshot](screenshot.png)

## How to Play

Compile elisp file.

    M-x byte-compile-file /path/to/command-chain.el

Add following sexps in your `init.el`

    (push "/path/to/command-chain_directory" load-path)
    (require 'command-chain)

And play the game.

    M-x command-chain

## Play with Default Emacs Environment

If you want to play on fresh emacs, launch emacs with `-Q` option.

    $ emacs -Q

And load the compiled file.

    M-: (load "/path/to/command-chain.elc")
    M-x command-chain
