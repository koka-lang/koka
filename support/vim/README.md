# Koka support for (Neo)Vim

This directory functions as a plugin for (neo)vim, currently providing filetype detection
and some basic syntax highlighting.


## Installation

The code for this vim plugin is self-contained within the `support/vim` directory, and
can be installed with your (neo)vim plugin manager of choice.

For [vim-plug](https://github.com/junegunn/vim-plug), add the following to your
`~/.vimrc` or `~/.config/nvim/init.vim` file:


```
Plug 'https://github.com/koka-lang/koka', { 'rtp': 'support/vim' }
```

Then run the `:PlugInstall` (n)vim command.

