There are the main front-ends for the Koka compiler:

- `langserver`: the regular Koka compiler with VS code language server support.
  See also `support/vscode` for the VS code extension.

- `plain`: Koka compiler + interpreter without language server support; this is mainly
  to support platforms where the Haskell language server library does not build.


