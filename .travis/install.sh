rm src/Syntax/Lexer.hs

if [[ "${build_with_stack}" = '' ]]; then
  cabal install alex
else
  stack install alex
fi

npm install
