if [[ "${build_with_stack}" ]]; then
  mkdir -p ~/.local/bin
  export PATH=$HOME/.local/bin:$PATH
  travis_retry curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
else
  export PATH=$HOME/.cabal/bin:$PATH
  nvm install node
  nvm use node
fi
