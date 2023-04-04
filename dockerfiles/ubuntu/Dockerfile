FROM ubuntu:22.04

# Setup Haskell
RUN apt-get update
RUN apt-get install -y build-essential curl git libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libnuma-dev libtinfo5
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV PATH $HOME/.ghcup/bin:$PATH
ENV PATH $HOME/.cabal/bin:$PATH
RUN ghcup install ghc 9.4.4 --set
RUN ghcup install cabal 3.8.1.0 --set

# Setup Python
RUN curl https://pyenv.run | bash
ENV PATH $HOME/.pyenv/bin:$PATH
RUN pyenv install 3.7
RUN pyenv install 3.8
RUN pyenv install 3.9
RUN pyenv install 3.10
RUN pyenv install 3.11

# Build Vehicle
RUN git clone git@github.com:vehicle-lang/vehicle.git
