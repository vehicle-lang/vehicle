FROM ubuntu:22.04

# Install GHC & Cabal (via GHCup)
RUN apt-get update -y
RUN apt-get install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 -y
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV GHCUP_ROOT="$HOME/.ghcup"
RUN source $GHCUP_ROOT/env
RUN ghcup install ghc 9.4.4 --set
RUN ghcup install cabal 3.8.1.0 --set

# Install Python (via PyEnv)
RUN apt-get install build-essential git libbz2-dev libffi-dev liblzma-dev libreadline-dev libsqlite3-dev libssl-dev zlib1g-dev -y
RUN curl https://pyenv.run | bash
ENV PYENV_ROOT="$HOME/.pyenv"
ENV PATH="$PYENV_ROOT/bin:$PATH"
RUN eval "$(pyenv init -)"
RUN pyenv update
RUN pyenv install 3.7
RUN pyenv install 3.8
RUN pyenv install 3.9
RUN pyenv install 3.10
RUN pyenv install 3.11

# Build Vehicle
RUN apt-get install patchelf swig -y
RUN git clone https://github.com/vehicle-lang/vehicle.git vehicle
WORKDIR vehicle/py-vehicle-lang
RUN sh ./scripts/build-wheels
