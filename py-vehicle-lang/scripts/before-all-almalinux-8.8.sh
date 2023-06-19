#!/bin/sh

# Install prerequisites
python3.11 -m pip install wget

# Determine architecture
machine="$(python3.11 -c 'import platform; print(platform.machine())')"

# Link libgmp.so.10 to libgmp.so
cd "/usr/lib64" && ln -s "libgmp.so.10" "libgmp.so"

# GHC version
GHC_VERSION="9.4.4"

# GHC release URL
case "${machine}" in
    'aarch64')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-aarch64-deb10-linux.tar.xz"
    ;;
    'x86_64')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-deb10-linux.tar.xz"
    ;;
    *)
        echo "unsupported machine: ${machine}"
        exit 1
    ;;
esac

# Install GHC
python3.11 -c "import wget; wget.download('${GHC_RELEASE_URL}', '/tmp/ghc.tar.xz')"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1
cd "/tmp/ghc" && ./configure --prefix="/usr/local"
cd "/tmp/ghc" && make install

# Cabal version and release URL
CABAL_VERSION="3.10.1.0"
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v${CABAL_VERSION}.zip"

# Install Cabal
python3.11 -c "import wget; wget.download('${CABAL_RELEASE_URL}', '/tmp/cabal.zip')"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v${CABAL_VERSION}" "/tmp/cabal"
cd "/tmp/cabal" && python3.11 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-${GHC_VERSION}.json" -w "/usr/local/bin/ghc-${GHC_VERSION}"
"/tmp/cabal/_build/bin/cabal" v2-update
"/tmp/cabal/_build/bin/cabal" v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"
