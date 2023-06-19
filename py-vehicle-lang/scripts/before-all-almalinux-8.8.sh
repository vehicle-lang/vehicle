#!/bin/sh

# Install prerequisites
python3.11 -m pip install wget

# Determine architecture
machine="$(python3.11 -c 'import platform; print(platform.machine())')"

# Link libgmp.so.10 to libgmp.so
cd "/usr/lib64" && ln -s "libgmp.so.10" "libgmp.so"

# GHC release URL
case "${machine}" in
    'aarch64')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-aarch64-deb10-linux.tar.xz"
    ;;
    'x86_64')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-x86_64-deb10-linux.tar.xz"
    ;;
    *)
        echo "unsupported machine: ${machine}"
        exit 1
    ;;
esac

# Install GHC 9.4.4
python3.11 -c "import wget; wget.download('${GHC_RELEASE_URL}', '/tmp/ghc.tar.xz')"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1
cd "/tmp/ghc" && ./configure --prefix="/"
cd "/tmp/ghc" && make install

# Cabal release URL
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip"

# Install Cabal 3.10.1.0
python3.11 -c "import wget; wget.download('${CABAL_RELEASE_URL}', '/tmp/cabal.zip')"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v3.10.1.0" "/tmp/cabal"
cd "/tmp/cabal" && python3.11 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-9.4.4.json" -w "/bin/ghc-9.4.4"
"/tmp/cabal/_build/bin/cabal" v2-update
"/tmp/cabal/_build/bin/cabal" v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir="/"
