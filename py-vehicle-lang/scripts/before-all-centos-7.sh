#!/bin/sh

# Install prerequisites
python3 -m pip install wget

# Determine architecture
machine="$(python3 -c 'import platform; print(platform.machine())')"

# Link libgmp.so.10 to libgmp.so
case "${machine}" in
    'i386')
        cd "/usr/lib" && ln -s "libgmp.so.10" "libgmp.so"
    ;;
    'i686')
        cd "/usr/lib" && ln -s "libgmp.so.10" "libgmp.so"
    ;;
    'x86_64')
        cd "/usr/lib64" && ln -s "libgmp.so.10" "libgmp.so"
    ;;
    *)
        echo "unsupported machine: ${machine}"
        exit 1
    ;;
esac

# GHC release URL
case "${machine}" in
    'i386')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-i386-deb9-linux.tar.xz"
    ;;
    'i686')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-i386-deb9-linux.tar.xz"
    ;;
    'x86_64')
        GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-x86_64-centos7-linux.tar.xz"
    ;;
    *)
        echo "unsupported machine: ${machine}"
        exit 1
    ;;
esac

# Install GHC
python3 -c "import wget; wget.download('${GHC_RELEASE_URL}', '/tmp/ghc.tar.xz')"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1
cd "/tmp/ghc" && ./configure --prefix="/usr/local"
cd "/tmp/ghc" && make install

# Cabal version and release URL
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip"

# Install Cabal
python3 -c "import wget; wget.download('${CABAL_RELEASE_URL}', '/tmp/cabal.zip')"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v3.10.1.0" "/tmp/cabal"
sed -ie 's/+ofd-locking/-ofd-locking/' "/tmp/cabal/bootstrap/linux-9.4.4.json"
cd "/tmp/cabal" && python3 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-9.4.4.json" -w "/usr/local/bin/ghc-9.4.4"
"/tmp/cabal/_build/bin/cabal" v2-update
"/tmp/cabal/_build/bin/cabal" v2-install cabal-install --constraint='lukko -ofd-locking' --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"
