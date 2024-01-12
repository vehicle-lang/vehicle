# To build: docker build -o . .
FROM texlive/texlive:latest AS build-stage

# Install vehicle-lang
RUN <<EOT bash
apt-get update
apt-get install -y python3-pip
python3 -m pip install vehicle-lang --break-system-packages
EOT

# Add source files from build-context
WORKDIR /workdir

# Add your source files!
#
# The `ADD` command supports glob patterns for matching,
# but does not preserve the directory structure.
# https://docs.docker.com/engine/reference/builder/#add
#
# If you are using Dockerfile version 1.6 or later, you
# can use the `COPY` command with `--parents` to preserve
# the directory structure, e.g.,
#
#   COPY --parents **/* .

ADD sample.tex .

# Build with latexmk
RUN latexmk -shell-escape -pdf sample.tex

# Export paper.pdf to scratch
FROM scratch AS export-stage
COPY --from=build-stage /workdir/sample.pdf /
