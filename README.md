# markdown-toc-generator

<!-- TOC -->

1.  Usage
2.  Installation
    1.  Download Binary
    2.  Build from source
3.  Development

**Note:** this TOC is generated using markdown-toc-generator.

<!-- TOC END -->

## Usage

Checkout USAGE.txt for the latest instructions

or

Run `markdown-toc-generator --h`

## Installation

### Download Binary

Head over to the Releases page to donwload the latest release.

### Build from source

**Requirements:**

  - Docker (preferrably)
  - Stack and GHC (if not Docker)

Execute to following commands to setup the environment:

    git clone git@github.com:vaidik/markdown-toc-generator.git
    cd markdown-toc-generator
    docker build . -t markdown-toc-generator:dev
    docker run -it -v "$(pwd)":/opt/markdown-toc-generator markdown-toc-generator:dev bash

Inside the container started as the result of the last command, run the following
command:

    stack build

And start using the binary.

## Development

Build the image:

    docker build . -t markdown-toc-generator:dev

Use the image for running the container:

    docker run -it -v "$(pwd)":/opt/markdown-toc-generator markdown-toc-generator:dev bash

Run GHC in the container:

    stack ghci

or, build manually:

    stack build
