# markdown-toc-generator

[![Build Status](https://travis-ci.org/vaidik/markdown-toc-generator.svg?branch=master)](https://travis-ci.org/vaidik/markdown-toc-generator)

Builds Table of Contents for a given markdown file.

<!-- TOC -->

1.  Usage
2.  Installation
    1.  Build from source
3.  Development

**Note:** this TOC is generated using markdown-toc-generator.

<!-- TOC END -->

## Usage

CLI options:

    Usage:
      markdown-toc-generator [--with-heading] [--no-first-h1] [--bullets] [--append=<ending-markdown>] [--write] <file>

    Options:
      -h, --with-heading        Add heading for the generated TOC
      -a, --append=<markdown>   Append a string at the end of the TOC
      -n, --no-first-h1         Exclude the first h1-level heading in a file
      -b, --bullets             Use bullets instead of ordered list
      -w, --write               Write TOC in the beginning of the markdown file
                                instead of printing on the console (default
                                behaviour).

## Installation

As of now, downloadable binaries are not available. So build from source.

### Build from source

**Requirements:**

  - Stack and GHC (if not Docker)

Execute to following commands to setup the environment:

    git clone git@github.com:vaidik/markdown-toc-generator.git
    cd markdown-toc-generator
    stack build --copy-bins

After this, you will find the binary `markdown-toc-generator` in your `PATH`.

## Development

Build the image:

    docker build . -t markdown-toc-generator:dev

Use the image for running the container:

    docker run -it -v "$(pwd)":/opt/markdown-toc-generator markdown-toc-generator:dev bash

Run GHC in the container:

    stack ghci

or, build manually:

    stack build
