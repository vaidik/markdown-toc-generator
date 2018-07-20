FROM haskell

COPY . /opt/markdown-toc-generator
WORKDIR /opt/markdown-toc-generator

RUN stack setup
RUN stack install hlint
RUN stack build

RUN rm -rf /opt/markdown-toc-generator
