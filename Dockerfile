FROM haskell

COPY . /toc-master
WORKDIR /toc-master

RUN stack setup
RUN stack build

RUN rm -rf /toc-master
