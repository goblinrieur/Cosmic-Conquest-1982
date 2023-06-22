FROM debian:latest
RUN apt-get -y update && apt-get install -y gforth --fix-missing
RUN mkdir gamedata
COPY *.fs ./
COPY .score ./
COPY gamedata/* ./gamedata/
COPY LICENSE ./
RUN ls gamedata/*
ENTRYPOINT /usr/bin/gforth-fast ./cosmic_conquest_modern.fs
