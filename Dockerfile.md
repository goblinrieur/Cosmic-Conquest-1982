# HOW TO PLAY IN CONTAINER

first build your container _even, this looks useless to me_

```
docker build . -t gamecc
```

```
Build an image from a Dockerfile
Sending build context to Docker daemon  4.663MB
Step 1/9 : FROM debian:latest
 ---> 34b4fa67dc04
Step 2/9 : RUN apt-get -y update && apt-get install -y gforth --fix-missing
 ---> Using cache
 ---> 4a0e9d66e965
Step 3/9 : RUN mkdir gamedata
 ---> Using cache
 ---> 91ab9cf2ca22
Step 4/9 : COPY *.fs ./
 ---> Using cache
 ---> 32f24e1e5dcb
Step 5/9 : COPY .score ./
 ---> Using cache
 ---> 6d47f15bf83e
Step 6/9 : COPY gamedata/* ./gamedata/
 ---> Using cache
 ---> 70e18ba14bce
Step 7/9 : COPY LICENSE ./
 ---> Using cache
 ---> 5b15f77f58f2
Step 8/9 : RUN ls gamedata/*
 ---> Using cache
 ---> dc0575ccd2ae
Step 9/9 : ENTRYPOINT /usr/bin/gforth-fast ./cosmic_conquest_modern.fs
 ---> Using cache
 ---> 7afb6ccac7dc
Successfully built 7afb6ccac7dc
Successfully tagged gamecc:latest
```

# Then play it 

```
docker run gamecc:latest
```

