.PHONY: container run runcontainer test
run:
	gforth-fast cosmic_conquest_modern.fs

test:
	gforth-fast cosmic_conquest_modern.fs 2> /tmp/ccm.errlog

container: Dockerfile
	docker build -t cosmic_conquest .

Dockerfile: cosmic_conquest_modern.fs modernise.fs
	@echo "Generating Dockerfile..."
	@echo "FROM ubuntu:latest" > Dockerfile
	@echo "RUN apt-get update && apt-get install -y sed gforth" >> Dockerfile
	@echo "COPY cosmic_conquest_modern.fs cosmic_conquest_modern.fs" >> Dockerfile
	@echo "COPY modernise.fs modernise.fs" >> Dockerfile
	@echo "COPY go.sh go.sh" >> Dockerfile
	@echo "COPY gamedata gamedata" >> Dockerfile
	@echo "COPY .score .score" >> Dockerfile
	@echo "run chmod 755 gamedata" >> Dockerfile
	@echo "run ls -lrthR gamedata/* ">> Dockerfile
	@echo "run chmod +x ./go.sh">> Dockerfile
	@echo "run sed -i '701,710 s/readfile//'  cosmic_conquest_modern.fs" >> Dockerfile
	@echo "CMD bash -c \"./go.sh\"" >> Dockerfile

runcontainer: 
	docker run -it cosmic_conquest:latest

clean:
	rm -f Dockerfile

