.PHONY: all docker

all: docker

docker:
	docker build --pull -t copris .
