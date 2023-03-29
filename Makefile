
.PHONY: help docker.build docker.run grisp.image

APP_NAME := erlgame_grisp2
BUILD_VSN := 0.1.0
GKE_PROJECT := grisp

default: help

#❓ help: @ Displays this message
help:
	@grep -E '[a-zA-Z\.\-]+:.*?@ .*$$' $(firstword $(MAKEFILE_LIST))| tr -d '#'  | awk 'BEGIN {FS = ":.*?@ "}; {printf "\033[32m%-30s\033[0m %s\n", $$1, $$2}'

#🐳 docker.build: @ Builds a new local docker image
docker.build: SHELL:=/bin/bash
docker.build: MIX_ENV=prod
docker.build: TAG:=latest
docker.build:
	@echo "🐳  Building the grisp image builder with docker for ${APP_NAME}"
	@docker build --target candidate -t ${GKE_PROJECT}/${APP_NAME}:${BUILD_VSN} \
	-t ${GKE_PROJECT}/${APP_NAME}:latest -f devops/image-builder/Dockerfile .

#💻 docker.run: @ Run the docker image and provide a shell
docker.run: SHELL:=/bin/bash
docker.run: TAG:=latest
docker.run:
	@docker container run -it --rm -v `pwd`:/usr/local/app ${GKE_PROJECT}/${APP_NAME}:latest /bin/sh

#📦 grisp.image: @ Create the grisp image in the image folder
grisp.image: SHELL:=/bin/bash
grisp.image: TAG:=latest
grisp.image:
	@rm -rf ./image/*
	@docker container run -it --rm -v `pwd`:/usr/local/app ${GKE_PROJECT}/${APP_NAME}:latest /bin/sh -c "rebar3 grisp deploy"