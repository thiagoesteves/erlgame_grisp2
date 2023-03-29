# ./Dockerfile
ARG BASE_IMAGE=erlang:25.0.3-alpine

FROM ${BASE_IMAGE} AS candidate

WORKDIR /usr/local/app

RUN apk update
RUN apk add --no-cache 'build-base=~0.5' git openssh \
 && rm -rf /var/cache/apk/*
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

# Copies our app source code into the build container
COPY . .

# Create a fake project and run deploy to download the toolchain
RUN rebar3 new grispapp name=demo dest=/usr/local/app/demo/image \
    && cd demo && mkdir image \
    && sed -i 's/23/25.0.3/' rebar.config \
    && rebar3 grisp deploy

RUN rm -rf /usr/local/app/*