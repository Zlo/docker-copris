FROM ubuntu:24.04

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update \
 && apt-get -y upgrade \
 && apt-get -y install apt-transport-https apt-utils ca-certificates curl git gnupg \
 && apt-get -y install default-jdk-headless \
 && apt-get clean \
 && rm -rf -- /tmp/* /var/lib/apt/lists/*

RUN curl -o /tmp/scala.deb https://downloads.lightbend.com/scala/2.12.19/scala-2.12.19.deb \
 && dpkg -i /tmp/scala.deb \
 && rm -f /tmp/scala.deb

RUN cd /opt \
 && mkdir work \
 && git clone https://gitlab.com/cspsat/prog-copris.git \
 && cd prog-copris/examples \
 && scalac -cp ../build/copris-all_2.12-2.3.2.jar Examples.scala

COPY install/. /.

WORKDIR /work
