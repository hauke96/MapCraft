FROM ubuntu:bionic

# Avoid interaction with apt during installation
ARG DEBIAN_FRONTEND=noninteractive

# Dependencies
RUN apt update
RUN apt-get -y install git nginx apache2 erlang git postgresql php7.2 php7.2-pgsql php7.2-gd php-log make curl inotify-tools

# Incrementor for partial rebuilds. If the above is cached, just increase the number and the build starts from here.
RUN echo 16

# setup database
RUN git clone https://github.com/hauke96/MapCraft.git
RUN chmod -R a+wr MapCraft

RUN echo 4
ARG OAUTH_KEY
ARG OAUTH_SEC
RUN echo "$OAUTH_KEY // $OAUTH_SEC"
RUN sed -i 's/$$CONSKEY$$/$OAUTH_KEY/' /MapCraft/app/auth_osm.php
RUN sed -i 's/$$CONSKEY$$/$OAUTH_KEY/' /MapCraft/app/oauth_callback.php
RUN sed -i 's/$$CONSSEC$$/$OAUTH_SEC/' /MapCraft/app/auth_osm.php
RUN sed -i 's/$$CONSSEC$$/$OAUTH_SEC/' /MapCraft/app/oauth_callback.php

# Switch to user "postgres"
USER postgres
RUN service postgresql start && cd MapCraft && ./scripts/env init

RUN echo 1
USER root
COPY start.sh .
RUN chmod +x start.sh
ENTRYPOINT ./start.sh
