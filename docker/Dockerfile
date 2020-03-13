FROM ubuntu:bionic

# Avoid interaction with apt during installation
ARG DEBIAN_FRONTEND=noninteractive

# Dependencies
RUN apt update
RUN apt-get -y install git nginx apache2 erlang git postgresql php7.2 php7.2-dev php7.2-pgsql php7.2-gd php-log make curl inotify-tools
RUN pecl install oauth

# Incrementor for partial rebuilds. If the above is cached, just increase the number and the build starts from here.
RUN echo 23

# setup database
RUN git clone https://github.com/hauke96/MapCraft.git
RUN chmod -R a+wr MapCraft

RUN echo 6
ARG OAUTH_KEY
ARG OAUTH_SEC
ARG AUTH_TYPE
ARG DB_PORT
RUN echo "$OAUTH_KEY // $OAUTH_SEC"
RUN echo "$AUTH_TYPE"
RUN echo "$DB_PORT"
RUN sed -i 's/\$\$CONSKEY\$\$/'"$OAUTH_KEY"'/' /MapCraft/app/auth_osm.php
RUN sed -i 's/\$\$CONSKEY\$\$/'"$OAUTH_KEY"'/' /MapCraft/app/oauth_callback.php
RUN sed -i 's/\$\$CONSSEC\$\$/'"$OAUTH_SEC"'/' /MapCraft/app/auth_osm.php
RUN sed -i 's/\$\$CONSSEC\$\$/'"$OAUTH_SEC"'/' /MapCraft/app/oauth_callback.php

# Start database
RUN sed -i 's/port = .*/port = '"$DB_PORT"'/' /etc/postgresql/10/main/postgresql.conf
USER postgres
RUN service postgresql start && cd MapCraft && ./scripts/env init

RUN echo 1
USER root
COPY start.sh .
RUN chmod +x start.sh
ENTRYPOINT ./start.sh
