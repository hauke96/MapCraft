#!/bin/bash

service postgresql start

./MapCraft/scripts/env all-start

echo "LOGGING:"

tail -n 100 -f /MapCraft/logs/apache* | sed 's/^/Apache 2: /' &
tail -n 100 -f /MapCraft/logs/nginx* | sed 's/^/nginx: /' &
tail -n 100 -f /var/log/postgresql/postgresql-10-main.log | sed 's/^/PostgreSQL: /'
