#!/bin/bash

APACHE_PREFIX=/usr/local/apache2
APACHE_CTRL=$APACHE_PREFIX/bin/apachectl
APACHE_HTDOCS=$APACHE_PREFIX/htdocs/
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $SCRIPT_DIR
PELICAN_OUTPUT_DIR=${SCRIPT_DIR}/output/

function usage(){
  echo "usage: $0 start|stop|restart|rsync"
  echo "COMMANDS"
  echo "    start: Starts the Apache local server on localhost:8080"
  echo "    stop: Stops the Apache local server"
  echo "    restart: Restarts the Apache local server"
  echo "    rsync: Executes rsync to synchronize the latest changes from"
  echo "           the Pelican output directory to Apache's htdocs folder"
  echo "    clean: Wipes clean Apache's htdocs folder and synchronizes"
  echo "           everything from the Pelican output directory."
  exit 3
}

#-- MAIN -- #
[[ ($# -gt 2) ]] && usage

if [[ ($1 == "start") ]]; then
  $APACHE_CTRL -k start
elif [[ $1 == "stop" ]]; then
  $APACHE_CTRL -k stop
elif [[ $1 == "restart" ]]; then
  $APACHE_CTRL -k restart
elif [[ $1 == "rsync" ]]; then
    rsync -vr $PELICAN_OUTPUT_DIR $APACHE_HTDOCS
elif [[ $1 == "clean" ]]; then
    rm -r $(PELICAN_OUTPUT_DIR).*
    rsync -vr $PELICAN_OUTPUT_DIR $APACHE_HTDOCS
else
  usage
fi
