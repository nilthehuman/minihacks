#!/bin/bash

# This script will pull the newest version of each Vim plugin from GitHub

BUNDLEDIR="$HOME/.vim/bundle"
until [[ -d "$BUNDLEDIR" ]]; do
    echo "$BUNDLEDIR does not exist. Where do you keep your plugins?"
    read -p "BUNDLEDIR = " BUNDLEDIR
    BUNDLEDIR=`echo $BUNDLEDIR | sed s#~#\$HOME#g`
done

which git 1> /dev/null
if [[ $? != 0 ]]; then
    echo "You don't seem to have git installed."
    read -p "Install it now? (Y/n) " USERINPUT
    if [ "$USERINPUT" != "Y" ]; then
        echo "Abort."
        exit 0;
    fi
    sudo apt-get install git --yes
fi

MAX_PLUGIN_LENGTH=0
for plugin in $BUNDLEDIR/*/; do
    if [[ $MAX_PLUGIN_LENGTH -lt ${#plugin} ]]; then
        MAX_PLUGIN_LENGTH=${#plugin}
    fi
done
((MAX_PLUGIN_LENGTH--))
FORMAT="Upgrading %-${MAX_PLUGIN_LENGTH}s: "

for plugin in $BUNDLEDIR/*/; do
    if [ "$BASH_VERSINFO" -lt 4 ]; then
        printf "$FORMAT" $plugin
    else
        printf "$FORMAT" ${plugin::-1}
    fi
    cd $plugin
    git pull --no-stat
    cd ..
done

echo "Done."

