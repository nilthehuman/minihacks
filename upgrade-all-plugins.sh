#!/bin/bash

# This script will pull the newest version of each Vim plugin from GitHub

BUNDLEDIR="$HOME/.vim/bundle"
until [[ -d "$BUNDLEDIR" ]]; do
    if [[ ! -t 0 ]]; then
        # non-interactive shell
        exit 1;
    fi
    echo "$BUNDLEDIR does not exist. Where do you keep your plugins?"
    read -p "BUNDLEDIR = " BUNDLEDIR
    BUNDLEDIR=`echo $BUNDLEDIR | sed s#~#\$HOME#g`
done

which git 1> /dev/null
if [[ $? != 0 ]]; then
    if [[ ! -t 0 ]]; then
        # non-interactive shell
        exit 2;
    fi
    echo "You don't seem to have git installed."
    read -p "Install it now? (y/N) " USERINPUT
    if [ "$USERINPUT" != "y" ]; then
        unset BUNDLEDIR USERINPUT
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

pushd . > /dev/null
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
popd > /dev/null

unset BUNDLEDIR USERINPUT MAX_PLUGIN_LENGTH FORMAT plugin

echo "Done."

