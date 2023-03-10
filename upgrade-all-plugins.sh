#!/bin/bash

# This script will pull the newest version of each Vim plugin from GitHub

PACKDIR="$HOME/.vim/pack"
until [[ -d "$PACKDIR" ]]; do
    if [[ ! -t 0 ]]; then
        # non-interactive shell
        exit 1;
    fi
    echo "$PACKDIR does not exist. Where do you keep your plugins?"
    read -p "PACKDIR = " PACKDIR
    PACKDIR=`echo $PACKDIR | sed s#~#\$HOME#g`
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
        unset PACKDIR USERINPUT
        echo "Abort."
        exit 0;
    fi
    sudo apt-get install git --yes
fi

ALL_PLUGINS=()
MAX_PLUGIN_LENGTH=0
for vendor in $PACKDIR/*; do
    for optstart in $vendor/*; do
        for plugin in $optstart/*; do
            ALL_PLUGINS+=($plugin)
            if [[ $MAX_PLUGIN_LENGTH -lt ${#plugin} ]]; then
                MAX_PLUGIN_LENGTH=${#plugin}
            fi
        done
    done
done
FORMAT="Upgrading %-${MAX_PLUGIN_LENGTH}s: "

pushd . > /dev/null
for plugin in ${ALL_PLUGINS[@]}; do
    printf "$FORMAT" $plugin
    cd $plugin
    git pull --no-stat
done
popd > /dev/null

unset PACKDIR USERINPUT MAX_PLUGIN_LENGTH FORMAT plugin

echo "Done."

