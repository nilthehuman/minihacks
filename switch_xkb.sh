keyboard_layout=hungarian

keymap_folder="$HOME/.xkb/keymaps"
var_persist_file="$HOME/.xkb/current_keyboard"

function switch_keyboard_layout {
  if [ -f "$var_persist_file" ] ; then
    keyboard_layout=`cat $var_persist_file`
  fi

  if [ "$keyboard_layout" = hungarian ] ; then
    echo "Switching to Latin macrons."
    xkbcomp $keymap_folder/latin $DISPLAY
    keyboard_layout=latin
  else
    echo "Switching to Hungarian acutes."
    xkbcomp $keymap_folder/hungarian $DISPLAY
    keyboard_layout=hungarian
  fi

  echo "$keyboard_layout" > $var_persist_file
}

switch_keyboard_layout

