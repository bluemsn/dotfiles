# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/docpad"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "docpad"

# Split window into panes.
split_h 50
split_v 50

# Run commands.
run_cmd "tree -I node_modules" 3
run_cmd "vim" 1

# Paste text
send_keys "docpad run" 2

# Set active pane.
select_pane 1
