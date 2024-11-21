# red - Ed editor in Rust

## Features
- [x] `(.)a` - append after the addressed line, enter input mode.
- [x] `(.,.)c` - delete the addressed lines and enter input mode.
- [x] `(.,.)d` - delete the addressed lines.
- [x] `e file` - edit file & set default filename.
- [x] `e !command` - edit output of !command.
- [x] `E file` - edit file unconditionally.
- [x] `f file` - set default filename to file.
- [ ] `(1,$)g/re/command-list` - apply command list to addressed matching lines.
- [ ] `(1,$)G/re/` - interactively edit the addressed lines matching re.
- [x] `H` - toggle printing error messages.
- [x] `h` - print an explanation of the last error.
- [x] `(.)i` - insert text before the addressed line, enter input mode.
- [x] `(.,.+1)j` - join the addressed lines.
- [x] `(.)k<lowercase>` - mark the addressed line with a lowercase letter which can then be addressed with `'<lowercase>`.
- [x] `(.,.)l` - print the addressed lines unambiguously (.
   - [ ] implement paging for lines that overflow the screen.
- [x] `(.,.)m(.)` - move the addressed lines after the destination address.
- [x] `(.,.)n` - print the addressed lines with line numbers.
- [x] `(.,.)p` - print the addressed lines.
- [x] `P` - toggle the command prompt on and off.
    - [ ] Command line argument for prompt.
- [x] `q` - quit. Prompt to save if the buffer is dirty.
- [x] `Q` - quit unconditionally.
- [x] `($)r file` - read the contents of file after the addressed line.
- [x] `($)r !command` - read the output of `!command` after the addressed line.
- [ ] `(.,.)s/re/replacement/` - text substitution, first match in each line.
    - [ ] `(.,.)s/re/replacement/g` - all matches per line.
    - [ ] `(.,.)s/re/replacement/n` - nth match per line.
    - [ ] `(.,.)s` - repeat last substitution.
- [x] `(.,.)t(.)` - copy the addressed lines after the destination address.
- [ ] `u` - toggle undo.
- [ ] `(1,$)v/re/command-list` - apply command-list to each addressed line not matching re.
- [ ] `(1,$)V/re/` - interactively edit lines not matching re.
- [x] `(1,$)w file` - write the addressed lines to file. If there is no default filename, the default is set to file.
- [x] `(1,$)wq file` - write and quit.
- [x] `(1,$)w !command` - write the addressed lines to the output of !command.
- [x] `(1,$)W` file - append the addressed lines to file.
- [ ] `(.+1)zn - scroll n lines at a time starting at addressed line. If n is not specified, the current window size is used.
- [x] `!command` - execute a shell command. !! repeats the last such command. Unescaped % is replaced with the default filename.
- [x] `($)=` - print the line number of the addressed line.
- [x] `(.+1)<newline>` - print the addressed line and set the current address to that line.
