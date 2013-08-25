fgrep '#include' book/*.c eval/*.c io/*.c misc/*.c move/*.c \
search/*.c select/*.c old/*.c | \
ngrep -v 'stdio|sys|ctype|curses|math|signal|fcntl' | \
sed  -e 's/\.c/\.o/' -e 's/#include </ include\//' -e 's/>//'
