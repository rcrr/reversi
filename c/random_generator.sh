#!/bin/bash
#
# random_generator.sh
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# @copyright 2013 Roberto Corradini. All rights reserved.
#
# License
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.

program="random_generator.sh"
re='^[0-9]+$'

size=8
columns=4

while test $# -gt 0; do
    case "$1" in
        -h|--help)
            echo "$program - generates random 64 bit integer and compose an ANSI C array"
            echo "The generation of random nmbers is performed by the command \"od -An -tx2 -N8 < /dev/urandom\""
            echo " "
            echo "$program [options]"
            echo " "
            echo "options:"
            echo "-h, --help            show brief help"
            echo "-s, --size            specify the array size, default is 8"
            echo "-c, --columns         specify the number of columns used by the output format, default is 4"
            exit 0
            ;;
        -s|--size)
            shift
            if test $# -gt 0; then
                size=$1
                if ! [[ $size =~ $re ]] ; then
                    echo "error: Not a number" >&2; exit 1
                fi
            else
                echo "no size specified"
                exit 1
            fi
            shift
            ;;
        -c|--columns)
            shift
            if test $# -gt 0; then
                columns=$1
                if ! [[ $columns =~ $re ]] ; then
                    echo "error: Not a number" >&2; exit 1
                fi
                if [ $columns -le 0 ] ; then
                    echo "error: columns must be greater than zero" >&2; exit 1
                fi
            else
                echo "no columns specified"
                exit 1
            fi
            shift
            ;;
        *)
            break
            ;;
    esac
done

declare -a randoms

for (( i=0; i<=$(( $size -1 )); i++ ))
do
    # echo -n "$i: "
    random_string=$(od -An -tx2 -N8 < /dev/urandom)
    random_string_compacted=${random_string// /}
    random_string_uppercase=${random_string_compacted^^}
    u_rand_int_64="0x"$random_string_uppercase
    # echo $u_rand_int_64
    randoms[i]=$u_rand_int_64
done

echo "static uint64 array[] = {"
counter=0
while [ $counter -lt $size ]
do
    echo -n "  "
    for (( i=0; i<=$(( $columns - 1 )); i++ ))
    do
        echo -n ${randoms[$counter]}
        let counter+=1
        [ $counter -ge $size ] && break
        echo -n ","
        if [ $i -ne $(( $columns - 1 )) ]; then
            echo -n " "
        fi
    done
    echo ;
done
echo "};"

exit 0
