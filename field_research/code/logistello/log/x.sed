#!/bin/tcsh -f

foreach f (*.h *.c)
  echo $f
  sed -e s/SBYTE/sint1/g $f > $f.1
  mv $f.1 $f
end
