#!/bin/bash

rm -r /work/project/temp; wait;
rm -r /work/project/final; wait;
t_0=date
sh resort.sh; wait;
t_f=date

echo $t_0 \n $t_f | mail -r ekonagay@stanford.edu -s 'redo.sh fin' ekonagaya@berkeley.edu
echo EXITING SCRIPT
exit 1
