#!/bin/bash/
Rscript --verbose RMtest2.r
echo MAILING
printf "RMtest2.r finished\n$(date)" | mail -r ekonagay@stanford.edu -s "RMtest2.r fin" ekonagaya@berkeley.edu
echo EXITING SCRIPT

exit 1
