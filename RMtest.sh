#!/bin/bash/
Rscript --verbose RMtest.r
echo MAILING
printf "RMtest.r finished\n$(date)" | mail -r ekonagay@stanford.edu -s "RMtest.r fin" ekonagaya@berkeley.edu
echo EXITING SCRIPT

exit 1
