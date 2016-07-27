#!/bin/bash
inventory='/work/project/LOGS/inventory.csv'
counts='/work/project/LOGS/counts.csv'
printf 'state,yr,filename,truth,trainingset\n' > $inventory
printf 'state,yr,n\n' > $counts


# ======================================
#   PRODUCE INVENTORY.CSV & COUNTS.CSV
# ======================================

cd /work/project/final
sta=($(ls)); wait;
t_0=$(date)

for i in ${sta[@]}
do
	cd /work/project/final/$i
	n=$(find . -type f | wc -l); 		wait;
	printf "%s,,%d\n" "$i" "$n"		>> $counts; wait;
	yrs=($(ls)); 				wait;		
	for j in ${yrs[@]}
	  do
		cd /work/project/final/$i/$j
		n=$(find . -type f | wc -l); 		wait;
		printf "%s,%d,%d\n" "$i" "$j" "$n"	>> $counts; wait;
		files=($(ls));				wait;
		flpth=$(pwd);				wait;
		for k in ${files[@]}
		  do
			printf "%s,%d,%s/%s,,\n" "$i" "$j" "$flpth" "$k" >> $inventory; wait;
		  done
          done
done

t_f=$(date)
echo EXITING SCRIPT

printf "started: $t_0\nfinished: $t_f\n\n" | mailx -r ekonagay@stanford.edu -s "inventory.sh fin" -a $counts ekonagaya@berkeley.edu

exit 1
