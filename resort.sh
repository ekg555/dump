#!/bin/bash
manifest='/work/project/manifest.txt'
echo MANIFEST > ../manifest.txt

echo RUNNING SCRIPT: $(date)

# ==============================================================
#  STEP 0: COPY /data >>  /work/project/ORIGINAL
# ==============================================================

cd /data
wait
cp -r -n $(ls) /work/project/ORIGINAL
wait

# ==============================================================
#  STEP 1: GOTO ~/work/project/
# ==============================================================
cd /work/project/ORIGINAL
wait

# ==============================================================
#  STEP 2: CREATE "temp" FOLDER
# ===============================================================
mkdir /work/project/temp

sta=($(ls -d */)); wait;
for i in ${sta[@]}
	do
		mkdir ../temp/$i
	done

# ==============================================================
#  STEP 3: DIG FOR DOCS, COPY & RENAME
# ===============================================================
st=($(ls -d */)); printf '%s\n' "${st[@]}"; wait;
for i in ${st[@]}
  do
    cd $i; wait; echo $(pwd); echo "============================="; wait;
    pub=($(ls -d */)); printf '%s\n' "{$pub[@]}"; wait;
      for j in ${pub[@]}
        do
        cd $j; wait; echo $(pwd); echo "==================="; wait;
        yr=($(ls -d */)); printf '%s\n' "${yr[@]}"; wait;
          for k in ${yr[@]}
            do
            cd $k; wait; echo $(pwd);  echo "==================="; wait;
            doc=$(find . -type f); printf '%s\n' "${doc[@]}"; wait;
              for l in ${doc[@]}
                do
                m=${l##*/}; wait;
                cp $l /work/project/temp/$i${i%*/}${m%%.*}${j%*/}.${l##*.}; wait;
                done
            cd ../
            done
        cd ../
        done
  cd ../
done
wait


# =====================================
#  STEP 4: PRODUCE MANIFEST.TXT
# =====================================

echo "================================" >> $manifest
echo "  FILE COUNT CHECK"               >> $manifest
echo "================================" >> $manifest
echo "STATES: "                         >> $manifest
cd /work/project/temp
sta=($(ls -d */)); wait; printf '%s' "${sta[@]}" >> $manifest; wait;
echo ": n = '%s' {sta[@]} | wc -l" >> $manifest; wait;

echo "======================================" >> $manifest
echo "======================================" >> $manifest
cd ../ORIGINAL; wait;
sta=($(ls -d */)); wait;
cd ..; wait;

for i in ${sta[@]}
        do
                echo "STATE: ${i%*/}"			>> $manifest; wait;
                echo "ORIGINAL: "			>> $manifest; wait;
                cd /work/project/ORIGINAL/$i; wait;
                        echo $(pwd);			>> $manifest; wait;
                        find . -type f | wc -l		>> $manifest; wait;
                        echo "temp: "			>> $manifest; wait;
                cd /work/project/temp/$i; wait;
                        echo $(pwd)			>> $manifest; wait;
                        find . -type f | wc -l		>> $manifest; wait;
                        echo "============="		>> $manifest; wait;
        done

echo "  - END OF MANIFEST -  " >> $manifest; wait;


# ==============================================================
# STEP 5: RUN R SCRIPT TO SPLIT XML & WRITE TO "FINAL"
# ==============================================================
cd /work/project/SCRIPTS
wait

R CMD BATCH xmlsplit.r
wait

echo EXITING SCRIPT

exit 1
