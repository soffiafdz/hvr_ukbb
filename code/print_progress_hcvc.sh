
#!/usr/bin/env bash

## Print progress of the cnn_hcvcag.sh script.

BASE_DIR=/ipl/ipl27/sfernandez/hvr_ukbb
OUTDIR=${BASE_DIR}/data/derivatives/segmentations_2024
LIST=${BASE_DIR}/lists/subs_lng.csv
TOTAL=$(wc -l < $LIST)

while true
do
	FNUM=$(ls $OUTDIR | wc -l)
	FNUM0=$(find $OUTDIR -type f -size 0 | wc -l)
	FNUMREAL=$(($FNUM - $FNUM0))
	PERC=$(echo "scale=2; $FNUMREAL * 100 / $TOTAL" | bc)
	printf "%s\n" "$(date)"
	printf "— 0-sized files: %i\n" $FNUM0
	printf "— Completed files: %i/%i\n" $FNUMREAL $TOTAL
	printf "— Percentage: %.3f\n" $PERC
	sleep 180
done
