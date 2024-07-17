#!/usr/bin/env bash

### Parse HVR segmentations (detailed + ag)
## Extract voxel num for each label and calculate complete HV/CSF
## Labels:
## 111 - L-HC-Tail
## 112 - L-HC-Body
## 113 - L-HC-Head
## 121 - L-CSF-Tail
## 122 - L-CSF-Body
## 123 - L-CSF-Head
## 130 - L-AG
## 211 - R-HC-Tail
## 212 - R-HC-Body
## 213 - R-HC-Head
## 221 - R-CSF-Tail
## 222 - R-CSF-Body
## 223 - R-CSF-Head
## 230 - R-AG

set -ex

IN_DIR=$1
OUT_FILE=$2

[ $# -ne 2 ] || [ ! -d $IN_DIR ] && exit 1

[ -f $OUT_FILE ] && rm $OUT_FILE
printf "id,l_hc_t,l_hc_b,l_hc_h,l_vc_t,l_vc_b,l_vc_h,l_amy," > $OUT_FILE
printf "r_hc_t,r_hc_b,r_hc_h,r_vc_t,r_vc_b,r_vc_h,r_amy\n" >> $OUT_FILE

for img in $IN_DIR/*
do
	bname=$(basename $img .mnc)

	# Left side
	lhct=$(print_all_labels $img | awk '/Label: 111/ {print $3}')
	lhcb=$(print_all_labels $img | awk '/Label: 112/ {print $3}')
	lhch=$(print_all_labels $img | awk '/Label: 113/ {print $3}')
	lvct=$(print_all_labels $img | awk '/Label: 121/ {print $3}')
	lvcb=$(print_all_labels $img | awk '/Label: 122/ {print $3}')
	lvch=$(print_all_labels $img | awk '/Label: 123/ {print $3}')
	lamy=$(print_all_labels $img | awk '/Label: 130/ {print $3}')
	printf "%s,%i,%i,%i,%i,%i,%i,%i," \
		$bname $lhct $lhcb $lhch $lvct $lvcb $lvch $lamy >> $OUT_FILE

	# Right side
	rhct=$(print_all_labels $img | awk '/Label: 211/ {print $3}')
	rhcb=$(print_all_labels $img | awk '/Label: 212/ {print $3}')
	rhch=$(print_all_labels $img | awk '/Label: 213/ {print $3}')
	rvct=$(print_all_labels $img | awk '/Label: 221/ {print $3}')
	rvcb=$(print_all_labels $img | awk '/Label: 222/ {print $3}')
	rvch=$(print_all_labels $img | awk '/Label: 223/ {print $3}')
	ramy=$(print_all_labels $img | awk '/Label: 230/ {print $3}')
	printf "%i,%i,%i,%i,%i,%i,%i\n" \
		$rhct $rhcb $rhch $rvct $rvcb $rvch $ramy >> $OUT_FILE
done
