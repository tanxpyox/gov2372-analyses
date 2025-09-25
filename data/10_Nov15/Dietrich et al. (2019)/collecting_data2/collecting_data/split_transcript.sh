#!/bin/bash
IFS=","
while read f1 f2 f3 f4 f5
do
	echo $f5 > count.txt
        ffmpeg -i $f1 -ss $f2 -t $f3 $f4 -nostdin
done < file_index.csv