## Condenses the long rainfall data files and just gives us the
## monthly averages in a csv file.
sed -E '/([ ]+AVG|MX MN|^[ ]+TOT)/! d;' $1 | awk '{if ($1=="AVG") {max=$2; min=$3; avg=$4} else if ($1=="TOT") {rain=$2; print month,max,min,avg,rain} else {month=$1}; }'  
