#!/bin/sh

# cat links | while read f; do
#   wget -P years/ $f
# done;

for f in years/*; do
  unzip $f -d "${f%%.zip}";
done;
