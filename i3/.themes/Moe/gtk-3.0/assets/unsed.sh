#!/bin/sh
sed -i \
         -e 's/rgb(0%,0%,0%)/#fdfde7/g' \
         -e 's/rgb(100%,100%,100%)/#5f5f5f/g' \
    -e 's/rgb(50%,0%,0%)/#5f5f5f/g' \
     -e 's/rgb(0%,50%,0%)/#ff4b4b/g' \
 -e 's/rgb(0%,50.196078%,0%)/#ff4b4b/g' \
     -e 's/rgb(50%,0%,50%)/#fdfde7/g' \
 -e 's/rgb(50.196078%,0%,50.196078%)/#fdfde7/g' \
     -e 's/rgb(0%,0%,50%)/#5f5f5f/g' \
	"$@"
