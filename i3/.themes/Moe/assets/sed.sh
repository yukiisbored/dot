#!/bin/sh
sed -i \
         -e 's/#fdfde7/rgb(0%,0%,0%)/g' \
         -e 's/#5f5f5f/rgb(100%,100%,100%)/g' \
    -e 's/#5f5f5f/rgb(50%,0%,0%)/g' \
     -e 's/#ff4b4b/rgb(0%,50%,0%)/g' \
     -e 's/#fdfde7/rgb(50%,0%,50%)/g' \
     -e 's/#5f5f5f/rgb(0%,0%,50%)/g' \
	"$@"
