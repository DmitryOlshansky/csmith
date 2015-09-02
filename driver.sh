#!/bin/bash
let X=0
while true; do
	src/csmith > out.d

	if dmd -Iruntime runtime/csmith.d out.d 2>&1 | grep -q Assertion ; then
		echo -n "*"
		let X=X+1
		mkdir ice$X
		cp out.d ice$X
		dustmite 
	else
		echo -n "."
	fi
done
