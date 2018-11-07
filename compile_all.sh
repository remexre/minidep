#!/bin/sh

set -eu;

for f in examples/*; do
	if [[ $f != examples/stdlib.mdp ]]; then
		java -jar edu.umn.cs.melt.minidep.composed.Default.jar $f || exit 1
	fi
done
