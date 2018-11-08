#!/bin/sh

set -eu;

for f in examples/*; do
	java -jar edu.umn.cs.melt.minidep.composed.Default.jar $f || exit 1
done
