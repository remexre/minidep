#!/bin/sh

set -eu;

watchexec -cre sv -- "./build.sh && java -jar edu.umn.cs.melt.minidep.composed.Default.jar $1"
