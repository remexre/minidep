#!/bin/sh

set -eu;

watchexec -cre sv -- "./build.sh && java -jar edu.umn.cs.melt.minidep.compiler.jar $1; ./build.sh --clean --warn-all"
