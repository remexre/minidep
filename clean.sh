#!/bin/sh

set -e;

loudlyRm() {
	echo rm -f $@;
	rm -f $@;
};

loudlyRm build.xml;
loudlyRm edu.umn.cs.melt.minidep.composed.Default.jar
loudlyRm Parser_edu_umn_cs_melt_minidep_composed_Default_parse.copperdump.html;
