#!/bin/bash

#sbt assembly
#cp target/scala-2.13/textworldexpress-assembly-1.0.3.jar ../textworld_express/textworld-express-1.0.3.jar

sbt proguard
version=`cat build.sbt | grep version | cut -d '"' -f2`

cp target/scala-2.13/proguard/textworldexpress_2.13-${version}.jar ../textworld_express/textworld-express-${version}.jar
