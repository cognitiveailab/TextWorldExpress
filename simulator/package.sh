#!/bin/bash

#sbt assembly
#cp target/scala-2.13/textworldexpress-assembly-1.0.1.jar ../textworld_express/textworld-express-1.0.1.jar

sbt proguard
cp target/scala-2.13/proguard/textworldexpress_2.13-1.0.1.jar ../textworld_express/textworld-express-1.0.1.jar
