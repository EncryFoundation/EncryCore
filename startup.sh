#!/bin/bash
echo Starting EncryCore client..
#sbt assembly
#java -jar `find . -name Encry-assembly*.jar`
sbt compile
sbt run