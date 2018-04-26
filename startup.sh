#!/bin/bash
echo Building EncryCore client ...
sbt assembly
zip -d `find . -name EncryCore-assembly*.jar` META-INF/*.RSA META-INF/*.DSA META-INF/*.SF
echo Starting EncryCore client ...
if [ -z "$1" ];then
java -jar `find . -name EncryCore-assembly*.jar` "src/main/resources/application.conf"
else
java -jar `find . -name EncryCore-assembly*.jar` $1
fi