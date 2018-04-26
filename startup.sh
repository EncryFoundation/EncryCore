#!/bin/bash
echo Starting EncryCore client ...
sbt assembly
zip -d `find . -name EncryCore-assembly*.jar` META-INF/*.RSA META-INF/*.DSA META-INF/*.SF
java -jar `find . -name EncryCore-assembly*.jar`