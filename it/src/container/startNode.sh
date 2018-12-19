#!/bin/bash

echo Options: $OPTS
echo "java $OPTS -jar /opt/app/EncryCore.jar /opt/encry/template.conf"
java $OPTS -jar /opt/encry/EncryCore.jar /opt/encry/template.conf