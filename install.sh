#!/bin/bash

# set -x # @echo on

cd target/scala-2.12/ >/dev/null 2>/dev/null || {
  echo "Run sbt assembly first"
  exit 1
}
ls tessla*.jar >/dev/null 2>/dev/null || {
  echo "Run sbt assembly first"
  exit 1
}

set -e # cancel script if anything fails

mkdir -p /usr/local/opt/tessla
cp tessla*.jar /usr/local/opt/tessla/tessla.jar

printf '#!/bin/bash\njava -jar /usr/local/opt/tessla/tessla.jar $@\n' > /usr/local/bin/tessla
chmod +x /usr/local/bin/tessla
