#!/bin/bash

# set -x # @echo on

cd target/scala-2.13/ || {
  echo "Target directory not found. Run sbt clean assembly first!"
  exit 1
}
jarCount=$(ls tessla*.jar | wc -l)
[[ $jarCount -eq 1 ]] || {
  echo "Wrong number of jar files found. Run sbt clean assembly first!"
  exit 1
}

set -e # cancel script if anything fails

mkdir -p /usr/local/share/
cp tessla*.jar /usr/local/share/tessla.jar

printf '#!/bin/bash\njava -Xss16m -jar /usr/local/share/tessla.jar "$@"\n' > /usr/local/bin/tessla
chmod +x /usr/local/bin/tessla

printf '#!/bin/bash\njava -Xss16m -cp /usr/local/share/tessla.jar de.uni_luebeck.isp.tessla.tessladoc.Main "$@"\n' > /usr/local/bin/tessladoc
chmod +x /usr/local/bin/tessladoc
