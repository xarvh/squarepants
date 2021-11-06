#!/bin/bash

cd "$(dirname "$0")"/..

FILES=`find sp/* |grep SPCore.*sp`

OUT=generated/SpModulesAsStrings.elm



echo "module SpModulesAsStrings exposing (..)" >$OUT
echo "modules =" >>$OUT

line_prefix="["


for file_name in $FILES; do

  x=$file_name
  y=${x/sp\//}
  z=${y/.sp/}
  #w=${z/\//.}
  module_name=$z

  echo $file_name to $module_name

  echo '  '"$line_prefix (\"$module_name\", \"\"\"$(cat $file_name)\"\"\")" >>$OUT

  line_prefix=","

done

echo "  ]" >>$OUT

###

echo "meta = \"\"\"" >>$OUT
cat modules.sp >>$OUT
echo "\"\"\"" >>$OUT
