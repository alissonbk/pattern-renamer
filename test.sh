dune build; cp _build/install/default/bin/renamer test_project; 
pushd test_project; ./renamer -r -i "a, b" "textFile" "csvFile"; 
yes | rm renamer; popd