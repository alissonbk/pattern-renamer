rm test_project/*.tmp; rm -rf _build;
dune build; cp _build/install/default/bin/renamer test_project;
pushd test_project; ./renamer -r -i "README.md" "textFile, db" "csvFile, gb";
yes | rm renamer; popd