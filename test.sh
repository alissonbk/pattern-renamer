rm test_project/*.tmp; rm -rf _build;
dune build; cp _build/install/default/bin/renamer test_project;
pushd test_project; ./renamer -r --debug -i "README.md" --ip "json:\"$\"" --mf "textFile, db" --mt "csvFile, gb";
yes | rm renamer; popd