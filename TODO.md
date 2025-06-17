### Remaining

- Fix ignore patterns (test more, but i think its okay now)
- Fix usage as a simple renamer (there is only a single pattern but need to be renamed in multiple files. This should be working....)
- Fix ignore files
  - accept directory names without full path
  - not working when using 2 directories like: (pattern-renamer --debug -i ".next, node_modules" test testing)
- Add default famous directories and ask if want to ignore them when existing like (node_modules, java target folder, .next, anything that have non binary files...)
- Implement Gramatical type
  - Create assets for many language dictonaries
  - Create a flag and only use if flag is passed through args
  - then do a normalization:
    - aspell (aspell -d pt_PT -a) can be a good option, but maybe slow to call shell commands too many times
      - maybe the user should select the language if he specifies that he wants to use gramatical
      - with aspell could call with the normalized word (no accents) and get the first result if different
