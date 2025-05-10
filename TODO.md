### Remaining

- Fix ignore patterns
- Fix ignore files (accept directory names without full path)
- Code improvement: create a global state for the args, so it wont not be necessary to pass as args every time...
- Implement Gramatical type
  - Create assets for many language dictonaries
  - Create a flag and only use if flag is passed through args
  - then do a normalization:
    - aspell (aspell -d pt_PT -a) can be a good option, but maybe slow to call shell commands too many times
      - maybe the user should select the language if he specifies that he wants to use gramatical
      - with aspell could call with the normalized word (no accents) and get the first result if different
