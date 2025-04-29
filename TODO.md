### Remaining


- Ignore pattern
    - for instance in go i want to change everything from potassio to potassium but not in the db:"potassio" tag (dont want to rename the database), so would be interesting to have a ignore pattern to ignore cases like db:".*potassio.*"
- Implement Gramatical type
  - Create assets for many language dictonaries
  - Create a flag and only use if flag is passed through args
  - then do a normalization:
    - aspell (aspell -d pt_PT -a) can be a good option, but maybe slow to call shell commands too many times
      - maybe the user should select the language if he specifies that he wants to use gramatical
      - with aspell could call with the normalized word (no accents) and get the first result if different
