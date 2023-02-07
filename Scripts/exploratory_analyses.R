# Second EFA, after item pruning (I'm not sure this is best practice, but it's what they did in the PXI validation)
# Selected items have loadings of > .5 on intended factor, and < .3 on all others; selection among high-performing items 
# was made based on theoretical coherence/face validity and coverage 
# (see https://docs.google.com/spreadsheets/d/1phM2KW03xJ6ThUV5kXdIuSLhdBq9MjGB_9t_jwb43nc/edit#gid=483999237)

best_performers <- c("as05", "as06", "as01", "as09",
                     "af04", "af02", "af16", "af01",
                     "cs06", "cs03", "cs02", "cs12",
                     "cf01", "cf04", "cf05", "cf14",
                     "rs10", "rs14", "rs02", "rs13",
                     "rf07", "rf08", "rf05", "rf02")
worst_performers_neg <- c(
  # Round 1
  "cs08","rs06","rf06","cs13","cs14","af09","rf14","rf12","cf10","rf10","cs15","rs12","cs09","af13","rf15","as02","af03","rf03","af06"
  # Round 2
  ,"cs05","af07","as12","af16","rf13","cf15"
  # Round 3 
  ,"as13","cs04","af08","af12"
  # Round 4
  ,"af01","af05"
)

worst_performers_pos <- c(
  # Round 1
  "cf11","cs08","af03","rf14","rf12","rf06","cf10"
  # Round 2
  ,"cs13","as14","as06","cf13","cf07","rs12"
  # Round 3
  ,"af01","af05","af12","as09","cf15","af06","rs06","rf10","rf03","cf16","rf01","rf04","cf06",
)

# Uniquely bad in positive group: as09, cf16, rf01, rf04, cf06, as14, as06, cf13, cf07, cf11