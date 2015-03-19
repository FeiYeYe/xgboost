xgb.importance <- function(feature_names, filename_dump) {
  fmap <- list()
  nmap <- list()
  total <- 0
  sapply(seq_along(feature_names), 
    function(x) {
      fmap[[feature_names[x]]] <<- 0
      nmap[[x]] <<- feature_names[x]
    })
  sapply(readLines(filename_dump), 
    function(x) {
      m <- regexec("\\[f.*\\]", x)
      p <- regmatches(x, m)
      if (length(p[[1]]) > 0) {
        splits <- strsplit(sub("\\]", "", sub("\\[f", "", p[[1]])), "<")[[1]]
        fmap[[nmap[[as.integer(splits[1]) + 1]]]] <<- fmap[[nmap[[as.integer(splits[1]) + 1]]]] + 1
        total <<- total + 1
      }
    })
  stopifnot(total > 0)
  lapply(fmap, function(x) x / total * 100)
}
