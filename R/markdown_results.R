convert_to_markdown <- function(table) {
  if (ncol(table) > 3) {
    names(table) <- c(names(table)[[1]],
                      pandoc.strong.return(names(table)[[2]]),
                      names(table)[3:length(names(table))])
    return(pandoc.table(table, emphasize.strong.cols = 2))
  } else {
    names(table) <- c(pandoc.strong.return(names(table)[[1]]),
                      names(table)[2:length(names(table))])
    return(pandoc.table(table, emphasize.strong.cols = 1))
  }
}
