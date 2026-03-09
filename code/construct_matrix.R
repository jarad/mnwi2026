construct_matrix <- function(d, Col = "home", n_teams = NULL) {
  n_games <- nrow(d)
  
  # Would be nice to get rid of this and automatically determine from the 
  # number of levels of the Col. Unfortunately these 
  # nlevels(d[, Col]) or nlevels(d$Col) don't work
  if (is.null(n_teams)) 
    n_teams <- length(unique(d[,Col]))
  
  m <- matrix(0, 
              nrow = n_games, 
              ncol = n_teams)
  
  for (g in 1:n_games) {
    m[g, as.numeric(d[g, Col])] <-  1
  }
  
  return(m)
}
