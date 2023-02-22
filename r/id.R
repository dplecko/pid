
is_id <- function(g, x, y, show_plot = FALSE) {
  
  if (is.null(g)) return(FALSE)
  
  ig <- graphModel(adj.mat = g$adj, cfd.mat = g$cfd)
  suc <- try(causal.effect(x = as.character(x), y = as.character(y), G = ig), 
             silent = TRUE)
  
  if (show_plot) plot(ig)
  
  return(class(suc) != "try-error")
}

is_id_fast <- function(g, x, y) {
  
  ancy <- sort(c(getAncestors(y, g$adj), y))
  g$adj <- g$adj[ancy, ancy]
  g$cfd <- g$cfd[ancy, ancy]
  
  x <- which(ancy == x)
  
  if (length(x) == 0) return(TRUE) # X -> Y does not exist
  
  cc_x <- confoundedComponent(x, g$cfd)
  ch_x <- which(g$adj[x, ] > 0)
  return(!any(ch_x %in% cc_x))
}

check_bd <- function(g, x, y, show_plot = FALSE) {
  
  if (show_plot) plot(graphModel(g$adj, g$cfd))
  
  y_an <- sort(c(getAncestors(y, g$adj), y))
  if (!is.element(x, y_an)) {
    return(TRUE)
  }
  
  y <- which(y_an == y)
  x <- which(y_an == x)
  g$adj <- g$adj[y_an, y_an]
  g$cfd <- g$cfd[y_an, y_an]
  
  x_de <- sort(setdiff(getDescendants(x, g$adj), y))
  
  # project the descedants of x different from y
  for (xde in rev(x_de)) {
    
    g <- proj_single(g, xde)
    if (xde < y) y <- y - 1
  }
  
  x_ch <- which(g$adj[x, ] > 0)
  if (!is.element(y, x_ch)) browser()
  x_cc <- confoundedComponent(x, g$cfd)
  #cat("ch(X) = ", x_ch, ", C-comp(X) =", x_cc, "\n")
  length(intersect(x_ch, x_cc)) == 0
}

pid_single <- function(g, type = "fast") {
  
  if (is.null(g) | is.scalar(g$adj)) return(c(FALSE, FALSE, FALSE))
  
  # xy <- sort(sample.int(nrow(g$adj), size = 2L, replace = FALSE))
  x <- sample.int(nrow(g$adj)-1, size = 1L) # xy[1]
  y <- nrow(g$adj) # xy[2]
  if (type == "fast") {
    
    gen_id <- is_id_fast(g, x, y)
  } else gen_id <- is_id(g, x, y)
  
  # not a complete criterion
  bd_id <- check_bd(g, x, y)
  
  # is it connected?
  conn <- is.element(x, getAncestors(y, g$adj))
  
  if (gen_id < bd_id) browser()
  if (bd_id < !conn) browser()
  c(!conn, bd_id, gen_id)
}
