
proj_single <- function(g, idx) {
  
  pa <- which(g$adj[, idx] > 0)
  ch <- which(g$adj[idx, ] > 0)
  ng <- which(g$cfd[, idx] > 0)
  
  # add parent-child combinations
  dir <- expand.grid(pa, ch)
  g$adj[dir$Var1, dir$Var2] <- 1
  
  # add bidirected for child-child and neighbour-child combinations
  bidir <- expand.grid(c(ch, ng), ch)
  bidir <- bidir[bidir$Var1 != bidir$Var2, ]
  g$cfd[bidir$Var1, bidir$Var2] <- g$cfd[bidir$Var2, bidir$Var1] <- 1L
  
  # add bidirected bidir-child combinations
  expand.grid(ng, ch)
  
  g$adj <- g$adj[-idx, -idx]
  g$cfd <- g$cfd[-idx, -idx]
  
  g
}

proj <- function(g, uc_idx) {
  
  uc <- sort(which(uc_idx == 1))
  
  if (length(uc) == nrow(g$adj)) return(NULL) # nothing observed -> FAIL
  
  for (idx in rev(uc)) g <- proj_single(g = g, idx = idx)
  
  g
}