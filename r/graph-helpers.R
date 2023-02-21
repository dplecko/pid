
confoundedComponent <- function(var, cfd.mat) {
  
  assert_that(identical(cfd.mat, t(cfd.mat)))
  
  num.walks <- cfd.mat
  
  cfd.mat <- cfd.mat + diag(dim(cfd.mat)[1L])
  
  for (i in seq_len(nrow(cfd.mat) + 1L)) {
    num.walks <- cfd.mat + num.walks %*% cfd.mat
  }
  
  sort(which(num.walks[var, ] > 0))
}

confoundedComponent_ig <- function(var, cfd.mat) {
 
  assert_that(identical(cfd.mat, t(cfd.mat)))
  diag(cfd.mat) <- 1
  g <- graph_from_adjacency_matrix(cfd.mat)
  
  setdiff(as.integer(subcomponent(g, var, "all")), var)
}

getAncestors <- function(var, adj.mat) {
  
  num.walks <- adj.mat
  
  for (i in seq_len(nrow(adj.mat))) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }
  
  sort(which(num.walks[, var] > 0))
}

getAncestors_ig <- function(var, adj.mat) {
  
  g <- graph_from_adjacency_matrix(adj.mat)
  setdiff(as.integer(subcomponent(g, var, "in")), var)
}

getDescendants_ig <- function(var, adj.mat) {
  
  g <- graph_from_adjacency_matrix(adj.mat)
  setdiff(as.integer(subcomponent(g, var, "out")), var)
}

getDescendants <- function(var, adj.mat) {
  
  num.walks <- adj.mat
  
  for (i in seq_len(nrow(adj.mat))) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }
  
  sort(which(num.walks[var, ] > 0))
}

topologicalOrdering <- function(adj.mat) {
  
  nrw <- nrow(adj.mat)
  num.walks <- adj.mat
  
  for (i in seq_len(nrw + 1L)) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }
  
  comparison.matrix <- num.walks > 0
  
  top.order <- seq_len(nrw)
  
  for (i in seq_len(nrw - 1L)) {
    
    for (j in seq.int(i + 1L, nrw)) {
      
      if (comparison.matrix[top.order[j], top.order[i]]) {
        top.order <- swap(top.order, i, j)
      }
    }
    
  }
  
  top.order
}

swap <- function(x, i, j) {
  
  keep <- x[i]
  x[i] <- x[j]
  x[j] <- keep
  
  x
}

graphModel <- function(adj.mat, cfd.mat = NULL, res.vars = NULL) {
  
  if (is.null(cfd.mat)) {
    cfd.mat <- matrix(0L, nrow = nrow(adj.mat), ncol = ncol(adj.mat),
                      dimnames = dimnames(adj.mat))
  }
  
  res <- graph_from_adjacency_matrix(adj.mat)
  
  E(res)$curved <- 0
  E(res)$lty <- "solid"
  
  diag(cfd.mat) <- 0
  
  cfg <- graph_from_adjacency_matrix(cfd.mat)
  
  e.list <- as_edgelist(cfg, names = FALSE)
  curved <- (e.list[, 1] < e.list[, 2]) - 0.5
  
  lty <- ifelse((e.list[, 1] < e.list[, 2]), "dashed", "blank")
  res <- add_edges(res, as.vector(t(e.list)), curved = curved, lty = lty,
                   description = "U")
  
  E(res)$color <- "black"
  E(res)$arrow.size <- 0.35
  V(res)$color <- "white"
  V(res)$color[which(names(V(res)) %in% res.vars)] <- "red"
  V(res)$name <- as.character(seq_len(nrow(adj.mat)))
        
  res
}
