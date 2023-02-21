
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

# p_nonid_sim <- function(n, p, q, nrep = 1000) {
#   is_nonid <- mclapply(
#     seq_len(nrep),
#     function(i) {
#       g <- gen_Gnpq(n, p, q)
#       !is_id(g, sample(n - 1, 1), n)
#     }, mc.cores = get_cores()
#   )
#   
#   mean(unlist(is_nonid))
# }

check_graph_id <- function(g, x, y) {
  
  cc <- sort(confoundedComponent(x, g$cfd))
  if (length(cc) == 1L) return(TRUE)
  nx <- which(g$cfd[x,] > 0)
  nx <- setdiff(nx, x)
  
  # get de(X) and de(N(X))
  g_cc <- list(adj = g$adj[cc, cc], cfd = g$cfd[cc, cc]) 
  dex_cc <- cc[getDescendants(which(cc == x), g_cc$adj)]
  denx_cc <- c()
  for (var in nx) {
    
    var_cc <- which(cc == var)
    denx_cc <- c(denx_cc, var, cc[getDescendants(var_cc, g_cc$adj)])
  }
  denx_cc <- unique(denx_cc)
  
  an_y <- getAncestors(y, g$adj)
  
  length(
    intersect(intersect(denx_cc, dex_cc), c(an_y, y))
  ) == 0L
  
}

n <- 4
p <- 0.8
q <- 0.2
nrep <- 1000
for (i in seq_len(nrep)) {
  
  set.seed(i)
  g <- gen_Gnpq(n, p, q)
  x <- sample(n - 1, 1)
  if (is_id(g, x, n) != check_graph_id(g, x, n)) {
    
    cat(i, "\n")
    break
  }
  
}

plot(graphModel(g$adj, g$cfd))
