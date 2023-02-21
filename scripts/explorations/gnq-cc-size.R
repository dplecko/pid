
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

#' * size of connected component story *

cc_distr <- function(n, p) {

  dp <- array(0, dim = c(n+2, n+2, n+2))
  
  dp[1, (1) + 1, 1] <- 1
  
  
  for (i in 1+seq_len(n)) {
    
    for (u in c(0, seq_len(n))) {
      
      for (s in seq_len(n)) {
        
        # if (s == 3 & u == 1 & i == 3) browser()
        add <- 0
        for (n_disc in c(0, seq_len(u))) {
          
          if ((s - n_disc) == n) next
          # if (u == n_disc) next
          add <- add + 
            dp[s - n_disc, (u + 1 - n_disc) + 1, i-1] * 
            choose(n - s + n_disc, n_disc) * p^(n_disc) * (1 - p)^(n - s)
        }
        
        if (length(add) == 0) next
        
        dp[s, (u) + 1, i] <- dp[s, (u) + 1, i] + add
      }
    }
  }
  
  c(
    rowSums(dp[seq_len(n-1), 1, ]), 
    sum(dp[n, ,])
  )
}

# cc_sizes <- mclapply(seq_len(nrep), function(i) cc_size(gen_Gnp_undir(n, p)))

# cc_sizes <- tabulate(unlist(cc_sizes), nbins = n)
# cc_sizes <- cc_sizes / sum(cc_sizes)
# 
# plot(1:n, cc_sizes, pch = 19)
# lines(1:n, cc_distr(n, p), col = "red")

#' * X -> Y path probability *

p_xany <- function(n, p) {
  
  dp <- array(0, dim = c(n, n))
  dp[1, 1] <- 1
  for (i in 1+seq_len(n-1)) {
    
    for (x in seq.int(1, n)) {
      #print(dp)
      dp[x, i] <- (1-p)^x * dp[x, i-1]
      if (x - 1 > 0) dp[x, i] <- dp[x, i] + (1 - (1 - p)^(x-1)) * dp[x-1, i-1]
      
    }
  }
  
  nseq <- seq_len(n-1)
  sum((1 - (1 - p)^nseq) * dp[nseq, n - 1])
}

# test X -> Y dp
# for (n in c(3, 4, 5, 6)) {
#   
#   nrep <- 10^4
#   is_an <- mclapply(
#     seq_len(nrep),
#     function(i) {
#       A <- gen_Gnp(n, p)
#       is.element(1, getAncestors(n, A))
#     }
#   )
#   
#   cat(100 * (mean(unlist(is_an)) - p_xany(n, p)), "\n")
# }

# general ID algorithm
p_nonid <- function(n, p, q) {
  
  cc <- cc_distr(n, q)
  
  xany <- vapply(seq_len(n), function(i) p_xany(n, p), numeric(1L))
  
  nonid_cc <- vapply(
    seq_len(n),
    function(i) {
      sum(xany[1 + seq_len(i-1)] * 1 / (i-1)) * (i-1) / (n-1)
    }, numeric(1L)
  )
  
  sum(cc * nonid_cc)
}

# p_nonid from simulation
p_nonid_sim <- function(n, p, q, nrep = 1000) {
  is_nonid <- mclapply(
    seq_len(nrep),
    function(i) {
      g <- gen_Gnpq(n, p, q)
      !is_id(g, sample(n - 1, 1), n)
    }, mc.cores = get_cores()
  )
  
  mean(unlist(is_nonid))
}

n <- 4
p <- 0.7
q <- seq(0.1, 0.9, 0.1)

res <- lapply(seq_along(q), function(i) c(p_nonid(n, p, q[i]), p_nonid_sim(n, p, q[i])))
res <-  do.call(rbind, res)

plot(res[, 1], res[, 2], pch = 19)
abline(0, 1)



for (i in seq_len(10^3)) {
  
  set.seed(i)
  cat("\r", i)
  g <- gen_Gnpq(n, p, q[4])
  if (is_id(g, 2, 4) != anc_ln(g, 2, 4)) break
}

# is there a path in the ancestral line?

anc_ln <- function(g, x, y) {
  
  cc <- sort(confoundedComponent(y, g$cfd))
  if (!is.element(x, cc)) return(TRUE)
  
  y <- which(cc == y)
  x <- which(cc == x)
  
  g$adj <- g$adj[cc, cc]
  
  !is.element(x, getAncestors(y, g$adj))
  
}

plot(graphModel(g$adj, g$cfd))
