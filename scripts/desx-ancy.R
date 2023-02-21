
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
cpp_dir <- file.path(root, "cpp")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(cpp_dir, full.names = TRUE), sourceCpp))


p <- 0.5


n <- 10
pos <- n-1
sol <- rep(0, n+1)
sol <- index_from_0(sol)
for (not_reached in seq_len(pos+1)-1) {
  
  for (must in seq_len(pos+1-not_reached)-1) {
    
    for (can in seq_len(pos+1-not_reached-must)-1) {
      
      must_not <- pos - not_reached - must - can
      ja <- dp[not_reached, must, can, must_not]
      
      if (must > 0) {
        sol[must+can+1] <- sol[must+can+1] + ja * bin_p[must,must] * bin_p[must_not,0]
      } else {
        sol[0] <- sol[0] + ja * bin_p[must_not, 0]
      }
    }
  }
}

# simulate this thing
nsamp <- 10^5
smp <- vapply(
  seq_len(nsamp),
  function(i) {
    
    adj <- gen_Gnp(n, p)
    desx <- c(1, getDescendants(1, adj))
    ancy <- c(n, getAncestors(n, adj))
    
    length(intersect(desx, ancy))
  }, integer(1L)
)

smp <- table(smp) / nsamp
smp <- c(smp[1], 0, smp[-1])

plot(sol, pch = 19)
lines(sol)
points(smp, col = "red", pch = 19)
lines(smp, col = "red")


# obtain the joint distribution with ancestors

joint <- array(0, dim = c(n+1, n+1))
joint <- index_from_0(joint)
for (not_reached in seq_len(pos+1)-1) {
  
  for (must in seq_len(pos+1-not_reached)-1) {
    
    for (can in seq_len(pos+1-not_reached-must)-1) {
      
      must_not <- pos - not_reached - must - can
      ja <- dp[not_reached, must, can, must_not]
      
      if (must > 0) {
        joint[must+can+1, must+can+1+must_not] <- joint[must+can+1, must+can+1+must_not] + 
          ja * bin_p[must,must] * bin_p[must_not,0]
      } else {
        joint[0, must_not] <- joint[0, must_not] + ja * bin_p[must_not, 0]
      }
    }
  }
}

