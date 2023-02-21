
pid <- function(n, p, q) {
  
  pid_bd_xy <- vapply(
    seq_len(n-1) + 1,
    function(x) pid_xy(x, n, p, q),
    numeric(1L)
  )
  
  mean(pid_bd_xy)
}

chxany_prc <- function(n, x, p) {
  
  dp <- array(0, dim = c(n+1, n+1, n+1))
  dp <- index_from_0(dp)
  
  dp[1, 1, 0] <- 1
  
  # transitions before x
  for (a in seq_len(x-2)+1) {
    
    for (i in seq_len(a)) {
      
      dp[a, i, 0] <- dp[a, i, 0] + dp[a-1, i-1, 0] * (1 - (1-p)^(i-1)) +
        dp[a-1, i, 0] * (1-p)^i
    }
  }
  
  # transition at x
  for (i in seq_len(x)) {
    
    for (j in seq_len(i)-1) {
      
      if (j == 0) {
        
        dp[x, i, j] <- dp[x, i, j] + dp[x-1, i, j] * (1-p)^i 
      } else {
        
        dp[x, i, j] <- dp[x, i, j] + dp[x-1, i-1, 0] * choose(i-1, j) * p^j * (1-p)^(i-j-1) 
      }
    }
  }
  
  # transitions after x
  if (n == x) return(dp)
  
  for (a in seq.int(x+1, n)) {
    
    for (i in seq_len(a)) {
      
      for (j in seq_len(x)-1) {
        
        dp[a, i, j] <- dp[a, i, j] + dp[a-1, i-1, j] * (1 - (1-p)^(i-1)) +
          dp[a-1, i, j] * (1-p)^i
      }
    }
  }
  
  dp
}

pcg_powj <- function(r, j, p) {
  
  #' * for loops similar to cmp_pcg  *
  n <- r + j
  pn <- rep(0, n)
  pn <- index_from_0(pn)
  pn[0] <- 1 # (1-p)^j
  # pn[1] <- 1 - (1-p)^j # point connected to at least one of the three
  
  if (r < 1) return(pn[0:r])
  
  for (i in seq.int(1, r)) {
    
    smp <- 0
    for (r0 in seq.int(0, i-1)) {
      smp <- smp + choose(i, r0) * pn[r0] * (1-p)^(r0 * (i - r0)) * (1-p)^(j * (i - r0))
    }
    
    pn[i] <- 1 - smp
  }
  
  pn[0:r]
}

pid_xy <- function(x, n, p, q) { # y = 1 in this convention; n size of the graph
  
  pid <- 0
  
  dp <- chxany_prc(n, x, p)
  
  #' * for loops over j, i, r *
  
  for (j in seq_len(x)-1) {
    
    if (j == 0) { # ch(X) \cap an(Y) = \emptyset => ID
       
      pid <- pid + sum(dp[n, , j])
      next
    }
    
    pcg_j <- pcg_powj(n-j, j, q) # shouldn't this be n-j?
    
    for (i in seq.int(j, n, 1)) {
      
      for (r in seq_len(i-j)-1) {
        
        pid <- pid +
          choose(i-j-1, r) * pcg_j[r] * (1-q)^(r * (i-j-1-r)) * 
          (1-q)^(j * (i-j-1-r)) * dp[n, i, j] * (1 - q)^(r + j)
      }
    }
  }
  
  pid
}

