
pid_backdoor <- function(n, p, q) {
  
  pid_bd_xy <- vapply(
    seq_len(n-1),
    function(i) pid_backdoor_xy(i, n, p, q),
    numeric(1L)
  )
  
  mean(pid_bd_xy)
}

cmp_pcg <- function(n, p) {
  
  pn <- rn <- rep(0, n)
  
  pn[1] <- 1
  
  for (i in seq_len(n-1) + 1) {
    
    smp <- smr <- 0
    for (k in seq.int(1, i-1)) {
      smp <- smp + choose(i-1, k-1) * pn[k] * (1-p)^(k * (i - k))
    }
    
    pn[i] <- 1 - smp
  }
  
  pn
}

pid_backdoor_xy <- function(x, y, p, q) {
  
  assert_that(x < y, msg = "x must precede y.")
  
  dp <- array(0, dim = c(x, x, y-x+2, y-x+2))
  dp <- index_from_0(dp)

  dp[0, 0, ,] <- jnt_prc(y-x+1, p)
  
  # get the dynamic prog for G(n, p)
  for (a in seq_len(x-1)) {
    
    for (i in seq_len(a+1)-1) {
      
      for (j in seq_len(y-x+2)-1) {
        
        for (k in seq_len(y-x+2)-1) {
          
          if (i == 0) {
            
            dp[a, i, j, k] <- dp[a, i, j, k] +
              dp[a-1, i, j, k] * (1 - p)^(i+k)
          } else {
            
            dp[a, i, j, k] <- dp[a, i, j, k] + 
              dp[a-1, i-1, j, k] * (1 - (1-p)^(i-1+k)) +
              dp[a-1, i, j, k] * (1 - p)^(i+k)
          }
        }
      }
    }
  }
  
  pcg <- cmp_pcg(y, q)
  # browser()
  pid <- sum(dp[x-1, , 0, ])
  
  for (l in seq_len(x) - 1) {
    
    for (j in seq_len(y - x + 1)) {
      
      for (k in seq.int(j, y - x + 1)) {
        
        for (cp in seq_len(l + k - j + 1) - 1) {
          
          cps <- l + k - j
          
          pid <- pid + choose(cps, cp) * pcg[cp+1] * (1 - q)^((cp+1) * (cps - cp)) * 
            (1 - q)^((cp+1) * (j-1)) * dp[x-1, l, j, k]
          
          if (is.na(pid)) browser()
          # if (l == 0) {
          #   
          #   pid <- pid + dp[x-1, l, j, k] * (1 - q)^(k-1) #' * check j vs. j-1 *
          # } else {
          #   
          #   pid <- pid + choose(cps, c) * pcg[c+1] * (1 - q)^((c+1) * (l - c)) * 
          #     (1 - q)^((c+1) * (j-1)) * dp[x-1, l, j, k]
          # }
        }
      }
    }
  }
  
  as.numeric(pid)
}

jnt_prc <- function(n, p) {
  
  root <- rprojroot::find_root(".git/config")
  load(file.path(root, "data", "dexany.rda"))
  
  if (is.null(dexany[[as.character(p)]])) {
    
    cat("Computing UMNC dynamic prog. for p =", p, "\n")
    prc_umcn(n, p)
  }
  
  load(file.path(root, "data", "dexany.rda"))
  dp <- dexany[[as.character(p)]]
  
  bin_p <- precalc_bin(p)
  bin_p <- index_from_0(bin_p)
  
  joint <- array(0, dim = c(n+1, n+1))
  pos <- n-1
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
  
  joint
}

prc_umcn <- function(n, p) {
  
  root <- rprojroot::find_root(".git/config")
  if (file.exists(file.path(root, "data/dexany.rda"))) {
    
    load(file.path(root, "data/dexany.rda"))
    if (!is.null(dexany[[as.character(p)]])) {
      
      return(invisible(TRUE))
    }
  } else dexany <- list()
  
  MAX <- 20
  if (n > MAX) stop("n value too large")
  n <- MAX - 1
  dp <- array(0, dim = c(MAX, MAX, MAX, MAX))
  bin_p <- precalc_bin(p)
  
  dp <- index_from_0(dp)
  bin_p <- index_from_0(bin_p)
  
  dp[0, 0, 0, 1] <- 1
  dp[0, 0, 0, 1]
  dp[0, 1, 0, 0] <- 1
  dp[0, 1, 0, 0]
  
  for (pos in 1:(n-1)) {
    
    for (not_reached in seq_len(pos+1)-1) {
      
      for (must in seq_len(pos+1-not_reached)-1) {
        
        for (can in seq_len(pos+1-not_reached-must)-1) {
          
          must_not <- pos - not_reached - must - can
          ja <- dp[not_reached, must, can, must_not]
          
          # not reached
          dp[not_reached+1,must,can,must_not] <- dp[not_reached+1,must,can,must_not] + 
            ja * bin_p[must + can + must_not,0]
          
          # reached
          dp[not_reached,must,can,must_not + 1] <-  dp[not_reached,must,can,must_not + 1] + 
            ja * (1 - bin_p[must + can + must_not, 0])
          
          for (edge_must in seq_len(must+1)-1) {
            
            prob <- bin_p[must, edge_must] * bin_p[must_not, 0]
            if (edge_must == 0) prob <- prob * (1 - bin_p[can, 0])
            
            dp[not_reached,must - edge_must + 1,can + edge_must,must_not] <-
              dp[not_reached,must - edge_must + 1,can + edge_must,must_not] +
              ja * prob
          }
        }
      }
    }
  }
  
  dexany[[as.character(p)]] <- dp
  save(dexany, file = file.path(root, "data/dexany.rda"))
  return(invisible(TRUE))
}
