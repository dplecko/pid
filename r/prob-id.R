
prob_id_grid <- function(adj_gen, cfd_gen, grid, nsamp = 10^4) {
  
  grid_df <- expand.grid(grid)
  nparams <- length(grid)
  var_params <- setdiff(names(grid), "n")
  res <- mclapply(
    seq_len(nrow(grid_df)),
    function(iter) {
      
      params <- lapply(unlist(grid_df[iter, ]), function(x) x)
      smp <- vapply(
        seq_len(nsamp), 
        function(i) {
          
          g <- gen_graph(adj = adj_gen, cfd = cfd_gen, params = params)
          pid_single(g)
        }, logical(3L)
      )
      smp <- t(smp)
      dt <- data.table(smp)
      # dt <- setnames(dt, names(dt), c("ncon", "b_id", "g_id"))
      for (j in seq_along(params)) {
        dt[, c(paste0("param", j)) := params[[j]]]
      }
      dt <- setnames(dt, names(dt), c("ncon", "b_id", "g_id", 
                                      paste0("params", seq_len(nparams - 1)), "n"))
      dt[, adj := adj_gen]
      dt[, cfd := cfd_gen]
      dt
    }, mc.cores = get_cores()
  )
  
  do.call(rbind, res)
}

prob_id_fuzzy <- function(adj_gen, cfd_gen, range, nsamp = 10^4) {
  
  nparams <- length(range)
  res <- mclapply(
    seq_len(nsamp),
    function(i) {
      
      params <- lapply(
        range, 
        function(r) {
          
          if (length(r) == 2) return(runif(1, min = r[1], max = r[2]))
          return(r)
        }
      )
      names(params) <- names(range)
      g <- gen_graph(adj = adj_gen, cfd = cfd_gen, params = params)
      c(pid_single(g), unlist(params))
    }, mc.cores = get_cores()
  )
 
  dt <- data.table(do.call(rbind, res))
  dt <- setnames(dt, names(dt), c("ncon", "b_id", "g_id", 
                                  paste0("params", seq_len(nparams - 1)), "n"))
  dt[, adj := adj_gen]
  dt[, cfd := cfd_gen]
  dt
}

vis_prob_id <- function(df, var_params_true) {
  
  assert_that(length(unique(df$n)) == 1L, 
              msg = "Multiple n values currently not supported.")
  
  adj_gen <- attr(df, "adj_gen")
  cfd_gen <- attr(df, "cfd_gen")
  
  var_params <- setdiff(names(df), c("n", "ncon", "b_id", "g_id", "adj", "cfd"))
  df <- setnames(df, var_params, var_params_true)
  var_params <- var_params_true
  
  if (length(var_params) == 1L) {
    
    
    df <- melt(df[, ncon := NULL], id.vars = c(var_params, "n", "adj", "cfd"))
    vis_prob_id_1d(df, var_params)
  } else {
    
    vis_prob_id_2d(df, var_params[1], var_params[2])
  }
}

vis_prob_id_1d <- function(df, var) {
  # browser()
  ggplot(df, aes_string(x = var, y = "value", color = "variable")) +
    geom_line() + geom_point() + theme_bw() +
    ylab("P(identification)") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_discrete(name = "ID approach", labels = c("Back-door", "ID Algorithm")) +
    theme(legend.position = c(0.8, 0.8), legend.box.background = element_rect())
}

vis_prob_id_2d <- function(df, var1, var2) {
  
  # plotly ...
  adj <- unique(df$adj)
  cfd <- unique(df$cfd)
  contour1 <- reshape2::acast(df, as.formula(paste(var1, "~", var2)), 
                              value.var = "g_id", fun.aggregate = mean)
  
  contour2 <- reshape2::acast(df, as.formula(paste(var1, "~", var2)), 
                              value.var = "b_id", fun.aggregate = mean)
  
  
  xseq <- as.numeric(attr(contour1, "dimnames")[[1]])
  yseq <- as.numeric(attr(contour1, "dimnames")[[2]])

  fig <- plot_ly(scene = paste(adj, cfd, sep = "+"))
  fig <- fig %>% add_surface(z = ~contour2, x = ~xseq, y = ~yseq, 
                             colorscale = list(c(0, 1), c("red", "red")),
                             colorbar = list(title = "Back-door  ",
                                             tickfont = list(color = "white"),
                                             ticklen = 0,
                                             orientation = "v"#,
                                             # x = 0.862, y = 0.7, len = 0.175
                                             ),
                             opacity = 0.5, showscale = TRUE,
                             name = "Back-door")
  fig <- fig %>% add_surface(z = ~contour1, x = ~xseq, y = ~yseq, 
                             colorscale = list(c(0, 1), c("blue", "blue")),
                             colorbar = list(title = "ID Algorithm",
                                             tickfont = list(color = "white"),
                                             ticklen = 0,
                                             orientation = "v"#,
                                             # x = 0.85, y = 0.85, len = 0.2
                                             ),
                             opacity = 0.5, showscale = TRUE,
                             name = "ID Algorithm")
  
  fig <- fig %>% layout(
    scene = list(xaxis=list(title = var1),
                 yaxis=list(title = var2),
                 zaxis=list(title = "P(identification)")
    )
  )
  
  fig
}

# dynamic programming high-level functionality

prob_id_dp <- function(grid) {
  
  df <- expand.grid(grid)
  dp_pid <- lapply(
    seq_len(nrow(df)),
    function(i) {
      
      c(
        g_id = pid(df$n[i], df$p[i], df$q[i]), 
        b_id = pid_backdoor(df$n[i], df$p[i], df$q[i])
      )
    }
  )
  df <- cbind(df, do.call(rbind, dp_pid))
  as.data.table(df)
}
