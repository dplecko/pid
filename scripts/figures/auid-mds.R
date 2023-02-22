
root <- rprojroot::find_root(".git/config")
r_dir <- file.path(root, "r")
cpp_dir <- file.path(root, "cpp")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(cpp_dir, full.names = TRUE), sourceCpp))

auid_cov <- function(fspec, nsamp) {
  
  fuzzy_n <- list()
  n_graph <- c(5, 10, 15, 20)
  for (j in seq_along(n_graph)) {
    
    fuzzy <- list()
    for (i in seq_along(fspec)) {
      
      range <- fspec[[i]]$grid
      range[["n"]] <- n_graph[j]
      fuzzy[[i]] <- prob_id_fuzzy(adj_gen = fspec[[i]]$adj, cfd_gen = fspec[[i]]$cfd, 
                                  range = range, nsamp = nsamp)
    }
    fuzzy_n[[j]] <- rbindlist(fuzzy, fill = TRUE)
  }
  
  fexp <- do.call(rbind, fuzzy_n)
  fexp

  # Area under Identification
  auid <- fexp[, lapply(.SD, mean, na.rm = TRUE), by = c("n", "adj", "cfd"),
               .SDcols = c("ncon", "b_id", "g_id")]
  auid <- melt(auid, id.vars = c("n", "adj", "cfd"))
  
  auid$adj <- as.factor(auid$adj)
  levels(auid$adj) <- c(Gnp = latex2exp::TeX("$G(n, p)$"),
                        scale_free = latex2exp::TeX("$BA(n, a, \\alpha)$"),
                        unif = latex2exp::TeX("Uniform"))
  
  auid$cfd <- as.factor(auid$cfd)
  levels(auid$cfd) <- c(Gnq = latex2exp::TeX("$G(n, q)$"),
                        prunning = latex2exp::TeX("IP(q)"))
  
  p1 <- ggplot(auid, aes(x = n, y = value, color = variable)) + geom_line() + 
    geom_point() + 
    facet_grid(cfd ~ adj, labeller = label_parsed) +
    xlab("Graph Size") +
    ylab("P(Identification)") +
    scale_color_discrete(labels = c("0-ID", "Back-door", "ID Algorithm"),
                         name = "ID Approach") +
    theme_bw() + theme(
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 12)
    )
  
  # Back-door Coverage
  p2 <- ggplot(
    fexp[g_id == 1 & ncon == 0, list(bd_cov = mean(b_id)), 
         by = c("n", "adj", "cfd")],
    aes(x = n, y = bd_cov, color = adj, linetype = cfd)
  ) + geom_line() + theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    xlab("Graph Size") + ylab("Backdoor Coverage") +
    geom_point() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 12)
    ) +
    scale_color_discrete(name = "Adjacency",
                         labels = c(Gnp = latex2exp::TeX("$G(n, p)$"),
                                    scale_free = latex2exp::TeX("$BA(n, a, \\alpha)$"),
                                    unif = latex2exp::TeX("Uniform"))) +
    scale_linetype_discrete(name = "Confounding",
                            labels = c(Gnq = latex2exp::TeX("$G(n, q)$"),
                                       prunning = latex2exp::TeX("IP(q)")))
  
  list(p1, p2)
}

nsamp <- 10^4
pgen <- auid_cov(fspec, nsamp)
ggsave(file.path("paper", "figures", "auid_gen.png"), plot = pgen[[1]],
       width = 12, height = 8)

ggsave(file.path("paper", "figures", "cov_gen.png"), plot = pgen[[2]],
       width = 12, height = 8)

pdps <- auid_cov(fspec_dps, nsamp)
ggsave(file.path("paper", "figures", "auid_dps.png"), plot = pdps[[1]],
       width = 12, height = 8)

ggsave(file.path("paper", "figures", "cov_dps.png"), plot = pdps[[2]],
       width = 12, height = 8)
