
root <- rprojroot::find_root(".git/config")
r_dir <- file.path(root, "r")
cpp_dir <- file.path(root, "cpp")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(cpp_dir, full.names = TRUE), sourceCpp))

# grid experiment
n_graph <- 10L
res <- replicate(length(gspec), NULL)

for (i in seq_along(gspec)) {
  
  grid <- gspec[[i]]$grid
  grid[["n"]] <- n_graph
  df <- prob_id_grid(adj_gen = spec[[i]]$adj, cfd_gen = spec[[i]]$cfd, 
                     grid = grid, nsamp = 10)
  # cat("Iteration", i, "\n")
  res[[i]][["df"]] <- df
  res[[i]][["vis"]] <- vis_prob_id(df)
}

# for (i in 4:6) {
#   
#   ggsave(paste0("fdfplot_", i, ".png"), res[[i]][["vis"]])
#   
#   htmlwidgets::saveWidget(
#     widget = res[[i]][["vis"]], #the plotly object
#     file = paste0("fdfplot_", i, ".html"), #the path & file name
#     selfcontained = TRUE #creates a single html file
#   )
# }

# fuzzy experiment
fuzzy_n <- list()
n_graph <- c(5, 10, 15, 20)
for (j in seq_along(n_graph)) {
  
  fuzzy <- list()
  for (i in seq_along(fspec)) {
    
    range <- fspec[[i]]$grid
    range[["n"]] <- n_graph[j]
    fuzzy[[i]] <- prob_id_fuzzy(adj_gen = fspec[[i]]$adj, cfd_gen = fspec[[i]]$cfd, 
                                range = range, nsamp = 10^4)
  }
  fuzzy_n[[j]] <- rbindlist(fuzzy, fill = TRUE)
}

fexp <- do.call(rbind, fuzzy_n)
fexp

# Part A plot
ggplot(
  fexp[b_id == 1 & ncon == 0, list(bd_cov = mean(g_id)), 
       by = c("n", "adj", "cfd")],
  aes(x = n, y = bd_cov, color = adj, linetype = cfd)
) + geom_line() + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Graph Size") + ylab("Backdoor Coverage") +
  geom_point()

# Part C plot
auid <- melt.data.table(fexp, id.vars = c("n", "adj", "cfd"))
ggplot(
  auid[, list(value = mean(value)), by = c("n", "adj", "cfd")],
  aes(x = n, y = value, color = variable)
) + geom_line() + geom_point() + 
  facet_wrap(rows = vars(adj), cols = vars(cfd)) +
  theme(
    legend.position = "bottom"
  )
