
root <- rprojroot::find_root(".git/config")
r_dir <- file.path(root, "r")
cpp_dir <- file.path(root, "cpp")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(cpp_dir, full.names = TRUE), sourceCpp))

#' * for-loop over renders for different values of n *
for (n_graph in c(5, 10, 15, 20)) {
  
  rmarkdown::render(
    file.path("scripts", "figures", "dash.Rmd"), 
    params = list(n_graph = n_graph, n_samp = 10^4),
    output_file = file.path(root, "dashboards", 
                            paste0("dashboard_", n_graph, ".html"))
  )
}