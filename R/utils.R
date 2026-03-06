# ---- helpers ----
fmt_set <- function(s) {
  s <- if (is.null(s)) character(0) else if (is.list(s)) unlist(s, use.names = FALSE) else s
  s <- as.character(s)
  paste0("{", paste(sort(unique(s)), collapse = ","), "}")
}

power_set <- function(xs) {
  out <- list()
  n   <- length(xs)
  if (n == 0) return(out)
  for (k in seq_len(n)) out <- c(out, utils::combn(xs, k, simplify = FALSE))
  out
}

# Remove all outgoing directed edges from one or more nodes
remove_out_edges <- function(dag, nodes_to_cut) {
  nodes_to_cut <- as.character(nodes_to_cut)

  ed <- edges(dag)

  keep    <- !(ed$e == "->" & ed$v %in% nodes_to_cut)
  ed_keep <- ed[keep, , drop = FALSE]

  if (nrow(ed_keep) == 0) {
    spec <- paste0("dag {\n", paste(names(dag), collapse = "\n"), "\n}")
    return(dagitty(spec))
  }

  edge_strings <- paste(ed_keep$v, ed_keep$e, ed_keep$w)
  spec <- paste0(
    "dag {\n",
    paste(names(dag), collapse = "\n"),
    "\n",
    paste(edge_strings, collapse = "\n"),
    "\n}"
  )

  dagitty(spec)
}
