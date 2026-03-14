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

find_paths <- function(g, X, Y, curr_path = c(X), seen = c()) {
  if (X == Y) {
    return(list(curr_path))
  }

  seen <- c(seen, X)
  paths <- list()

  for (nbr in c(children(g, X))) {
    if (nbr %in% seen) next

    subpaths <- find_paths(
      g,
      nbr,
      Y,
      curr_path = c(curr_path, nbr),
      seen = seen
    )

    if (length(subpaths)) {
      paths <- c(paths, subpaths)
    }
  }

  paths
}

subset_paths <- function(paths, X, Y) {
  # For each path, generate all non-empty subsets (excluding X and Y)
  path_subsets <- list()

  for (path in paths) {
    # Remove X and Y from the path
    path_nodes <- setdiff(path, c(X, Y))

    # Skip if no intermediate nodes
    if (length(path_nodes) == 0) next

    path_subs <- list()
    for (i in 1:length(path_nodes)) {
      path_subs <- c(path_subs, combn(path_nodes, i, simplify = FALSE))
    }
    path_subsets[[length(path_subsets) + 1]] <- path_subs
  }

  # Now generate Cartesian product of all path subsets
  if (length(path_subsets) == 0) return(list())

  result <- lapply(path_subsets[[1]], function(x) list(x))

  if (length(path_subsets) > 1) {
    for (i in 2:length(path_subsets)) {
      new_result <- list()
      for (existing_combo in result) {
        for (new_subset in path_subsets[[i]]) {
          combined <- unlist(c(existing_combo, list(new_subset)), use.names = FALSE)
          new_result[[length(new_result) + 1]] <- combined
        }
      }
      result <- new_result
    }
  } else {
    result <- lapply(result, function(x) x[[1]])
  }

  # Remove duplicates
  result <- unique(lapply(result, function(x) sort(unique(x))))

  result
}
