#' Find unconditional FDs in a directed acyclic graph (DAG)
#'
#' @param dag DAG (in dagitty) in which front-doors should be found.
#' @param X Exposure (String).
#' @param Y Outcome (String).
#' @param verbose Whether to print intermediary output (boolean).
#'
#' @returns A list of valid front-door mediator sets (each element is a
#'   character vector of node names). Returns \code{NULL} invisibly if no
#'   valid front-door exists.
#' @importFrom dagitty ancestors children dagitty descendants dseparated edges
#' @importFrom utils combn
#' @export
#'
#' @examples
#' dag1 <- dagitty::dagitty("dag {
#' X -> M
#' M -> Y
#' X [pos=\"0,0\"]
#'   M [pos=\"1,0\"]
#'   Y [pos=\"2,0\"]
#' }")
#'
#' find_fd(dag1, "X", "Y")
#'
#' dag2 <- dagitty::dagitty("dag {
#' A -> B
#' B -> C
#' U1 -> A
#' U1 -> B
#' U2 -> B
#' U2 -> C
#' U1 [pos=\"0,1\"]
#'   A  [pos=\"0,0\"]
#'   B  [pos=\"1,0.5\"]
#'   C  [pos=\"2,0\"]
#'   U2 [pos=\"2,1\"]
#' }")
#'
#' find_fd(dag2, "A", "C", verbose=FALSE)

find_fd <- function(dag, X, Y, verbose=TRUE) {
  stopifnot(is.character(X), length(X) == 1L,
            is.character(Y), length(Y) == 1L)
  nodes <- names(dag)
  if (!(X %in% nodes)) stop("Node '", X, "' not found in DAG.")
  if (!(Y %in% nodes)) stop("Node '", Y, "' not found in DAG.")

  # ---- STEP 0: fail-safe (direct edge X->Y?) ----
  if (Y %in% children(dag, X)) {
    if (verbose) message("No valid front-door: direct edge ", X, " -> ", Y, " exists.")
    cat("No valid front-door\n")
    return(invisible(NULL))
  }

  # ---- STEP 1: candidate sets (intercept X -> ... -> Y) ----
  cand_nodes <- setdiff(intersect(descendants(dag, X), ancestors(dag, Y)), c(X, Y))
  if (!length(cand_nodes)) {
    if (verbose) message("No candidates on directed X->...->Y paths.")
    cat("No valid front-door\n")
    return(invisible(NULL))
  }
  cand_sets <- power_set(cand_nodes)
  if (verbose) {
    message("Candidate nodes: ", paste(sort(cand_nodes), collapse = ", "))
    message("Total candidate sets: ", length(cand_sets))
  }

  # Pre-compute dag_Xcut once (remove outgoing edges from X)
  dag_Xcut <- remove_out_edges(dag, X)

  # ---- STEP 2: check two blocking criteria (method: remove outgoing edges, find back-doors) ----
  valid_sets <- list()

  for (M in cand_sets) {
    # Criterion 2: Back-doors X -> ... -> M do not exist
    if (!dseparated(dag_Xcut, X, M, list())) {
      next
    }
    if (verbose) message("Criterion 2 OK: ", fmt_set(M))

    # Criterion 3: Back-doors M -> ... -> Y do not exist, given X
    dag_Mcut <- remove_out_edges(dag, M)
    if (!dseparated(dag_Mcut, M, Y, X)) {
      next
    }
    if (verbose) message("Criterion 3 OK: ", fmt_set(M))

    valid_sets[[length(valid_sets) + 1]] <- M
    if (verbose) message("VALID FRONT-DOOR: ", fmt_set(M))
  }

  # ---- STEP 3: report results ----
  if (!length(valid_sets)) {
    if (verbose) message("No valid front-door sets found.")
    cat("No valid front-door\n")
    return(invisible(NULL))
  }

  cat("==== Valid front-door sets ====\n")
  for (s in valid_sets) cat(" ", fmt_set(s), "\n")
  invisible(valid_sets)
}
