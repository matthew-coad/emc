#' Start a local cluster and register it with the multi-tasking backend.
#'
#' The cluster is registered with the global environment. Only one cluster can be running
#' at one time.
#'
#' @param p Ratio of the available logical cores to use. Rounds down to a minimum of 2.
#' @param  envir Environment to save the cluster to. Defaults to the global environment.
#'
#' @export
emc_start_cluster <- function(p = 3/4, envir = .GlobalEnv) {

    if (exists("emc_cluster", envir = envir)) {
        stop("Cluster is already running")
    }

    max_cores <- parallel::detectCores()
    cores <- max(floor(max_cores * p), 2)
    emc_cluster <<- parallel::makeCluster(cores)
    doParallel::registerDoParallel(emc_cluster)
    assign("emc_cluster", emc_cluster, envir = envir)
}

#' Stop the local cluster
#'
#' @param  envir Environment the cluster was registered with
#'
#' @export
emc_stop_cluster <- function(envir = .GlobalEnv) {

    if (!exists("emc_cluster", envir = envir)) {
        stop("Cluster is not running")
    }

    emc_cluster <- get("emc_cluster", envir = envir)
    parallel::stopCluster(emc_cluster)
    remove("emc_cluster", envir = envir)
}
