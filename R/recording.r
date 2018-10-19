
#' Record an expression
#'
#' Executes an expression recording all messages, warnings and errors along the way.
#'
#' @return A tibble that contains the result, error, execution times and the replay log.
#' @export
emc_record <- function(..., .label = NULL, .verbose = TRUE, .sep = "_", .cache_path = NULL) {

    quos <- rlang::enquos(..., .named = TRUE)
    quo_names <- rlang::names2(quos)
    label <- rlang::eval_tidy(enquo(.label), data = list(.arg_name = quo_name))
    cache_path <- rlang::eval_tidy(enquo(.cache_path), data = list(.arg_name = quo_name, .label = label))

    stopifnot(length(quos) == 1)

    quo <- quos[[1]]
    quo_name <- quo_names[[1]]

    if (!is.null(cache_path) && file.exists(cache_path)) {
        return (readRDS(cache_path))
    }

    result <- list(NULL)
    succeded <- FALSE
    started <- lubridate::now()
    handler <- evaluate::new_output_handler(
        value = function(v) {
            result <<- v
            succeded <<- TRUE
        }
    )

    if (!.verbose && !is.null(label)) {
        message(">>>> ", label, " <<<<")
    }

    eval_quo <- function() {

        if (!is.null(label)) {
            message(">>>> ", label, " <<<<")
        }

        result <- list(rlang::eval_tidy(quo))
        evaluate::flush_console()
        result
    }

    log <- evaluate::evaluate(eval_quo, output_handler = handler, stop_on_error = 1)
    finished <- lubridate::now()
    messages <- log %>% purrr::keep(evaluate::is.message) %>% length()
    if (!is.null(label)) {
        messages <- messages - 1
    }
    warnings <- log %>% purrr::keep(evaluate::is.warning) %>% length()
    errors <- log %>% purrr::keep(evaluate::is.error) %>% length()
    first_error <- log %>% purrr::keep(evaluate::is.error) %>% first()
    if (!is.null(first_error)) {
        error <- conditionMessage(first_error)
    }
    else {
        error <- ""
    }
    recording <- list()
    value_name <- function(base_name, value) {
        if (!purrr::is_empty(.sep)) {
            paste0(quo_name, .sep, base_name)
        }
        else {
            base_name
        }
    }
    recording[[quo_name]] <- result
    recording[[value_name("log")]] <- list(log)
    recording[[value_name("error")]] = error
    recording[[value_name("errors")]] = errors
    recording[[value_name("messages")]] = messages
    recording[[value_name("warnings")]] = warnings
    recording[[value_name("duration")]] = as.numeric(difftime(finished, started, units = "secs"))

    if (.verbose) {
        evaluate::replay(purrr::discard(log, evaluate::is.source))
    }
    final <- tibble(!!! recording)
    if (!is.null(cache_path)) {
        dir.create(dirname(cache_path), showWarnings = FALSE)
        saveRDS(final, cache_path)
    }
    final
}

#' @export
emc_replay <- function(.log) {
    for (log in .log) {
        evaluate::replay(purrr::discard(log, evaluate::is.source))
    }
    invisible()
}

