
#' @export
emc_performance <- function(.train) {

    train <- .train[[1]]
    if (is.null(train)) {
        return(tibble::tibble()[1,])
    }
    stopifnot("train" %in% class(train))

    caret_performance <- caret::getTrainPerf(train) %>% dplyr::select(-method)

    cols <- colnames(caret_performance)
    out_cols <- gsub("^Train", "", cols)

    vars <- purrr::map2(cols, out_cols, ~ rlang::list2(!! .y := sym(.x) ) ) %>% rlang::flatten()
    performance <- dplyr::rename(caret_performance, !!!vars )
    performance
}

#' @export
emc_resamples <- function(.train) {

    train <- .train[[1]]
    if (is.null(train)) {
        return(tibble::tibble()[1,])
    }
    stopifnot("train" %in% class(train))
    resamples <- train$resample
    tibble(resamples = list(resamples))
}

#' @export
emc_t_test <- function(.formula, .data, alternative = c("two.sided", "less", "greater"),
                       mu = 0, paired = FALSE, var.equal = FALSE,
                       conf.level = 0.95) {

    formula <- rlang::eval_tidy(enquo(.formula))
    data <- rlang::eval_tidy(enquo(.data))

    t_test <-stats::t.test(formula = formula, data = data, alternative = alternative, mu = mu, paired = paired, var.equal = var.equal, conf.level = conf.level)

    p_value <- t_test$p.value
    estimates <- rlang::list2(!!! t_test$estimate)
    conf_int_low <- t_test$conf.int[1]
    conf_int_high <- t_test$conf.int[2]

    tibble(p_value = p_value, !!! estimates, mean_diff_low = conf_int_low, mean_diff_high = conf_int_high)

}

#' @export
emc_variable_importance <- function(.train) {
    train <- .train[[1]]
    if (is.null(train)) {
        return(tibble::tibble()[1,])
    }
    importance <- caret::varImp(train)
    importance <- importance$importance %>% tibble::rownames_to_column(var = "Variable") %>% as_tibble()
    tibble(importance = list(importance))
}
