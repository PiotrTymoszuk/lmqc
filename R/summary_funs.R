# This script provides functions for obtaining inference summary from an lm object
# In development: functions for calculating model quality stats such as R2 (explained variance), AUC and so on

#' Extract model estimates with inference statistics.
#'
#' @description extracts coefficients (beta) with 95\% confidence intervals (CI)
#' and p values from a linear, generalized linear model, ordinal regression or generalized additive model.
#' @details the output estimates and ci are transformed with the 'transf_fun' argument.
#' The ci_method option: calculates the CI per \code{\link[stats]{confint}} method by default, from the t or normal distribution
#' as appropriate for the model type ('distribution') or explicitly from the normal distribution ('normal').
#' based on the normality assumption: i. e. SEM * critical normal distribution value. 95\%CI for the intercepts ordinal models
#' can be calculated assuming the normal distribution (ci_method = 'normal'), otherwise, 95\% CI are returned only for the
#' non-intercept terms. For ordinal regression, p values are calculated based on the normal distribution (t = z) and hence
#' approximate and valid only for large sample sizes. For GAMs (still an experimental feature), 95\%CI and p values
#' are calculated based on the distribution (t or z statistic), the returned df value refers to the residual degrees of freedom.
#' The general GAM-coping idea comes from https://stats.stackexchange.com/questions/398846/confidence-interval-for-non-smooth-term-in-gam-mgcv
#' No correction for multiple testing is applied to the beta significance testing.
#' @param linear_model lm, glm, polr or gam class model or an object of the lm_analysis class.
#' @param transf_fun function used for transformation of the estimates and confidence intervals, identity() by default.
#' @param ci_method specifies how the confidence intervals should be calculated, see details.
#' @param silent_messages logical, silence the messages returned during confidence interval calculation.
#' @param ... extra arguments passed to \code{\link[stats]{confint}}.
#' @return a data frame with the parameter names (as specified by the summary() generic function),
#' numbers of complete observations used for model construction (n_complete),
#' transformed beta estimate and confidence interval values (lower_ci, upper_ci)
#' and p value.
#' @export

  get_estimates <- function(linear_model,
                            transf_fun = identity,
                            ci_method = c('default', 'distribution', 'normal'),
                            silent_messages = TRUE, ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'polr')) {

      if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm, glm or polr class object.', call. = FALSE)

    }

    if(!is.function(transf_fun)) stop("The 'trans_fun' argument is not a valid function.", call. = FALSE)

    ci_method <- match.arg(ci_method[1], c('default', 'distribution', 'normal'))
    stopifnot(is.logical(silent_messages))

    ## model summary: to get SEM and p values

    mod_summary <- summary(linear_model)

    ## identifying the model type

    if(all(class(linear_model) == 'lm')) {

      mod_family <- 'gaussian'
      lm_fun <- 'identity'
      inf_stat <- 't'

    } else if(any(class(linear_model) == 'glm')) {

      mod_family <- mod_summary$family$family
      lm_fun <- mod_summary$family$link

      inf_stat <- if(mod_family == 'gaussian') 't' else 'z'

    } else {

      mod_family <- 'polr'
      lm_fun <- 'log'
      inf_stat <- 't'

    }

    ## model estimates, error and CI, transforming

    if(any(class(linear_model) == 'gam')) {

      ## the idea for SE calculation from the variance-covariance matrix comes from
      ## https://stats.stackexchange.com/questions/398846/confidence-interval-for-non-smooth-term-in-gam-mgcv

      mod_se <- stats::vcov(linear_model, unconditional = TRUE)

      mod_se <- sqrt(diag(mod_se))

      mod_df <- df.residual(linear_model)

      model_coef_tbl <- data.frame(coefficient = coef(linear_model),
                                   se = mod_se)

      model_coef_tbl <- dplyr::mutate(model_coef_tbl,
                                      stat = coefficient/se)

      if(inf_stat == 't') {

        model_coef_tbl <- dplyr::mutate(model_coef_tbl,
                                        p_value = pt(q = abs(stat), df = mod_df, lower.tail = FALSE) * 2)

      } else {

        model_coef_tbl <- dplyr::mutate(model_coef_tbl,
                                        p_value = pnorm(q = abs(stat), lower.tail = FALSE) * 2)

      }

    } else {

      model_coef_tbl <- stats::coef(mod_summary)

      mod_df <- mod_summary$df[2]

    }

    model_coefs <- model_coef_tbl[, 1]

    if(all(class(linear_model) == 'polr')) {

      coef_names <- names(model_coefs)

    } else {

      coef_names <- names(coef(linear_model))

    }

    model_se <- model_coef_tbl[, 2]

    if(any(class(linear_model) == 'gam')) {

      if(ci_method == 'default') ci_method <- 'distribution'

    }

    if(ci_method == 'default') {

      model_ci <- if(silent_messages) suppressMessages(stats::confint(linear_model, ...)) else stats::confint(linear_model, ...)

      if(!is.matrix(model_ci)) model_ci <- purrr::reduce(model_ci, cbind)

      model_ci <- rlang::set_names(tibble::as_tibble(model_ci),
                                   c('lower_ci', 'upper_ci'))

    } else if(ci_method == 'normal') {

      model_ci <- tibble::tibble(lower_ci = model_coefs + stats::qnorm(0.025) * model_se,
                                 upper_ci = model_coefs + stats::qnorm(0.975) * model_se)

    } else if(inf_stat == 'z') {

      model_ci <- tibble::tibble(lower_ci = model_coefs + stats::qnorm(0.025) * model_se,
                                 upper_ci = model_coefs + stats::qnorm(0.975) * model_se)

    } else {

      model_ci <- tibble::tibble(lower_ci = model_coefs + stats::qt(p = 0.025, df = mod_df) * model_se,
                                 upper_ci = model_coefs + stats::qt(p = 0.975, df = mod_df) * model_se)

    }

    model_ci <- rbind(model_ci,
                      tibble::tibble(lower_ci = rep(NA, length(model_coefs) - nrow(model_ci)),
                                     upper_ci = rep(NA, length(model_coefs) - nrow(model_ci))))

    est_tibble <- dplyr::mutate(model_ci,
                                estimate = unname(model_coefs))

    est_tibble <- purrr::map_dfc(est_tibble, transf_fun)

    if(mod_family == 'polr') {

      p_values <- pnorm(mod_summary$coefficients[, 3], lower.tail = FALSE) * 2


    } else {

      p_values <- model_coef_tbl[, 4]

    }

    est_tibble <- dplyr::mutate(est_tibble,
                                parameter = coef_names,
                                se = unname(model_se),
                                n_complete = nrow(stats::model.frame(linear_model)),
                                p_value = p_values,
                                stat_name = inf_stat,
                                df = mod_df,
                                stat = model_coef_tbl[, 3],
                                family = mod_family,
                                link_fun = lm_fun,
                                ci_method = ci_method)

    est_tibble[c('parameter',
                 'n_complete',
                 'family',
                 'link_fun',
                 'stat_name',
                 'stat',
                 'df',
                 'estimate',
                 'se',
                 'ci_method',
                 'lower_ci',
                 'upper_ci',
                 'p_value')]

  }

#' Extract model quality statistics.
#'
#' @description calculates AIC, BIC, R squared, mean squared and absolute error of the given linear model.
#' @details R squared is calculated as described for \code{\link[rsq]{rsq}}.
#' @param linear_model lm, glm, polr or gam class model or an object of the lm_analysis class.
#' @param r_sq_type the type of R-squared (only applicable for generalized linear models), please refer to \code{\link[rsq]{rsq}} for details.
#' @param type.residuals type of the residuals used for error calculation, defaults to 'working'. Residuals for a polr class
#' models are calculated with \code{\link[sure]{resids}}.
#' @return a data frame with the n number of complete observations, AIC, BIC, R squared, deviance, mae (mean absolute error),
#' mse (mean squared error) and rmse (root mean squared error).
#' @details For polr ordinal models, the residuals used to estimated the fir errors are obtained with \code{\link[sure]{resids}}.
#' The R-squared value for such models is based on the full models - null model deviance ratio (McFadden's pseudo-R-squared).
#' @export

  get_stats <- function(linear_model,
                        rsq_type = c('v','kl','sse','lr','n'),
                        type.residuals = c('working', 'response', 'deviance', 'pearson','partial')) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'polr')) {

      if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm, glm or polr class object.', call. = FALSE)

    }

    rsq_type <- match.arg(rsq_type[1], c('v','kl','sse','lr','n'))
    type.residuals <- match.arg(type.residuals[1], c('working', 'response', 'deviance', 'pearson','partial'))

    if(all(class(linear_model) == 'polr')) {

      resids <- sure::resids(linear_model)

      ## for ordinal regression: pseudo-R2 by McFadden

      null_form <- as.formula(paste(as.character(formula(linear_model))[[2]], '~1'))

      null_model <- MASS::polr(formula = null_form,
                               data = model.frame(linear_model),
                               method = linear_model$method)

      raw_rsq <- 1 - (stats::deviance(linear_model)/stats::deviance(null_model))
      adj_rsq <- raw_rsq

    } else {

      resids <- stats::residuals(linear_model, type = type.residuals)

      raw_rsq <- rsq::rsq(linear_model, adj = FALSE, type = rsq_type)
      adj_rsq <- rsq::rsq(linear_model, adj = TRUE, type = rsq_type)

    }

    tibble::tibble(n_complete = nrow(stats::model.frame(linear_model)),
                   aic = stats::AIC(linear_model),
                   bic = stats::BIC(linear_model),
                   raw_rsq = raw_rsq,
                   adj_rsq = adj_rsq,
                   deviance = stats::deviance(linear_model),
                   mae = mean(abs(resids)),
                   mse = mean(resids^2),
                   rmse = sqrt(mean(resids^2)))

  }

#' Extract variable n numbers.
#'
#' @description extracts the numbers of complete cases for numeric features and level counts for categorical variables.
#' @param linear_model lm, glm, polr or gam class model or an object of the lm_analysis class.
#' @return a data frame with counts
#' @export

  count_model <- function(linear_model) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'polr')) {

      if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm, glm or polr class object.', call. = FALSE)

    }

    mod_data <- tibble::as_tibble(model.frame(linear_model))[, -1] ## the response is skipped

    mod_vars <- names(mod_data)

    purrr::map_dfr(mod_vars,
                   ~count_(data = mod_data, variable = .x))

  }

