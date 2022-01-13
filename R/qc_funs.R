# Contains code for the functions used for stripping the model residuals
# and visual quality control by generating residual plots.

#' Extended residuals of a linear model
#'
#' @description Extracts extended residuals of a linear model and potantial outliers,
#' @details a wrapper around \code{\link[broom]{augment}} with some extra output including confidence intervals
#' of the fit, squared residuals and expected normal distribution for the standardized residuals and the true outcome.
#' Potential outliers are identified by the two-SE criterion.
#' @param linear_model lm or glm class model or an object of the lm_analysis class
#' @param type.predict prediction type, as specified for \code{\link[broom]{augment}}, defaults to 'link'.
#' @param type.residuals type of the calculated residuals, as specified for \code{\link[broom]{augment}}, defaults to 'pearson'.
#' @param ... extra arguments passed to \code{\link[broom]{augment}}.
#' @return a data frame with the fitted values, true outcome, residuals and candidate outliers.
#' @export

  get_qc_tbl <- function(linear_model,
                         type.predict = c('link', 'response', 'terms'),
                         type.residuals = c('pearson', 'deviance'), ...) {

    if(any(class(linear_model) == 'lm_analysis')) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm or glm class object.', call. = FALSE)

    type.predict <- match.arg(type.predict[1], c('link', 'response', 'terms'))
    type.residuals <- match.arg(type.residuals[1], c('pearson', 'deviance'))

    if(!any(class(linear_model) == 'glm') & type.predict == 'link') {

      warning("'type.predict' argument specified to 'link' for a lm object is not correct. Calulating with 'response' instead", call. = FALSE)

      type.predict <- 'response'

    }

    qc_tbl <- broom::augment(linear_model,
                             se_fit = TRUE,
                             type.predict = type.predict,
                             type.residuals = type.residuals, ...)

    qc_tbl <- dplyr::mutate(qc_tbl,
                            .outcome = predict(linear_model, type = type.predict),
                            .observation = 1:nrow(qc_tbl),
                            .sq.std.resid = .std.resid^2,
                            .lower_ci.fit = .fitted + .se.fit * qnorm(0.025),
                            .upper_ci.fit = .fitted + .se.fit * qnorm(0.975),
                            .candidate_missfit = ifelse(abs(.std.resid) > qnorm(0.975), 'yes', 'no'))

    lmqc:::calc_expected_(qc_tbl, '.std.resid')

  }

#' Diagnostic plots of model residuals
#'
#' @description Plots a series of diagnostic plots of model residuals with potential outliers indicated.
#' @param linear_model lm or glm class model or an object of the lm_analysis class.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to \code{\link[broom]{augment}} and \code{\link{get_qc_tbl}}.
#' @return returns a series of ggplot objects with the diagnostic residuals plots.
#' @export


  get_qc_plots <- function(linear_model,
                           cust_theme = ggplot2::theme_classic(), ...) {

    if(any(class(linear_model) == 'lm_analysis')) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm or glm class object.', call. = FALSE)
    if(!any(class(cust_theme) == 'theme')) stop('Please provide a valid ggplot2 theme class object.', call. = FALSE)

    ## QC table

    qc_tbl <- lmqc::get_qc_tbl(linear_model, ...)

    ## QC plots

    qc_plotting_lst <- list(x_var = c('.fitted', '.fitted', '.fitted', '.expect.norm', '.sigma'),
                            y_var = c('.resid', '.std.resid', '.sq.std.resid', '.std.resid', '.cooksd'),
                            plot_title = c('Residuals vs. fitted',
                                           'Standardized residuals vs. fitted',
                                           'Sqared residuals vs. fitted',
                                           'QQ standardized residuals vs expected normal',
                                           'Cook distance vs dropout sigma'),
                            method = c('loess', 'loess', 'loess', 'lm', 'loess'),
                            smooth = c(TRUE, TRUE, TRUE, TRUE, FALSE))

    qc_plots <- purrr::pmap(qc_plotting_lst,
                            lmqc:::point_plot_,
                            data = qc_tbl,
                            cust_theme = cust_theme)

    rlang::set_names(qc_plots,
                     c('resid_fitted',
                       'std.resid_fitted',
                       'sq.resid_fitted',
                       'qq.std.resid',
                       'cook_sigma'))


  }
