# Contains code for the functions used for stripping the model residuals
# and visual quality control by generating residual plots.

# Residuals and residual plots ------

#' Extended residuals of a linear model
#'
#' @description Extracts extended residuals of a linear model and potential outliers,
#' @details a wrapper around \code{\link[broom]{augment}} with some extra output including confidence intervals
#' of the fit, squared residuals and expected normal distribution for the standardized residuals and the true outcome.
#' Potential outliers are identified by the two-SE criterion.
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param type.predict prediction type, as specified for \code{\link[broom]{augment}}, defaults to 'link'.
#' @param type.residuals type of the calculated residuals, as specified for \code{\link[broom]{augment}}, defaults to 'pearson'.
#' @param ... extra arguments passed to \code{\link[broom]{augment}}.
#' @return a data frame with the fitted values, true outcome, residuals and candidate outliers.
#' @export

  get_qc_tbl <- function(linear_model,
                         type.predict = c('link', 'response', 'terms'),
                         type.residuals = c('pearson', 'deviance'), ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'polr')) {

      if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm, glm or polr class object.', call. = FALSE)

    }

    type.predict <- match.arg(type.predict[1], c('link', 'response', 'terms'))
    type.residuals <- match.arg(type.residuals[1], c('pearson', 'deviance'))

    if(all(class(linear_model) == 'polr')) {

      warning("Arguments 'type.predict' and 'type.residuals' igrored for polr class models.", call. = FALSE)

      qc_tbl <- tibble::as_tibble(model.frame(linear_model))

      qc_tbl <- dplyr::mutate(qc_tbl,
                              .fitted = predict(linear_model, type = 'class'),
                              .resid = sure::resids(linear_model))

      qc_tbl <- dplyr::mutate(qc_tbl,
                              .resid = sure::resids(linear_model),
                              .std.resid = scale(.resid)[, 1],
                              .outcome = predict(linear_model, type = 'class'),
                              .observation = 1:nrow(qc_tbl),
                              .sq.std.resid = .std.resid^2,
                              .candidate_missfit = ifelse(abs(.std.resid) > qnorm(0.975), 'yes', 'no'))

    } else {

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
                              .std.resid = scale(.resid)[, 1],
                              .sq.std.resid = .std.resid^2,
                              .lower_ci.fit = .fitted + .se.fit * qnorm(0.025),
                              .upper_ci.fit = .fitted + .se.fit * qnorm(0.975),
                              .candidate_missfit = ifelse(abs(.std.resid) > qnorm(0.975), 'yes', 'no'))

    }

    lmqc:::calc_expected_(qc_tbl, '.std.resid')

  }

#' Diagnostic plots of model residuals
#'
#' @description Plots a series of diagnostic plots of model residuals with potential outliers indicated.
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to \code{\link[broom]{augment}} and \code{\link{get_qc_tbl}}.
#' @return returns a series of ggplot objects with the diagnostic residuals plots.
#' @export


  get_qc_plots <- function(linear_model,
                           cust_theme = ggplot2::theme_classic(), ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(!any(class(linear_model) == 'polr')) {

      if(!any(class(linear_model) == 'lm')) stop('Please provide a valid lm, glm or polr class object.', call. = FALSE)

    }

    if(!any(class(cust_theme) == 'theme')) stop('Please provide a valid ggplot2 theme class object.', call. = FALSE)

    ## QC table

    qc_tbl <- lmqc::get_qc_tbl(linear_model, ...)

    ## QC plots

    if(all(class(linear_model) == 'polr')) {

      qc_plotting_lst <- list(x_var = c('.fitted', '.fitted', '.fitted', '.expect.norm'),
                              y_var = c('.resid', '.std.resid', '.sq.std.resid', '.std.resid'),
                              plot_title = c('Residuals vs. fitted',
                                             'Standardized residuals vs. fitted',
                                             'Sqared residuals vs. fitted',
                                             'QQ standardized residuals vs expected normal'),
                              method = c('loess', 'loess', 'loess', 'lm'),
                              smooth = c(TRUE, TRUE, TRUE, TRUE))

      plot_names <- c('resid_fitted',
                      'std.resid_fitted',
                      'sq.resid_fitted',
                      'qq.std.resid')

    } else {

      qc_plotting_lst <- list(x_var = c('.fitted', '.fitted', '.fitted', '.expect.norm', '.sigma'),
                              y_var = c('.resid', '.std.resid', '.sq.std.resid', '.std.resid', '.cooksd'),
                              plot_title = c('Residuals vs. fitted',
                                             'Standardized residuals vs. fitted',
                                             'Sqared residuals vs. fitted',
                                             'QQ standardized residuals vs expected normal',
                                             'Cook distance vs dropout sigma'),
                              method = c('loess', 'loess', 'loess', 'lm', 'loess'),
                              smooth = c(TRUE, TRUE, TRUE, TRUE, FALSE))

      plot_names <- c('resid_fitted',
                      'std.resid_fitted',
                      'sq.resid_fitted',
                      'qq.std.resid',
                      'cook_sigma')

    }

    qc_plots <- purrr::pmap(qc_plotting_lst,
                            lmqc:::point_plot_,
                            data = qc_tbl,
                            cust_theme = cust_theme)

    rlang::set_names(qc_plots, plot_names)


  }

# Normality and variance homogeneity of the residuals ------

#' Normality of the model residuals
#'
#' @description Tests normality of the model residuals by Shapiro-Wilk test.
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param ... extra arguments passed to \code{\link[broom]{augment}} and \code{\link{get_qc_tbl}}.
#' @return a tibble with the test results: test statistic and p value.
#' @export

  normality <- function(linear_model, ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    resid_tbl <- lmqc::get_qc_tbl(linear_model = linear_model, ...)

    tst_results <- shapiro.test(resid_tbl$.resid)

    tibble::tibble(type = 'normality',
                   test = 'Shapiro-Wilk test',
                   stat_name = 'W',
                   stat_value = tst_results[['statistic']],
                   df1 = NA,
                   df2 = NA,
                   p_value = tst_results[['p.value']])

  }

#' Variance homogeneity of the model residuals
#'
#' @description Tests homogeneity of the model residuals by Levene test (\code{\link[DescTools]{LeveneTest}}).
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param ... extra arguments passed to \code{\link[broom]{augment}} and \code{\link{get_qc_tbl}}.
#' @return a tibble with the test results: test statistic, degrees of freedom and p value.
#' @export

  homogeneity <- function(linear_model, ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    resid_tbl <- lmqc::get_qc_tbl(linear_model = linear_model, ...)

    model_frame <- tibble::as_tibble(model.frame(linear_model))

    num_vars <- purrr::map_lgl(model_frame[, -1], is.numeric)

    factor_vars <- names(model_frame)[-1][!num_vars]

    if(length(factor_vars) == 0) {

      warning('The model has no categorical explanatory variables, Levene test can not be calculated.', call. = FALSE)

      return(NULL)

    }

    lev_form <- as.formula(paste('.resid ~', paste(factor_vars, collapse = '*')))

    tst_results <- DescTools::LeveneTest(lev_form, data = resid_tbl)

    tibble::tibble(type = 'homogeneity',
                   test = 'Levene test',
                   stat_name = 'F',
                   stat_value = tst_results[['F value']][1],
                   df1 = tst_results[['Df']][1],
                   df2 = tst_results[['Df']][2],
                   p_value = tst_results[['Pr(>F)']][1])

  }

# Model type-specific assumptions ------

#' Test odds proportionality assumption for polr models
#'
#' @description Tests odds proportionality assumption for ordinal regression models.
#' An implementation of \code{\link[brant]{brant}}.
#' @param linear_model polr model of lm_analysis class model.
#' @param ... extra arguments passed to \code{\link[brant]{brant}}.
#' @return a tibble with the test results: test statistic, degrees of freedom and p value.
#' @export

  prop_odds <- function(linear_model, ...) {

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    if(any(class(linear_model) != 'polr')) stop('A MASS::polr model required.', call. = FALSE)

    tst_results <- as.data.frame(brant::brant(linear_model, ...))

    tst_results <- tibble::rownames_to_column(tst_results, 'variable')

    tst_results <- dplyr::mutate(tst_results,
                                 type = 'Odds proportionality',
                                 test = 'Brant test',
                                 stat_name = 'Chi-squred',
                                 stat_value = tst_results[['X2']],
                                 df1 = df,
                                 df2 = NA,
                                 p_value = probability)

    tst_results <- tibble::as_tibble(tst_results)

    tst_results[c('type', 'test', 'variable', 'stat_name', 'stat_value', 'df1', 'df2', 'p_value')]

  }

#' Graphical inspection of the linearity assumption
#'
#' @description Plots the relationship of the numeric model response and the explanatory factors.
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param cust_theme customized plot theme provided by the user.
#' @return a list of ggplot objects with point plots and fitted LOESS trends.
#' @export

  linearity <- function(linear_model,
                        cust_theme = ggplot2::theme_classic()) {

    ## entry control

    stopifnot(any(class(cust_theme) == 'theme'))

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    ## model frame

    model_frame <- model.frame(linear_model)

    model_frame <- purrr::map_dfc(model_frame,
                                  function(x) if(is.character(x)) factor(x) else x)

    model_frame <- purrr::map_dfc(model_frame,
                                  function(x) if(is.factor(x)) as.numeric(x) else x)

    model_response <- names(model_frame)[1]

    model_features <- names(model_frame)[-1]

    ## plotting

    plotting_list <- list(x_var = model_features,
                          plot_title = paste(model_response, model_features, sep = ' vs '))

    plot_list <- purrr::pmap(plotting_list,
                             lmqc:::point_plot_,
                             data = model_frame,
                             y_var = model_response,
                             smooth = TRUE,
                             method = 'loess',
                             cust_theme = cust_theme)

    rlang::set_names(plot_list, paste(model_response, model_features, sep = '_'))

  }

#' Distribution of the model response
#'
#' @description Plots a histogram of the model response or a series of histograms if the faceting formula argument
#' is provided by the user, see: \code{\link[ggplot2]{facet_grid}} for details.
#' @param linear_model lm, glm, polr or lm_analysis class model.
#' @param facet_formula formula for faceting the histogram plots. If NULL, no faceting is done.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_histogram}}.
#' @return a ggplot histogram.
#' @export

  distribution <- function(linear_model,
                           facet_formula = NULL,
                           cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    stopifnot(any(class(cust_theme) == 'theme'))

    if(is_lm_analysis(linear_model)) {

      linear_model <- linear_model$model

    }

    ## model frame

    model_frame <- model.frame(linear_model)

    model_response <- names(model_frame)[1]

    model_features <- names(model_frame)[-1]

    model_frame <- dplyr::mutate(model_frame,
                                 !!model_response := as.numeric(.data[[model_response]]))

    ## plotting

    model_histo <- ggplot2::ggplot(model_frame,
                                   ggplot2::aes(x = .data[[model_response]])) +
      ggplot2::geom_histogram(color = 'black',
                              fill = 'steelblue', ...) +
      cust_theme

    if(!is.null(facet_formula)) {

      model_histo <- model_histo +
        ggplot2::facet_grid(facet_formula)

    }

    model_histo

  }

