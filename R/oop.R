# This script provides S3 oop solutions for the lm_analysis class objects

# Generics -----

#' Extract features of an object
#'
#' @description S3 generic functions used to extract object-specific properties or other features
#' @return The requested feature.
#' @export

  extract <- function(x, ...) {

    UseMethod('extract', x)

  }

#' Extract numbers of complete observations
#'
#' @description S3 generic functions used to extract the numbers of complete observations
#' @return The number of complete observations.
#' @export

  nobs <- function(x, ...) {

    UseMethod('nobs', x)

  }

#' Optimize models by AIC-driven elimination.
#'
#' @description S3 generic functions used to optimize models by AIC-driven term elimination
#' @param x model object.
#' @return A model of the same class as x with the optimized set of explanatory variables.
#' @export

  step <- function(x, ...) {

    UseMethod('step', x)

  }

# Basic lm_analysis methods: summary, printing and plotting ------

#' Summarize inference, fit stats or assumptions for a lm_analysis object.
#'
#' @description Retrieves inference or fit statistics from an lm_analysis object.
#' @param object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param type type of the statistics returned.
#' @param ... extra arguments passed to the \code{\link{get_estimates}}, \code{\link{get_stats}} functions or
#' normality and variance equality testing functions.
#' @return a data frame with requested statistics as specified for \code{\link{get_estimates}}, \code{\link{get_stats}},
#' \code{\link{normality}}, \code{\link{homogeneity}} and \code{\link{prop_odds}}, .
#' @export summary.lm_analysis
#' @export

  summary.lm_analysis <- function(object,
                                  type = c('inference', 'fit', 'assumptions'), ...) {

    stopifnot(lmqc::is_lm_analysis(object))

    type <- match.arg(type[1], c('inference', 'fit', 'assumptions'))

    if(type == 'fit') {

      summ_tbl <- lmqc::get_stats(object, ...)

      summ_tbl <- dplyr::mutate(summ_tbl,
                                response = object$response,
                                family = if(!is.null(object$family)) object$family else NA)
      summ_tbl[c('response',
                 'family',
                 'n_complete',
                 'aic',
                 'bic',
                 'raw_rsq',
                 'adj_rsq',
                 'deviance',
                 'mae',
                 'mse',
                 'rmse')]

    } else if(type == 'inference') {


      summ_tbl <- get_estimates(object, ...)

      extr_regex <- paste(c(object$indep_variable,
                            object$confounder),
                          collapse = '|')

      summ_tbl <- dplyr::mutate(summ_tbl,
                                response = object$response,
                                variable = ifelse(parameter == '(Intercept)' | stringi::stri_detect(parameter, fixed = '|'),
                                                  'Intercept',
                                                  stringi::stri_extract_all(parameter, regex = extr_regex)),
                                variable = purrr::map_chr(variable, paste, collapse = ':'),
                                level = ifelse(!is.na(variable),
                                               stringi::stri_replace_all(parameter,
                                                                         regex = extr_regex,
                                                                         replacement  = ''),
                                               stringi::stri_replace(parameter,
                                                                     fixed = object$confounder,
                                                                     replacement  = '')),
                                level = ifelse(level == '',
                                               NA,
                                               ifelse(variable == 'Intercept',
                                                      'baseline',
                                                      level)))

      if(is.null(object$confounder)) {

        summ_tbl <- dplyr::mutate(summ_tbl,
                                  confounder = 'no')

      } else {

        summ_tbl <- dplyr::mutate(summ_tbl,
                                  confounder = ifelse(variable == object$confounder,
                                                      'yes', 'no'))

      }


      summ_tbl <- dplyr::left_join(summ_tbl,
                                   count_model(object),
                                   by = c('variable', 'level'))

      summ_tbl[c('response',
                 'parameter',
                 'variable',
                 'level',
                 'confounder',
                 'n',
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


    } else {

      tst_results <- rbind(lmqc::normality(object),
                           lmqc::homogeneity(object))

      if(object$model_type == 'polr') {

        tst_results <- rbind(dplyr::mutate(tst_results, variable = NA),
                             lmqc::prop_odds(object))

      }

      tst_results

    }

  }

#' Prints the base properties of a lm_analysis object.
#'
#' @description Prints a handy summary of a lm_analysis object.
#' @param x an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @return Nothing, called for side effects.
#' @export

  print.lm_analysis <- function(x) {

    stopifnot(is_lm_analysis(x))

    cat('LM Analysis object')
    cat('Formula:\n')
    print(formula(x$model))

  }

#' Generates diagnostic plots for a lm_analysis object.
#' @description Makes a series of plots of the model residuals for visual quality control.
#' @param x an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param type plot type: 'residuals' returns a series of diagnostic plots for model residuals,
#' 'relationship' plots relationship of the model response and explanatory variables,
#' 'distribution' plots distribution histogram of the response. The later one can be facetted by
#' the user with ggplot2::facet_grid().
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to \code{\link[broom]{augment}}, \code{\link{get_qc_tbl}} or \code{\link{distribution}}.
#' @return returns a series of ggplot objects with the diagnostic residuals plots.
#' @export plot.lm_analysis
#' @export

  plot.lm_analysis <- function(x,
                               type = c('residuals', 'relationship', 'distribution'),
                               cust_theme = ggplot2::theme_classic(), ...) {

    stopifnot(is_lm_analysis(x))
    stopifnot(any(class(cust_theme) == 'theme'))

    type <- match.arg(type[1], c('residuals', 'relationship', 'distribution'))

    switch(type,
           residuals = lmqc::get_qc_plots(linear_model = x,
                                          cust_theme = cust_theme, ...),
           relationship = lmqc::linearity(linear_model = x, cust_theme = cust_theme),
           distribution = lmqc::distribution(linear_model = x, cust_theme = cust_theme, ...))

  }

# Extractor methods -----

#' Number of complete observations in a lm_analysis object
#'
#' @description The number of complete observations as returned by the \code{\link[stats]{nobs}} generic.
#' @param x an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @return number of complete cases used for modeling.
#' @export

  nobs.lm_analysis <- function(x, ...) {

    stopifnot(is_lm_analysis(x))

    stats::nobs(x$model)

  }

#' Model frame of a lm_analysis object
#'
#' @description The model frame of a lm_analysis object as specified by \code{\link[stats]{model.frame}}.
#' @param formula an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @return the data frame used for modeling.
#' @export

  model.frame.lm_analysis <- function(formula, ...) {

    stopifnot(is_lm_analysis(formula))

    stats::model.frame(formula$model)

  }

#' Formula of a lm_analysis object
#'
#' @description The model formula of a lm_analysis object as specified by \code{\link[stats]{formula}}.
#' @param lm_analysis_object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @return the formula used for modeling.
#' @export

  formula.lm_analysis <- function(x, ...) {

    stopifnot(is_lm_analysis(x))

    stats::formula(x$model)

  }

#' Predictions for a lm_analysis model.
#'
#' @description Predictions for a lm_analysis model.
#' @param object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param ... extra arguments passed to specific prediction methods such as \code{\link[stats]{predict.lm}}
#' or \code{\link[stats]{predict.glm}}.
#' @return a vector or data frame with the predicted values
#' @export predict.lm_analysis
#' @export

  predict.lm_analysis <- function(object, ...) {

    stopifnot(is_lm_analysis(object))

    stats::predict(object$model, ...)

  }

#' Residuals of a lm_analysis model.
#' @description Extracts enhanced residuals for a lm_analysis model.
#' @param object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param ... extra arguments passed to \code{\link{get_qc_tbl}}.
#' @return a data frame with predicted values and residuals.
#' @export residuals.lm_analysis
#' @export

  residuals.lm_analysis <- function(object, ...) {

    stopifnot(is_lm_analysis(object))

    get_qc_tbl(object, ...)

  }

#' Extract features of lm_analysis objects
#'
#' @description a handy extractor function enabling access to the model frame, formulas and more.
#' @param x an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param what name of the requested feature.
#' @param ... extra parameters passed to inference (\code{\link{get_estimates}}), fit (\code{\link{get_stats}})
#' and prediction functions (\code{\link{predict.lm_analysis}})
#' @details 'data' returns the model frame, 'formula' returns the formula, 'inference' returns
#' the inference summary, 'fit' the summary of fit stats, 'assumptions' the normality of residuals and EOV stats,
#' 'n' the number of complete cases, 'n_levels' the number of complete observations within the variable levels
#' @return the requested feature.
#' @export extract.lm_analysis
#' @export

  extract.lm_analysis <- function(x,
                                  what = c('data', 'formula', 'inference', 'fit', 'assumptions', 'n', 'n_levels', 'prediction'), ...) {

    stopifnot(is_lm_analysis(x))

    what <- match.arg(what[1], c('data', 'formula', 'inference', 'fit', 'n', 'n_levels', 'prediction', 'assumptions'))

    switch(what,
           data = model.frame(x),
           formula = formula(x),
           inference = summary(x, type = 'inference', ...),
           fit = summary(x, type = 'fit', ...),
           assumptions = summary(x, type = 'assumptions', ...),
           n = nobs(x),
           prediction = predict(x, ...),
           n_levels = count_model(x))

  }

# Model optimization by elimination -----

#' Model optimization by AIC driven elimination
#'
#' @description Applies \code{\link[stats]{step}} to the lm_analysis object.
#' @param object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param step_fun a function used to eliminate the model terms.
#' @param ... other arguments passed to \code{\link[stats]{step}} or other elimination function.
#' @return A lm_analysis object with the reduced set of explanatory variables.
#' @export step.lm_analysis
#' @export

  step.lm_analysis <- function(object, step_fun = stats::step, ...) {

    stopifnot(is_lm_analysis(object))

    if(object$model_type == 'gam') {

      warning('Step method currently unavailable for the gam-type lm_analysis objects.', call. = FALSE)

      return(NULL)

    }

    new_model <- step_fun(object$model, ...)

    new_coefs <- new_model$coefficients

    extr_regex <- paste(c(object$indep_variable,
                          object$confounder),
                        collapse = '|')

    indep_variables <- names(new_coefs)[names(new_coefs) != '(Intercept)']

    indep_variables <- stringi::stri_extract(indep_variables, regex = extr_regex)

    if(object$model_type == 'polr') {

      mod_family <- NULL

    } else {

      mod_family <- if(class(new_model)[1] == 'lm') 'gaussian' else object$family

    }

    structure(list(response = object$response,
                   indep_variable = unique(indep_variables),
                   confounder = NULL,
                   weight_variable = object$weight_variable,
                   model_type = class(new_model)[1],
                   family = mod_family,
                   model = new_model),
              class = 'lm_analysis')


  }

#' Default method for AIC driven elimination.
#'
#' @description Applies \code{\link[stats]{step}} to the lm_analysis object.
#' @param object model object.
#' @param ... other arguments passed to \code{\link[stats]{step}}.
#' @export step.default
#' @export

  step.default <- function(object, ...) {

    stats::step(object, ...)

  }
