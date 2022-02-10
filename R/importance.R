# This script provides functions to test model term importance

# Wrappers around stats ANOVA methods -----

#' Default method for computing analysis of variance/deviance.
#'
#' @description Performs an analysis of variance/deviance for model objects as described
#' for the \code{\link[stats]{anova}} generic. In addition, percent of explained variance/deviance is returned.
#' @param object a model object.
#' @param ... additional arguments passed to specific methods.
#' @return a data frame with the analysis results.
#' @export anova.default
#' @export

  anova.default <- function(object, ...) {

    aov_res <- as.data.frame(stats::anova(object, ...))

    aov_res <- tibble::rownames_to_column(aov_res, 'variable')

    ## calculating percent explained variance

    if('Sum Sq' %in% names(aov_res)) {

      tibble::as_tibble(dplyr::mutate(aov_res,
                                      frac_explained = `Sum Sq`/sum(`Sum Sq`)))

    } else if('Deviance' %in% names(aov_res)){

      aov_res <- lmqc::outer_rbind(aov_res,
                                   data.frame(variable = 'Residuals',
                                              Df = aov_res[nrow(aov_res), 4],
                                              Deviance = aov_res[nrow(aov_res), 5]))

      tibble::as_tibble(dplyr::mutate(aov_res,
                                      frac_explained = Deviance/sum(Deviance, na.rm = TRUE)))

    } else {

      aov_res

    }

  }

# ANOVA S3 methods for lm_analysis ----

#' Analysis of variance or deviance table for lm_analysis objects.
#'
#' @description Performs analysis of variance (lm) or deviance (glm) for lm_analysis objects.
#' @details The terms are added sequentially, see: \code{\link[stats]{anova.lm}}, \code{\link[stats]{anova.glm}} or
#' \code{\link[mgcv]{anova.gam}} for details and additional arguments.
#' The fraction of explained variance is based on sum of squares for lm class objects or on deviance for glm models.
#' This parameter is currently absent from the output for GAM models. Polr models return NULL and a warning
#' @param object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param ... extra arguments passed to model-specific methods like \code{\link[stats]{anova.lm}}
#' or \code{\link[stats]{anova.glm}}.
#' @return a data frame with ANOVA or deviance analysis table, fraction of explained variance/deviance
#' and, optionally, test results.
#' @export anova.lm_analysis
#' @export

  anova.lm_analysis <- function(object, ...) {

    stopifnot(is_lm_analysis(object))

    if(object$model_type == 'polr') {

      warning('ANOVA method for polr-type lm_anaylsis objects is not implemented.', call. = FALSE)

      NULL

    } else if(object$model_type == 'gam') {

      aov_output <- mgcv::anova.gam(object$model, ...)

      stat_name <- if(object$family == 'gaussian') 'F' else 'Chi.sq'

      p_tibble <- aov_output$pTerms.table

      if(!is.null(aov_output$pTerms.table)) {

        p_tibble <- dplyr::mutate(as.data.frame(p_tibble),
                                  edf = NA)

        p_tibble <- rlang::set_names(p_tibble,
                                     c('df', stat_name, 'p_value', 'edf'))

      }

      s_tibble <- rlang::set_names(as.data.frame(aov_output$s.table),
                                   c('edf', 'df', stat_name, 'p_value'))

      tibble_lst <- purrr::compact(list(p_tibble,
                                        s_tibble))

      aov_output <- purrr::map_dfr(tibble_lst,
                                   ~tibble::rownames_to_column(.x, 'variable'))

      tibble::as_tibble(aov_output[c('variable', 'edf', 'df', stat_name, 'p_value')])

    } else {

      lmqc::anova.default(object$model, ...)

    }

  }
