# This script provides functions to test model term importance

# ANOVA S3 methods for lm_analysis ----

#' Analysis of variance or deviance table for lm_analysis objects.
#'
#' @description Performs analysis of variance (lm) or deviance (glm) for lm_analysis objects.
#' @details The terms are added sequentially, see: \code{\link[stats]{anova.lm}} or \code{\link[stats]{anova.glm}}
#' for details and additional arguments. The fraction of explained variance is based on sum of squares
#' for lm class objects or on deviance for glm models.
#' @param lm_analysis_object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param ... extra arguments passed to model-specific methods like \code{\link[stats]{anova.lm}}
#' or \code{\link[stats]{anova.glm}}.
#' @return a data frame with ANOVA or deviance analysis table, fraction of explained variance/deviance
#' and, optionally, test results.
#' @export anova.lm_analysis
#' @export

  anova.lm_analysis <- function(lm_analysis_object, ...) {

    aov_res <- as.data.frame(stats::anova(lm_analysis_object$model, ...))

    aov_res <- tibble::rownames_to_column(aov_res, 'variable')

    ## calculating percent explained variance

    if('Sum Sq' %in% names(aov_res)) {

      tibble::as_tibble(dplyr::mutate(aov_res,
                                      frac_explained = `Sum Sq`/sum(`Sum Sq`)))

    } else {

      aov_res <- lmqc::outer_rbind(aov_res,
                                   data.frame(variable = 'Residuals',
                                              Df = aov_res[nrow(aov_res), 4],
                                              Deviance = aov_res[nrow(aov_res), 5]))

      tibble::as_tibble(dplyr::mutate(aov_res,
                                      frac_explained = Deviance/sum(Deviance, na.rm = TRUE)))

    }

  }


