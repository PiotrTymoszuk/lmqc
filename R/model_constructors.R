# This function contains functions used for serial model construction

#' Construct a lm or glm analysis object.
#'
#' @description Builds a linear or generalized linear model, optionally in an error-resistant mode.
#' @param data data frame.
#' @param response name of the outcome variable, ignored if 'formula' provided.
#' @param indep_variable name of the explanatory variable or a vector with explanatory variable names,
#' ignored if 'formula' provided.
#' @param confounder name of a confounding variable, treated as such by the downstream methods.
#' @param weight_variable name of a model weight variable.
#' @param mod_fun modeling function. Currently supported are lm, glm, MASS::polr and mgcv::gam.
#' @param family modeling function family
#' @param error_resistant logical, if TRUE, NULL is returned without an error but a warning is raised
#' @param verbose logical, should the
#' @param ... extra arguments passed to the modeling function
#' @return An object of class 'lm_analysis'
#' @export

  make_lm <- function(data,
                      response = NULL,
                      indep_variable = NULL,
                      formula = NULL,
                      confounder = NULL,
                      weight_variable = NULL,
                      mod_fun = glm,
                      family = 'binomial',
                      error_resistant = TRUE,
                      verbose = FALSE, ...) {

    if(!any(class(data) == 'data.frame')) stop('Please provide a valida data.frame or tibble object.', call. = FALSE)
    if(all(c(is.null(response), is.null(indep_variable), is.null(formula)))) {

      stop('Names of response and independent variable or a valid formla are required', call. = FALSE)

    }
    if(!is.function(mod_fun)) stop('Please provide a valid modeling function', call. = FALSE)

    if(!is.null(weight_variable)) {

      if(!weight_variable %in% names(data)) stop('Weighting variable absent from data.', call. = FALSE)

    }

    stopifnot(is.logical(error_resistant))
    stopifnot(is.logical(verbose))

    start_time <- Sys.time()

    ## model formula

    if(is.null(formula)) {

      if(!response %in% names(data)) stop('Response variable absent from data.', call. = FALSE)
      if(any(!indep_variable %in% names(data))) stop('At least one idependent variable absent from data', call. = FALSE)
      if(!is.null(confounder)) {

       if(!confounder %in% names(data)) stop('Confounder variable absent from data', call. = FALSE)

      }

      formula <- paste(response,
                       '~',
                       paste(c(indep_variable,
                               confounder),
                             collapse = '+'))

      formula <- as.formula(formula)

    } else {

      form_string <- as.character(formula)

      response <- form_string[[2]]

      indep_variable <- stringi::stri_split(form_string[[3]], regex = '\\+|\\*')

      indep_variable <- unlist(indep_variable)

      indep_variable <- stringi::stri_replace_all(indep_variable, regex = '\\s+', replacement = '')

    }

    if(verbose) {

      char_formula <- as.character(formula)

      message(paste('Modeling response:',
                    response,
                    ', indep. variable:',
                    indep_variable,
                    ', formula:',
                    paste(char_formula[2], '~', char_formula[3])))

      on.exit(message(paste('Elapsed:', Sys.time() - start_time)))

    }

    ## lm model constructed via call to have an access to the stepAIC() method provided by MASS

    dots <- rlang::list2(...)

    if(is.null(family)) {

      model_call <- rlang::call2(mod_fun,
                                 formula = formula,
                                 data = data,
                                 weights = if(is.null(weight_variable)) NULL else data[[weight_variable]],
                                 !!!dots)

    } else {

      model_call <- rlang::call2(mod_fun,
                                 formula = formula,
                                 family = family,
                                 data = data,
                                 weights = if(is.null(weight_variable)) NULL else data[[weight_variable]],
                                 !!!dots)

    }

    if(error_resistant) {

      model <- try(eval(model_call, envir = rlang::caller_env()),
                   silent = TRUE)

      if(any(class(model) == 'try-error')) {

        warning(paste('Model construction failed with the following exception:',
                      model[[1]]),
                call. = FALSE)

        return(NULL)

      }

    } else {

      model <- eval(model_call, envir = rlang::caller_env())

    }

    ## output

    structure(list(response = response,
                   indep_variable = indep_variable,
                   confounder = confounder,
                   weight_variable = weight_variable,
                   model_type = class(model)[1],
                   family = if(class(model)[1] == 'lm') 'gaussian' else family,
                   model = model),
              class = 'lm_analysis')

  }
