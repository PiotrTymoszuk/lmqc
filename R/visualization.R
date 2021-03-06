# This script provides tools for visualization of the modeling results
# such as forest plots and plots of explained variance

# Generics -----

#' The generic function used for drawing of Forest plots.
#'
#' @description Draws a Forest plot given a data frame or lm_analysis object.
#' @param x object providing data for drawing such as estimates, confidence intervals and variable names.
#' @param ... extra arguments passed to specific methods.
#' @export

  plot_forest <- function(x, ...) {

    UseMethod('plot_forest', x)

  }

# Forest plot methods ----

#' Display modeling results in a Forest plot.
#'
#' @description Generates a customized Forest plot with the model estimates and 95\% confidence intervals (CI).
#' @details Designed to work optimally with the output of the \code{\link{summary.lm_analysis}} method applied
#' to the objects of 'lm_analysis' class generated e.g. by \code{\link{make_lm}}.
#' @param x data frame.
#' @param variable name of the variable storing explanatory feature labels.
#' @param level name of the variable with storing the labels of levels for categorical explanatory features.
#' @param confounder name of a text variable (coded yes/no) indicating if the explanatory feature has to be treated as a confounder
#' and optionally skipped from the plot.
#' @param n name of the variable storing the n numbers for the levels of categorical explanatory features.
#' @param n_complete name of the variable storing the number of complete observations.
#' @param estimate name of the variable with the estimate values.
#' @param lower_ci name of the variable with the lower CI values.
#' @param upper_ci name of the variable with the upper CI values.
#' @param p_value name of the variable storing p values.
#' @param plot_title text displayed as a plot title.
#' @param plot_subtitle text displayed as a plot subtitle.
#' @param plot_tag text displayed as a plot tag.
#' @param x_lab text displayed as a X axis title.
#' @param point_size size of the points (coding for estimate values) in the Forest plot.
#' @param point_shape shape of the points (coding for estimate values) in the Forest plot.
#' @param label_estimate logical, should estimate value with 95\% CI be displayed in the plot?
#' @param estimate_size size of the estimate label.
#' @param estimate_hjust horizontal justification of the estimate label.
#' @param estimate_vjust vertical justification of the estimate label.
#' @param hide_confounder logical, should confounder features be displayed in the plot?
#' @param baseline_label text to be displayed in the Y axis for the baseline/model intercept.
#' @param signif_digits significant digits, used for rounding of the estimate and 95\% CI values.
#' @param x_text_separator separator of the Y axis text labels.
#' @param cutpoint regulation cutoff displayed in the plots as a dashed vertical line.
#' @param x_axis_trans transformation of the X axis, as specified for \code{\link[ggplot2]{scale_x_continuous}}.
#' @param cust_theme custom ggplot2 theme.
#' @return a Forest plot as a ggplot object.
#' @export plot_forest.default
#' @export

  plot_forest.default <- function(x,
                                  variable = 'variable',
                                  parameter = 'parameter',
                                  level = 'level',
                                  confounder = 'confounder',
                                  n = 'n',
                                  n_complete = 'n_complete',
                                  estimate = 'estimate',
                                  lower_ci = 'lower_ci',
                                  upper_ci = 'upper_ci',
                                  p_value = 'p_value',
                                  plot_title = NULL,
                                  plot_subtitle = NULL,
                                  plot_tag = NULL,
                                  x_lab = expression(beta*', 95% CI'),
                                  point_size = 2,
                                  point_shape = 16,
                                  label_estimate = TRUE,
                                  estimate_size = 2.75,
                                  estimate_hjust = 0.2,
                                  estimate_vjust = -0.7,
                                  hide_confounder = TRUE,
                                  hide_baseline = FALSE,
                                  baseline_label = 'Baseline',
                                  signif_digits = 2,
                                  x_text_separator = '\n',
                                  cutpoint = 0,
                                  x_axis_trans = 'identity',
                                  cust_theme = ggplot2::theme_classic()) {

    ### user entry control

    if(!any(class(x) == 'data.frame')) stop('Please provide a data frame as data.', call. = FALSE)
    if(any(!c(variable, level, confounder, n, n_complete, estimate, lower_ci, upper_ci, p_value) %in% names(x))) {

      stop('Not all variables forun in data', call. = FALSE)

    }

    stopifnot(is.logical(label_estimate))
    stopifnot(is.logical(hide_confounder))
    stopifnot(is.logical(hide_baseline))
    stopifnot(is.numeric(signif_digits))

    if(!any(class(cust_theme) == 'theme')) stop('Please provide a valid ggplot2 theme object', call. = FALSE)

    signif_digits <- as.integer(signif_digits)

    ## plotting data

    x <- x[c(variable,
             parameter,
             level,
             confounder,
             n,
             n_complete,
             estimate,
             lower_ci,
             upper_ci,
             p_value)]

    if(hide_confounder) x <- dplyr::filter(x, .data[[confounder]] == 'no')

    if(hide_baseline) x <- dplyr::filter(x, .data[[level]] != 'baseline' | is.na(.data[[level]]))

    x <- dplyr::mutate(x,
                       y_ax = ifelse(is.na(.data[[level]]),
                                     paste0(.data[[variable]], x_text_separator, 'n = ', .data[[n_complete]]),
                                     ifelse(.data[[level]] == 'baseline',
                                            ifelse(stringi::stri_detect(parameter, fixed = '|'),
                                                   paste(baseline_label, parameter, sep = ': '),
                                                   baseline_label),
                                            paste0(.data[[variable]], ': ', .data[[level]],
                                                   x_text_separator, 'n = ', .data[[n]]))),
                       estimate_lab = paste0(signif(.data[[estimate]], signif_digits),
                                             ' [', signif(.data[[lower_ci]], signif_digits),
                                             ' - ', signif(.data[[upper_ci]], signif_digits), ']'),
                       regulation = ifelse(.data[[p_value]] >= 0.05,
                                           'ns',
                                           ifelse(.data[[estimate]] < cutpoint, 'negative', 'positive')),
                       regulation = factor(regulation, c('negative', 'ns', 'positive')))

    ## for a nice plotting order, the baseline is placed on the top

    x <- dplyr::mutate(x,
                       plot_order = ifelse(level == 'baseline', 0, 100))

    x <- dplyr::arrange(x,
                        plot_order,
                        variable,
                        level)

    x <- dplyr::mutate(x,
                       plot_order = 1:nrow(x))

    ## forest plot

    forest <- ggplot2::ggplot(x,
                              ggplot2::aes(x = .data[[estimate]],
                                           y = stats::reorder(.data[['y_ax']], -.data[['plot_order']]),
                                           color = .data[['regulation']])) +
      ggplot2::geom_vline(xintercept = cutpoint,
                          linetype = 'dashed') +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data[[lower_ci]],
                                           xmax = .data[[upper_ci]]),
                              height = 0) +
      ggplot2::geom_point(size = point_size,
                          shape = point_shape) +
      ggplot2::scale_color_manual(values = c('negative' = 'steelblue',
                                             'ns' = 'gray60',
                                             'positive' = 'coral3')) +
      ggplot2::scale_x_continuous(trans = x_axis_trans) +
      cust_theme +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    x = x_lab)

    if(label_estimate) {

      forest <- forest +
        ggplot2::geom_text(ggplot2::aes(label = .data[['estimate_lab']]),
                           size = estimate_size,
                           hjust = estimate_hjust,
                           vjust = estimate_vjust)

    }

    return(forest)

  }

#' Display modeling results of a lm_anaylsis object in a Forest plot.
#'
#' @description Generates a customized Forest plot with the model estimates and 95\% confidence intervals (CI).
#' @details A method specific method for lm_analysis object (constructed e.g. by \code{\link{make_lm}}).
#' Generates a ggplot with estimate and CI values as described for \code{\link{plot_forest.default}}
#' and presents extra model statistics in the sub-title or tag.
#' @param x an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param transf_fun function used for transformation of the estimates and confidence intervals, identity() by default.
#' @param ci_method ci_method specifies how the confidence intervals should be calculated, see \code{\link{get_estimates}}
#' for details.
#' @param plot_title text to be displayed in the plot title.
#' @param stats_position position, where the fit stats are displayed, subtitle by default.
#' @param show_stats fit stats to be presented in the plot, see \code{\link{summary.lm_analysis}} for details.
#' @param cust_theme cust_theme custom ggplot2 theme.
#' @param signif_digits significant digits, used for rounding of the estimate, 95\% CI values and fit stats.
#' @param ... extra arguments passed to \code{\link{plot_forest.default}}.
#' @return a Forest plot as a ggplot object.
#' @export plot_forest.lm_analysis
#' @export

  plot_forest.lm_analysis <- function(x,
                                      transf_fun = identity,
                                      ci_method = c('default', 'distribution', 'normal'),
                                      plot_title = NULL,
                                      stats_position = c('subtitle', 'tag', 'none'),
                                      show_stats = c('n_complete', 'adj_rsq', 'aic', 'mae'),
                                      cust_theme = ggplot2::theme_classic(),
                                      signif_digits = 2, ...) {

    stopifnot(is_lm_analysis(x))

    stats_position <- match.arg(stats_position[1], c('subtitle', 'tag', 'none'))

    ci_method <- match.arg(ci_method[1], c('default', 'distribution', 'normal'))

    plotting_tbl <- summary(x,
                            type = 'inference',
                            ci_method = ci_method,
                            transf_fun = transf_fun)

    fit_stats <- summary(x, 'fit')

    if(any(!show_stats %in% names(fit_stats))) stop('Please specify the fit stats as in the output of summary.lm_analysis()',
                                                    call. = FALSE)

    stats_lab <- c(n_complete = paste('n =', fit_stats$n_complete),
                   aic = paste('AIC =', signif(fit_stats$aic, signif_digits)),
                   bic = paste('BIC =', signif(fit_stats$bic, signif_digits)),
                   raw_rsq = paste('R\u00B2 =', signif(fit_stats$raw_rsq, signif_digits)),
                   adj_rsq = paste('adj. R\u00B2 =', signif(fit_stats$adj_rsq, signif_digits)),
                   deviance = paste('Dev. =', signif(fit_stats$deviance, signif_digits)),
                   mae = paste('MAE =', signif(fit_stats$mae, signif_digits)),
                   rmse = paste('RMSE =', signif(fit_stats$rmse, signif_digits)))

    stats_lab <- paste(stats_lab[show_stats],
                       collapse = ', ')

    forest <- lmqc::plot_forest.default(x = plotting_tbl,
                                        variable = 'variable',
                                        parameter = 'parameter',
                                        level = 'level',
                                        confounder = 'confounder',
                                        n = 'n',
                                        n_complete = 'n_complete',
                                        estimate = 'estimate',
                                        lower_ci = 'lower_ci',
                                        upper_ci = 'upper_ci',
                                        p_value = 'p_value',
                                        plot_title = plot_title,
                                        signif_digits = signif_digits,
                                        cust_theme = cust_theme, ...)

    switch(stats_position,
           none = forest,
           subtitle = forest + ggplot2::labs(subtitle = stats_lab),
           tag = forest + ggplot2::labs(tag = stats_lab))

  }

# Variable importance scree and bar plots -----

#' Plot percentage of variance/deviance for lm_analysis objects.
#'
#' @description Presents percent data set variance/deviance explained by the model components
#' as a bar or scree plot.
#' @details Percentage of variance/deviance is calulated as described for \code{\link{anova.lm_analysis}}.
#' @param lm_analysis_object an object of class 'lm_analysis' created e.g. by \code{\link{make_lm}}.
#' @param plot_form form of the plot, defaults to bar.
#' @param plot_title text presented in the plot title.
#' @param plot_subtitle text presented in the plot subtitle.
#' @param plot_tag text presented in the plot tag.
#' @param cust_theme custom ggplot2 theme.
#' @param bar_color bar fill color, ignored for the scree plot option.
#' @param line_color line color, ignored for the bar plot option.
#' @param line_size line size, ignored for the bar plot option.
#' @param show_points logical, should point be displayed in the scree plot? ignored for the bar plot option.
#' @param point_color point color, ignored it show_points is FALSE, ignored for the bar plot option.
#' @param point_size point size, ignored it show_points is FALSE, ignored for the bar plot option.
#' @param residual_color color of the residual bar/point.
#' @param show_p logical, should ANOVA p values be displayed in the plot?
#' @param p_size size of the p value text, ignored if show_p is FALSE.
#' @param p_hjust horizontal justification of the p value text, ignored if show_p is FALSE.
#' @param p_vjust vertical justification of the p value text, ignored if show_p is FALSE.
#' @param signif_digits significant digits, used for rounding of the p value.
#' @param ... additional arguments passed to ANOVA calculating functions, see: \code{\link{anova.lm_analysis}}.
#' @return a bar or scree ggplot graphic.
#' @export

  plot_importance <- function(lm_analysis_object,
                              plot_form = c('bar', 'scree'),
                              plot_title = NULL,
                              plot_subtitle = NULL,
                              plot_tag = NULL,
                              cust_theme = ggplot2::theme_classic(),
                              bar_color = 'steelblue',
                              line_color = 'gray70',
                              line_size = 0.5,
                              show_points = TRUE,
                              point_color = 'steelblue',
                              point_size = 2,
                              residual_color = 'gray60',
                              show_p = TRUE,
                              p_size = 2.75,
                              p_hjust = -0.2,
                              p_vjust = 0,
                              signif_digits = 2, ...) {

    ## user entry control

    if(!is_lm_analysis(lm_analysis_object)) stop('Please provide a valid lm_analysis object.', call. = FALSE)
    if(!any(class(cust_theme) == 'theme')) stop('Please provide a valid ggplot2 theme.', call. = FALSE)

    stopifnot(all(is.numeric(c(line_size, point_size, p_size, p_hjust, p_vjust, signif_digits))))

    stopifnot(is.logical(show_points))
    stopifnot(is.logical(show_p))

    signif_digits <- as.integer(signif_digits)

    plot_form <- match.arg(plot_form[1], c('bar', 'scree'))

    ## plotting table

    plotting_tbl <- anova(lm_analysis_object, ...)

    if(!'frac_explained' %in% names(plotting_tbl)) {

      warning('No fraction of explained variance/deviance could be extracted from the lm_anaylsis object.', call. = FALSE)

      return(NULL)

    }

    if(!'variable' %in% names(plotting_tbl)) {

      warning('No fraction of explained variance/deviance could be extracted from the lm_anaylsis object.', call. = FALSE)

      return(NULL)

    }

    plotting_tbl <- dplyr::filter(plotting_tbl, variable != 'NULL')

    plotting_tbl <- dplyr::mutate(plotting_tbl,
                                  plot_order = ifelse(variable == 'Residuals', 1, 2),
                                  var_type = ifelse(variable == 'Residuals', 'null', 'model'))

    plotting_tbl <- dplyr::arrange(plotting_tbl, plot_order, frac_explained)

    plotting_tbl <- dplyr::mutate(plotting_tbl, plot_order = 1:nrow(plotting_tbl))

    ## variance plot

    if(plot_form == 'bar') {

      imp_plot <- ggplot2::ggplot(plotting_tbl,
                                  ggplot2::aes(x = .data[['frac_explained']],
                                               y = stats::reorder(.data[['variable']], .data[['plot_order']]),
                                               fill = .data[['var_type']])) +
        ggplot2::geom_bar(stat = 'identity',
                          color = 'black') +
        ggplot2::scale_fill_manual(values = c('model' = bar_color,
                                              'null' = residual_color)) +
        ggplot2::guides(fill = FALSE) +
        cust_theme +
        ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::labs(title = plot_title,
                      subtitle = plot_subtitle,
                      tag = plot_tag,
                      x = if('Deviance' %in% names(plotting_tbl)) '% deviance' else '% variance')

    } else {

      plotting_tbl <- dplyr::mutate(plotting_tbl,
                                    group_var = 'group')

      imp_plot <- ggplot2::ggplot(plotting_tbl,
                                  ggplot2::aes(y = .data[['frac_explained']],
                                               x = stats::reorder(.data[['variable']], -.data[['plot_order']]),
                                               group = .data[['group_var']])) +
        ggplot2::geom_line(color = line_color,
                           size = line_size)

      if(show_points) {

        imp_plot <- imp_plot +
          ggplot2::geom_point(size = point_size,
                              ggplot2::aes(color = .data[['var_type']]))

      }

      imp_plot <- imp_plot +
        ggplot2::scale_color_manual(values = c('model' = bar_color,
                                               'null' = residual_color)) +
        ggplot2::guides(color = FALSE) +
        cust_theme +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::labs(title = plot_title,
                      subtitle = plot_subtitle,
                      tag = plot_tag,
                      y = if('Deviance' %in% names(plotting_tbl)) '% deviance' else '% variance')

    }

    ## p value display option

    p_variable <- names(plotting_tbl)[stringi::stri_detect(names(plotting_tbl), fixed = 'Pr')]

    if(show_p & length(p_variable) > 0) {

      imp_plot$data <- dplyr::mutate(imp_plot$data,
                                     p_label = ifelse(.data[[p_variable[1]]] < 0.05,
                                                      paste('p =', signif(.data[[p_variable[1]]], signif_digits)),
                                                      paste0('ns (p = ', signif(.data[[p_variable[1]]], signif_digits), ')')))

      imp_plot <- imp_plot +
        ggplot2::geom_text(ggplot2::aes(label = .data[['p_label']]),
                           size = p_size,
                           hjust = p_hjust,
                           vjust = p_vjust)

    }

    return(imp_plot)

  }
