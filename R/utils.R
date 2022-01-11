# This script contains auxiliary functions used internally and not intended for export

#' Generate a customized point plot.
#'
#' @description draws a simple point plot for model diagnostic purposes.
#' @details draws a simple point plot for diagnostic purposes. As per design, takes the output
#' of get_qc_tbl() as data argument, color-codes model potential outliers.
#' @param data data frame.
#' @param x_var name of the variable to be shown in the x axis.
#' @param y_var name of the variable to be shown in the y axis.
#' @param x_lab x axis title.
#' @param y_lab y axis title.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param smooth logical, should a trend line be displayed.
#' @param silent logical, display warnings?
#' @param ... extra arguments passed to geom_smooth().
#' @return a ggplot graphic

  point_plot_ <- function(data, x_var, y_var,
                          x_lab = x_var, y_lab = y_var,
                          plot_title = NULL, plot_subtitle = NULL, plot_tag = NULL,
                          smooth = TRUE, silent = TRUE, ...) {

    ## table for plotting

    if('.rownames' %in% names(data)) {

      data <- dplyr::mutate(data, misslab = ifelse(.candidate_missfit == 'yes',
                                                   .rownames,
                                                   NA))

    } else {

      data <- dplyr::mutate(data, misslab = ifelse(.candidate_missfit == 'yes',
                                                   .observation,
                                                   NA))

    }

    ## fill colors

    fill_colors <- c(no = 'cornflowerblue',
                     yes = 'firebrick4')

    ## point plot

    point_plot <- ggplot2::ggplot(data,
                                  ggplot2::aes(x = .data[[x_var]],
                                               y = .data[[y_var]],
                                               fill = .candidate_missfit)) +
      ggplot2::geom_point(size = 2,
                          shape = 21) +
      ggrepel::geom_text_repel(ggplot2::aes(label = misslab),
                               show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = fill_colors,
                                 name = 'Candidate outlier') +
      ggplot2::labs(x = x_lab,
                    y = y_lab,
                    title = plot_title)

    if(smooth) {

      if(silent) {

        suppressWarnings(point_plot <- point_plot +
                           ggplot2::geom_smooth(show.legend = FALSE,
                                                color = 'black',
                                                fill = 'dodgerblue2', ...))

      } else {

        point_plot <- point_plot +
          ggplot2::geom_smooth(show.legend = FALSE,
                               color = 'black',
                               fill = 'dodgerblue2', ...)

      }

    }

    return(point_plot)

  }


#' Calculate expected normal values for the given variable.
#'
#' @param data data frame.
#' @param observed name of the variable of interest
#' @details credits to: https://stackoverflow.com/questions/43217104/coloring-points-in-a-geom-qq-plot
#' @return A data frame with the extra variable .expect.norm with the expected normal distribution values

  calc_expected_ <- function(data, observed) {

    dplyr::mutate(data[order(data[[observed]]), ],
                  .expect.norm = stats::qnorm(stats::ppoints(nrow(data))))

  }

#' Count levels of a variable.
#'
#' @description Count factor variables or returns the number of complete cases for numeric features.
#' @param data data frame.
#' @param variable name of the variable of interest
#' @return a data frame with counts

  count_ <- function(data, variable) {

    data <- dplyr::filter(data, !is.na(.data[[variable]]))

    if(is.numeric(data[[variable]])) {

      tibble::tibble(variable = variable,
                     level = NA_character_,
                     n = nrow(data))

    } else {

      count_tbl <- dplyr::count(data, .data[[variable]])

      count_tbl <- dplyr::mutate(count_tbl,
                                 level = .data[[variable]],
                                 variable = variable)

      count_tbl[c('variable',
                  'level',
                  'n')]

    }

  }

#' Bind two data frames differing in column names by rows.
#'
#' @description binds two data frames with different sets of variables.
#' The non-common ones are filled with NAs.
#' @param tbl1 data frame.
#' @param tbl2 data frame.
#' @return a data frame bound by rows.
#' @export

  outer_rbind <- function(tbl1, tbl2) {

    ## missing variables

    miss1 <- names(tbl2)[!names(tbl2) %in% names(tbl1)]
    miss2 <- names(tbl1)[!names(tbl1) %in% names(tbl2)]

    ## filling the tables

    for(i in miss1){

      tbl1 <- dplyr::mutate(tbl1, !!rlang::sym(i) := NA)

    }

    for(i in miss2){

      tbl2 <- dplyr::mutate(tbl2, !!rlang::sym(i) := NA)

    }

    rbind(tbl1, tbl2)

  }
