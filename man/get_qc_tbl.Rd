% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_funs.R
\name{get_qc_tbl}
\alias{get_qc_tbl}
\title{Extended residuals of a linear model}
\usage{
get_qc_tbl(
  linear_model,
  type.predict = c("link", "response", "terms"),
  type.residuals = c("pearson", "deviance"),
  ...
)
}
\arguments{
\item{linear_model}{lm, glm, polr or lm_analysis class model.}

\item{type.predict}{prediction type, as specified for \code{\link[broom]{augment}}, defaults to 'link'.}

\item{type.residuals}{type of the calculated residuals, as specified for \code{\link[broom]{augment}}, defaults to 'pearson'.}

\item{...}{extra arguments passed to \code{\link[broom]{augment}}.}
}
\value{
a data frame with the fitted values, true outcome, residuals and candidate outliers.
}
\description{
Extracts extended residuals of a linear model and potential outliers,
}
\details{
a wrapper around \code{\link[broom]{augment}} with some extra output including confidence intervals
of the fit, squared residuals and expected normal distribution for the standardized residuals and the true outcome.
Potential outliers are identified by the two-SE criterion.
}
