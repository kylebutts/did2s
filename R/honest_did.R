#' @title get_honestdid_obj_did2s
#'
#' @description a helper function that takes a fixest feols object (likely
#'   from `did2s`) that plugs into `honest_did`. Note this function assumes
#'   the event study coefficients are using `i()` syntax, e.g. `i(rel_year)`.
#'   This should also work for a TWFE event-study model estimated by `feols`.
#'
#' @param est A `fixest` object, likely from `did2s`.
#' @param coef_name Character. The name of the event-study relative-year
#'   variable name, from `i(rel_year)`.
#' @return A list containing the vector of event-study coefficients `beta`, the
#'   variance-covariance matrix of `beta`, `V`, and a vector of relative years,
#'   `event_time`.
#' @export
get_honestdid_obj_did2s <- function(est, coef_name = "rel_year") {
  beta <- stats::coef(est, keep = coef_name)
  V <- stats::vcov(est, keep = coef_name)
  event_time <- as.numeric(gsub(paste0(coef_name, "::"), "", names(beta)))

  res <- list(
    beta = beta,
    V = V,
    event_time = event_time
  )

  return(res)
}

#' @title honest_did_did2s
#'
#' @description a function to compute a sensitivity analysis using the
#'   approach of Rambachan and Roth (2021) when the event study is
#'   estimated using the `did2s` package. Note that you should first
#'   use the helper function `get_honestdid_obj_did2s` to create the
#'   object, `obj`, that you will then pass into this function with
#'   `honest_did(obj)`
#'
#' @param es an object of class `honestdid_obj_did2s` from the function `get_honestdid_obj_did2s`
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDiD::createSensitivityResults_relativeMagnitudes
#' @param ... Ignored.
#'
#' @export
honest_did_did2s <- function(
  es,
  e = 0,
  type = c("smoothness", "relative_magnitude"),
  method = NULL,
  bound = "deviation from parallel trends",
  Mvec = NULL,
  Mbarvec = NULL,
  monotonicityDirection = NULL,
  biasDirection = NULL,
  alpha = 0.05,
  parallel = FALSE,
  gridPoints = 10^3,
  grid.ub = NA,
  grid.lb = NA,
  ...
) {
  # Function body
  type <- type[1]

  beta <- es$beta
  V <- es$V
  event_time <- es$event_time

  referencePeriodIndex <- which(event_time == -1)

  # Remove reference period
  beta <- beta[-referencePeriodIndex]
  V <- V[-referencePeriodIndex, -referencePeriodIndex]

  nperiods <- nrow(V)
  npre <- sum(1 * (event_time < -1))
  npost <- nperiods - npre

  baseVec1 <- HonestDiD::basisVector(index = (e + 1), size = npost)

  orig_ci <- HonestDiD::constructOriginalCS(
    betahat = beta,
    sigma = V,
    numPrePeriods = npre,
    numPostPeriods = npost,
    l_vec = baseVec1
  )

  if (type == "relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = beta,
      sigma = V,
      numPrePeriods = npre,
      numPostPeriods = npost,
      bound = bound,
      method = method,
      l_vec = baseVec1,
      Mbarvec = Mbarvec,
      monotonicityDirection = monotonicityDirection,
      biasDirection = biasDirection,
      alpha = alpha,
      gridPoints = 100,
      parallel = parallel
    )
  } else if (type == "smoothness") {
    robust_ci <- HonestDiD::createSensitivityResults(
      betahat = beta,
      sigma = V,
      numPrePeriods = npre,
      numPostPeriods = npost,
      method = method,
      l_vec = baseVec1,
      monotonicityDirection = monotonicityDirection,
      biasDirection = biasDirection,
      alpha = alpha,
      parallel = parallel
    )
  }

  return(
    list(robust_ci = robust_ci, orig_ci = orig_ci, type = type)
  )
}
