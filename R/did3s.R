#' (WIP!) Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @import fixest
#'
#' @param data The dataframe containing all the variables
#' @param yname Variable name for outcome variable
#' @param gname Variable name for group variable
#' @param tname Variable name for treatment variable
#' @param treatment A variable that = 1 if treated, = 0 otherwise
#' @param time_invariant vector of variable names of time-invariant covariates.
#'   For factor variables, write "i.varname".
#' @param time_varying vector of variable names of time-varying covariates.
#' @param third_stage Second stage, these should be the treatment indicator(s)
#'   (e.g. treatment variable or event-study leads/lags).
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Use `i()` for factor variables, see \code{\link[fixest:i]{fixest::i}}.
#' @param cluster_var What variable to cluster standard errors. This can be IDs
#'   or a higher aggregate level (state for example)
#' @param weights Optional. Variable name for regression weights.
#' @param bootstrap Optional. Should standard errors be calculated using bootstrap?
#'   Default is `FALSE`.
#' @param n_bootstraps Optional. How many bootstraps to run.
#'   Default is `250`.
#' @param return_bootstrap Optional. Logical. Will return each bootstrap second-stage
#'   estimate to allow for manual use, e.g. percentile standard errors and empirical
#'   confidence intervals.
#' @param verbose Optional. Logical. Should information about the two-stage
#'   procedure be printed back to the user?
#'   Default is `TRUE`.
#'
#' @return `fixest` object with adjusted standard errors
#'   (either by formula or by bootstrap). All the methods from `fixest` package
#'   will work, including \code{\link[fixest:esttable]{fixest::esttable}} and
#'   \code{\link[fixest:coefplot]{fixest::coefplot}}
#'
#' @section Examples:
#'
#' Load example dataset which has two treatment groups and homogeneous treatment effects
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Load Example Dataset
#' data("df_hom")
#' ```
#'
#' ### Static TWFE
#'
#' You can run a static TWFE fixed effect model for a simple treatment indicator
#' ```{r, comment = "#>", collapse = TRUE}
#' static <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = ~ 0 | unit + year,
#'     second_stage = ~ i(treat, ref=FALSE))
#'
#' fixest::esttable(static)
#' ```
#'
#' ### Event Study
#'
#' Or you can use relative-treatment indicators to estimate an event study estimate
#' ```{r, comment = "#>", collapse = TRUE}
#' es <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = ~ 0 | unit + year,
#'     second_stage = ~ i(rel_year, ref=c(-1, Inf)))
#'
#' fixest::esttable(es)
#' ```
#'
#' ```{r, eval = F}
#' # plot rel_year coefficients and standard errors
#' fixest::coefplot(es, keep = "rel_year::(.*)")
#' ```
#'
#' ### Example from Cheng and Hoekstra (2013)
#'
#' Here's an example using data from Cheng and Hoekstra (2013)
#' ```{r, comment = "#>", collapse = TRUE}
#' # Castle Data
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did2s(
#' 	data = castle,
#' 	yname = "l_homicide",
#' 	first_stage = ~ 0 | sid + year,
#' 	second_stage = ~ i(post, ref=0),
#' 	treatment = "post",
#' 	cluster_var = "state", weights = "popwt"
#' )
#' ```
#'
#' @export
did3s <- function(data, yname, gname, tname, time_varying,
				  third_stage, treatment, cluster_var,
				  time_invariant = c("1"), weights = NULL,
				  n_bootstraps = 250,
				  return_bootstrap = FALSE, verbose = TRUE) {

	bootstrap = TRUE

	# Check Parameters ---------------------------------------------------------

	if(!inherits(data, "data.frame")) stop("`did3s` requires a data.frame like object for analysis.")

	# Extract vars from formula
	if(inherits(third_stage, "formula")) third_stage = as.character(third_stage)[[2]]

	# Check that treatment is a 0/1 or T/F variable
	if(!all(
		unique(data[[treatment]]) %in% c(1, 0, T, F)
	)) {
		stop(sprintf(
			"'%s' must be a 0/1 or T/F variable indicating which observations are untreated/not-yet-treated.",
			treatment
		))
	}

	# Point Estimates ----------------------------------------------------------

	est = did3s_estimate(
		data = data,
		yname = yname,
		gname = gname,
		tname = tname,
		time_invariant = time_invariant,
		time_varying = time_varying,
		third_stage = third_stage,
		treatment = treatment,
		weights = weights,
		bootstrap = bootstrap
	)

	# Bootstrap Standard Errors ------------------------------------------------

	cli::cli_alert("Starting {n_bootstraps} bootstraps at cluster level: {cluster_var}")

	# Unique values of cluster variable
	cl = unique(data[[cluster_var]])

	stat <- function(x, i) {
		# select the observations to subset based on the cluster var
		block_obs = unlist(lapply(i, function(n) which(x[n] == data[[cluster_var]])))
		# run regression for given replicate, return estimated coefficients
		stats::coefficients(
			did3s_estimate(
				data = data[block_obs,],
				yname = yname,
				gname = gname,
				tname = tname,
				time_invariant = time_invariant,
				time_varying = time_varying,
				third_stage = third_stage,
				treatment = treatment,
				weights = weights,
				bootstrap = TRUE
			)$third_stage
		)
	}

	boot = boot::boot(cl, stat, n_bootstraps)

	# Get estimates and fix names
	estimates = boot$t
	colnames(estimates) = names(stats::coef(est$third_stage))

	# Bootstrap Var-Cov Matrix
	cov = stats::cov(estimates)

	if(return_bootstrap) {
		return(estimates)
	}

	# summary creates fixest object with correct standard errors and vcov

	# Once fixest updates on CRAN
	# rescale cov by G/(G-1) and use t(G-1) distribution
	# G = length(cl)
	# cov = cov * G/(G-1)

	return(base::suppressWarnings(
		# summary(
		#   est$second_stage,
		#   .vcov = list("Two-stage Adjusted" = cov),
		#   ssc = ssc(adj = FALSE, t.df = G-1)
		# )
		summary(est$third_stage, .vcov = cov)
	))
}


# Point estimate for did2s
did3s_estimate = function(data, yname, gname, tname, time_invariant, time_varying, third_stage, treatment,
						  weights = NULL, bootstrap = FALSE) {

	## We'll use fixest's formula expansion macros to swap out first and second
	## stages (see: ?fixest::xpd)
	fixest::setFixest_fml(..third_stage = third_stage)


	untreat = data[data[[treatment]]==0, ]
	if(is.null(weights)) {
		weights_vector = NULL
	} else {
		weights_vector = untreat[[weights]]
	}

# ------------------------------------------------------------------------------
# Stage 1: Regress X_{it}(0) on FEs and time-invariant
#          and estimate X_{it}(0) for treated
# ------------------------------------------------------------------------------

	# interact time-invariant with time coefficients
	time_inv_fml = "1 "
	for(var in time_invariant) {
		if(var == "1" | var == "0") {

		} else {
			time_inv_fml = paste0(time_inv_fml, " + ", "i(", tname, ", ", var, ")")
		}
	}

	if(length(time_varying) > 1) {
		time_var_fml = paste0("c(", paste(time_varying, collapse=", "), ")")
	} else {
		time_var_fml = time_varying
	}

	first_stage_formula = as.formula(paste0(
		time_var_fml,
		"~ ",
		time_inv_fml,
		" | ", gname, " + ", tname
	))


	first_stage = fixest::feols(first_stage_formula,
				  data = untreat,
				  weights = weights_vector,
				  warn = FALSE,
				  notes = FALSE
				  )

	if(inherits(first_stage, "fixest_multi")) {
		for(est in as.list(first_stage)) {
			varname = all.vars(est$fml)[attr(terms(est$fml), "response")]
			data[[varname]] = data[[varname]] - predict(est, newdata = data)
		}
	} else {
		varname = all.vars(est$fml)[attr(terms(est$fml), "response")]
		data[[varname]] = data[[varname]] - predict(est, newdata = data)
	}

# ------------------------------------------------------------------------------
# Stage 2: Regress y on FEs, time-invariant, and X_{it}(0)
#          and residualize y for treated observations
# ------------------------------------------------------------------------------

	time_var_fml = " "
	for(var in time_varying) {
		time_var_fml = paste0(time_var_fml, " + ", "i(", tname, ", ", var, ")")
	}

	second_stage_formula = as.formula(paste0(
		yname,
		"~ ",
		time_var_fml, " + ",
		time_inv_fml,
		" | ", gname, " + ", tname
	))


	second_stage = fixest::feols(second_stage_formula,
								data = untreat,
								weights = weights_vector,
								warn=FALSE,
								notes=FALSE)

	# Residualize outcome variable but keep same yname
	second_u = data[[yname]] - stats::predict(second_stage, newdata = data)
	data[[yname]] = second_u

	# Zero out residual rows with D_it = 1 (for analytical SEs later on)
	if (!bootstrap)	second_u[data[[treatment]] == 1] = 0


# ------------------------------------------------------------------------------
# Stage 3: Regress tilde{y} on third_stage
# ------------------------------------------------------------------------------

	if(!is.null(weights)) weights_vector = data[[weights]]

	third_stage = fixest::feols(fixest::xpd(~ 0 + ..third_stage, lhs = yname),
								 data = data,
								 weights = weights_vector,
								 warn=FALSE,
								 notes=FALSE)


	ret = list(third_stage = third_stage)

	return(ret)
}
