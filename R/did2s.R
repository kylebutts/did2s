#' Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage Fixed effects and other covariates you want to residualize with in first stage, use i() for fixed effects., following fixest::feols.
#' @param second_stage Second stage, these should be the treatment indicator(s) (e.g. treatment variable or es leads/lags), use i() for factor variables, following fixest::feols.
#' @param treatment A variable that = 1 if treated, = 0 otherwise
#' @param cluster_var What variable to cluster standard errors. This can be IDs or a higher aggregate level (state for example)
#' @param weights Optional variable to run a weighted first- and second-stage regressions
#' @param bootstrap Should standard errors be calculated using bootstrap? Default is FALSE.
#' @param n_bootstraps How many bootstraps to run. Default is 250
#' @param verbose Logical. Should information about the two-stage procedure be
#'   printed back to the user? Default is TRUE.
#'
#' @return fixest::feols point estimate with adjusted standard errors (either by formula or by bootstrap)
#' @export
#' @section Examples:
#'
#'
#' Load example dataset which has two treatment groups and homogeneous treatment effects
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Load Example Dataset
#' data("df_hom")
#' ```
#'
#' You can run a static TWFE fixed effect model for a simple treatment indicator
#' ```{r, comment = "#>", collapse = TRUE}
#' static <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = "i(state) + i(year)",
#'     second_stage = "i(treat, ref=FALSE)")
#'
#' fixest::esttable(static)
#' ```
#'
#' Or you can use relative-treatment indicators to estimate an event study estimate
#' ```{r, comment = "#>", collapse = TRUE}
#' es <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = "i(state) + i(year)",
#'     second_stage = "i(rel_year, ref=c(-1, Inf))")
#'
#' fixest::esttable(es)
#' ```
#'
#' Here's an example using data from Castle (2013)
#' ```{r, comment = "#>", collapse = TRUE}
#' # Castle Data
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did2s(
#' 	data = castle,
#' 	yname = "l_homicide",
#' 	first_stage = ~ i(sid) + i(year),
#' 	second_stage = ~ i(post, ref=0),
#' 	treatment = "post",
#' 	cluster_var = "state", weights = "popwt"
#' )
#' ```
#'
did2s <- function(data, yname, first_stage, second_stage, treatment, cluster_var,
				  weights = NULL, bootstrap = FALSE, n_bootstraps = 250,
				  verbose = TRUE) {

	# Check Parameters ---------------------------------------------------------

	# Extract vars from formula
	if(inherits(first_stage, "formula")) first_stage = as.character(first_stage)[[2]]
	if(inherits(second_stage, "formula")) second_stage = as.character(second_stage)[[2]]

	# Print --------------------------------------------------------------------
	if(verbose){
    cli::cli_h1("Two-stage Difference-in-Differences")
    cli::cli_alert("Running with first stage formula {.var {paste0('~ ', first_stage)}} and second stage formula {.var {paste0('~ ', second_stage)}}")
    cli::cli_alert("The indicator variable that denotes when treatment is on is {.var {treatment}}")
    if(!bootstrap) cli::cli_alert("Standard errors will be clustered by {.var {cluster_var}}")
    if(bootstrap) cli::cli_alert("Standard errors will be block bootstrapped with cluster {.var {cluster_var}}")
    cli::cat_line()
	}

	# Point Estimates ----------------------------------------------------------

	est = did2s_estimate(
		data = data,
		yname = yname,
		first_stage = first_stage,
		second_stage = second_stage,
		treatment = treatment,
		weights = weights,
		bootstrap = bootstrap
	)

	# Analytic Standard Errors -------------------------------------------------

	if(!bootstrap) {

		# Extract weights
		if(is.null(weights)) {
			weights_vector = rep.int(1L, nrow(data))
		} else {
			weights_vector = sqrt(data[[weights]])
		}

		# Extract first stage
		first_u = est$first_u

		# x1 is matrix used to predict Y(0)
		x1 = sparse_model_matrix(data, est$first_stage)

		# Extract second stage
		second_u = stats::residuals(est$second_stage)
		x2 = sparse_model_matrix(data, est$second_stage)

		# multiply by weights
		first_u = weights_vector * first_u
		x1 = weights_vector * x1
		second_u = weights_vector * second_u
		x2 = weights_vector * x2

		# x10 is matrix used to estimate first stage (zero out rows with D_it = 1)
		x10 = x1
		x10[data[[treatment]] == 1L] = 0

		# Unique values of cluster variable
		cl = unique(data[[cluster_var]])

		# x2'x1 (x10'x10)^-1
		V = make_V(x1, x10, x2)

		meat = lapply(cl, function(c) {
			idx = data[[cluster_var]] == c

			x2g = Matrix::Matrix(x2[idx,], sparse = TRUE)
			x10g = Matrix::Matrix(x10[idx,], sparse = TRUE)
			first_ug = first_u[idx]
			second_ug = second_u[idx]

			# W_g = X_2g' e_2g - (X_2' X_12) (X_11' X_11)^-1 X_11g' e_1g
			# W_g = X_2g' e_2g - V X_11g' e_1g
			W = make_W(x2g, x10g, first_ug, second_ug, V)

			# = W_g W_g'
			return(tcrossprod(W))
		})

		meat_sum = Reduce("+", meat)

		# (X_2'X_2)^-1 (sum W_g W_g') (X_2'X_2)^-1
		cov = make_cov(x2, meat_sum)
	}


	# Bootstrap Standard Errors ------------------------------------------------

	if(bootstrap) {

		cli::cli_alert("Starting {n_bootstraps} bootstraps at cluster level: {cluster_var}")

		samples = rsample::bootstraps(data, times = n_bootstraps, strata = all_of(cluster_var), pool = 0)


		estimates = purrr::map_dfr(samples$splits, function(x) {
			data = rsample:::as.data.frame.rsplit(x)

			estimate = did2s_estimate(
				data = data,
				yname = yname,
				first_stage = first_stage,
				second_stage = second_stage,
				treatment = treatment,
				weights = weights,
				bootstrap = TRUE
			)

			return(coef(estimate$second_stage))
		})

		cov = cov(estimates)

	}

	# summary creates fixest object with correct standard errors and vcov
	return(base::suppressWarnings(summary(est$second_stage, .vcov = cov)))
}


# Point estimate for did2s
did2s_estimate = function(data, yname, first_stage, second_stage, treatment,
						  weights = NULL, bootstrap = FALSE) {
	## We'll use fixest's formula expansion macros to swap out first and second
	## stages (see: ?fixest::xpd)
	fixest::setFixest_fml(..first_stage = first_stage,
						  ..second_stage = second_stage)


	# First stage among untreated
	untreat = data[data[, treatment][[1]]==0, ]
	if(is.null(weights)) {
		weights_vector = NULL
	} else {
		weights_vector = untreat[[weights]]
	}

	first_stage = fixest::feols(fixest::xpd(~ 0 + ..first_stage, lhs = yname),
								data = untreat,
								weights = weights_vector,
								warn=FALSE,
								notes=FALSE)

	# Residualize outcome variable but keep same yname
	first_u = data[[yname]] - stats::predict(first_stage, newdata = data)
	data[[yname]] = first_u

	# Zero out residual rows with D_it = 1 (for analytical SEs later on)
	if (!bootstrap)	first_u[data[[treatment]] == 1] = 0

	# Second stage

	if(!is.null(weights)) weights_vector = data[[weights]]

	second_stage = fixest::feols(fixest::xpd(~ 0 + ..second_stage, lhs = yname),
								 data = data,
								 weights = weights_vector,
								 warn=FALSE,
								 notes=FALSE)

	ret = list(first_stage = first_stage,
			   second_stage = second_stage)

	if (!bootstrap) ret$first_u = first_u

	return(ret)
}

# Make a sparse_model_matrix for fixest
sparse_model_matrix = function(data, fixest) {
	Z = NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z = as(model.matrix(fixest, data = data), "sparseMatrix")

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		fixef_list = fixest::fixef(fixest)

		mats = lapply(seq_along(fixef_list), function(i) {
			var = names(fixef_list)[i]
			vals = names(fixef_list[[i]])

			ind = lapply(vals, function(val){
				Matrix::Matrix(as.numeric(data[[var]] == val), ncol = 1, sparse = TRUE)
			})

			do.call("cbind", ind)
		})

		Z = cbind(Z, do.call("cbind", mats))
	}

	return(Z)
}
