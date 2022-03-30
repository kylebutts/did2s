# ---- did2s -------------------------------------------------------------------
#
# data(df_hom)
# data = df_hom
# yname="dep_var"; treatment = "treat"; cluster_var = "state"; weights = NULL; bootstrap = FALSE
# first_stage = ~ 1 + i(year,state) | unit; second_stage = ~ i(rel_year, ref = c(-1, Inf))
# verbose = FALSE
# did2s(data = data, yname = yname, first_stage = first_stage, second_stage = second_stage, treatment = treatment, cluster_var = cluster_var)
#

# ---- Profile code ------------------------------------------------------------
# lineprof::lineprof(
# 	did2s(
# 		df_hom, yname = "dep_var", treatment = "treat", cluster_var = "state",
# 		first_stage = ~ 0 | unit + year, second_stage = ~ i(treat, ref=FALSE)
# 	)
# )

# ---- imputation_plot ---------------------------------------------------------

# data(df_hom)
# data = df_hom
# data$id = data$state*1000 + (data$unit - 1)
# yname="dep_var"; treatment = "treat"; idname = "id"; tname="year"; weights = NULL;
# first_stage = ~ 1 + i(year,state) | unit; second_stage = ~ i(rel_year, ref = c(-1, Inf))
# verbose = FALSE

# imputation_plot(df_hom, yname = "dep_var", treatment = "treat", idname = "id", tname = "year", first_stage = ~ 0 | unit + year)

# ---- event_study -------------------------------------------------------------

# data(df_hom)
# data = df_hom
# yname = "dep_var"; idname = "unit"; gname = "g"; tname = "year"
# xformla = NULL; horizon = NULL; weights = NULL
# event_study(data = data, yname = yname, idname = idname, gname = gname, tname = tname)


# ---- did2s bootstrap ---------------------------------------------------------
#
# data(df_hom)
# data = df_hom
# yname="dep_var"; treatment = "treat"; cluster_var = "state"; weights = NULL; bootstrap = TRUE
# first_stage = ~ 0 | year + unit; second_stage = ~ i(rel_year, ref = c(-1, Inf))
# verbose = FALSE; n_bootstraps = 100;
# did2s(data = data, yname = yname, first_stage = first_stage, second_stage = second_stage, treatment = treatment, cluster_var = cluster_var, bootstrap = TRUE, n_bootstrap = n_bootstraps)
