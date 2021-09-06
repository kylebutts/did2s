# ---- did2s -------------------------------------------------------------------
#
# data(df_hom)
# data = df_hom
# yname="dep_var"; treatment = "treat"; cluster_var = "state"; weights = NULL; bootstrap = FALSE
# first_stage = ~ 0 | year + unit; second_stage = ~ i(rel_year, ref = c(-1, Inf))
# verbose = FALSE
# did2s(data = data, yname = yname, first_stage = first_stage, second_stage = second_stage, treatment = treatment, cluster_var = cluster_var)


# ---- event_study -------------------------------------------------------------

# data(df_hom)
# data = df_hom
# yname = "dep_var"; idname = "unit"; gname = "g"; tname = "year"
# xformla = NULL; horizon = NULL; weights = NULL
# event_study(data = data, yname = yname, idname = idname, gname = gname, tname = tname)
