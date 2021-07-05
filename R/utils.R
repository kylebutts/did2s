# Make a sparse_model_matrix for fixest estimate
sparse_model_matrix = function(data, fixest) {
	Z = NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z = as(model.matrix(fixest, data = data), "sparseMatrix")

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		frmla <- as.formula(paste("~ 0 + ", paste(glue::glue("factor({all.vars(fixest$fml_all$fixef)})"), collapse = " + ")))

		Z_fixef = Matrix::sparse.model.matrix(frmla, data = data)

		temp = fixest::fixef(first_stage_est)
		select =	lapply(names(temp), function(var){
			names = names(temp[[var]])
			names = names[temp[[var]] != 0]

			glue::glue("factor({var}){names}")
		})


		Z = cbind(Z, Z_fixef[, unlist(blank)])
	}

	return(Z)
}
