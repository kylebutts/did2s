# Make a sparse_model_matrix for fixest estimate
sparse_model_matrix = function(data, fixest) {
	Z = NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z = as(model.matrix(fixest, data = data), "sparseMatrix")

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		frmla <- as.formula(paste("~ 0 + ", paste(glue::glue("factor({all.vars(first_stage_est$fml_all$fixef)})"), collapse = " + ")))

		Z = cbind(Z, Matrix::sparse.model.matrix(frmla, data = data))
	}

	return(Z)
}
