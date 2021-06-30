# Make a sparse_model_matrix for fixest estimate
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
