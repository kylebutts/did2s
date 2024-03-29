i = function(factor_var, var, ref, keep, bin, ref2, keep2, bin2, ...){
    # Used to create interactions

    # Later: binning (bin = 1:3 // bin = list("a" = "[abc]")). Default name is bin name (eg "1:3")

    # gt = function(x) cat(sfill(x, 20), ": ", -(t0 - (t0<<-proc.time()))[3], "s\n", sep = "")
    # t0 = proc.time()

    dreamerr:::validate_dots(valid_args = c("f2", "f_name", "ref_special", "sparse"))

    dots = list(...)
    is_sparse = isTRUE(dots$sparse)

    mc = match.call()

    # Finding if it's a call from fixest
    FROM_FIXEST = fixest:::is_fixest_call()

    # General checks
    dreamerr::check_arg(factor_var, "mbt vector")

    # NOTA:
    # the user can use the prefix "i." to tell the algorithm to consider the
    # variable var as a factor. This requires a non standard evaluation
    #

    var_name = NULL
    if(!is.null(mc$f2)){
        # should be only used internally

        var_name = fixest:::deparse_long(mc$f2)
        var = dots$f2
        dreamerr::check_value(var, "vector", .arg_name = "f2")
    }

    # General information on the factor variable
    if(!is.null(dots$f_name)){
        f_name = dots$f_name
    } else {
        f_name = fixest:::deparse_long(mc$factor_var)
    }
    f = factor_var # renaming for clarity

    # checking var
    IS_INTER_NUMERIC = IS_INTER_FACTOR = FALSE
    if(!missing(var)){
        # Evaluation of var (with possibly, user prepending with i.)
        if(is.null(var_name)){
            var_name = fixest:::deparse_long(mc$var)
            if(grepl("^i\\.", var_name)){
                var_name = gsub("^i\\.", "", var_name)
                var = str2lang(var_name)
                dreamerr::check_value_plus(var, "evalset vector", .data = parent.frame(), .arg_name = "var")
                IS_INTER_FACTOR = TRUE
            } else {
                dreamerr::check_value(var, "vector", .arg_name = "var")
            }
        } else {
            # f2 was used
            IS_INTER_FACTOR = TRUE
        }

        # is it an interaction with a factor or with a continuous variable?
        if(length(var) == length(f)){
            # Logical => numeric by default
            # otherwise, just setting the flags

            if(IS_INTER_FACTOR == FALSE){
                # If previous condition == TRUE, means was requested by the user

                if(is.logical(var)){
                    var = as.numeric(var)
                    IS_INTER_NUMERIC = TRUE
                } else if(is.numeric(var)){
                    IS_INTER_NUMERIC = TRUE
                } else {
                    IS_INTER_FACTOR = TRUE
                }
            }

        } else if(length(var) <= 2 && !"ref" %in% names(mc)){
            # ref is implicitly called via the location of var
            ref = var
            dots$ref_special = FALSE
        } else {

            if(grepl("^[[:alpha:]\\.][[:alnum:]\\._]*:[[:alpha:]\\.][[:alnum:]\\._]*$", var_name)){
                info = strsplit(var_name, ":")[[1]]
                stop("In i(): When `var` is equal to a product, please use I(", info[1], "*", info[2], ") instead of ", var_name, ".")
            } else {
                stop("The arguments `var` and `f` must be of the same length (currently ", length(var), " vs ", length(f), ").")
            }
        }
    }

    IS_INTER = IS_INTER_NUMERIC || IS_INTER_FACTOR

    if(isTRUE(dots$ref_special) && (!IS_INTER || IS_INTER_FACTOR)){
        ref = TRUE
    }

    #
    # QUFing + NA
    #

    if(IS_INTER){
        is_na_all = is.na(f) | is.na(var)
    } else {
        is_na_all = is.na(f)
    }

    if(!missing(bin)){
        bin = fixest:::error_sender(fixest:::eval_dot(bin), arg_name = "bin")

        if(!is.null(bin)){
            f = fixest:::bin_factor(bin, f, f_name)
        }
    }

    if(IS_INTER_FACTOR && !fixest:::missnull(bin2)){
        bin2 = fixest:::error_sender(fixest:::eval_dot(bin2), arg_name = "bin2")

        if(!is.null(bin2)){
            var = fixest:::bin_factor(bin2, var, f_name)
        }
    }

    if(IS_INTER_FACTOR){
        info = to_integer(f, var, add_items = TRUE, items.list = TRUE, sorted = TRUE, multi.join = "__%%__")
    } else {
        info = to_integer(f, add_items = TRUE, items.list = TRUE, sorted = TRUE)
    }

    fe_num = info$x
    items = info$items

    if(!IS_INTER_NUMERIC){
        # neutral var in C code
        var = 1
    }

    if(IS_INTER_FACTOR){
        items_split = strsplit(items, "__%%__", fixed = TRUE)

        f_items = sapply(items_split, `[`, 1)
        var_items = sapply(items_split, `[`, 2)
    } else {
        f_items = items
    }

    dreamerr::check_arg(ref, "logical scalar | vector no na")

    dreamerr::check_arg(ref2, keep, keep2, "vector no na")

    NO_ERROR = FALSE
    if(fixest:::is_calling_fun("fixest_model_matrix_extra", full_search = TRUE, full_name = TRUE)){
        NO_ERROR = TRUE
    }

    no_rm = TRUE
    id_drop = c()
    if(!missing(ref)){
        if(isTRUE(ref)){
            # We always delete the first value
            # Que ce soit items ici est normal (et pas f_items)
            id_drop = which(items == items[1])
        } else {
            id_drop = fixest:::items_to_drop(f_items, ref, "factor_var", no_error = NO_ERROR)
        }
        ref_id = id_drop
    }


    if(!missing(keep)){
        id_drop = c(id_drop, fixest:::items_to_drop(f_items, keep, "factor_var", keep = TRUE, no_error = NO_ERROR))
    }

    if(IS_INTER_FACTOR){
        if(!missing(ref2)){
            id_drop = c(id_drop, fixest:::items_to_drop(var_items, ref2, "var", no_error = NO_ERROR))
        }

        if(!missing(keep2)){
            id_drop = c(id_drop, fixest:::items_to_drop(var_items, keep2, "var", keep = TRUE, no_error = NO_ERROR))
        }
    }

    if(length(id_drop) > 0){
        id_drop = unique(sort(id_drop))
        if(length(id_drop) == length(items)){
            if(FROM_FIXEST) {
                # we return something neutral in an estimation
                return(rep(0, length(f)))
            }

            stop("All items from the interaction have been removed.")
        }
        who_is_dropped = id_drop
        no_rm = FALSE
    } else {
        # -1 is neutral
        who_is_dropped = -1
    }

    # The column names

    if(length(id_drop) > 0){
        items_name = items[-id_drop]
        f_items = f_items[-id_drop]
        if(IS_INTER_FACTOR){
            var_items = var_items[-id_drop]
        }
    } else {
        items_name = items
    }

    if(FROM_FIXEST || is_sparse){
        # Pour avoir des jolis noms c'est un vrai gloubiboulga,
        # mais j'ai pas trouve plus simple...
        if(IS_INTER_FACTOR){
            col_names = paste0("__CLEAN__", f_name, "::", f_items, ":", var_name, "::", var_items)
            col_names_full = NULL

        } else if(IS_INTER_NUMERIC){
            col_names = paste0("__CLEAN__", f_name, "::", f_items, ":", var_name)
            col_names_full = paste0(f_name, "::", items, ":", var_name)

        } else {
            col_names = paste0("__CLEAN__", f_name, "::", items_name)
            col_names_full = paste0(f_name, "::", items)
        }
    } else {

        if(IS_INTER_FACTOR){
            items_name = gsub("__%%__", ":", items_name, fixed = TRUE)
        }

        col_names = items_name
    }

    if(is_sparse){
        # Internal call: we return the row ids + the values + the indexes + the names

        if(length(who_is_dropped) > 0){
            valid_row = !is_na_all & !fe_num %in% who_is_dropped
        } else {
            valid_row = !is_na_all
        }

        # we need to ensure the IDs go from 1 to # Unique
        fe_colid = to_integer(fe_num[valid_row], sorted = TRUE)

        values = if(length(var) == 1) rep(1, length(valid_row)) else var

        # Clean names
        col_names = sub("^.*__CLEAN__", "", col_names)

        res = list(rowid = which(valid_row), values = values,
                   colid = fe_colid, col_names = col_names)

        class(res) = "i_sparse"

        return(res)
    }

    res = fixest:::cpp_factor_matrix(fe_num, is_na_all, who_is_dropped, var, col_names)
    # res => matrix with...
    #  - NAs where appropriate
    #  - appropriate number of columns
    #  - interacted if needed
    #


    # We send the information on the reference
    if(FROM_FIXEST){
        is_GLOBAL = FALSE
        for(where in 1:min(8, sys.nframe())){
            if(exists("GLOBAL_fixest_mm_info", parent.frame(where))){
                GLOBAL_fixest_mm_info = get("GLOBAL_fixest_mm_info", parent.frame(where))
                is_GLOBAL = TRUE
                break
            }
        }

        if(is_GLOBAL && !IS_INTER_FACTOR){
            # NOTA:
            # if you change stuff here => change them also in sunab()

            info = list()
            info$coef_names_full = col_names_full
            info$items = items
            if(!missing(ref)){
                info$ref_id = ref_id
                info$ref = items[ref_id]
            }
            info$is_num = is.numeric(items)
            info$f_name = f_name
            info$is_inter_num = IS_INTER_NUMERIC
            if(IS_INTER_NUMERIC){
                info$var_name = var_name
            }
            info$is_inter_fact = IS_INTER_FACTOR

            GLOBAL_fixest_mm_info[[length(GLOBAL_fixest_mm_info) + 1]] = info
            # re assignment
            assign("GLOBAL_fixest_mm_info", GLOBAL_fixest_mm_info, parent.frame(where))
        }

    }

    res
}


i_ref = function(factor_var, var, ref, bin, keep, ref2, keep2, bin2){
    # To automatically add references when i(x) is used

    mc = match.call()

    mc[[1]] = as.name("i")

    if(!any(c("ref", "keep", "ref2", "keep2") %in% names(mc))){
        mc$ref_special = TRUE
    }

    return(fixest:::deparse_long(mc))
}

i_noref = function(factor_var, var, ref, bin, keep, ref2, keep2, bin2){
    # Used only in predict => to create data without restriction

    mc = match.call()

    mc[[1]] = as.name("i")

    mc$ref = mc$keep = mc$ref2 = mc$keep2 = NULL

    return(fixest:::deparse_long(mc))
}

#' Design matrix of a `fixest` object returned in sparse format
#'
#' This function creates the left-hand-side or the right-hand-side(s) of a [`femlm`], [`feols`] or [`feglm`] estimation. Note that this function currently does not accept a formula
#'
#' @param object A `fixest` estimation object
#' @param data If missing (default) then the original data is obtained by evaluating the `call`. Otherwise, it should be a `data.frame`.
#' @param type Character vector or one sided formula, default is "rhs". Contains the type of matrix/data.frame to be returned. Possible values are: "lhs", "rhs", "fixef", "iv.rhs1" (1st stage RHS), "iv.rhs2" (2nd stage RHS), "iv.endo" (endogenous vars.), "iv.exo" (exogenous vars), "iv.inst" (instruments).
#' @param na.rm Default is `TRUE`. Should observations with NAs be removed from the matrix?
#' @param collin.rm Logical scalar, default is `TRUE`. Whether to remove variables that were found to be collinear during the estimation. Beware: it does not perform a collinearity check.
#' @param combine Logical scalar, default is `TRUE`. Whether to combine each resulting sparse matrix 
#' @param ... Not currently used.
#'
#' @return
#' It returns either a single sparse matrix a list of matrices, depending whether `combine` is `TRUE` or `FALSE`. The sparse matrix is of class `dgCMatrix` from the `Matrix` package.
#'
#' @seealso
#' See also the main estimation functions [`femlm`], [`feols`] or [`feglm`]. [`formula.fixest`], [`update.fixest`], [`summary.fixest`], [`vcov.fixest`].
#'
#'
#' @author
#' Laurent Berge, Kyle Butts
#'
#' @examples
#'
#' est = feols(wt ~ i(vs) + hp | cyl, mtcars)
#' sparse_model_matrix(est)
#' 
#'
#' @export
sparse_model_matrix = function(object, data, type = "rhs", na.rm = TRUE,  collin.rm = TRUE, combine = TRUE, ...) {
    # We evaluate the formula with the past call
    # type: lhs, rhs, fixef, iv.endo, iv.inst, iv.rhs1, iv.rhs2
    # if fixef => return a DF

    # Checking the arguments
    if (fixest:::is_user_level_call()) {
        dreamerr:::validate_dots(suggest_args = c("data", "type"))
    }

    # We allow type to be used in the location of data if data is missing
    if (!missing(data) && missing(type)) {
        sc = sys.call()
        if (!"data" %in% names(sc)) {
            if (!is.null(data) && (is.character(data) || "formula" %in% class(data))) {
                # data is in fact the type
                type = data
                data = NULL
            }
        }
    }

    type = fixest:::check_set_types(type, c("lhs", "rhs", "fixef", "iv.endo", "iv.inst", "iv.exo", "iv.rhs1", "iv.rhs2"))

    if (isTRUE(object$is_fit)) {
        stop("model.matrix method not available for fixest estimations obtained from fit methods.")
    }

    if (any(grepl("^iv", type)) && !isTRUE(object$iv)) {
        stop("The type", dreamerr::enumerate_items(grep("^iv", type, value = TRUE), "s.is"), " only valid for IV estimations.")
    }

    dreamerr::check_arg(subset, "logical scalar | character vector no na")

    dreamerr::check_arg_plus(collin.rm, "logical scalar")

    # Evaluation with the data
    original_data = FALSE
    if (fixest:::missnull(data)) {
        original_data = TRUE

        data = fixest:::fetch_data(object, "To apply 'sparse_model_matrix', ")
    }

    # control of the data
    if (is.matrix(data)) {
        if (is.null(colnames(data))) {
            stop("If argument 'data' is to be a matrix, its columns must be named.")
        }
        data = as.data.frame(data)
    }

    if (!"data.frame" %in% class(data)) {
        stop("The argument 'data' must be a data.frame or a matrix.")
    }

    # data = as.data.frame(data)

    # Allows passage of formula to sparse_model_matrix. A bit inefficient, but it works.
    isFormula = FALSE
    split_fml = NULL
    if ("formula" %in% class(object)) {
      split_fml = fixest:::fml_split_internal(object)

      if (collin.rm == TRUE | length(split_fml) == 3) {
        message("Formula passed to sparse_model_matrix with collin.rm == TRUE or iv. Estimating feols with formula.")
        object = feols(object, data = data)
      } else {
        isFormula = TRUE
      }
    }

    # na.rm = FALSE doesn't work with type = "fixef" (which FE col gets NA?)
    if (("fixef" %in% type & !na.rm)) {
        # na.rm = TRUE
        message("na.rm = FALSE doesn't work with type = 'fixef'. It has been set to TRUE.")
    }


    # Panel setup
    if (!isFormula) {
      panel__meta__info = fixest:::set_panel_meta_info(object, data)
    }

    # The formulas
    if (isFormula) {
      fml_linear = split_fml[[1]]
      
      fml_0 = attr(stats::terms(fml_linear), "intercept") == 0
      fake_intercept = !is.null(split_fml[[2]]) | fml_0
    
    } else {
      fml_linear = stats::formula(object, type = "linear")
      
      # we kick out the intercept if there is presence of fixed-effects
      fml_0 = attr(stats::terms(fml_linear), "intercept") == 0
      fake_intercept = !is.null(object$fixef_vars) && !(!is.null(object$slope_flag) && all(object$slope_flag < 0)) 
      fake_intercept = fake_intercept | fml_0
    }

    res = list()

    if ("lhs" %in% type) {
        lhs_text = fixest:::deparse_long(fml_linear[[2]])
        lhs = eval(fml_linear[[2]], data) 
        lhs = Matrix::Matrix(lhs, sparse = TRUE, ncol = 1)

        colnames(lhs) = lhs_text

        res[["lhs"]] = lhs
    }

    if ("rhs" %in% type && !isTRUE(object$onlyFixef)) {

        fml = fml_linear

        if (isFormula && (length(split_fml) == 3)) {
          fml_iv = split_fml[[3]]
          fml = fixest:::.xpd(..lhs ~ ..endo + ..rhs, ..lhs = fml[[2]], ..endo = fml_iv[[2]], ..rhs = fml[[3]])
        } else if (isTRUE(object$iv)) {
          fml_iv = object$fml_all$iv
          fml = fixest:::.xpd(..lhs ~ ..endo + ..rhs, ..lhs = fml[[2]], ..endo = fml_iv[[2]], ..rhs = fml[[3]])
        }

        vars = attr(stats::terms(fml), "term.labels")

        linear.mat = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm, type = "rhs", add_intercept = !fake_intercept)

        res[["rhs"]] = linear.mat
    }

    if ("fixef" %in% type) {

        if (isFormula && (length(split_fml) < 2)) {
          mat_FE = NULL
        } else if (!isFormula & length(object$fixef_id) == 0) {
          mat_FE = NULL
        } else { 

            if (isFormula) {
              fixef_fml = fixest:::.xpd(~ ..fe, ..fe = split_fml[[2]])
            } else {
              fixef_fml = object$fml_all$fixef
            }

            fixef_terms_full = fixest:::fixef_terms(fixef_fml)
            fe_vars = fixef_terms_full$fe_vars
            slope_var_list = fixef_terms_full$slope_vars_list
            
            fixef_df = fixest:::prepare_df(fe_vars, data, fastCombine = FALSE)

            # Check for slope vars
            if (any(fixef_terms_full$slope_flag > 0)) {
              slope_df = fixest:::prepare_df(unlist(slope_var_list), data)
            }

            cols_lengths = c(0)
            total_cols = 0
            n_FE = 0
            nrows = nrow(data)
            for (i in seq_along(fe_vars)) {

              fe_var = fe_vars[i]
              slope_vars = slope_var_list[[i]]
              n_slope_vars = if (is.null(slope_vars)) 0 else length(slope_vars)

              fe = fixef_df[[fe_var]]
              unique_fe = unique(fe)
              n_cols = length(unique_fe[!is.na(unique_fe)])

              total_cols = total_cols + n_cols * (1 + n_slope_vars)
              cols_lengths = c(cols_lengths, rep(n_cols, 1 + n_slope_vars))
              n_FE = n_FE + 1 + n_slope_vars
            }
            running_cols = cumsum(cols_lengths)

            id_all = names_all = val_all = vector("list", n_FE)
            rowid = 1:nrows
            j = 1
            for (i in seq_along(fe_vars)) {

              fe_var = fe_vars[i]
              xi = fixef_df[[fe_var]]

              keep = which(!is.na(xi))
              if (length(keep) == 0) stop("All values of the fixed-effect variable '", fe_var, "' are NA.")

              xi = xi[keep]
              xi_quf = fixest:::quickUnclassFactor(xi, addItem = TRUE)

              col_id = xi_quf$x
              col_levels = as.character(xi_quf$items)

              slope_vars = slope_var_list[[i]]
              n_slope_vars = if (is.null(slope_vars)) 0 else length(slope_vars)

              # Be careful with NAs
              # First fixed-effect by itself
              val_all[[j]] = c(rep(1, length(col_id)), rep(NA, nrows - length(keep)))
              id_all[[j]] = cbind(
                c(
                  rowid[keep],
                  rowid[-keep]
                ), 
                c(
                  running_cols[j] + col_id, 
                  rep(running_cols[j] + 1, nrows - length(keep))
                )
              )
              names_all[[j]] = paste0(fe_var, "::", col_levels)
              j = j + 1

              for (k in seq_along(slope_vars)) {
                slope_var = slope_vars[k]
                slope = slope_df[[slope_var]]

                val_all[[j]] = c(as.numeric(slope[keep]), rep(NA, nrows - length(keep)))
                id_all[[j]] = cbind(
                  c(
                    rowid[keep],         
                    rowid[-keep]
                  ), 
                  c(
                    running_cols[j] + col_id, 
                    rep(running_cols[j] + 1, nrows - length(keep))
                  )
                )
                names_all[[j]] = paste0(fe_var, "[[", slope_var, "]]", "::", col_levels)
                j = j + 1
              }
            }

            id_mat = do.call(rbind, id_all)
            val_vec = unlist(val_all)
            names_vec = unlist(names_all)

            mat_FE = Matrix::sparseMatrix(
              i = id_mat[, 1],
              j = id_mat[, 2],
              x = val_vec,
              dimnames = list(NULL, names_vec)
            )


            # Keep non-zero FEs
            if (collin.rm == TRUE) {
              fixefs = fixef(object, sorted = TRUE)

              select =	lapply(
                names(fixefs), 
                function(var) {
                  names = names(fixefs[[var]])
                  names = names[fixefs[[var]] != 0]

                  paste0(var, "::", names)
                }
              )
              select = unlist(select)

              # When original_data isn't used, some FEs may not be in the new dataset, add them in
              missing_cols = setdiff(select, colnames(mat_FE))
              mat_FE = cbind(
                mat_FE, 
                Matrix::Matrix(0, ncol = length(missing_cols), nrow = nrow(mat_FE), sparse = TRUE, dimnames = list(NULL, missing_cols))
              )

              # Subset fixef and order columns to match fixef(object)
              idx = which(colnames(mat_FE) %in% select)
              mat_FE = mat_FE[, idx, drop = FALSE]
              
              # Reorder columns to match fixef(object)
              reorder = match(unlist(select), colnames(mat_FE))
              mat_FE = mat_FE[, reorder[!is.na(reorder)], drop = FALSE]
            }

        }
        
        res[["fixef"]] = mat_FE
    }

    #
    # IV
    #
    if ("iv.endo" %in% type) {
        fml = object$iv_endo_fml
        vars = attr(stats::terms(fml), "term.labels")
        endo.mat = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm)

        res[["iv.endo"]] = endo.mat
    }

    if ("iv.inst" %in% type) {
        fml = object$fml_all$iv
        vars = attr(stats::terms(fml), "term.labels")
        inst.mat = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm)

        res[["iv.inst"]] = inst.mat
    }

    if ("iv.exo" %in% type) {

        fml = object$fml_all$linear
        vars = attr(stats::terms(fml), "term.labels")
        exo.mat = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm, type = "iv.exo", add_intercept = !fake_intercept)

        res[["iv.exo"]] = exo.mat
    }

    if ("iv.rhs1" %in% type) {
        fml = object$fml
        if (object$iv_stage == 2) {
            fml_iv = object$fml_all$iv
            fml = fixest:::.xpd(..lhs ~ ..inst + ..rhs, ..lhs = fml[[2]], ..inst = fml_iv[[3]], ..rhs = fml[[3]])
        }
        vars = attr(stats::terms(fml), "term.labels")
        iv_rhs1 = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm, type = "iv.rhs1", add_intercept = !fake_intercept)
        
        res[["iv.rhs1"]] = iv_rhs1
    }

    if ("iv.rhs2" %in% type) {
        # Second stage
        if (!object$iv_stage == 2) {
            stop("In model.matrix, the type 'iv.rhs2' is only valid for second stage IV models. This estimation is the first stage.")
        }

        # I) we get the fitted values
        stage_1 = object$iv_first_stage

        fit_vars = c()
        for (i in seq_along(stage_1)) {
            fit_vars[i] = v = paste0("fit_", names(stage_1)[i])
            data[[v]] = stats::predict(stage_1[[i]], newdata = data, sample = "original")
        }

        # II) we create the variables
        fml = object$fml
        fml = fixest:::.xpd(..lhs ~ ..fit + ..rhs, ..lhs = fml[[2]], ..fit = fit_vars, ..rhs = fml[[3]])
        vars = attr(stats::terms(fml), "term.labels")
        iv_rhs2 = vars_to_sparse_mat(vars = vars, data = data, object = object, collin.rm = collin.rm, type = "iv.rhs2", add_intercept = !fake_intercept)

        res[["iv.rhs2"]] = iv_rhs2
    }

    if (na.rm) {
      na_rows = lapply(res, function(mat) {
        # Get rows with NA 
        1 + mat@i[is.na(mat@x)]
      })

      na_rows = unlist(na_rows, use.names = FALSE)

      if (original_data) {
        na_rows = c(na_rows, -1 * object$obs_selection$obsRemoved)
      }

      na_rows = unique(na_rows)

      if (length(na_rows) > 0) {
        res = lapply(res, function(mat) { 
          mat[-na_rows, , drop = FALSE]
        })
      }
    } 

    # Formatting res
    if (length(res) == 0) {
        return(NULL)
    } else if (length(type) > 1) {
        if (combine) {
          res = res[type]
          res = do.call(cbind, unname(res))
        }
    } else {
        res = res[[1]]
    }

    res
}



# Internal: modifies the calls so that each variable/interaction is evaluated with mult_sparse
mult_wrap = function(x) {
	# x: character string of a variable to be evaluated
	# ex: "x1" => mult_sparse(x1)
	#     "x1:factor(x2):x3" => mult_sparse(x3, factor(x2), x1)
	#
	# We also add the argument sparse to i()
	#     "x1:i(species, TRUE)" => mult_sparse(x1, i(species, TRUE, sparse = TRUE))

	x_call = str2lang(x)

	res = (~ mult_sparse())[[2]]

	if (length(x_call) == 1 || x_call[[1]] != ":") {
		res[[2]] = x_call

	} else {
		res[[2]] = x_call[[3]]
		tmp = x_call[[2]]

		while (length(tmp) == 3 && tmp[[1]] == ":") {
			res[[length(res) + 1]] = tmp[[3]]
			tmp = tmp[[2]]
		}

		res[[length(res) + 1]] = tmp
	}

	# We also add sparse to i() if found
	for (i in 2:length(res)) {
		ri = res[[i]]
		if (length(ri) > 1 && ri[[1]] == "i") {
			ri[["sparse"]] = TRUE
			res[[i]] = ri
		}
	}

	if (length(res) > 2) {
		# we restore the original order
		res[-1] = rev(res[-1])
	}

	return(res)
}

# Internal function to evaluate the variables (and interactions) in a sparse way
mult_sparse = function(...) {
	# Only sparsifies factor variables
	# Takes care of interactions

	dots = list(...)
	n = length(dots)

	num_var = NULL
	factor_list = list()
	info_i = NULL
	is_i = is_factor = FALSE
	# You can't have interactions between i and factors, it's either

	for (i in 1:n) {
		xi = dots[[i]]
		if (is.numeric(xi)) {
			# We stack the product
			num_var = if (is.null(num_var)) xi else xi * num_var
		} else if (inherits(xi, "i_sparse")) {
			is_i = TRUE
			info_i = xi
		} else {
			is_factor = TRUE
			factor_list[[length(factor_list) + 1]] = xi
		}
	}

	# numeric
	if (!is_i && !is_factor) {
		return(num_var)
	}
	# factor()
	if (is_factor) {
		factor_list$add_items = TRUE
		factor_list$items.list = TRUE

		fact_as_int = do.call(to_integer, factor_list)

		values = if (is.null(num_var)) rep(1, length(fact_as_int$x)) else num_var

		rowid = seq_along(values)
		res = list(rowid = rowid, colid = fact_as_int$x, values = values,
				   col_names = fact_as_int$items, n_cols = length(fact_as_int$items))
	# i()
	} else {

		values = info_i$values
		if (!is.null(num_var)) {
			num_var = num_var[info_i$rowid]
			values = values * num_var
		}

		res = list(rowid = info_i$rowid, colid = info_i$colid,
				   values = values[info_i$rowid],
				   col_names = info_i$col_names,
				   n_cols = length(info_i$col_names))
	}

	class(res) = "sparse_var"

	res
}

# Takes a vector of strings denoting the variables (including terms like `poly()`, `i()`, `I()`, `lag()`, etc.) and returns a sparse matrix of the variables extracted from `data`.
vars_to_sparse_mat = function(vars, data, collin.rm = FALSE, object = NULL, type = NULL, add_intercept = FALSE) {

    if (length(vars) == 0) {
      # Case only FEs
      mat = NULL
    } else {

      # Since we don't want to evaluate the factors,
      # the code is a bit intricate because we have to catch them before
      # any interaction takes place
      #
      # that's why I wrap interactions in a function (mult_sparse())
      #

      # Below, we evaluate all the variables in a "sparse" way

      vars_calls = lapply(vars, mult_wrap)

      n = length(vars)
      variables_list = vector("list", n)
      for (i in 1:n) {
        variables_list[[i]] = eval(vars_calls[[i]], data)
      }

      # To create the sparse matrix, we need the column indexes
      total_cols = 0
      running_cols = c(0)
      for (i in 1:n) {
        xi = variables_list[[i]]
        if (inherits(xi, "sparse_var")) {
          total_cols = total_cols + xi$n_cols
        } else {
          total_cols = total_cols + NCOL(xi)
        }
        running_cols[i + 1] = total_cols
      }

      # We just create a sparse matrix and fill it

      # 1) creating the indexes + names

      # NOTA: I use lists to avoid creating copies
      rowid = 1:nrow(data)
      id_all = values_all = names_all = vector("list", n)
      for (i in 1:n) {
        xi = variables_list[[i]]
        
        if (inherits(xi, "sparse_var")) {

          id_all[[i]] = cbind(xi$rowid, running_cols[i] + xi$colid)
          values_all[[i]] = xi$values
          names_all[[i]] = xi$col_names
        
        } else if (NCOL(xi) == 1) {

          id_all[[i]] = cbind(rowid, running_cols[i] + 1)
          values_all[[i]] = xi
          names_all[[i]] = vars[[i]]
        
        } else {

          colid = rep(1:NCOL(xi), each = nrow(data))
          id_all[[i]] = cbind(rep(rowid, NCOL(xi)), running_cols[i] + colid)
          values_all[[i]] = as.vector(xi)
          if (!is.null(colnames(xi))) {
            names_all[[i]] = paste0(vars[[i]], colnames(xi))
          } else {
            names_all[[i]] = paste0(vars[[i]], 1:NCOL(xi))
          }
        
        }
      }

      id_mat = do.call(rbind, id_all)
      values_vec = unlist(values_all)
      names_vec = unlist(names_all)

      # 2) filling the matrix: one shot, no copies

      mat = Matrix::Matrix(0, nrow(data), total_cols, dimnames = list(NULL, names_vec))
      mat[id_mat] = values_vec

    }

    # TODO: Should I use error_sender?
    # mat = fixest:::error_sender(fixest_model_matrix_extra(
    #     object = object, newdata = data, original_data = original_data,
    #     fml = fml, fake_intercept = fake_intercept,
    #     subset = subset),
    #     "In 'model.matrix', the RHS could not be evaluated: ")

    if (collin.rm & is.null(object)) {
      stop("You need to provide the 'object' argument to use 'collin.rm = TRUE'.")
    }

    if (collin.rm) {
        qui = which(colnames(mat) %in% object$collin.var)
        if (length(qui) == ncol(mat)) {
            mat = NULL
        } else if (length(qui) > 0) {
            mat =  mat[, -qui, drop = FALSE]
        }

        coefs = names(object$coefficients)
        if (isTRUE(object$iv)) {
          fml_iv = object$fml_all$iv
          endo = fml_iv[[2]]
          
          # Trick to get the rhs variables as a character vector
          endo = fixest:::.xpd(~ ..endo, ..endo = endo)
          endo = attr(stats::terms(endo), "term.labels")

          exo = lapply(object$iv_first_stage, function(x) names(stats::coef(x)))
          exo = unique(unlist(exo, use.names = FALSE))
        }

        # Custom subsetting for na.rm depending on `type`
        if (!is.null(type)) {
          if (type == "rhs") {
            if (isTRUE(object$iv)) {
              keep = c(endo, coefs)
            } else {
              keep = coefs
            }
          } else if (type == "iv.exo") {
            keep = coefs
          } else if (type == "iv.exo") {
            keep = c(endo, coefs)
          } else if (type == "iv.rhs1") {
            keep = c(exo, coefs)
          } else if (type == "iv.rhs2") {
            keep = coefs
          }

          keep = keep[!keep %in% object$collin.var]
          if (length(keep) == 0) {
              mat = NULL
          } else {
              idx = which(colnames(mat) %in% keep)
              mat =  mat[, idx, drop = FALSE]
          }
        }
        
        if (length(coefs) == ncol(mat) && any(colnames(mat) != names(coefs))) {
            # we reorder the matrix
            # This can happen in multiple estimations, where we respect the
            # order of the user

            if (all(names(coefs) %in% colnames(mat))) {
                mat = mat[, names(coefs), drop = FALSE]
            }
        }
    }

    if (add_intercept) {
      mat = cbind(1, mat)
      colnames(mat)[1] = "(Intercept)"
    }

    return(mat)
}
