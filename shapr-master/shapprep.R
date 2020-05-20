#' prep SHAP values into long format for plotting
#'
#' Produce a dataset of 6 columns: ID of each observation, variable name, SHAP
#' value, variable values (feature value), deviation of the feature value for
#' each observation (for coloring the point), and the mean SHAP values for each
#' variable. You can view this example dataset included in the package:
#' \code{\link{shap_long_iris}}
#'
#' The ID variable is added for each observation in the `shap_contrib` dataset
#' for better tracking, it is created as `1:nrow(shap_contrib)` before melting
#' `shap_contrib` into long format.
#'
#' @param xgb_model a xgboost model object, will derive the SHAP values from it
#' @param shap_contrib optional to directly supply a SHAP values dataset. If
#'   supplied, it will overwrite the `xgb_model` if `xgb_model` is also supplied
#' @param X_train the dataset of predictors used for the xgboost model, it
#'   provides feature values to the plot, must be supplied
#' @param top_n to choose top_n variables ranked by mean|SHAP| if needed
#' @param var_cat if supplied, will provide long format data, grouped by this
#'   categorical variable
#'
#' @import data.table
#' @export shap.prep
#'
#' @return a long format data.table, named as `shap_long`
#'
#' @example R/example/example_fit_summary.R
#' @example R/example/example_categorical.R
#'
shap.prep <- function(shap_contrib = NULL, # optional to directly supply SHAP values
                      X_train,
                      top_n = NULL,
                      var_cat = NULL
){
  if (is.null(xgb_model) & is.null(shap_contrib)) stop("Please provide either `xgb_model` or `shap_contrib`")
  if (!is.null(shap_contrib)){
    if(paste0(dim(shap_contrib), collapse = " ") != paste0(dim(X_train), collapse = " ")) stop("supply correct shap_contrib, remove BIAS column.\n")
  }

  # prep long-data
  shap <- if (is.null(shap_contrib)) shap.values(xgb_model, X_train) else list(
    shap_score = shap_contrib,
    mean_shap_score = colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = TRUE)]
  )
  std1 <- function(x){
    return ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

  # choose top n features
  if (is.null(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  top_n <- as.integer(top_n)
  if (!top_n%in%c(1:dim(X_train)[2])) {
    message ('Please supply correct top_n, by default use all features.\n')
    top_n <- dim(X_train)[2]
  }

  # arrange variables in descending order, thus the summary plot could be
  # plotted accordingly.
  shap_score_sub <- setDT(shap$shap_score)[, names(shap$mean_shap_score)[1:top_n], with = FALSE]
  shap_score_sub[, ID:= .I]
  # fv: feature values: the values in the original dataset
  # fv_sub: subset of feature values
  # since dayint is int, the package example will throw a warning here
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]

  if(is.null(var_cat)){
    # shap_score_sub contains the sample ID
    shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(fv_sub))
    vars_wanted <- colnames(fv_sub)

  } else if (var_cat%in%colnames(fv_sub)) {
    # exclude var_cat as it is used as a categorical group
    shap_score_long <- melt.data.table(shap_score_sub[,-..var_cat], measure.vars = colnames(fv_sub)[!colnames(fv_sub) %in% c(var_cat, "ID")])
    vars_wanted <- colnames(fv_sub)[!colnames(fv_sub) %in% var_cat]
  } else {
    stop("Please provide a correct var_cat variable, a categorical variable that
         exists in the dataset.")
  }
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = vars_wanted, value.name = "rfvalue")
  fv_sub_long[, stdfvalue := std1(rfvalue), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue;
  # standarized: stdfvalue
  if(is.null(var_cat)){
    shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  } else {
    shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue', var_cat), with = FALSE])
  }
  # mean_value: mean abs SHAP values by variable, used as the label by
  # `geom_text` in the summary plot
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2)
}
