#' @keywords internal
unique_features <- function(x) {
  unique(
    unlist(
      strsplit(x, split = ":", fixed = TRUE)
    )
  )
}

#' Helper function that checks a conditioning index against a particular ordering.
#'
#' @param index_given
#'
#' @keywords internal
respects_order <- function(index_given, ordering) {
  
  for (i in index_given) {
    
    idx_position <- Position(function(x) i %in% x, ordering, nomatch = 0)
    
    stopifnot(idx_position > 0) # It should always be in the ordering
    
    # check for precedents (only relevant if not root set)
    if (idx_position > 1) {
      
      # get precedents
      precedents <- unlist(ordering[1:(idx_position-1)])
      # all precedents must be in index
      # print(precedents)
      # print(intersect(precedents, index))
      # print(setequal(precedents, intersect(precedents, index)))
      if (!setequal(precedents, intersect(precedents, index_given))) {
        # print("here")
        return (FALSE)
      }
    }
  }
  
  TRUE
}
