
#' Make a data frame of jpg data
#'
#' @param file A file path
#'
#' @return A tidy dataframe describing r, g, b and hex values
#' @export
#' @importFrom magrittr %>%
jpg_df <- function(file) {
  jpg <- jpeg::readJPEG(file)

  jpg_list <- vector(mode = "list", length = 3)

  vals <- c("r", "g", "b")

  for(i in 1:3) {
    jpg_list[[i]] <- jpg[,,i] %>%
      as.data.frame() %>%
      dplyr::mutate(row = 1:nrow(.)) %>%
      tidyr::gather(col, value, -row) %>%
      dplyr::mutate(col = as.numeric(sub("V", "", col)),
             row = row,
             value = value)
    names(jpg_list[[i]])[3] <- vals[i]
  }

  jpg_full <- purrr::reduce(jpg_list, dplyr::left_join) %>%
    dplyr::mutate(hex = rgb(r, g, b))

  return(jpg_full)
}

#' Make a pointillist ggplot object
#'
#' @param jpg_df A tidy data frame - output from \code{\link{jpg_df()}}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#' @param density Numeric, controls how many points are included
#'
#' @return a ggplot2 object
#' @export
#'
# pointillise <- function(jpg_df, depth = 8, density = 1) {
#   if(!names(jpg_df) == )
#   p_out <- ggplot(jpg_df, aes(col, row)) + geom_point(colour = jpg_sample$hex) +
#     guides(colour = FALSE) +
#     scale_y_reverse()
#
# }
