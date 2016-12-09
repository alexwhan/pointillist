
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

#' Make a pointillist ggplot2 object
#'
#' @param jpg_df A tidy data frame - output from \code{\link{jpg_df()}}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#' @param density Numeric, controls how many points are included
#'
#' @return a ggplot2 object
#' @export
pointillise <- function(jpg_df, depth = 8, density = 1) {
  if(!all(names(jpg_df) == c("row", "col", "r", "g", "b", "hex"))) {
    stop("The jpg dataframe doesn't look right. It should be output from jpg_df()")
  }
  if(max(c(jpg_df$r, jpg_df$g, jpg_df$b)) > 1) {
    stop("The colour range in the jpg dataframe is too big. The dataframe should be output from jpg_df()")
  }

  coord_ratio <- max(jpg_df$row) / max(jpg_df$col)

  p_out <- ggplot2::ggplot(jpg_df, ggplot2::aes_string("col", "row")) +
    ggplot2::geom_point(colour = jpg_df$hex) +
    ggplot2::guides(colour = FALSE) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = coord_ratio)

  return(p_out)
}
