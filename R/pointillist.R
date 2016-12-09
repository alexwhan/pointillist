
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
    dims <- dim(jpg[,,i])
    jpg_df <- jpg[,,i] %>%
      as.data.frame() %>%
      dplyr::mutate_("row" = 1:dims[1]) %>%
      tidyr::gather_("col", "value", paste0("V", 1:dims[2]))
    jpg_df$col <- as.numeric(sub("V", "", col))
    names(jpg_df)[3] <- vals[i]
    jpg_list[[i]] <- jpg_df
  }

  jpg_full <- purrr::reduce(jpg_list, dplyr::left_join)
  jpg_full$hex <- grDevices::rgb(jpg_full$r,
                                 jpg_full$g,
                                 jpg_full$b)

  return(jpg_full)
}


#' Set colour depth to either 8 or 24 bit
#'
#' @param jpg_df A tidy data frame - output from \code{jpg_df()}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#'
#' @return A tidy data frame with colour depth adjusted
#' @export
colour_depth <- function(jpg_df, depth) {
  if(!depth %in% c(8, 24)) {
    stop("The depth argument must be either 8 or 24")
  }
  if(depth == 8) {
    rg_vals <- 7
    b_vals <- 3
  } else {
    rg_vals <- 255
    b_vals <- 255
  }
  jpg_df$r <- round(jpg_df$r * rg_vals, 0) / rg_vals
  jpg_df$g <- round(jpg_df$g * rg_vals, 0) / rg_vals
  jpg_df$b <- round(jpg_df$b * b_vals, 0) / b_vals
  jpg_df$hex <- grDevices::rgb(jpg_df$r,
                    jpg_df$g,
                    jpg_df$b)
  return(jpg_df)
}

#' Randomly sample a jpg df to reduce points
#'
#' @param jpg_df A tidy data frame - output from \code{jpg_df()}
#' @param density Numeric, controls how many points are included
#'
#' @return A tidy data frame with a reduced number of rows
#' @export
sample_df <- function(jpg_df, density) {
  n <- ceiling(nrow(jpg_df) * density)
  jpg_df <- jpg_df[sample(nrow(jpg_df), n, replace = FALSE)]
  return(jpg_df)
}

#' Make a pointillist ggplot2 object
#'
#' @param jpg_df A tidy data frame - output from \code{jpg_df()}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#' @param density Numeric, controls how many points are included
#'
#' @return a ggplot2 object
#' @export
pointillise <- function(jpg_df, depth = 8, density = 0.1) {
  if(!all(names(jpg_df) == c("row", "col", "r", "g", "b", "hex"))) {
    stop("The jpg dataframe doesn't look right. It should be output from jpg_df()")
  }
  if(max(c(jpg_df$r, jpg_df$g, jpg_df$b)) > 1) {
    stop("The colour range in the jpg dataframe is too big. The dataframe should be output from jpg_df()")
  }

  jpg_df <- colour_depth(jpg_df, depth)

  coord_ratio <- max(jpg_df$row) / max(jpg_df$col)

  p_out <- ggplot2::ggplot(jpg_df, ggplot2::aes_string("col", "row")) +
    ggplot2::geom_point(colour = jpg_df$hex) +
    ggplot2::guides(colour = FALSE) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = coord_ratio)

  return(p_out)
}
