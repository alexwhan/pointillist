
#' Make a data frame of img data
#'
#' @param file A file path
#' @param type File type - either "png" or "jpg"
#'
#' @return A tidy dataframe describing r, g, b and hex values
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")
#'
#' png_df <- img_df(img_path)
img_df <- function(file, type = NULL) {
  if(is.null(type)) {
    ext <- tools::file_ext(file)
    if(ext %in% c("jpg", "jpeg", "JPG", "JPEG")) {
      type <- "jpg"
    } else
      if(ext %in% c("png", "PNG")) type <- "png"
  }
  if(type == "jpg") img <- jpeg::readJPEG(file)
  if(type == "png") img <- png::readPNG(file)

  img_list <- vector(mode = "list", length = 3)

  vals <- c("r", "g", "b")

  for(i in 1:3) {
    dims <- dim(img[,,i])
    img_df <- as.data.frame(img[,,i])
    img_df$row <- 1:dims[1]
    img_df <- img_df %>%
      tidyr::gather_("col", "value", paste0("V", 1:dims[2]))
    img_df$col <- as.numeric(sub("V", "", img_df$col))
    names(img_df)[3] <- vals[i]
    img_list[[i]] <- img_df
  }

  img_full <- purrr::reduce(img_list, dplyr::left_join)
  img_full$hex <- grDevices::rgb(img_full$r,
                                 img_full$g,
                                 img_full$b)

  return(img_full)
}


#' Set colour depth to either 8 or 24 bit
#'
#' @param img_df A tidy data frame - output from \code{img_df()}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#'
#' @return A tidy data frame with colour depth adjusted
#' @export
#'
#' @examples
#' img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")
#'
#' png_df <- img_df(img_path)
#' png_df_8bit <- colour_depth(png_df, 8)
colour_depth <- function(img_df, depth) {
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
  img_df$r <- round(img_df$r * rg_vals, 0) / rg_vals
  img_df$g <- round(img_df$g * rg_vals, 0) / rg_vals
  img_df$b <- round(img_df$b * b_vals, 0) / b_vals
  img_df$hex <- grDevices::rgb(img_df$r,
                    img_df$g,
                    img_df$b)
  return(img_df)
}

#' Randomly sample a img df to reduce points
#'
#' @param img_df A tidy data frame - output from \code{img_df()}
#' @param frac Numeric, between 0 and 1, fraction of total points to be included
#'
#' @return A tidy data frame with a reduced number of rows
#' @export
#'
#' @examples
#' img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")
#'
#' png_df <- img_df(img_path)
#' nrow(png_df)
#' png_df_sample <- sample_df(0.3)
#' nrow(png_df_sample)
sample_df <- function(img_df, frac) {
  n <- ceiling(nrow(img_df) * frac)
  img_df <- img_df[sample(nrow(img_df), n, replace = FALSE),]
  return(img_df)
}

#' Make a pointillist ggplot2 object
#'
#' @param img_df A tidy data frame - output from \code{img_df()}
#' @param depth Numeric, describing colour depth - either 8 or 24 (bit)
#' @param point_range Numeric vector defining the min and max point size
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")
#'
#' png_df <- img_df(img_path)
#' png_df <- colour_depth(png_df, 8)
#' png_df_sample <- sample_df(png_df, 0.3)
#' seurat <- pointillise(png_df_sample)
#' seurat
pointillise <- function(img_df, point_range = c(1, 2)) {
  if(!all(names(img_df) == c("row", "col", "r", "g", "b", "hex"))) {
    stop("The img dataframe doesn't look right. It should be output from img_df()")
  }
  if(max(c(img_df$r, img_df$g, img_df$b)) > 1) {
    stop("The colour range in the img dataframe is too big. The dataframe should be output from img_df()")
  }

  coord_ratio <- max(img_df$row) / max(img_df$col)

  sizes <- sample(seq(from = point_range[1],
                      to = point_range[2],
                      by = 0.1),
                nrow(img_df),
                replace = TRUE)

  p_out <- ggplot2::ggplot(img_df, ggplot2::aes_string("col", "row")) +
    ggplot2::geom_point(colour = img_df$hex, size = sizes) +
    ggplot2::guides(colour = FALSE) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = coord_ratio)

  return(p_out)
}

#' Animate ggplot pointillism
#'
#' @param img_df A tidy data frame - output from \code{img_df()}
#' @param output_file the file name of the gif
#' @param nframes Integer - the number of frames in the animation
#' @param interval Numeric - the interval between frames in seconds
#'
#' @return An animated gif file
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' ## ImageMagick must be installed on your system
#' img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")
#'
#' png_df <- img_df(img_path)
#' png_df <- colour_depth(png_df, 8)
#' png_df_sample <- sample_df(png_df, 0.3)
#' pointillist_gif(png_df_sample, "seurat.gif", 5, 0.2)
pointillist_gif <- function(img_df, output_file, nframes, interval) {
  coord_ratio <- max(img_df$row) / max(img_df$col)

  sample_seq <- ceiling(seq(from = 1, to = nrow(img_df), length.out = nframes))

  sample_df_list <- sample_seq %>%
    purrr::map(~png_df_sample[1:.x,])

  sample_df_all <- sample_df_list %>%
    dplyr::bind_rows(.id = "frame")
  sample_df_all$frame <- as.numeric(sample_df_all$frame)

  saveGIF(
    for(i in unique(sample_df_all$frame)) {
      p <- ggplot2::ggplot(sample_df_all[sample_df_all$frame == i,],
                           ggplot2::aes_string("col", "row")) +
        ggplot2::geom_point(colour = sample_df_all$hex[sample_df_all$frame == i]) +
        ggplot2::scale_y_reverse() +
        ggplot2::coord_fixed(ratio = coord_ratio)
      plot(p)
    },
    interval = interval,
    movie.name = output_file,
    ani.width = max(sample_df_all$col) * 1.2,
    ani.height = max(sample_df_all$row) * 1.2
  )

}
