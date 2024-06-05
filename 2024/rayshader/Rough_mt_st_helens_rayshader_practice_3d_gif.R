library(rayshader)
library(raster)
library(rgl)

# Replace with your actual file path
zip_file <- "C:/Users/v-brooksg/Downloads/tifs.zip"

# Load the zip file
unzip_dir <- tempdir()
unzip(zip_file, exdir = unzip_dir)

# Assuming the unzipped files are GeoTIFF files, load them
tif_files <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE)

# Load the first GeoTIFF file
r <- raster(tif_files[1])

# Open a new 3D rendering context
rgl::open3d()

# Plot the raster with rayshader
elmat <- raster_to_matrix(r)

#elmat %>%
#  plot_3d(elmat, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
    #    zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
#render_snapshot(title_text = "Test",
         #       title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(elmat), color = "lightblue") %>%
  add_shadow(cloud_shade(elmat, zscale = 10, start_altitude = 50, end_altitude = 1000,), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.5, phi = 45, windowsize = c(1000, 800),
          background="black")
render_camera(theta = 20, phi=40,zoom= 0.5, fov= 56 )

save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}



transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}

# calculate input vectors for gif frames
n_frames <- 180
waterdepths <- transition_values(from = 0, to = min(elmat), steps = n_frames) 
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
# generate gif
zscale <- 50

elmat %>% 
  sphere_shade(texture = "imhof1", zscale = zscale) %>%
  add_shadow(ambient_shade(elmat, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(elmat, zscale = zscale, lambert = TRUE), 0.5) %>%
  save_3d_gif(elmat, file = "elmat.gif", duration = 6,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "imhof3", wateralpha = 0.8, 
              waterlinecolor = "#ffffff", waterlinealpha = 0.5,
              waterdepth = waterdepths/zscale, 
              theta = thetas, phi = 45)
