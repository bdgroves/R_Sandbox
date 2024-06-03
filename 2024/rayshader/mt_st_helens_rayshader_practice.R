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
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, phi = 45, windowsize = c(1000, 800))

#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

#And here we add an ambient occlusion shadow layer, which models 
#lighting from atmospheric scattering:

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)
render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")
render_snapshot(clear=TRUE)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "lightblue") %>%
  add_shadow(cloud_shade(elmat, zscale = 10, start_altitude = 500, end_altitude = 1000,), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800),
          background="black")
render_camera(theta = 20, phi=40,zoom= 0.64, fov= 56 )

render_clouds(elmat, zscale = 10, start_altitude = 800, end_altitude = 1000, attenuation_coef = 2, clear_clouds = T)
render_snapshot(clear=TRUE)