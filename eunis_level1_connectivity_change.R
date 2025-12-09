# Remove and clean up envirionment
rm(list = ls())
gc()

# import libraries
library(terra)

terraOptions(memmax = 50000 , memfrac = 0.6, tempdir = "C:\\temp", todisk = TRUE) # Set memory fraction and progress bar


# Define baseline path
base_line_path <- "C:\\EUNIS"

# Year to be assess
year_1 <- 2000
year_2 <- 2018
group <- "T"

# Load land cover data
group_dir_2000 <- file.path(base_line_path, year_1, group)
group_dir_2018 <- file.path(base_line_path, year_2, group)

# List all the files 
meff_T_y2000 <- list.files(
  group_dir_2000, 
  pattern = "*.tif$", 
  full.names = TRUE)

meff_T_y2018 <- list.files(
  group_dir_2018, 
  pattern = "*.tif$", 
  full.names = TRUE)

# check if all files are included
length(meff_T_y2000)
length(meff_T_y2018)

# Create empty layer for 2000
T_2000 <- NULL

# Loop thourhg each map and obtain the max values
for (i in seq_along(meff_T_y2000)){
  r <- rast(meff_T_y2000[i])
  r[r==0] <- NA
  if(is.null(T_2000)){
    T_2000 <- r
  } else {
    T_2000 <- max(T_2000, r, na.rm = TRUE)
  }
}

# Write raster to avoid recalculation
outdir <- "S:\\Emmanuel_OcegueraConchas\\forest_conn_change"
fileName <- file.path(outdir, paste0("EUNIS_T_", year_1, ".tif"))

# Write raster to avoid recalculation
writeRaster(
  T_2018, 
  filename = fileName, 
  overwrite=TRUE,
  gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

# Open dataset
T_2000 <- rast(file.path(outdir, "EUNIS_T_2000.tif"))


# Create empty layer for 2018
T_2018 <- NULL

for (i in seq_along(meff_T_y2018)){
  cat("Processing", i, "of", length(meff_T_y2018), ":\n")
  r <- rast(meff_T_y2018[i])
  cat("Year 2018 – processing", i, "of", length(meff_T_y2018), ":\n")
  r[r==0] <- NA
  if(is.null(T_2018)){
    T_2018 <- r
  } else {
    T_2018 <- max(T_2018, r, na.rm = TRUE)
  }
}

# Write to avoid recalculation
outdir <- "S:\\Emmanuel_OcegueraConchas\\forest_conn_change"
fileName <- file.path(outdir, paste0("EUNIS_T_", year_2, ".tif"))

# Write raster to avoid recalculation
writeRaster(
  T_2018, 
  filename = fileName, 
  overwrite=TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))


# Open dataset
T_2018 <- rast(file.path(outdir, "EUNIS_T_2018.tif"))

# Check if they have same ext
ext(T_2018)
ext(T_2000)

# Create delta
# By how much did the connectivity increase or decrease from 2000 to 2018?
delta_T <- T_2018 - T_2000
#plot(delta_T)

# Save to avoid recompute
writeRaster(
  delta_T, 
  file.path(outdir, "T_meffpw_norm_delta_2018_2000_level1.tif"),
  overwrite = TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))

# Open delta
delta_T <- rast(file.path(outdir, "T_meffpw_norm_delta_2018_2000_level1.tif"))



# Reclassify
delta <- delta_T
delta_class <- delta
qg <- global(r, quantile)



delta_class[delta < -0.20] <- 1  # strong loss
delta_class[delta >= -0.20 & delta < -0.05]   <- 2  # moderate loss
delta_class[delta > -0.05 & delta <= 0.05]    <- 3  # stable
delta_class[delta > 0.05 & delta <= 0.20]     <- 4  # moderate gain
delta_class[delta > 0.20]   <- 5


plot(delta_class,
     col = c(
       "#a50026",  # strong loss
       "#f46d43",  # moderate loss
       "#ffffbf",  # stable
       "#abdda4",  # moderate gain
       "#1a9850"   # strong gain
     ),
     main = "Change in structural forest connectivity (EUNIS T, 2018–2000)"
)

# Write the final output
writeRaster(
  delta_class, 
  file.path(outdir, "T_meffpw_norm_delta_2018_2000_level1_class.tif"),
  overwrite = TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))



# Best-case structural connectivity of any forest type present here.
# ΔT_max = T_max_2018 − T_max_2000
# → Change in best available forest connectivity at Level 1.


###############################################################################
# We do the same calculation but we use the min value for the aggregation step

# “Worst-case structural connectivity among all forest types present here.”
# ΔT_min = T_min_2018 − T_min_2000
# → Change in worst-connected forest type where multiple forests coexist.


T_2000_min <- NULL

for (i in seq_along(meff_T_y2000)){
  cat("Processing", i, "of", length(meff_T_y2000), ":\n")
  r <-  rast(meff_T_y2000[i])
  r[r==0] <- NA
  if (is.null(T_2000_min)){
    T_2000_min <- r
  } else {
    T_2000_min <- min(T_2000_min, r, na.rm = TRUE)
  }
}

# Write to avoid recalculation
outdir <- "S:\\Emmanuel_OcegueraConchas\\forest_conn_change"
fileName_min <- file.path(outdir, paste0("EUNIS_T_", year_1, "min.tif"))

# Write raster to avoid recalculation
writeRaster(
  T_2000_min, 
  filename = fileName_min, 
  overwrite=TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))



T_2018_min <- NULL

for (i in seq_along(meff_T_y2018)){
  cat("Processing", i, "of", length(meff_T_y2018), ":\n")
  r <- rast(meff_T_y2018[i])
  r[r==0] <- NA
  if (is.null(T_2018_min)){
    T_2018_min <- r
  } else {
    T_2018_min <- min(T_2018_min, r, na.rm = TRUE)
  }
}

# Write to avoid recalculation
outdir <- "S:\\Emmanuel_OcegueraConchas\\forest_conn_change"
fileName_min <- file.path(outdir, paste0("EUNIS_T_", year_2, "_min.tif"))

# Write raster to avoid recalculation
writeRaster(
  T_2018_min, 
  filename = fileName_min, 
  overwrite=TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))


# Open dataset
T_2018_min <- rast(file.path(outdir, "EUNIS_T_2018_min.tif"))
T_2000_min <- rast(file.path(outdir, "EUNIS_T_2000_min.tif"))

# Check if they have same ext
ext(T_2018_min)
ext(T_2000_min)

# Create delta
# By how much did the connectivity increase or decrease from 2000 to 2018?
delta_T_min <- T_2018_min - T_2000_min
#plot(delta_T_min)

# Save to avoid recompute
writeRaster(
  delta_T_min, 
  file.path(outdir, "T_meffpw_norm_delta_2018_2000_level1_min.tif"),
  overwrite = TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))

# Reclassify
delta_min <- delta_T_min
delta_class_min <- delta_min
# terra::plot(delta_class_min)


# Ask Nestor about the classification, which thresholds to use
delta_class_min[delta_min < -0.20] <- 1  # strong loss
delta_class_min[delta_min >= -0.20 & delta_min < -0.05]   <- 2  # moderate loss
delta_class_min[delta_min > -0.05 & delta_min <= 0.05]    <- 3  # stable
delta_class_min[delta_min > 0.05 & delta_min <= 0.20]     <- 4  # moderate gain
delta_class_min[delta_min > 0.20]   <- 5

plot(delta_class,
     col = c(
       "#a50026",  # strong loss
       "#f46d43",  # moderate loss
       "#ffffbf",  # stable
       "#abdda4",  # moderate gain
       "#1a9850"   # strong gain
     ),
     main = "Change in structural forest connectivity (EUNIS T, 2018–2000)"
)

# Write the final output
writeRaster(
  delta_class_min, 
  file.path(outdir, "T_meffpw_norm_delta_2018_2000_level1_min_class.tif"),
  overwrite = TRUE,
  gdal=  c("COMPRESS=DEFLATE", "TFW=YES"))

# Best-case structural connectivity of any forest type present here.
# ΔT_max = T_max_2018 − T_max_2000
# → Change in best available forest connectivity at Level 1.


#################################################################################
# plotting the data in a very nice way and high quality for publication

# install libraries
install.packages("rasterVis")
library(devtools)
devtools::install_github("nschiett/fishualize", force = TRUE)

system("R")
library(ggplot2)
library(viridis)
library(sf)
library(cols4all)
library(RColorBrewer)

# Load data 
eu <- st_read("I:/biocon/Emmanuel_Oceguera/projects/Fragmentation_analysis/data/eu_mask_single.shp")
eu <- st_read("I:/biocon/Emmanuel_Oceguera/projects/Fragmentation_analysis/data/eea_v_3035_100_k_adm-boundaries-eea38-plus_i_2018-2020_v01_r00/NUTS2021_3035.shp")
r_stack_2000 <- rast(meff_T_y2000)

# Defina the global min and max across all layers
mins <- numeric(nlyr(r_stack_2000))
maxs <- numeric(nlyr(r_stack_2000))

for(i in seq_len(nlyr(r_stack_2000))) {
  cat("Processing layer", i, "of", nlyr(r_stack_2000), "\n")
  lyr <- r_stack_2000[[i]]
  g <- global(lyr, c("min", "max"), na.rm=TRUE)
  mins[i] <- g$min
  maxs[i] <- g$max
}

# Global range across all layers
r_min <- min(mins)
r_max <- max(maxs)
# Alternatively, set manually
r_min <-  0
r_max <-  0.9403

# Short names for plotting
full_names  <- names(r_stack_2000)
short_names <- sub("_.*", "", full_names)

# colour palette (change if you like)
cols <- rev(terrain.colors(100))

# Create the plot first before saving and putting in the pages
r_stack <- rast(meff_T_y2000)
lyrs <- nlyr(r_stack)

# Define short names
short_names <- sub("_.*", "", names(r_stack))  # T11, T12, ...

df_list <- vector("list", lyrs)

for (i in seq_len(lyrs)) {
  cat("Converting layer", i, "of", lyrs, "\n")
  
  r <- r_stack[[i]]
  
  # aggregate ONLY for plotting – this is what saves you from bad_alloc
  r_small <- aggregate(r, fact = 10, fun = "mean", na.rm = TRUE)  # 100m->1km, change fact if needed
  
  d <- as.data.frame(r_small, xy = TRUE, na.rm = FALSE)
  names(d) <- c("x", "y", "value")
  
  # 0 -> NA by filtering
  d <- d[!is.na(d$value) & d$value > 0, ]
  
  d$layer <- short_names[i]
  
  df_list[[i]] <- d
}

df_all <- do.call(rbind, df_list)
head(df_all)

# Save the dataframe to avoid recomputation
saveRDS(df_all, file = file.path(outdir, "df_meff_T_2000.rds"))

# Read the dataframe
df_all <- readRDS(file.path(outdir, "df_meff_T_2000.rds"))



df_all$layer <- factor(df_all$layer, levels = sort(unique(df_all$layer)))
df_all[df_all$layer == short_names[1], ]

# We subset only 5 layers-habitats for testing
test <- df_all[df_all$layer %in% short_names[1:5], ]
unique(test$layer)

make_page_plot <- function(data_frame) {
  ggplot() +
    geom_sf(
      data = eu, 
      fill = "#f0f0f0",
      color = "#a1a1a1", 
      linewidth = 0.2
      ) +
      # Raster tiles
    geom_raster(
      data = data_frame,
      aes(x = x, y = y, fill = value),
      na.rm = TRUE
      ) +
      # Colour scale
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      limits = c(0, 0.94),
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(8, "cm"),
        barheight = unit(0.4, "cm")
      )
    ) + 
      # Set coordinate system and zoom extent
    coord_sf(
      xlim = c(2485648, 5915765), 
      ylim = c(1240572, 5445681),
      expand = FALSE
    ) +
      # Configure theme
    theme_minimal() +
    theme(
      panel.grid = element_blank(),

      # remove axis repetition
      axis.text  = element_blank(),
      axis.ticks = element_blank(),

      # keep axis titles
      axis.title.x = element_text(size = 10, margin = margin(t = 6)),
      axis.title.y = element_text(size = 10, margin = margin(r = 6)),

      # strip styling
      strip.background = element_rect(fill = "#D9D9D9", colour = "grey60"),
      strip.text       = element_text(face = "bold", size = 9),

      # spacing between maps
      panel.spacing = unit(0.8, "lines"),

      # global legend
      legend.position = "top"
    ) +
      # Set labels
    labs (
      x = "Longitude",
      y = "Latitude",
      title    = "Structural Forest Connectivity",
      subtitle = "EUNIS T Level 1 - Year 2000",
      fill    = "Connectivity"
    ) +
      # Facet by layer
    facet_wrap(~ layer, nrow = 5, ncol = 4)
}
 
 
out_dir_plot <- paste0(outdir, "/T_meffpw_2000_pages")
dir.create(out_dir_plot, recursive = TRUE, showWarnings = FALSE)

# all unique layer names, already in df_all
layers <- levels(df_all$layer)

layer_chunks <- split(layers, ceiling(seq_along(layers) / 20))

for (i in seq_along(layer_chunks)) {
  df_i <- df_all[df_all$layer %in% layer_chunks[[i]], ]
  p_i  <- make_page_plot(df_i)
  
  ggsave(
    file.path(out_dir_plot, sprintf("T_meffpw_2000_page_v4_%02d.png", i)),
    p_i,
    width  = 11,
    height = 14,
    dpi    = 1000
  )
}




 # Save the plot DPI 800
ggsave(
    filename = file.path(outdir, "EUNIS_T_meff_2000_test.png"),
    plot     = p,
    width    = 11,
    height   = 14,
    dpi      = 800
  )
    








##### The below code is using the rasterVis package to create levelplots and save them as PNG files
# Lay out
nl <- nlyr(r_stack_2000)
nrow <- 4
ncol <- 5
per_page <- nrow * ncol
n_pages  <- ceiling(nl / per_page)

out_dir <- "C:/EUNIS/plots/T/2000"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (p  in seq_len(n_pages)) {
  idx_start <- (p - 1) * per_page + 1
  idx_end <- min(p * per_page, nl)
  idx <- idx_start:idx_end
  
  fname <- file.path(out_dir, sprintf("T_meff_2000_page_%02d.png", p))

  png(fname, width = 3000, height = 2400, res = 300)

  print(
    levelplot(
      r_stack_2000[[idx]],
      layout = c(ncol, nrow),
      scales = list(draw = FALSE),
      margin = FALSE,
      par.settings = rasterTheme(region = cols),
      at = seq(r_min, r_max, length.out = 101),
      names.attr   = short_names[idx], ,
      colorkey = list(space = "right")
    )
  )
  dev.off()
}


