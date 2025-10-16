#' LBI
#'
#' Computes the Latitudinal Bias Index (LBI) for a given shapefile, by
#' calculating the distance between two random locations within the shape
#' multiple times (see details).
#'
#' @param study_area_id Character string. Name ID of the study case area or
#' country name.
#' 
#' @param study_area_polygon Polygon shapefile. It should be a sfc object, of
#' class POLYGON or MULTIPOLYGON.
#' 
#' @param nobs Numeric. Number of random observations in each sample. 250 by
#' default.
#' 
#' @param nboot Numeric. Determines how many times the random shifts are
#' calculated. 1,000 by default.
#' 
#' @param fact_location Numeric. fact_location x nobs determine all the
#' possible coordinates that can be sampled within the provided polygon for all
#' bootstraps (faster than to generate a set of random location at each
#' bootstrap)
#' 
#' @param elevation Elevation raster. elevation in wgs84; if not provided,
#' NA will be returned for null-model elevational shifts.
#'
#' @param raw_output Logical. FALSE by default. If TRUE, all bootstraps
#' are returned as a data.frame. => say that the raw outputs are accessible
#'
#' @return
#' A data.frame or a list of two data.frames if raw_output is set to TRUE.
#'
#' @details The main output contains the following columns:
#' - study_area_id: ID or name of the study case region
#' - distance_km: average expected geographic distance shift between t1 and
#'  t2
#' - null_mod_SN_shift: average expected South-North shift between t1 and t2,
#' in absolute values
#' - null_mod_EW_shift: average expected East-West shift between t1 and t2,
#' in absolute values
#' - null_mod_elevation_shift: average expected elevation shift between t1 and
#'  t2, in absolute values
#' - LBI: the Latitudinal Bias Index value
#' 
#' LBI formula is
#' LBI = 2 x (mean(|Anlat/Anlon|)/(1+mean(|Anlat/Anlon|)-0.5)
#' with Anlat and Anlon denoting the geographic displacement of the centroid
#' positions of both sets of observation in the nth iteration by means in the
#'  latitudinal and the longitudinal dimension.
#' 
#' @references
#'      Sanczuk et al. submitted.
#'
#' @examples
#' 
#' study_area <- rnaturalearth::ne_countries(scale = 110, continent = "Europe",
#' country = "Sweden", type = "map_units", returnclass = "sf")
#' study_area <- sf::st_union(study_area)
#' 
#' LBI(study_area_id = "Sweden", study_area_polygon = study_area,
#' nobs = 10, nboot = 10, fact_location = 10, elevation = NULL)
#' \donttest{
#' # With elevation
#' elevation_df <- elevatr::get_elev_raster(
#' locations = sf::st_as_sf(study_area), z = 5)
#' }
#' 
#' @importFrom sf sf_use_s2 st_make_valid st_transform st_centroid st_bbox
#' @importFrom sf st_sample st_as_sf st_distance
#' @importFrom terra extract
#' @importFrom reshape2 melt
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom dplyr filter group_by summarize select
#' @importFrom tidyr pivot_wider
#' @importFrom units set_units
#' @importFrom geosphere bearingRhumb
#' 
#' @export

LBI <- function(study_area_id, study_area_polygon, nobs = 250, nboot = 1000,      
                fact_location = 10, elevation = NULL,
                raw_output = FALSE){
  
  # Control arguments
  controls(args = study_area_id, data = NULL,
           type = "character_or_positive_integer")
  study_area_polygon <- check_study_area(study_area = study_area_polygon)
  controls(args = nobs, data = NULL, type = "strict_positive_integer")
  controls(args = nboot, data = NULL, type = "strict_positive_integer")
  controls(args = fact_location, data = NULL, type = "strict_positive_integer")
  controls(args = raw_output, data = NULL, type = "logical")
  
  if(!is.null(elevation)){
    # if(is.na(elevation)){
    #   stop("elevation must be an unprojected (WGS84) RasterLayer.")
    # }
    if(as.character(class(elevation)) != "RasterLayer"){
      stop("class(elevation) != 'RasterLayer'
           elevation must be an unprojected (WGS84) RasterLayer.")
    }
  }
  
  # Visible binding for global variable
  survey <- elev <- X <- Y <- mean_X <- mean_Y <- km <- m <- NULL
  distance_km <- NS_dist <- EW_dist <- abs_elev <- p_lat <- NULL
  
  # Spherical geometry off
  sf::sf_use_s2(FALSE)
  
  # coordinate reference system
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Mollweide equal area CRS
  moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"
  
  # Defining local projection
  # adjust projection to centroid of study area
  local_centre <- sf::st_make_valid(study_area_polygon)
  # to equal area for exact centroid estimation
  local_centre <- sf::st_transform(local_centre, crs = moll) 
  local_centre <- sf::st_centroid(local_centre)
  local_centre <- sf::st_transform(local_centre, wgs84)
  local_centre <- sf::st_bbox(local_centre)
  local_centre <- round(local_centre)  
  
  # reproject study area to LAEA with origin = the study area centroid
  proj <- paste0("+proj=laea +x_0=0 +y_0=0 +lon_0=",
                 local_centre[1], " +lat_0=", local_centre[2])
  
  # generate random points
  # define pool of location that can be sampled from
  suppressMessages({
    random_points <- 
      sf::st_sample(study_area_polygon, nobs * 2 * fact_location)
  })
  random_points <- sf::st_as_sf(random_points)
  random_points <- sf::st_transform(random_points, wgs84)
  
  # add ID info
  random_points$study_area_id <- study_area_id
  
  # If elevation is provided, extract elevation data for each location
  random_points$elev <- ifelse(class(elevation) == "RasterLayer",
                               terra::extract(elevation, random_points), NA)

  # project to local projection to avoid spatial distortion
  random_points <- sf::st_transform(random_points, proj) 
  
  # divide records into bootstrap replicates
  # sample observation at t1 and t2 for each bootstrap replicate.
  # NA are locations not sampled
  random_points <- 
    cbind(random_points,
          data.frame(
            subset =
              sapply(1:nboot,
                     FUN = function(x){
                       sample(c(rep(c("t1", "t2"),
                                    nobs), # sample subset locations at t1 & t2
                                rep(NA, 2*nobs*fact_location - 2*nobs)),
                              replace = FALSE)
                     })))
  
  # make spatialPointsDataFrame
  coords <- sf::st_coordinates(random_points)
  coords <- data.frame(coords)
  
  # add lat and lon data
  random_points$X <- coords$X
  random_points$Y <- coords$Y
  
  random_points <- sp::SpatialPointsDataFrame(
    coords = cbind(random_points$X, random_points$Y), 
    data.frame(random_points[, c(colnames(random_points) != "x")]), 
    proj4string = sp::CRS(proj))
  random_points <- sf::st_as_sf(random_points)
  
  random_points_no_geom <- random_points
  sf::st_geometry(random_points_no_geom) <- NULL
  
  # reshape data frame to long format
  if(!is.null(elevation)){
    # param_positions <- melt(random_points,
    #                         id.vars = c("geometry", "study_area_id", "elev", "X",
    #                                     "Y"),
    #                         variable.name = "rep",
    #                         value.name = "survey")
    param_positions <- tidyr::pivot_longer(random_points_no_geom,
                                           cols = dplyr::contains("subset"), #c("elev", "X", "Y"),
                                           names_to = "rep",
                                           values_to = "survey")
  } else{
    param_positions <- tidyr::pivot_longer(random_points_no_geom,
                                           cols = dplyr::contains("subset"), #c("X", "Y"),
                                           names_to = "rep",
                                           values_to = "survey")
  }
  
  # only keep locations that were sampled within each replicate
  param_positions <- dplyr::filter(param_positions, survey != "NA")
  
  # calculate the elevation and centroid at for each bootstrap replicate for
  # surveys t1 and t2 
  param_positions <- dplyr::group_by(param_positions, study_area_id, rep,
                                     survey)
  
  # mean elevation across 250 observations of t1 and t2. Used to calculate
  # random elevational changes due to topography across the study area
  if(!is.null(elevation)){
    param_positions <- dplyr::summarize(param_positions,
                                        elev = mean(elev, na.rm = TRUE), 
                                        mean_X = mean(X, na.rm = TRUE), 
                                        mean_Y = mean(Y, na.rm = TRUE))
  } else{
    param_positions <- dplyr::summarize(param_positions,
                                        mean_X = mean(X, na.rm = TRUE), 
                                        mean_Y = mean(Y, na.rm = TRUE))
  }
  # spatial points dataframe            
  param_positions <- sp::SpatialPointsDataFrame(
    coords = cbind(param_positions$mean_X, param_positions$mean_Y), 
    param_positions, 
    proj4string = sp::CRS(proj)) 
  param_positions <- sf::st_as_sf(param_positions)
  
  param_positions <- as.data.frame(param_positions)
  param_positions <- dplyr::select(param_positions, -mean_X, -mean_Y)
  if(!is.null(elevation)){
    param_positions <- tidyr::pivot_wider(param_positions,
                                          names_from = "survey",
                                          values_from = c("geometry", "elev")) 
  } else{
    param_positions <- tidyr::pivot_wider(param_positions,
                                          names_from = "survey",
                                          values_from = c("geometry")) 
  }
  
  # Rename columns (needed when elevation is NULL)
  if("t1" %in% colnames(param_positions)){
    colnames(param_positions)[colnames(param_positions) == "t1"] <-
      "geometry_t1"
    colnames(param_positions)[colnames(param_positions) == "t2"] <-
      "geometry_t2"
  }
  
  # Absolute distance
  param_positions$distance_km <-
    sf::st_distance(param_positions$geometry_t1,
                    param_positions$geometry_t2,
                    by_element = TRUE)
  
  param_positions$distance_km <-
    units::set_units(param_positions$distance_km, km)
  if(!is.null(elevation)){
    param_positions$abs_elev <- abs(param_positions$elev_t1 -
                                      param_positions$elev_t2)
    param_positions$abs_elev <- units::set_units(param_positions$abs_elev, m)
  }
  
  # Bearing Rhumb
  param_positions_bearing_t1 <- sf::st_transform(param_positions$geometry_t1,
                                                 crs = wgs84)
  param_positions_bearing_t1 <- sf::st_coordinates(param_positions_bearing_t1)
  param_positions_bearing_t2 <- sf::st_transform(param_positions$geometry_t2,
                                                 crs = wgs84)
  param_positions_bearing_t2 <- sf::st_coordinates(param_positions_bearing_t2)
  
  param_positions$bearing <-
    geosphere::bearingRhumb(param_positions_bearing_t1, 
                            param_positions_bearing_t2)
  
  # Calculate projected South-North and East-West distances
  param_positions$NS_dist <- abs(param_positions$distance_km *
                                   cos((param_positions$bearing)*(pi/180)))
  param_positions$EW_dist <- abs(param_positions$distance_km *
                                   sin((param_positions$bearing)*(pi/180)))
  
  # Saving all bootstraps when raw_output is TRUE
  if(isTRUE(raw_output)){
    all_bootstraps <- param_positions
  }
  
  # Summarize data
  param_positions_summary <- dplyr::group_by(param_positions, study_area_id)
  if(!is.null(elevation)){
    param_positions_summary <- dplyr::summarize(
      param_positions_summary,
      distance_km = mean(distance_km, na.rm = TRUE),
      # null-model expectation on absolute south-north shifts
      null_mod_SN_shift = as.numeric(mean(NS_dist, na.rm = TRUE)),
      # null-model expectation on absolute west-east shifts
      null_mod_EW_shift = as.numeric(mean(EW_dist, na.rm = TRUE)),
      # null-model expectation on absolute elevational shifts
      null_mod_elevation_shift = mean(abs_elev, na.rm = TRUE)                   
    )
  } else{
    param_positions_summary <- dplyr::summarize(
      param_positions_summary,
      distance_km = mean(distance_km, na.rm = TRUE),
      # null-model expectation on absolute south-north shifts
      null_mod_SN_shift = as.numeric(mean(NS_dist, na.rm = TRUE)),
      # null-model expectation on absolute west-east shifts
      null_mod_EW_shift = as.numeric(mean(EW_dist, na.rm = TRUE))                  
    )
  }
  
  # Calculate LBI
  param_positions_summary$p_lat <-
    (param_positions_summary$null_mod_SN_shift/
       param_positions_summary$null_mod_EW_shift ) /
    (1 + param_positions_summary$null_mod_SN_shift/
       param_positions_summary$null_mod_EW_shift )
  param_positions_summary$LBI <- (param_positions_summary$p_lat -0.5 ) * 2
  
  param_positions_summary <- dplyr::select(param_positions_summary, -p_lat)
  
  # Convert as data.frame
  param_positions_summary <- as.data.frame(param_positions_summary)
  
  if(isTRUE(raw_output)){
    return(list(all = all_bootstraps,
                summary = param_positions_summary))
  } else{
    return(param_positions_summary)
  }
}
