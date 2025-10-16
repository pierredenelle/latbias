# Controls #####################################################################
controls <- function(args = NULL, data = NULL,
                     type = "strict_positive_integer") {
  
  lstype <- c("strict_positive_integer",
              "character_or_positive_integer",
              "positive_numeric",
              "logical")
  
  if(!(type %in% lstype)){
    stop("Control type not defined!", call. = FALSE)
  }
  
  # Strict positive integer ###################################################
  if (type == "strict_positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
           call. = FALSE)
    } else {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
             call. = FALSE
        )
      } else {
        if (args <= 0) {
          stop(paste0(deparse(substitute(args)),
                      " must be strictly higher than 0."), call. = FALSE)
        }
      }
    }
  }
  
  # Character or positive integer ##############################################
  if (type == "character_or_positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.character(args) && !is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), 
                  " must be a character string or a positive integer."),
           call. = FALSE
      )
    }
    if (is.numeric(args)) {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)),
                    " must be a character string or a positive integer."),
             call. = FALSE
        )
      }
      if (args <= 0) {
        stop(paste0(deparse(substitute(args)),
                    " must be strictly higher than 0."), 
             call. = FALSE)
      }
    }
  }
  
  # Positive numeric ##########################################################
  if (type == "positive_numeric") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
           call. = FALSE)
    } else {
      if (args < 0) {
        stop(paste0(deparse(substitute(args)), " must be higher than 0."),
             call. = FALSE
        )
      }
    }
  }
  
  # Logical ###################################################################
  if (type == "logical") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
           call. = FALSE
      )
    }
    if (!is.logical(args) || is.na(args)) {
      stop(paste0(deparse(substitute(args)), " must be a logical."),
           call. = FALSE
      )
    }
  }
}

# Checking study_area argument
#
# Authors: Pierre Denelle
#
# Stop if study_area argument is not having the right format
#
# Args:
#   study_area  a shapefile class sfc, POLYGON or MULTIPOLYGON
#
# Returns:
#   shows an error message if needed

check_study_area <- function(study_area) {
  if(!is.null(study_area) && !"sfc" %in% class(study_area)){
    stop("'study_area' must be an object of class 'sfc', with a CRS
         set to WGS84 (EPSG: 4326).")
  }
  
  if(!is.null(study_area) && length(study_area) > 1){
    stop("study_area should have only one POLYGON.")
  }
  
  if(!is.null(study_area) && "sfc_POINT" %in% class(study_area)){
    stop("'study_area_polygon' should be of class POLYGON or MULTIPOLYGON.")
  }
  
  if(!is.null(study_area) && "sfc_MULTIPOINT" %in% class(study_area)){
    stop("'study_area_polygon' should be of class POLYGON or MULTIPOLYGON.")
  }
  
  if(!is.null(study_area) && "sfc_LINESTRING" %in% class(study_area)){
    stop("'study_area_polygon' should be of class POLYGON or MULTIPOLYGON.")
  }
  
  if(!is.null(study_area) && "sfc_MULTILINESTRING" %in% class(study_area)){
    stop("'study_area_polygon' should be of class POLYGON or MULTIPOLYGON.")
  }
  
  # EPSG should be 4326 (WGS84), if not, reprojection and warning message
  if(!is.null(study_area)){
    if(is.na(sf::st_crs(study_area))){
      warning("There is no CRS defined for your shapefile.
          We define it as being WGS 84 (EPSG 4326).")
      sf::st_crs(study_area) <- 4326
    } else if(sf::st_crs(study_area) != sf::st_crs(4326)){
      warning(paste0(
        "The CRS of the supplied shapefile is not equal to sf::st_crs(4326) as
        it should be.\n
        Your shapefile will be reprojected accordingly."))
      
      study_area <- sf::st_transform(study_area, crs = 4326)
    }
  }
  return(study_area)  
}
