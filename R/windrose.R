#' windrose
#'
#' Plot the directional shifts for a given bootstrap.
#'
#' @param data data frame with bearing and speed
#' 
#' @param spd speed column
#' 
#' @param dir bearing column
#' 
#' @param spdres = 30, #adapt here the legend and breaks
#' 
#' @param dirres = 30,  #resolution of bearings
#' 
#' @param spdmin = 0, #and here min
#' 
#' @param spdmax = 150,#and here max; check as max
#' 
#' @param spdseq = NULL
#' 
#' @param palette = "Spectral"
#' 
#' @param countmax = NA
#' 
#' @param debug = 
#' 
#' 
#' @return
#' A plot.
#'
#' @details Windrose.
#' 
#' @references
#'      Sanczuk et al. submitted.
#'
#' @examples
#' 
#' \donttest{
#' study_area <- rnaturalearth::ne_countries(scale = 110, continent = "Europe",
#' country = "Sweden", type = "map_units", returnclass = "sf")
#' study_area <- sf::st_union(study_area)
#' po <- LBI(study_area_id = "Sweden", study_area_polygon = study_area,
#' nobs = 10, nboot = 10, fact_location = 10, elevation = NA,
#' bootstrap_output = TRUE)
#' test <- as.data.frame(po$all[, c("study_area_id", "abs_distance_km",
#' "bearing", "rep")])
#' test$abs_distance_km <- as.numeric(test$abs_distance_km)
#' pop <- windrose(data = test, spd = "abs_distance_km", dir = "bearing")
#' }
#' 
#' @importFrom psych r.test
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_discrete coord_polar
#' @importFrom ggplot2 scale_fill_manual element_blank guide_legend labs
#' @importFrom ggplot2 theme_bw theme element_line element_text ylim waiver
#' @importFrom utils packageVersion
#' @importFrom grDevices colorRampPalette
#' @importFrom stats na.omit
#' 
#' @export

windrose <- function(data, spd, dir, spdres = 30, dirres = 30, spdmin = 0,
                          spdmax = 150, spdseq = NULL, palette = "Spectral",
                          countmax = NA, debug = 0){
  
  controls(args = spdres, data = NULL, type = "positive_numeric")
  controls(args = dirres, data = NULL, type = "positive_numeric")
  controls(args = spdmin, data = NULL, type = "positive_numeric")
  controls(args = spdmax, data = NULL, type = "positive_numeric")
  
  if(spdmin > spdmax){
    stop("spdmin should be inferior to spdmax.")
  }
  
  if(!(palette %in% rownames(RColorBrewer::brewer.pal.info))){
    stop("palette should be one of the palettes available in RColorBrewer.
         Run rownames(RColorBrewer::brewer.pal.info) to see available options.")
  }
  
  # Visible binding for global variable
  spd.binned <- waiver <- NULL
  
  #
  # ral.r <- psych::r.test(na.omit(dir), degree = TRUE)
  
  # Look to see what data was passed in to the function
  # if(is.numeric(spd) & is.numeric(dir)){
  #   # assume that we've been given vectors of the speed and direction vectors
  #   data <- data.frame(spd = spd,
  #                      dir = dir)
  #   spd <- "spd"
  #   dir <- "dir"
  # }else if (exists("data")){
  #   # Assume that we've been given a data frame, and the name of the speed 
  #   # and direction columns. This is the format we want for later use.    
  # } 
  
  # > out_dat[out_dat$country == "GMB",]
  # country  abs_dist      bear nrep
  # 2190     GMB 29.815198 265.81020    1
  # 2191     GMB 25.327875  67.73514    2
  # 2192     GMB 17.664284 222.07720    3
  # 2193     GMB 10.640191 289.13077    4
  # GA = plot.windrose(spd = out_dat[out_dat$country == "GMB",]$abs_dist,
  #                    dir =  out_dat[out_dat$country == "GMB",]$bear,
  #                    spdres = 400, #adapt here the legend and breaks
  #                    dirres = 30,  #resolutie van bearings
  #                    spdmin = 0, #and here min
  #                    spdmax =  400 
  
  # Tidy up input data --------------------------------------------------------
  n.in <- NROW(data)
  
  # Control for NA in data
  
  # dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  # data[[spd]][dnu] <- NA
  # data[[dir]][dnu] <- NA
  
  # Figure out the wind speed bins --------------------------------------------
  if(missing(spdseq)){
    spdseq <- seq(spdmin, spdmax, spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- rev(colorRampPalette(brewer.pal(min(max(3, n.colors.in.range),
                                                    min(9, n.colors.in.range)),                                               
                                                palette))(n.colors.in.range))
  
  if(max(data[[spd]], na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]), "-",
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax, "-", max(data[[spd]], na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]), "-",
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]], breaks = spd.breaks,
                         labels = spd.labels, ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2, "-", dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2, "-", dirres/2))
  
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required -----------------------------------------------------
  if (debug > 0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    data$spd.binned <- with(data, factor(spd.binned,
                                         levels = rev(levels(spd.binned))))
    spd.colors <- rev(spd.colors)
  }
  
  # data$dir.binned = as.character(data$dir.binned)
  #  for (d in as.character(data$dir.binned))  {
  #   
  #   if (d %in% unique(data$dir.binned)) {
  #    #print (d)
  #   new_d = strsplit(d, " ")
  #    new_d = paste0(new_d[[1]][1], " ", new_d[[1]][2], "\n", new_d[[1]][3])
  #    data[data$dir.binned == d,]$dir.binned = new_d
  #    }
  #  }
  
  # Create the plot -----------------------------------------------------------
  p.windrose <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = ggplot2::waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = element_blank(), # "distance of \nshift (km)", 
                      values = spd.colors,
                      drop = FALSE,
                      guide = guide_legend(reverse = TRUE) ) +
    labs(y = "             N replicates") +
    #xlab( paste0("Rayleigh's r = ",round(ral.r$r.bar, 2),
    # ifelse(ral.r$p.value < 0.05, "*", ""))) +
    theme_bw() +
    theme(#panel.border = element_rect(colour = "blank"),
      panel.grid.major = element_line(colour="grey65"),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text( size = 11),
      axis.title.y = element_text(face = "italic", size = 11),
      axis.title.x = element_text(face = "italic", size = 11, hjust = 0.85),
    )
  
  # adjust axes if required
  if(!is.na(countmax)){
    p.windrose <- p.windrose + ylim(c(0, countmax))
  }
  
  # print the plot
  print(p.windrose)
  
  # return the handle to the wind rose
  return(p.windrose)
}
