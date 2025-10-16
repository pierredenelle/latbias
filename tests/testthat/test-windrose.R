
study_area <- rnaturalearth::ne_countries(scale = 50, continent = "Europe",
                                          country = "Sweden",
                                          type = "map_units",
                                          returnclass = "sf")
study_area <- sf::st_union(study_area)
po <- LBI(study_area_id = "Sweden", study_area_polygon = study_area,
          nobs = 10, nboot = 10, fact_location = 10, elevation = NULL,
          raw_output = TRUE)
test <- as.data.frame(po$all[, c("study_area_id", "distance_km",
                                 "bearing", "rep")])
test$distance_km <- as.numeric(test$distance_km)

# Tests for invalid outputs ----
test_that("invalid outputs", {
  pop <- windrose(data = test, spd = "distance_km", dir = "bearing")
  expect_identical(as.numeric(dim(pop$data)), c(10, 6))
  expect_in("ggplot", class(pop))
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    windrose(data = test, spd = "distance_km", dir = "bearing",
             spdres = "zz"),
    "spdres must be numeric.", fixed = TRUE)
  
})
