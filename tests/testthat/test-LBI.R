
study_area <- rnaturalearth::ne_countries(scale = 50, continent = "Europe",
                                          country = "Sweden",
                                          type = "map_units",
                                          returnclass = "sf")
study_area <- sf::st_union(study_area)

# Tests for invalid outputs ----
test_that("invalid outputs", {
  test <- LBI(study_area_id = "Sweden", study_area_polygon = study_area,
              nobs = 10, nboot = 10, fact_location = 10, elevation = NULL)
  expect_identical(class(test), c("data.frame"))
})

# Tests for invalid inputs ----
test_that("invalid inputs", {
  expect_error(
    LBI(study_area_id = "Sweden", study_area_polygon = study_area,
        nobs = "250", nboot = 1000, fact_location = 10, elevation = NULL,
        bootstrap_output = FALSE),
    "nobs must be numeric.", fixed = TRUE)
  
  expect_error(
    LBI(study_area_id = "Sweden", study_area_polygon = study_area,
        nobs = 250, nboot = 1000, fact_location = 10, elevation = NA,
        bootstrap_output = FALSE),
    "class(elevation) != 'RasterLayer'
           elevation must be an unprojected (WGS84) RasterLayer.",
    fixed = TRUE)
  
  expect_error(
    LBI(study_area_id = "Sweden", study_area_polygon = study_area,
        nobs = 250, nboot = 1000, fact_location = 10, elevation = NULL,
        bootstrap_output = "zz"),
    "bootstrap_output must be a logical.", fixed = TRUE)
  
})
