defineModule(sim, list(
  name = "WB_NonForestedVegClasses",
  description = paste("Create a map of non-forested areas based on forested areas and a land cover product."),
  keywords = c("non-forested area", "western boreal"),
  authors =  c(
    person("Pierre", "Racine", email= "pierre.racine@sbf.ulaval.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(WB_LichenBiomass = "0.0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  # citation = list("citation.bib"),
  # documentation = list("NEWS.md", "README.md", "WB_LichenBiomass.Rmd"),
  reqdPkgs = list("reproducible"),
  loadOrder = list(after = c("Biomass_core")),
  parameters = rbind(
    defineParameter("WB_NonForestedVegClassesTimeStep", "numeric", 1, NA, NA,
                    "Simulation time at which the non-forested map is regenerated."),
    defineParameter("baseLCCYear", "numeric", 2010, NA, NA,
                    "Year of the default LCC to load.")
  ),
  inputObjects = rbind(
    expectsInput(objectName = "pixelGroupMap",
                 objectClass = "SpatRast",
                 desc = paste("pixelGroupMap from the ",
                              "boimass_core module used to determine ",
                              "forested areas"),
                 sourceURL = NA),
    expectsInput(objectName = "WB_NonForestedVegClassesBaseLCCMap", 
                 objectClass = "SpatRast", 
                 desc = "", 
                 sourceURL = "")
  ),
  outputObjects = rbind(
    createsOutput(objectName = "WB_NonForestedVegClassesMap", 
                  objectClass = "SpatRast", 
                  desc = "Raster of classified non-forested areas.")
  )
))

doEvent.WB_NonForestedVegClasses = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- Init(sim)
      sim <- scheduleEvent(sim, time(sim), "WB_NonForestedVegClasses", "reComputeNonForestedAreaMap", 2)
    },
    
    reComputeNonForestedAreaMap = {
      sim <- reComputeNonForestedAreaMap(sim)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$WB_NonForestedVegClassesTimeStep, "WB_NonForestedVegClasses", "reComputeNonForestedAreaMap")
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim){
  # If WB_NonForestedVegClassesBaseLCCMap is not provided in the objects, make it a copy of sim$rstLCC
  if (!suppliedElsewhere("WB_NonForestedVegClassesBaseLCCMap", sim) && !is.null(sim$rstLCC)){
    sim$WB_NonForestedVegClassesBaseLCCMap <- sim$rstLCC
    # Reclass any disturbed values assigned by prepInputs_NTEMS_LCC_FAO() (240) to shrub (50)
    sim$WB_NonForestedVegClassesBaseLCCMap[sim$WB_NonForestedVegClassesBaseLCCMap == 240] <- 50
  }
  # Project and crop the base LCC map to pixelGroupMap
  # This is done only once wherever the LCC was instanciated from (default, rstLCC or simInit)
  if (!.compareRas(sim$WB_NonForestedVegClassesBaseLCCMap, sim$pixelGroupMap, stopOnError = FALSE))
    sim$WB_NonForestedVegClassesBaseLCCMap <- postProcess(
      sim$WB_NonForestedVegClassesBaseLCCMap,
      projectTo = sim$pixelGroupMap,
      cropTo = sim$pixelGroupMap
    )
  return(invisible(sim))
}

reComputeNonForestedAreaMap <- function(sim) {
  message("Recomputing sim$WB_NonForestedVegClassesMap...")
  sim$WB_NonForestedVegClassesMap <- computeNonForestedAreaMap(
    sim$WB_NonForestedVegClassesBaseLCCMap,
    sim$pixelGroupMap
  )

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  userTags <- c(currentModule(sim), "function:.inputObjects")
  ##############################################################################
  # Create a dummy pixelGroupMap if none is provided
  ##############################################################################
  if(!suppliedElsewhere("pixelGroupMap", sim)){
    nbGroup <- 200
    pixelGroupRastWidth <- 1000
    message("##############################################################################")
    message("pixelGrouMap not supplied.")
    message("Please provide one. Creating random map ", pixelGroupRastWidth, " pixels by ",
            pixelGroupRastWidth, " pixels with ", nbGroup, " groups...")

    sim$pixelGroupMap <- Cache(
      getRandomCategoricalMap,
      origin = c(-667296, 1758502),
      ncol = pixelGroupRastWidth,
      nrow = pixelGroupRastWidth,
      crs = "ESRI:102002",
      nbregion = nbGroup,
      seed = 100,
      userTags = c(userTags, "WB_pixelGroupMap"),
      omitArgs = c("userTags")
    )
  }

  
  if (!is.null(sim$pixelGroupMap)){
    baseRast <- sim$pixelGroupMap
  }
  else if (!is.null(sim$rasterToMatch)){
    baseRast <- sim$rasterToMatch
  }
  else {
    stop(paste("At least one of pixelGroupMap or rasterToMatch must be defined ",
               "in sim before WB_NonForestedVegClasses can be initialized..."))
  }
  ##############################################################################
  # Download a LLC raster if necessary
  ##############################################################################
  if(!suppliedElsewhere("rstLCC", sim) && 
     !suppliedElsewhere("WB_NonForestedVegClassesBaseLCCMap", sim)){
    message("##############################################################################")   
    message("Neither rstLCC nor WB_NonForestedVegClassesBaseLCCMap were supplied.")   
    message("Please provide one or the other. By default we are using NTEMS land cover")   
    message("from https://opendata.nfis.org/mapserver/nfis-change_eng.html...")

    # sim$WB_NonForestedVegClassesBaseLCCMap <- Cache(prepInputs_NTEMS_LCC_FAO,
    #   year = P(sim)$baseLCCYear,
    #   maskTo = baseRast,
    #   cropTo = baseRast,
    #   projectTo = baseRast,
    #   disturbedCode = 50, # set FAO disturbed areas to shrub
    #   destinationPath = asPath(inputPath(sim), 1),
    #   overwrite = TRUE,
    #   # writeTo = .suffix("rstLCC.tif", paste0("_", P(sim)$.studyAreaName, "_", P(sim)$dataYear)),
    #   userTags = c("WB_NonForestedVegClassesBaseLCCMap", currentModule(sim),
    #                "FAO_NTEMS", P(sim)$baseLCCYear))

    year <- P(sim)$baseLCCYear
    lccURL <- paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", year, ".zip")
    lccTF <- paste0("CA_forest_VLCE2_", year, ".tif")
    sim$WB_NonForestedVegClassesBaseLCCMap <- Cache(
      prepInputs,
      url = lccURL,
      targetFile = lccTF,
      destinationPath = getPaths()$cachePath,
      fun = terra::rast,
      cropTo = baseRast,
      projectTo = baseRast,
      method = "near",
      overwrite = TRUE,
      writeTo = .suffix("rstLCC.tif", paste0("_NTEMS_", year)),
      userTags = c("WB_NonForestedVegClassesBaseLCCMap", currentModule(sim),
                   "NTEMS", P(sim)$baseLCCYear)
    )
    # Convert to factor and add more descriptive labels
    sim$WB_NonForestedVegClassesBaseLCCMap <- terra::as.factor(sim$WB_NonForestedVegClassesBaseLCCMap)
    levels(sim$WB_NonForestedVegClassesBaseLCCMap) <- data.frame(
      value = c( 20L,        31L,           32L,              33L,         40L,         50L,        80L,          81L,                100L,        210L,             220L,            230L),
      class = c("20-water", "31-snow_ice", "32-rock_rubble", "33-barren", "40-bryoid", "50-shrub", "80-wetland", "81-treed_wetland", "100-herbs", "210-coniferous", "220-broadleaf", "230-mixed_wood")
    )

    # Assign names (for nicer plotting) 
    names(sim$WB_NonForestedVegClassesBaseLCCMap) <- "nonForestedVegClasses"
    varnames(sim$WB_NonForestedVegClassesBaseLCCMap) <- "nonForestedVegClasses"
  }

  return(invisible(sim))
}
