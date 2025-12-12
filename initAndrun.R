modelPath <- "projects/base_withSimInit"

# install Require and SpaDES.project
# repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
# source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
# 
# getOrUpdatePkg(
#   c("Require",    "reproducible", "SpaDES.core", "SpaDES.project"),
#   c("1.0.1.9020", "2.1.2.9082",   "2.1.8.9001",  "0.1.1.9043")
# )
# 
# getOrUpdatePkg(
#   c("Require",    "reproducible", "SpaDES.core", "SpaDES.project"),
#   c("1.0.1.9020", "2.1.2.9070",   "2.1.8.9001",  "0.1.1.9043")
# )
# remotes::install_version("reproducible", version = "2.1.2.9082", repos=repo)
# install.packages(c("Require", "SpaDES", "SpaDES.project", "LandR"), repos=repos, dependencies = TRUE)
# packageVersion("Require")
# packageVersion("reproducible")
# packageVersion("SpaDES.core")
# packageVersion("SpaDES.project")

# install.packages(c("mapview", "future.apply"), dependencies = TRUE)
# source(file.path(modelPath, "modules/rutils/rutils.R"))
source("https://raw.githubusercontent.com/pedrogit/rUtils/refs/heads/main/rutils.R")

library(SpaDES)
library(LandR)
library(mapview)
library(terra)
library(data.table)

setBasePath(modelPath)
getPaths() # shows where the 4 relevant paths are

#--- Download the module if not done already
SpaDES.project::getModule(modulePath = getPaths()$modulePath,
                          c("pedrogit/WB_LichenBiomass@main"),
                          overwrite = FALSE)

modelTimeStep <- 10
# options(spades.DTthreads = 20)

# options('reproducible.interactiveOnDownloadFail' = FALSE)

# rast(nrow=2, ncol=2, crs="EPSG:3035", vals=1:4)
# Sys.setenv(PROJ_LIB = "C:/Program Files/R/R-4.5.2/library/rgdal/proj")
# 
# install.packages("devtools")
# install.packages("G:/Home/working/rgdal_1.6-7.tar.gz", repos = NULL, type = "source")
sim <- SpaDES.core::simInit(
  times = list(start = 0, end = 20),
  # modules = list("Biomass_core", "WB_HartJohnstoneForestClasses", "WB_VegBasedDrainage"),
  # modules = list("Biomass_core", "WB_HartJohnstoneForestClasses"),
  # modules = list("Biomass_core"),
  # modules = list("WB_VegBasedDrainage"),
  modules = list(
    # "Biomass_speciesData"
    # , "Biomass_borealDataPrep"
    # , "Biomass_speciesParameters"
    # , "Biomass_core"
    # , "WB_HartJohnstoneForestClasses"
    # , "WB_VegBasedDrainage"
    # , "WB_LichenBiomass"
    "WB_NonForestedVegClasses"
  ),
  params = list(
    .globals = list(sppEquivCol = 'LandR'),
    # Biomass_borealDataPrep = list(overrideAgeInFires = FALSE
    # ),
    Biomass_speciesData = list(
      .plots = NA
    ),
    Biomass_core = list(successionTimestep = modelTimeStep,
                        sppEquivCol = "LandR",
                        seedingAlgorithm = "noSeeding",
                        .plots = NA,
                        calcSummaryBGM = NULL,
                        .useCache = FALSE
    ),
    
    WB_HartJohnstoneForestClasses = list(
      WB_HartJohnstoneForestClassesTimeStep = modelTimeStep,
      .saveInitialTime = 0,
      .saveInterval = modelTimeStep,
      useDrainage = TRUE
    ),
    
    WB_VegBasedDrainage = list(
      WB_VegBasedDrainageTimeStep = modelTimeStep,
      searchDistInPixelNb = 2
    ),
    
    WB_LichenBiomass = list(
      WB_LichenBiomassTimeStep = modelTimeStep
    )
    ,
    
    WB_NonForestedVegClasses = list(
      WB_NonForestedVegClassesTimeStep = modelTimeStep
    )
  ),
  objects = list(
    studyArea = {
      # create and use a random study area
      # Lambert Conformal Conic for Canada: this is used in NRCan's "KNN" products
      Biomass_corecrs <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      centre <- terra::vect(cbind(-104.757, 55.68663), crs = "epsg:4326") # Lat Long
      centre <- terra::project(centre, Biomass_corecrs)
      studyArea <- LandR::randomStudyArea(centre, size = 2e8, seed = 1234)
    },
    studyAreaLarge = terra::buffer(studyArea, width = 3e4)
  )
)

sim <- spades(sim)
