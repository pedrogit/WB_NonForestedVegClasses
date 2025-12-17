computeNonForestedAreaMap <- function(baseLCCMap, pgm){
  nonForestedVegClassesMap <- baseLCCMap
  # Remove forested areas from the WB_NonForestedVegClasses map
  # For now those area do not change, but eventually we will consider areas where
  # biomass is negligible for a long time as non-forested by keeping a history 
  # of total biomass by pixelGroup.
  nonForestedVegClassesMap <- mask(
    nonForestedVegClassesMap, 
    pgm, 
    inverse = TRUE
  )
  # Reassign it names (for nicer plotting) 
  names(nonForestedVegClassesMap) <- "nonForestedVegClasses"
  varnames(nonForestedVegClassesMap) <- "nonForestedVegClasses"
  return (nonForestedVegClassesMap)
}

