#' Find confluence reaches over a strahler order
#'
#'This function finds the reaches immediately above confluences, that have a strahler order above a threshold
#'@param RECNetwork An REC V2 network, with at least a "reach identifier"nzsegment", "TO_NODE", "FROM_NODE" and "Stream_Orde" attributes.
#'@param OrderThreshold The strahler order which to aggregate the watersheds to
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return A vector of reach ID's (nzsegment numbers)
#'@keywords REC River Environment Classification
#'@export
ReachJunctionFinderByOrder <- function(RECNetwork=RECReachNetwork,OrderThreshold=7){

  #Throw away all the reaches below the threshold
  SubThresholdNetwork <- RECNetwork[which(RECNetwork$StreamOrde >= OrderThreshold),]

  #Keep only the reaches that have an immediately-downstream reach with a higher order than the threshold
  KeepOrReject <- sapply(SubThresholdNetwork$nzsegment, function(ReachOfInterest){

    #Get the Strahler order of the current reach
    CurrentOrder <- SubThresholdNetwork$StreamOrde[SubThresholdNetwork$nzsegment == ReachOfInterest]
    #Check order of reach downstream
    #browser()
    DownstreamOrder <- SubThresholdNetwork$StreamOrde[which(SubThresholdNetwork$FROM_NODE == SubThresholdNetwork$TO_NODE[SubThresholdNetwork$nzsegment == ReachOfInterest])]
    if(length(DownstreamOrder) == 0) Keep <- TRUE else if(DownstreamOrder > CurrentOrder) Keep <- TRUE else Keep <- FALSE
    return(Keep)
  })

  ReachesToKeep <- SubThresholdNetwork$nzsegment[KeepOrReject]



  return(ReachesToKeep)
}
