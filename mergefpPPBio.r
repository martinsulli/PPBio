mergefp.ppbio<-function (a, b, d) 
{
    if (is.data.frame(a)) {
        CD <- a
    }
    else {
        CD <- read.csv(a, header = TRUE, stringsAsFactors = FALSE)
    }
    if (is.data.frame(b)) {
        MT <- b
    }
    else {
        MT <- read.csv(b, header = TRUE, stringsAsFactors = FALSE)
    }
    if (is.data.frame(d)) {
        W <- d
    }
    else {
        W <- read.csv(d, header = TRUE, stringsAsFactors = FALSE)
    }
    CD$PlotID <- as.numeric(CD$PlotID)
    RNAS <- CD[!is.na(CD$PlotID), ]
    RNAS$Census.Mean.Date <- as.numeric(RNAS$Census.Date)
    RNAS$Census.No <- as.numeric(RNAS$Census.No)
    RNAS$PlotID <- as.numeric(RNAS$PlotID)
    RNAS$PlotViewID <- as.numeric(RNAS$PlotViewID)
    RNAS$TreeID <- as.numeric(RNAS$TreeID)
    RNAS$FamilyAPGID <- as.numeric(RNAS$FamilyAPGID)
    RNAS$GenusID <- as.numeric(RNAS$GenusID)
    RNAS$SpeciesID <- as.numeric(RNAS$SpeciesID)
    RNAS$D1 <- as.numeric(gsub("=", "", RNAS$D1))
    RNAS$DPOMtMinus1 <- as.numeric(gsub("=", "", RNAS$DPOMtMinus1))
    RNAS$D2 <- as.numeric(gsub("=", "", RNAS$D2))
    RNAS$D3 <- as.numeric(gsub("=", "", RNAS$D3))
    RNAS$D4 <- as.numeric(gsub("=", "", RNAS$D4))
    RNAS$POM <- as.numeric(gsub("=", "", RNAS$POM))
    RNAS$F1 <- (gsub("=", "", RNAS$F1))
    RNAS$F2 <- (gsub("=", "", RNAS$F2))
    RNAS$F3 <- (gsub("=", "", RNAS$F3))
    RNAS$F4 <- (gsub("=", "", RNAS$F4))
    RNAS$Height <- as.numeric(gsub("=", "", RNAS$Height))
    RNAS$F5 <- as.numeric(gsub("=", "", RNAS$F5))
    RNAS$CI <- ifelse(RNAS$CI == "", NA, (gsub("=", "", RNAS$CI)))
    RNAS$LI <- ifelse(RNAS$LI == "", NA, (gsub("=", "", RNAS$LI)))
    RNAS$Sub.Plot.T1<-gsub("=","",RNAS$Sub.Plot.T1)
    RNASu <- RNAS[, c("PlotID", "Plot.Code", "Country", "Census.Mean.Date", 
        "Census.No", "PlotViewID", "TreeID", "FamilyAPGID", "Family", 
        "GenusID", "Genus", "SpeciesID", "Species", "D1", "DPOMtMinus1", 
        "D2", "D3", "D4", "POM", "F1", "F2", "F3", "F4", "Height", 
        "F5", "CI", "LI","Sub.Plot.T1")]
    Udfile <- unique(RNASu)
    W$PlotID <- as.numeric(W$PlotID)
    W$PlotViewID <- as.numeric(W$PlotViewID)
    W$TreeID <- as.numeric(W$TreeID)
    W$WD <- as.numeric(gsub("=", "", W$WD))
    MT$PlotID <- as.numeric(gsub("=", "", MT$PlotID))
    MT$PlotViewID <- as.numeric(MT$PlotViewID)
    MT$Altitude <- as.numeric(MT$Altitude)
    MT$LatitudeDecimal <- as.numeric(MT$LatitudeDecimal)
    MT$LongitudeDecimal <- as.numeric(MT$LongitudeDecimal)
    MT$PlotArea <- as.numeric(gsub("=", "", MT$PlotArea))
    MT$AllometricRegionID <- as.numeric(gsub("=", "", MT$AllometricRegionID))
    MT$AllometricRegionID <- as.numeric(gsub("NULL", "NA", MT$AllometricRegionID))
    MT$ClusterID <- as.numeric(gsub("=", "", MT$ClusterID))
    MT$ClusterID <- as.numeric(gsub("NULL", "NA", MT$ClusterID))
    MT$ForestMoistureID <- as.numeric(gsub("=", "", MT$ForestMoistureID))
    MT$ForestMoistureID <- as.numeric(gsub("NULL", "NA", MT$ForestMoistureID))
    MT$ForestEdaphicID <- as.numeric(gsub("=", "", MT$ForestEdaphicID))
    MT$ForestEdaphicID <- as.numeric(gsub("NULL", "NA", MT$ForestEdaphicID))
    MT$ForestEdaphicHeightID <- as.numeric(gsub("=", "", MT$ForestEdaphicHeightID))
    MT$ForestEdaphicHeightID <- as.numeric(gsub("NULL", "NA", 
        MT$ForestEdaphicHeightID))
    MT$ForestElevationID <- as.numeric(gsub("=", "", MT$ForestElevationID))
    MT$ForestElevationID <- as.numeric(gsub("NULL", "NA", MT$ForestElevationID))
    MT$ForestElevationHeightID <- as.numeric(gsub("=", "", MT$ForestElevationHeightID))
    MT$ForestElevationHeightID <- as.numeric(gsub("NULL", "NA", 
        MT$ForestElevationHeightID))
    MT$BiogeographicalRegionID <- as.numeric(gsub("=", "", MT$BiogeographicalRegionID))
    MT$BiogeographicalRegionID <- as.numeric(gsub("NULL", "NA", 
        MT$BiogeographicalRegionID))
    MT <- MT[, c("PlotID", "PlotViewID", "PlotCode", "Altitude", 
        "LatitudeDecimal", "LongitudeDecimal", "PlotArea", "ClusterName", 
        "ClusterID", "ForestMoistureName", "ForestMoistureID", 
        "ForestEdaphicName", "ForestEdaphicID", "ForestEdaphicHeightName", 
        "ForestEdaphicHeightID", "ForestElevationName", "ForestElevationID", 
        "ForestElevationHeightName", "ForestElevationHeightID", 
        "BiogeographicalRegionName", "BiogeographicalRegionID", 
        "RegionName", "Continent", "AllometricRegionID")]
    dataset <- merge(Udfile, MT, by = c("PlotViewID", "PlotID"), 
        x.all = TRUE)
    datasetb <- merge(dataset, W, by = c("PlotViewID", "TreeID", 
        "PlotID", "PlotCode"), x.all = TRUE)
    datasetc <- datasetb[, c("Continent", "Country", "PlotID", 
        "PlotCode", "PlotViewID", "LatitudeDecimal", "LongitudeDecimal", 
        "Altitude", "PlotArea", "TreeID", "FamilyAPGID", "Family", 
        "GenusID", "Genus", "SpeciesID", "Species", "Census.No", 
        "Census.Mean.Date", "D1", "DPOMtMinus1", "D2", "D3", 
        "D4", "POM", "F1", "F2", "F3", "F4", "Height", "F5", 
        "LI", "CI", "WD", "AllometricRegionID", "ClusterID", 
        "ForestMoistureID", "ForestEdaphicID", "ForestEdaphicHeightID", 
        "ForestElevationID", "ForestElevationHeightID", "BiogeographicalRegionID","Sub.Plot.T1")]
    datasetc$Monocot <- ifelse(grepl("Arecaceae", datasetc$Family) == 
        TRUE | grepl("Strelitziaceae", datasetc$Family) == TRUE | 
        grepl("Poaceae", datasetc$Family) == TRUE | grepl("Cyatheaceae", 
        datasetc$Family) == TRUE, 1, 0)
    datasetc$PomChange <- ifelse(grepl("6", datasetc$F4) == TRUE, 
        1, 0)
    datasetc$Recruit <- ifelse(grepl("n", datasetc$F1) == TRUE, 
        1, 0)
    datasetc
}
