#set working directory
setwd("C:/Users/oyeda/Desktop/APPLIED_SPATIAL_STAT/Excerise4/")

#Install and/or load libraries 
# install.packages("spgwr")
library(spdep)
library(maptools)
library(rgeos)
library(spgwr)

# Import the shapefile columbus as a spatial polygons data-frame:
columbus <- readShapeSpatial("columbus.shp")

# You can summarize the attribute data as usual by running the command:
summary(columbus)

# You can overview your dependent variable (CRIME) and polygon geometry by running the
# command (with its default color scale and a couple of custom-defined titles):
spplot(columbus, "CRIME", main="Columbus, OH", sub="Residential
        burglaries and vehicle thefts per 1000 households")      

# dependent variable is CRIME, and we assume that its global variation in crime (not taking
# into account local variation between polygons) can be explained adequately by variables indicating
# income (INC), property value (HOVAL), and distance to the city center (DISCBD). The list of the
# dependent and independent variables is as follows:
# CRIME residential burglaries and vehicle thefts per 1000 households
# HOVAL housing value (in $1,000)
# INC household income (in $1,000)
# DISCBD distance to CBD

# Run an ordinary least squares (OLS) regression to see the average effects of INC, HOVAL,
# and DISCBD on CRIM for the whole study area:
OLS <- lm(CRIME ~ INC + HOVAL + DISCBD, data=columbus)
summary(OLS)

# Bandwidth selection. Since GWR repeats the estimation for geographical subsets of your data
# (so it can get the local variation of the beat coefficients), it needs to know how extensive these
# geographical subsets should be. This is the idea behind bandwidth - it is loosely connected to the
# idea of spatial weights.

# Option 1: you set the bandwidth by yourself.
# Option 2: you employ an automatic search algorithm for finding the optimal bandwidth by running
# the command grw.sel and saving the result in the data-frame named bw. In our case:

bw <- gwr.sel(CRIME ~ INC + HOVAL + DISCBD, data=columbus, method="aic")

# The search stops when no further improvement in the performance of the bandwidth choice can
# be achieved. Performance can be measured by a couple of options and in our case we use the
# Akaike information criterion (AIC):

# The Gaussian function that is implied by bandwidth = 1.129466:

# Important: If your shapefile is not in a projected coordinate system (e.g. in meters or arbitrary
# units), you need to include the setting longlat=TRUE (i.e. inform the algorithm that your file'                                                                                                                            geographical units are in degrees). In that case, the bandwidth will be measured in kilometers. If
# the shapefile is projected, then the bandwidth will be measured in the units of the file's projection
# system. The columbus.shp shapefile is projected, but its metadata do not tell us what the
# arbitrary units are. So, the bandwidth number 1.129466 cannot be interpreted:

# Estimation of a GWR model. After finding the optimal bandwidth, you can estimate a GWR
# model by running the command gwr. The note about the longlat setting is valid for this command
# as well. In our case, the syntax will be:
gwr <- gwr(CRIME ~ INC + HOVAL + DISCBD, data=columbus, bandwidth=bw,
             hatmatrix=TRUE)

# You can display the results by typing gwr (i.e. no summary command is needed):
gwr


# Mapping the estimations. The gwr dataframe borrows geographical structure from its input
# data (columbus.shp). This means that you can create thematic maps of the local coefficients by
# using the spplot command. For our three explanatory variables, the commands will be:
spplot(gwr$SDF, "INC", main="Income")
spplot(gwr$SDF, "HOVAL", main="Housing value")
spplot(gwr$SDF, "DISCBD", main="Distance to the CBD")

# For the local R-squared of the estimates, the command will be:
spplot(gwr$SDF, "localR2", main="local R-squared")


# Calculate t-values for assessing the significance of the estimated effects. The GWR
# estimations also include the standard errors of the estimated coefficients. If you divide the estimated
# coefficient value by its standard error, you derive a t-statistic score:
gwr$SDF$INC_t <- gwr$SDF$INC/gwr$SDF$INC_se
gwr$SDF$HOVAL_t <- gwr$SDF$HOVAL/gwr$SDF$HOVAL_se
gwr$SDF$DISCBD_t <- gwr$SDF$DISCBD/gwr$SDF$DISCBD_se


# And plot the three significance maps in a common figure:
spplot(gwr$SDF, c("INC_t", "HOVAL_t", "DISCBD_t"))


# Deviations from global mean. In addition to step 3.3, you can visualize spatial non-stationarity
# in the estimated coefficients by calculating their % deviation from the global mean in each location.
# For the case of INC,
# Append the % deviation in the spatial data-frame that holds the GWR results (-0.9677 is the
#global mean):

gwr$SDF$INC_pctdelta <- ((gwr$SDF$INC + 0.9677)/-0.9677)*100

# And the thematic map with a custom color ramp:
spplot(gwr$SDF, "INC_pctdelta", col.regions=cm.colors(20), main="INC",
         sub="% deviation from global mean value of 0.9677")

# Save the GWR results as a new shapefile. You will want to save the estimations in a new
# shapefile (they are not attached to your source columbus file) for more analytical or mapping
# flexibility. For instance, you may want to use more advanced mapping software or to analyze the
# estimations with GeoDa. The command to do the exporting is:
writeSpatialShape(gwr$SDF, "columbus_gwr")
