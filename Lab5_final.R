#GEOG 418 Final Project: Jessica MacLean

#################################
######## LOAD LIBRARIES #########
#################################

install.packages("sf")
install.packages("plyr")
install.packages("dplyr")
install.packages("spdep")
install.packages("GISTools")
install.packages("raster")
install.packages("maptools")
install.packages("rgdal")
install.packages("spatstat")
install.packages("sp")
install.packages("tmap")
install.packages("gstat")
install.packages("shinyjs")
install.packages("spgwr")

library(sf)
library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(spatstat)
library(tmap)
library(gstat)
library(spgwr)
library(tmaptools)

#Set working directory
dir <- "E:/Geog418/Lab5/Working"
setwd(dir)

#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data

#Select only columns 1 and 2
pm25 <- pm25[,1:2]

#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")
pm25 <- na.omit(pm25)

#Reading in postal code shapefile
postalcodes <- shapefile("BC_Postal_Codes") #Read in related postal code data

#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("BC_DA.shp") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

summary(census.tracts)

### Let's take a closer look at difficult to see areas of GVRD ############

plot(income.tracts)
#locator()

west_region <- st_bbox(c(xmin = -123.2569, xmax = -122.7775,
                       ymin = 49.09066, ymax = 49.36257), crs = st_crs(income.tracts)) %>%
  st_as_sfc()

############## mapping median income #################

map_income <- tm_shape(income.tracts)+
  tm_polygons(col = "Income",
              title = "Median Income", 
              style = "fisher", 
              palette = "viridis", n = 6, alpha = .75) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "BOTTOM")) +
  tm_legend(position = c("left", "bottom"))+
  tm_layout(title = "Median Income by Dissemination Area\n                      GVRD, 2016", title.position = c("right", "top"), title.size = 1 )
map_income

map_income_2<- tm_shape(pm.income.poly, bbox = west_region)+
  tm_fill(col = "Income",
              title = "Median Income", 
              style = "fisher", 
              palette = "viridis", n = 6, alpha = .75) +
  tm_scale_bar(text.size = 0.5, position = c("LEFT", "BOTTOM"))+
  tm_legend(legend.outside=TRUE)
map_income_2


################# Select postal codes that fall within dissemination tracts) ############################
postalcodes <- intersect(postalcodes,income.tracts)

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")

#Aggregate the PM2.5 values in each DA in order to have a single value per DA. Here we aggregate based on the max.
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID,FUN=max)

#Re-join aggregated data to the income.tracts layer.
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") #Select only ID and Income columns

income.pm25 <- merge(income.tracts,pm25.aggregate, by = "DAUID") #Merge income and dissemination data

################Re-join aggregated data to the pm25.spatial points layer ################# 

pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID") #multiple points within each DA but we have forced them to all have the same value 

#Create a subsample of the datapoints provided in the PM2.5 dataset using the sample n provided on CourseSpaces

#set.seed(Sys.time())
set.seed(220)
sampleSize=220
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate),sampleSize),]

plot(income.tracts)
plot(spSample, type = "p", col = "blue", add = TRUE) #change colour of spSample points 

##################################################
############### STUDY AREA MAP ###################
##################################################

install.packages("bcmaps")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')
install.packages("grid")

library("bcmaps")
library('bcmapsdata')
library("grid")

bc <- as_Spatial(bc_neighbours()) #Get shp of BC bounds
bc <- spTransform(bc, CRS("+init=epsg:3005")) #project to WGS84 geographic (Lat/Long)

bc <- bc[which(bc$name == "British Columbia" ),]

map_studyarea<- tm_shape(income.tracts)+
  tm_polygons(col = "lightgrey") +
  tm_shape(spSample)+
  tm_dots(col="PM25AGG", palette = "Reds", 
          title="Sampled PM2.5\n(ug/m3)", size=0.1)+
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "BOTTOM"))+
  tm_compass(north = 0, position = c("left", "top"))+
  tm_layout(main.title = "Maximum annual average PM2.5 concentration per DA, 2016", main.title.size = 1,
            legend.position = c("left", "bottom"))

map_bc <- tm_shape(bc)+
  tm_polygons(col = "grey50")+
  tm_shape(income.tracts)+
  tm_fill("red")

map_studyarea
print(map_bc, vp = viewport(0.83, 0.74, width = 0.3, height = 0.3))


############## Global and Local Moran's I - Is income spatially autocorrelated? If so, how? #####################

income.nb <- poly2nb(income.tracts) #defines our neighborhoods and builds a mesh net for them
income.net <- nb2lines(income.nb,coords=coordinates(income.tracts))

queens <- tm_shape(income.tracts) + tm_borders(col='darkgrey') + 
  tm_shape(income.net) + tm_lines(col='red', legend.col.show = TRUE) + 
  tm_style("white", title = "Queens Neighborhood") +
  tm_scale_bar(text.size = 0.5) +
  tm_layout(attr.position = c("LEFT","BOTTOM"), title.position = c("right", "top"))
 
queens

#we are going to create a weight matrix - are things neighbors, a bunch of 1s and 0s, 1 = neighbor

income.lw <- nb2listw(income.nb, zero.policy = TRUE, style = "W")
print.listw(income.lw, zero.policy = TRUE) #this provides us with a summary of our weights

income.tracts$IncLagMeans = lag.listw(income.lw, income.tracts$Income, zero.policy = TRUE) #this maps the lagged means 

map_LagMean <- tm_shape(income.tracts) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Income\nLagged Means", 
              style = "jenks", #try fisher
              palette = "viridis", n = 6) +
  tm_scale_bar(text.size = 0.5) +
  tm_style("white", title = "Lagged Means of Income per DA", legend.position = c("left", "bottom"), title.position = c("right", "top"))

map_LagMean

############# Calculating Moran's I #######################

mi <- moran.test(income.tracts$Income, income.lw, zero.policy = TRUE)
mi 

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(income.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <- (mI-eI)/(sqrt(var))

########################  
#local morans I - we will have an I value for each polygon. 
lisa.test <- localmoran(income.tracts$Income, income.lw, alternative = "two.sided")

#create new columns in census.tracts based on lisa test dataframe
income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2] #expected I 
income.tracts$Var.Ii<- lisa.test[,3] #variance 
income.tracts$Z.Ii<- lisa.test[,4] #z-score
income.tracts$P<- lisa.test[,5]#p-value
########################
#we are mapping the local morans I - we will use the natural breaks LISA + local indicator of spatial autocorrelation 

map_LISA <- tm_shape(income.tracts) +
  tm_polygons(col = "Ii", 
              title = "Local Moran's I", 
              style = "jenks", 
              palette = "Reds", n = 6,  midpoint = NA) +
  tm_scale_bar(text.size = 0.5) +
  tm_style("white", title = "Local Moran's I Values for Income\n             Vancouver, BC 2016")+
  tm_layout(legend.position = c("left", "bottom"), title.position = c("right", "top"))

map_LISA

map_LISAPvalues <- tm_shape(income.tracts) + 
  tm_polygons(col = "P", 
              title = "P-values", 
              style = "jenks", 
              palette = "Reds", n = 6, midpoint = NA) +
  tm_scale_bar(text.size = 0.5) +
  tm_style("white", title = "P-values of Local Moran's I Analysis\n            Vancouver, BC 2016")+
  tm_layout(legend.position = c("left","bottom"), title.position = c("right", "top"))


map_LISAPvalues


########################
#we will now make a scatter plot, positive slope = positive spatial autocorrelation

moran.plot(income.tracts$Income, income.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Income", 
           ylab="Spatially Lagged Means", quiet=NULL, 
           main = "Moran's I Plot for Median Income per Dissemination Area\n Vancouver, BC 2016")
dev.off()


########################

################################################
################ Interpolation #################
################################################

# Every polygon should have an income and PM2.5

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(spSample, "regular", n=30000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)

#################################################
###########Polynomial Trend Analysis#############

# Define the 2nd order polynomial equation
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Run the regression model
lm.2 <- lm( f.2, data=spSample)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

r.second   <- raster(dat.2nd)
r.m <- mask(r.second, census.tracts)

# Plot the map and compare sampled PM2.5 data point values to the modelled trend
secondorder <- tm_shape(r.m) + 
  tm_raster(n=10, palette="Reds", 
            title=" \n Predicted PM2.5 (ug/m3)") +
  tm_shape(spSample) +
  tm_dots(col="PM25AGG", palette = "Reds", 
          title="Sampled PM2.5 \n(ug/m3)", size=0.3) +
  tm_legend(legend.outside=TRUE)+
  tm_layout(main.title = "Predicted PM2.5 using Second-order Polynomial Trend", main.title.size = 1)

png("SecondOrderTrend.png")
secondorder
dev.off()

################################################################
##########Spatial Interpolation with Universal Kriging########## 

spSample$X <- coordinates(spSample)[,1]
spSample$Y <- coordinates(spSample)[,2]

#Create Semivariogram
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
var.smpl <- variogram(f.2, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=1.0, model="Sph", range=10, nugget=0))

plot(var.smpl, dat.fit, 
     ylab="Semivariance",
     xlab= "Distance (km)",
     col="black", main = "Universal Kriging Semivariogram", lw = 2)
dev.off()

# Define the second order polynomial trend model
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
#dat.krg <- krige( f.1, spSample, grd, dat.fit)
dat.krg <- krige( f.2, spSample, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, census.tracts)

# Plot the final UK map
UK <- tm_shape(r.m) + 
  tm_raster(n=10, palette="Reds",  
            title="Predicted PM2.5 \n(ug/m3)") +
  tm_shape(spSample) + tm_dots(size=0.09) +
  tm_compass(north = 0, position = c("right", "top"))+
  tm_scale_bar(text.size = 0.5) +
  tm_legend(legend.outside=TRUE)+
  tm_layout(main.title = "Interpolated PM 2.5 using Universal Kriging, Vancouver BC", main.title.size = 1)
UK


r.var   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r.var, census.tracts)

UK_Var <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \nin squared PM2.5 \n(ug/m3)") +tm_shape(spSample) + tm_dots(size=0.09) +
  tm_legend(legend.outside=TRUE)+
  tm_scale_bar(text.size = 0.5)

UK_Var


r.CI   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r.CI, census.tracts)

UK_CI <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Confidence Interval Map \n \nUK interpolated \nPM2.5 (ug/m3) CI") +tm_shape(spSample) + tm_dots(size=0.09) +
  tm_legend(legend.outside=TRUE)+
  tm_scale_bar(text.size = 0.5)

UK_CI

#shows edge effects due to lack of training data - where training data exists we have low CIs over a large area 
#################################################
######################################################################################################

#These steps will help you combine the outputs from your spatial interpolation with your income data.

#If you have too many cells, you can reduce the number by aggregating values
step.1 <- raster::aggregate(r, fact=1, fun=mean)

#Convert the raster dataset to points
CRS <- crs(census.tracts)
step.2 <- rasterToPoints(step.1,fun=NULL, spatial=FALSE, crs=CRS)
step.2 <- as.data.frame(step.2) #convert the point dataset to a spatial dataframe
colnames(step.2)[3] <- "PM25"
Coords <- step.2[,c("x", "y")]  #assign coordinates to a new object
step.3 <- SpatialPointsDataFrame(coords = Coords, data = step.2, proj4string = CRS) #create a spatial points dataframe
step.4 <- aggregate(x=step.3,by=income.tracts, FUN=mean) #aggregate points into census tracts
step.5 <- intersect(step.4,income.tracts)  #get the intersection of step.4 with the income.tracts dataset (this will take a while) 

##############################Linear Regression#######################################################
#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.

pm.income.poly <- step.5

#there were NA values so I removed them in order to calculate correlation and other statistics 
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]
#removed negative values below 0

pm.income.poly <- pm.income.poly[which(pm.income.poly$PM25 >= 0),]
pm.income.poly <- pm.income.poly[which(pm.income.poly$PM25 <= 5.7),]

#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$Income, pm.income.poly$PM25, xlab = "Income", ylab = "PM2.5")

#descriptive stats for the PM25 and Income per polygon data 
hist(pm.income.poly$PM25)
hist(pm.income.poly$Income)

normalityPM25 <- shapiro.test(pm.income.poly$PM25)
normalityPM25

normality.income <- shapiro.test(pm.income.poly$Income)
normality.income

cor(pm.income.poly$PM25, pm.income.poly$Income, method = c("spearman"))

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$PM25~pm.income.poly$Income)

#Add the regression model to the plot you created
plot(pm.income.poly$Income, pm.income.poly$PM25, xlab = "Income", ylab = "PM2.5")
abline(lm.model, col = "red", lw = 2)

#Get the summary of the results
summary(lm.model)

#You want to determine if the model residuals are spatially clustered. 
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))

#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$residuals <- residuals.lm(lm.model)

pmincome <- as.data.frame(pm.income.poly)
normalityRes <- shapiro.test(pmincome$residuals)
normalityRes
hist(pmincome$residuals)

#Observe the result to make sure it looks correct
head(pm.income.poly)


#Now, create choropleth map of residuals - If Income is the dependent variable than the residuals
#will be very large numbers as they are proportional to the Income values 

map_residuals <- tm_shape(pm.income.poly)+
  tm_polygons(col = "residuals",
              title = "Residuals (PM2.5)",
              style = "fisher",
              palette = "Reds", n = 6, midpoint = NA) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT","BOTTOM"))+
  tm_legend(legend.outside=TRUE)

map_residuals

###################### Global and Local Moran's I for Regression residuals #############################

residuals.nb <- poly2nb(pm.income.poly) #defines our neighborhoods and builds a mesh net for them
residuals.net <- nb2lines(residuals.nb,coords=CRS)

residuals.lw <- nb2listw(residuals.nb, zero.policy = TRUE, style = "W")
print.listw(residuals.lw, zero.policy = TRUE) #this provides us with a summary of our weights

pm.income.poly$IncLagMeans = lag.listw(residuals.lw, pm.income.poly$residuals, zero.policy = TRUE) #this maps the lagged means 

map_LagMean <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Residuals\nLagged Means", 
              style = "jenks", #try fisher
              palette = "RdYlBu", n = 6) +
  tm_scale_bar(text.size = 0.5)

map_LagMean


mi2 <- moran.test(pm.income.poly$residuals, residuals.lw, zero.policy = TRUE)
mi2 

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(residuals.lw)


mI2 <- mi2$estimate[[1]]
eI2 <- mi2$estimate[[2]]
var2 <- mi2$estimate[[3]]

z <- (mI2-eI2)/(sqrt(var2))

########################  
#local morans I - we will have an I value for each polygon. 
lisa.test2 <- localmoran(pm.income.poly$residuals, residuals.lw, alternative = "two.sided")

#create new columns in census.tracts based on lisa test dataframe
pm.income.poly$Ii <- lisa.test2[,1]
pm.income.poly$E.Ii<- lisa.test2[,2] #expected I 
pm.income.poly$Var.Ii<- lisa.test2[,3] #variance 
pm.income.poly$Z.Ii<- lisa.test2[,4] #z-score
pm.income.poly$P<- lisa.test2[,5]#p-value

########################
#we are mapping the local morans I - we will use the natural breaks LISA + local indicator of spatial autocorrelation 


map_LISA2 <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I", 
              style = "fisher", 
              palette = "RdYlBu", n = 6,  midpoint = NA) +
  tm_scale_bar(text.size = 0.5) +
  tm_style("white", title = "Local Moran's I Values for Residuals")

map_LISA2

########################
#we will now make a scatter plot, positive slope = positive spatial autocorrelation

moran.plot(pm.income.poly$residuals, residuals.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Residuals", 
           ylab="Spatially Lagged Means", quiet=NULL, 
           main = "Moran's I Plot for Income Regression Residuals")


########################

#Map LISA p values
map_LISAPvalues.res <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "P", 
              title = "P-values", 
              style = "fisher", 
              palette = "Reds", n = 6) +
  tm_scale_bar(text.size = 0.5) +
  tm_style("white", title = "P-values of Local Moran's I Analysis\n Income regression residuals")

map_LISAPvalues.res

###################################################################################
####################### Geographically Weighted Regression ########################
###################################################################################

#The first thing you need to do is to add the polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)

#Observe the result
head(pm.income.poly.coords)

#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(pm.income.poly$PM25~pm.income.poly$Income, 
                        data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(pm.income.poly$PM25~pm.income.poly$Income, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
pm.income.poly$localr <- results$localR2
pm.income.poly$coeff <- results$pm.income.poly.Income

pm.income.poly <- pm.income.poly[which(pm.income.poly$localr >= 0),]

plot(income.tracts)
#locator()

dt_region <- st_bbox(c(xmin = -123.2669, xmax = -122.9954,
                       ymin = 49.2002, ymax = 49.30214), crs = st_crs(income.tracts)) %>%
  st_as_sfc()

larger_region <- st_bbox(c(xmin = -123.2947, xmax = -122.9065,
                       ymin = 49.19222, ymax = 49.36991), crs = st_crs(income.tracts)) %>%
  st_as_sfc()

east_region <- st_bbox(c(xmin = -122.7338, xmax = -122.9539,
                           ymin = 49.09487, ymax = 49.22003), crs = st_crs(income.tracts)) %>%
  st_as_sfc()

############## mapping local r squared values #################

map_rsquared <- tm_shape(pm.income.poly)+
  tm_polygons(col = "localr",
              title = "Local R-squared Values", 
              style = "fisher", 
              palette = "RdGy", n = 4, alpha = .8) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "GVRD", title.size = 1)
map_rsquared


map_rsquared_2<- tm_shape(pm.income.poly, bbox = larger_region)+
  tm_polygons(col = "localr",
              title = "Local R-squared Values", 
              style = "fisher", 
              palette = "RdGy", n = 4, alpha = .8) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "City of Vancouver,\nNorth and West Vancouver", title.size = 1)
map_rsquared_2

map_rsquared_3<- tm_shape(pm.income.poly, bbox = east_region)+
  tm_polygons(col = "localr",
              title = "Local R-squared Values", 
              style = "fisher", 
              palette = "RdGy", n = 4, alpha = .8) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "Surrey", title.size = 1)
map_rsquared_3

localr_maps <- tmap_arrange(map_rsquared_2, map_rsquared_3, ncol = 1)

localr_maps


########### Time for more magic. Let's map the coefficient #################

map_coeff <- tm_shape(pm.income.poly)+
  tm_polygons(col = "coeff",
              title = "Income Coefficient", 
              style = "fisher", 
              palette = "RdBu", n = 4, alpha = .8, midpoint = NA) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "GVRD", title.size = 1)
map_coeff


map_coeff_2<- tm_shape(pm.income.poly, bbox = larger_region)+
  tm_polygons(col = "coeff",
              title = "Income Coefficient", 
              style = "fisher", 
              palette = "RdBu", n = 4, alpha = .8, midpoint = NA) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "City of Vancouver,\nNorth and West Vancouver", title.size = 1)
map_coeff_2

map_coeff_3<- tm_shape(pm.income.poly, bbox = east_region)+
  tm_polygons(col = "coeff",
              title = "Income Coefficient", 
              style = "fisher", 
              palette = "RdBu", n = 4, alpha = .8,midpoint = NA) +
  tm_scale_bar(text.size = 0.5, position = c("RIGHT", "TOP")) +
  tm_legend(legend.outside=TRUE, title = "Surrey", title.size = 1)
map_coeff_3

localcoeff_maps <- tmap_arrange(map_coeff_2, map_coeff_3, ncol = 1)

localcoeff_maps

#########################################################################
##################### Point Pattern Analysis ############################
#########################################################################

spSample.t <- spTransform(spSample, CRS("+init=epsg:3005"))
income.tracts.t <- spTransform(income.tracts, CRS("+init=epsg:3005"))

spSample.t$x <- coordinates(spSample.t)[,1]
spSample.t$y <- coordinates(spSample.t)[,2]

#check for and remove duplicated points
#check for duplicated points
#finds zero distance among points
zd <- zerodist(spSample.t)
zd

#remove duplicates
spSample.t <- remove.duplicates(spSample.t)
spSample.t

#create an "extent" object which can be used to create the observation window for spatstat
spSample.ext <- as.matrix(extent(spSample.t)) 
spSample.ext
#observation window
window <- as.owin(list(xrange = spSample.ext[1,], yrange = spSample.ext[2,]))

#create ppp oject from spatstat
spSample.ppp <- ppp(x = spSample.t$x, y = spSample.t$y, window = window)

################ QUADRAT ANALYSIS ##############################
quads <- 10

qcount <- quadratcount(spSample.ppp, nx = quads, ny = quads)

plot(spSample.ppp, pch = "+", cex = 0.5, add = T)
plot(qcount, add = T, col = "red")
plot(income.tracts, add = TRUE)

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")


sum.f.x2 <- sum(qcount.df$f*qcount.df$x^2)

M <- quads*quads

N <- sum(qcount.df$f*qcount.df$x)

sum.fx.2 <- sum((qcount.df$f*qcount.df$x)^2)


VAR <- ((sum.f.x2)-(sum.fx.2/M))/(M-1)

MEAN <- N/M

VMR <- VAR/MEAN 

##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square =  VMR*(M-1)
p = 1 - pchisq(chi.square, (M - 1))


###KERNEL DENSITY ESTIMATION
#2D (gaussian) kernel, compare how bandwidth (sigma) selection influences the point density estimates
#since data are projected, sigma is represented in metres
#eps is the width and height of the pixels (1000m X 1000m)
#coerce to a SpatialGridDataFrame for plotting
kde.100 <- density(spSample.ppp, sigma = 100, at = "pixels", eps = c(100, 100))
kde.SG <- as(kde.100, "SpatialGridDataFrame")
kde.500 <- density(spSample.ppp, sigma = 500, at = "pixels", eps = c(100, 100))
kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))

  
#plot
x11() #opens a new plot window
spplot(kde.SG)

#can see how the bandwidth selection influences the density estimates
summary(kde.SG)

#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(spSample.ppp)

#density using the cross-validation bandwidth
kde.bwo <- density(spSample.ppp, sigma = bw.d, at = "pixels", eps = c(100, 100))

color <- heat.colors(100, alpha=1, rev=TRUE)
hood_colors <- grey.colors(n=14, alpha=0.2)

par(oma=c(1, 1, 3, 1))
plot(kde.bwo, col=color, main = "Kernel Density Estimate of sample points")
mtext("Density (points/m^2)", side=4, line=6)
plot(income.tracts.t, col=hood_colors, lwd = .005,add=T)

