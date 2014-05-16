setwd("/Users/martin/Dev/datasciencecoursera-repos/datasciencecoursera-explo-a2")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")


pollutantSet <- unique(NEI$Pollutant) # 1 single pollutent
cateogrySet <- unique(NEI$SCC) # 5480 categories
typeSet <- unique(NEI$type) # "POINT"    "NONPOINT" "ON-ROAD"  "NON-ROAD"
yearSet <- unique(NEI$year) # 1999 2002 2005 2008
# emissionNa <- sum(is.na(NEI$Emissions)) # 0 missing values

#neiMat <- as.matrix(NEI)

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
yearTotalEmissions <- aggregate(NEI$Emissions ~ NEI$year, NEI, sum)
plot(yearTotalEmissions)


# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
select <- (NEI$fips == "24510")
n <- sum(select) # 2096
baltimore <- NEI[select,]
baltimoreYearTotalEmissions <- aggregate(Emissions ~ year, baltimore, sum)
plot(baltimoreYearTotalEmissions, type="l")

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
s <- split(baltimore, typeSet)
pointSums <- aggregate(s$POINT$Emissions ~ s$POINT$year, s$POINT, sum)
nonPointSums <- aggregate(s$NONPOINT$Emissions ~ s$NONPOINT$year, s$NONPOINT, sum)
onRoadSums <- aggregate(s$"ON-ROAD"$Emissions ~ s$"ON-ROAD"$year, s$"ON-ROAD", sum)
nonRoadSums <- aggregate(s$"NON-ROAD"$Emissions ~ s$"NON-ROAD"$year, s$"NON-ROAD", sum)

# Use the ggplot2 plotting system to make a plot answer this question.
plot(pointSums, type="l", ylab="year", col = "red")
lines(nonPointSums, col = "cyan")
lines(onRoadSums, col = "yellow")
lines(nonRoadSums, col = "blue")


# Which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# answer : points, nonraad, onroad

# Which have seen increases in emissions from 1999–2008? 
# answer : nonpoint

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coalCombustionIndex <- grep("coal.*combustion", SCC$"Short.Name", ignore.case=T)
sum(coalCombustionIndex) # 604 categories selected
coalCombustionSccs <- SCC[coalCombustionIndex,"SCC"]

usCoalCombustionIndex <- (NEI$SCC %in% coalCombustionSccs) 
sum(usCoalCombustionIndex)
usCoalCombustion <- NEI[usCoalCombustionIndex, ]

coalCombPerYear <- aggregate(Emissions ~ year, usCoalCombustion, sum)
plot.new()
plot(coalCombPerYear, type="l")

# answerstarted to decrease from 1999 to 2002 and then started to to increase from 2002 to 2008

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources  
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?