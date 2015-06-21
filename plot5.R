require(plyr)
require(dplyr)
require(ggplot2)
require(grid)

## Because these data take a while to load, we check if we haven't already loaded them
## before doing so.
if (!exists("NEI")){
	NEI <- readRDS("summarySCC_PM25.rds")
}

if (!exists("SCC")) {
	SCC <- readRDS("Source_Classification_Code.rds")
}

# We find emissions related to vehicles by examining EI.Sector field.
# I interprest the question to be asking about automobiles, so I only
# include on-road vehicles. By visual inspection of the levels, this
# is all factors containing the word "Vehicles".
sector_levels <- levels(SCC$EI.Sector)
vehicle_sectors_levels_index <-  grep("Vehicles", sector_levels)
vehicle_sectors_levels <- sector_levels[vehicle_sectors_levels_index] 
scc_vehicle_subset <- filter(SCC, EI.Sector %in% vehicle_sectors_levels)

# Now we join this to the Baltimore City emissions 
baltimore_emissions <- filter(NEI,fips==24510 & SCC %in% scc_vehicle_subset$SCC)

total_emissions <- ddply(baltimore_emissions, 
	.(year), 
	summarize, 
	total=sum(Emissions))

png("plot5.png")

# For this, a line graph will better show how it's changed rather than a line of best
# fit, which would be better suited to determining if there has been a change.
graph <- ggplot(total_emissions, aes(year, total)) + 
	geom_line() +
	xlab("Year") + 
	ylab("PM2.5 Emissions (tons)") +
	ggtitle("Total Annual PM2.5 Emissions in the City of Baltimore\nfrom Motor Vehicle Sources") 

print(graph)

dev.off()
