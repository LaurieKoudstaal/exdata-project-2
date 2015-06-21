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

baltimore_emissions <- filter(NEI,fips==24510)

emissions_by_type <- ddply(baltimore_emissions, 
	.(year, type), 
	summarize, 
	total=sum(Emissions))

png("plot3.png", width = 480, height = 3*480)

# Here I opted for a trendline. In the situation where you have to demonstrate there
# is or isn't an increase, I think trendlines and confidence intervals demonstrate
# how much certainty you can actually have.
graph <- ggplot(emissions_by_type, aes(year, total)) + 
	# I layout the graph vertically, so it's easier to compare the trend in each 
	# type
	facet_wrap( ~ type, nrow = 4, ncol = 1) +
	geom_point(color = "Blue" )+ 
	geom_smooth(method = "lm", color = "Black") + 
	xlab("Year") + 
	ylab("PM2.5 Emissions (tons)") +
	ggtitle("Total Annual PM2.5 Emissions\nin Baltimore City by Type") 

print(graph)

dev.off()
