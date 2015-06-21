library(plyr)
library(dplyr)
library(ggplot2)
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

graph <- ggplot(emissions_by_type, aes(year, total)) + 
	# I layout the graph vertically, so it's easier to compare the trend in each 
	# type
	facet_wrap( ~ type, nrow = 4, ncol = 1) +
	geom_point() + 
	geom_smooth(method = "lm", color = "Blue") + 
	# Append the subtitle to the x label
	xlab("Year\nSource: EPA (http://www.epa.gov/ttn/chief/eiinformation.html)") + 
	ylab("PM2.5 Emissions") +
	ggtitle("Total Annual PM2.5 Emissions in Baltimore by Type") +
	# It's hard to make out the trend if the panels are too wide, so shrint the
	# whole plot horizonally
	theme(plot.margin = unit(c(0, 1.75, 0, 1.75), "inches"))

ggsave("plot3.png", width = 7, height = 10)

