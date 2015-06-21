library(plyr)

## Because these data take a while to load, we check if we haven't already loaded them
## before doing so.
if (!exists("NEI")){
	NEI <- readRDS("summarySCC_PM25.rds")
}

if (!exists("SCC")) {
	SCC <- readRDS("Source_Classification_Code.rds")
}

total_yearly_emissions <- ddply(NEI, 
				.(year), 
				summarize, 
				total=sum(Emissions))

png("plot1.png")

# I thought a barplot would be most appropriate to see the change in total emissions
barplot(height = total_yearly_emissions$total,
	main = "Total Yearly PM2.5 Emissions in the United States",
	sub = "Source: EPA (http://www.epa.gov/ttn/chief/eiinformation.html)",
	xlab = "Year",
	ylab = "Total PM2.5 Emissions (tons)",
	col = "lightblue",
	names.arg = total_yearly_emissions$year
	)

dev.off()
