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

# We have to figure out which EI sectors are coal, then find the corresponding SCCs and
# use those to select the correct emissions data.
# From a visual inspection of the SCC data, I determined that those EI.Sectors
# listing "Coal", were the combusting coal and no others. Therefore a simple grep
# will find them (note: I did not see any EI Sectors that had Coal in the name
# that weren't wanted).
ei_sector_levels <- levels(SCC$EI.Sector)
coal_locs <- grep("Coal", ei_sector_levels)
coal_ei_sector_levels <- ei_sector_levels[coal_locs]
scc_for_coal <- filter(SCC, EI.Sector %in% coal_ei_sector_levels)
scc_for_coal <- scc_for_coal$SCC

coal_emissions <- filter(NEI, SCC %in% scc_for_coal)

emissions_by_year <- ddply(coal_emissions, 
	.(year), 
	summarize, 
	total=sum(Emissions))

png("plot4.png")

# For this, a line graph will better show how it's changed rather than a line of best
# fit, which would be better suited to determining if there has been a change.
graph <- ggplot(emissions_by_year, aes(year, total)) + 
	geom_line() +
	xlab("Year") + 
	ylab("PM2.5 Emissions (tons)") +
	ggtitle("Total Annual PM2.5 Emissions in the United States\nfrom the Combustion of Coal") 

print(graph)

dev.off()
