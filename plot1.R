library(plyr)

## Because these data take a while to load, we check if we haven't already loaded them
## before doing so.
if (!exists("NEI")){
	NEI <- readRDS("summarySCC_PM25.rds")
}

if (!exists("SCC")) {
	SCC <- readRDS("Source_Classification_Code.rds")
}

total_yearly_emissions <- ddply(NEI, .(year), summarize, total=sum(Emissions))

png("plot1.png")

plot(total_yearly_emissions, 
	main = "Total Yearly Emissions in the United States",
	sub = "Source: EPA (http://www.epa.gov/ttn/chief/eiinformation.html)",
	xlab = "Year",
	ylab = "Total Emissions from PM2.5",
	col = "blue",
	fg = "gray",
	pch = 16,
	axes = FALSE)

axis(1, total_yearly_emissions$year)

ysteps <- seq(min(total_yearly_emissions$total),
		max(total_yearly_emissions$total),
		along.with = total_yearly_emissions$total)
axis(2, ysteps, labels = prettyNum(ysteps, big.mark = ","))
	
abline(lm(total ~ year, total_yearly_emissions),
	col = "darkgray")

dev.off()
