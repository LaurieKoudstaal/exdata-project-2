require(plyr)
require(dplyr)
require(ggplot2)
require(grid)
require(reshape2)

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

# Now we join this to the emissions 
emissions <- filter(NEI, fips %in% c("24510", "06037") & 
			 SCC %in% scc_vehicle_subset$SCC)
emissions <- ddply(emissions, 
		   .(year, fips), 
		   summarize, 
	    	   total = sum(Emissions))

# Fix the labels on the municipalities to be more descriptive.
emissions$municipality <- factor(emissions$fips, 
				 levels = c("06037", "24510"), 
				 labels = c("Los Angeles County", "City of Baltimore"))
emissions <- select(emissions, -fips)

# We will examine the change by looking at the absolute difference, rather than
# relative. I decided to use this because I believe the absolute change is probably
# more relevant to human health.

# We apply a difference function and then rearrange the resulting columns into
# a variable.
diff_fun <- function(df) {
	change <- diff(df$total)
	names(change) <- paste(df$year[-length(df$year)], df$year[-1], sep = "-")
	return(change)
}

emissions <- ddply(emissions, .(municipality), diff_fun)

emissions <- melt(emissions, id.vars = c("municipality"), variable.name = "period", value.name = "change")

# We apply a similar process to get the cumulative change.
cumsum_fun <- function(df) {
	cs <- cumsum(df$change)
	names(cs) <- df$period
	return(cs)
}

cumulative_changes <- ddply(emissions, .(municipality), cumsum_fun)

cumulative_changes <- melt(cumulative_changes, id.vars = c("municipality"), variable.name = "period", value.name = "cumulative")

# The cumulative change is of interest to see if, over time, the changes have amounted
# to a net improvement or not.
emissions <- join(emissions, cumulative_changes)

# Plot the data with some tricks to ge the lines and bars on the plot
png("plot6.png", width = 920, height = 480)

plot <- ggplot(emissions, aes(x = period))
plot <- plot + facet_grid(. ~ municipality)
plot <- plot + geom_line(aes(y = cumulative, color = "Cumulative Change", group = municipality))
plot <- plot + geom_bar(aes(y = change, fill = "Change"), stat = "identity")

plot <- plot + scale_fill_manual(values = "lightblue", breaks = c("Change"), name = element_blank())
plot <- plot + scale_color_manual(values = "darkblue", breaks = c("Cumulative Change"), name = element_blank())

plot <- plot + xlab("Period of Change")
plot <- plot + ylab("PM2.5 (tons)")

plot <- plot + ggtitle("Change and Cumulative Change of PM2.5 in Baltimore City and Los Angeles County")

print(plot)

dev.off()
