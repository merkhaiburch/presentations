# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2022-12-22
# Updated... 2023-02-07
#
# Description:
# Presentation retrospective, 100 presentations
# ------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)

# Load data
counts <- read.csv("~/Box Sync/Cornell_PhD/presentations/other/presentation_count_100_retrospective.csv")

# Make fake level
counts$Format <- rep("", nrow(counts))

# Remove posters
counts <- counts %>% filter(Name != "Posters")

# Load color pallete
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Make two stacked barcharts
a <- ggplot(counts, aes(fill=Name, y=Count, x=Format, label = Count)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette) +
  coord_flip() +
  theme_bw() +
  xlab("Format") +
  ylab("Percent") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(legend.position = "bottom")

# Save plot
ggsave(plot = a, filename = "~/Box Sync/Cornell_PhD/presentations/other/presentation_count_100_retrospective.png",
       width = 13.26, height = 6, units = "in")


# ------------------------------------------------------------------------------
# Parse presentation names to get year-by-year breakdown
# ------------------------------------------------------------------------------

# Load in cd /presentation_dir | ll > txt.file
temp <- read.delim("~/Box Sync/Cornell_PhD/presentations/other/all_presentations.txt")

# change column names
colnames(temp) <- "ids"

# Remove stuff before year
temp$ids <- gsub(".* 20", "20", temp$ids)

# Remove file extension
temp$ids <- gsub(".pptx", "", temp$ids)
temp$ids <- gsub(".pdf", "", temp$ids)

# Remove first line (folder), turn back into a df
temp <- temp[-1,]
temp <- data.frame(temp)
colnames(temp) <- "ids"

# Parse out year
temp$Year <- gsub("_.*", "", temp$ids)

# Write to file to hand annotate
write.csv(temp, "~/Box Sync/Cornell_PhD/presentations/other/all_presentations.csv", quote = F, row.names = F)

# Read in modified version
temp <- read.csv("~/Box Sync/Cornell_PhD/presentations/other/all_presentations_modified.csv")

# Make year a factor
temp$Year <- as.factor(temp$Year)

# Make count by group
newCounts <- temp %>% group_by(Name, Year) %>% summarise(n = n())
newCounts

# Load color pallete
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Make stacked barchart by year
b <- ggplot(newCounts, aes(fill=Name, y=n, x=Year, label = n)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = cbPalette) +
  theme_bw() +
  xlab("Year") +
  ylab("Count of Presentations") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave(plot = b, filename = "~/Box Sync/Cornell_PhD/presentations/other/presentation_count_100_retrospective_byYear.png",
       width = 10, height = 8, units = "in")



