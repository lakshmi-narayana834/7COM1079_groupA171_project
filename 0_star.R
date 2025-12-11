# Commit: Data Import and Cleaning | Date: 4-12-2025
# Imported dataset, cleaned missing values, filtered N64 and EMU platforms.

data <- read.csv("data_0 Star.csv", stringsAsFactors = FALSE)


# check column names
names(data)
str(data)
# Convert to correct data types
data$platform <- factor(data$platform)
data$verified <- factor(data$verified)
data$primary_time_seconds <- as.numeric(data$primary_time_seconds)

# Filter to only N64 and EMU (remove VC and others)
rq2_data <- subset(data, platform %in% c("N64", "EMU") & !is.na(primary_time_seconds))

# Drop unused factor levels
rq2_data$platform <- droplevels(rq2_data$platform)

# Check final summary
table(rq2_data$platform)
summary(rq2_data$primary_time_seconds)

# Commit: Visualisation Added (Boxplot + Histograms) | Date: 5-12-2025
# Added boxplot and histograms to explore distribution and compare platforms.


# Histograms to observe distribution
par(mfrow = c(1,2))
hist(rq2_data$primary_time_seconds[rq2_data$platform == "N64"],
     main = "Histogram of Completion Time (N64)",
     xlab = "Primary Time Seconds")

hist(rq2_data$primary_time_seconds[rq2_data$platform == "EMU"],
     main = "Histogram of Completion Time (EMU)",
     xlab = "Primary Time Seconds")
par(mfrow = c(1,1))


# Boxplot
boxplot(primary_time_seconds ~ platform,
        data = rq2_data,
        ylim = c(300, 1200),     
        main = "Primary Time by Platform",
        xlab = "Platform",
        ylab = "Primary Time (Seconds)",
        col = c("lightgray", "lightgray"))

# Commit: Wilcoxon Test and Summary Output | Date: 6-12-2025
# Performed non-parametric Wilcoxon test and extracted median/mean results.


wilcox_result <- wilcox.test(primary_time_seconds ~ platform,
                             data = rq2_data,
                             exact = FALSE)

wilcox_result

aggregate(primary_time_seconds ~ platform, data = rq2_data, FUN = mean)
aggregate(primary_time_seconds ~ platform, data = rq2_data, FUN = median)



