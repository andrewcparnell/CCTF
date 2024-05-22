# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Set up standard width / height
std_width <- 6
std_height <- 4
# Set up ggplot theme
theme_set(theme_bw())

# Read data
dat <- read.table("LHR_temp.txt", sep=",", header=TRUE)
y <- 0.1 * dat[(dat[,5]==0) & (dat[,4] > (-9999)), 4]
t <- dat[(dat[,5]==0) & (dat[,4] > (-9999)), 3]
yr <- floor(t / 10000)

# Define the time periods
dat_filtered <- data.frame(Year = yr, Temp = y)
dat_filtered$Period <- factor(ifelse(dat_filtered$Year < 1992, "1960-1991", "1992-2023"),
                              levels = c("1960-1991", "1992-2023"),
                              ordered = TRUE)

# Plot showing the data values and time periods using ggplot
g1 <- ggplot(dat_filtered, aes(x = Year, y = Temp)) +
  geom_point(data = dat_filtered %>% filter(Temp < 25),
             colour = 'grey', alpha=0.5) +
  geom_point(data = dat_filtered %>% filter(Temp >= 25),
             aes(colour = Period), alpha=0.5) +
  geom_vline(xintercept=c(1991.5), linetype="dashed", color="black") +
  labs(x = "Year", y = "Maximum daily temperature (°C)") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_color_manual(values=c("blue", "red")) + 
  theme(legend.position = 'bottom')
ggsave("data_ggplot.jpeg", plot = g1,
       width = std_width, height = std_height)

# Density plot with rugs
g2 <- ggplot(dat_filtered, aes(x = Temp)) +
  geom_density(aes(color = Period), linewidth = 1) +
  coord_cartesian(clip = "off") +
  geom_rug(data = dat_filtered %>% filter(Period == "1992-2023"),
           color = "blue", sides = "b", alpha = 0.01, outside = TRUE) +
  geom_rug(data = dat_filtered %>% filter(Period == "1960-1991"),
           color = "red", sides = "b", alpha = 0.01) +
  geom_vline(aes(xintercept = mean(dat_filtered$`Temp`[dat_filtered$`Period` == "1992-2023"])), 
             colour = "blue", size = 1, linetype = 'dotted') +
  geom_vline(aes(xintercept = mean(dat_filtered$`Temp`[dat_filtered$`Period` == "1960-1991"])), 
             colour = "red", size = 1, linetype = 'dotted') +
  scale_color_manual(values=c("blue", "red")) +
  scale_x_continuous(breaks = seq(-10, 40, by = 5)) +
  labs(x = "Maximum daily temperature (°C)", y = "Probability density") #+ 
  # theme(legend.position = "none")
ggsave("density_ggplot.jpeg", plot = g2,
       width = std_width, height = std_height)

# Update the script for threshold analysis using ggplot
# u_values <- 25:36
u_values <- seq(25, 36, by = 0.1)

# Calculate number of days per year above threshold u_values
nu1 <- sapply(u_values, function(u) sum(dat_filtered$Temp[dat_filtered$Period == "1960-1991"] > u))
nu1 <- nu1 / length(unique(dat_filtered$Year[dat_filtered$Period == "1960-1991"]))
# Now do the same for the second period
nu2 <- sapply(u_values, function(u) sum(dat_filtered$Temp[dat_filtered$Period == "1992-2023"] > u))
nu2 <- nu2 / length(unique(dat_filtered$Year[dat_filtered$Period == "1992-2023"]))

threshold_data <- tibble(Threshold = u_values, 
                             `1960-1991` = nu1, 
                             `1992-2023` = nu2) %>% 
  pivot_longer(cols = c(`1960-1991`, `1992-2023`), 
               names_to = "Period", values_to = "Days")
# Turn period into ordered factor
threshold_data$Period <- factor(threshold_data$Period, 
                                levels = c("1960-1991", "1992-2023"),
                                ordered = TRUE)

g3 <- ggplot(threshold_data, aes(x = Threshold)) +
  geom_step(aes(y = Days, color = Period)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Temperature threshold (°C)", 
       y = "Average number of days/year above threshold",
       color = "Period") + 
  scale_x_continuous(breaks = seq(25, 36, by = 1)) # +
  # theme(legend.position = "None")
ggsave("distribution_ggplot.jpeg", plot = g3,
       width = std_width, height = std_height)

# g4
u=25
yr1=(yr<1992)
yr2=(yr>=1992)
n1=length(unique(yr[yr1]))
n2=length(unique(yr[yr2]))

x1=aggregate((y[yr1]>u)~yr[yr1], FUN=sum)
mean(x1[,2])
x2=aggregate((y[yr2]>u)~yr[yr2], FUN=sum)
mean(x2[,2])
h1=hist(x1[,2],breaks = seq(0, 60, by = 1), plot = FALSE)
h2=hist(x2[,2],breaks = seq(0, 60, by = 1), plot = FALSE)
sum(x1[,2]>u)
sum(x2[,2]>u)

# Create a data frame with the data for the plot
exceedance_data <- data.frame(Exceedance_Days = h1$breaks, 
                              Before_1992 = n1 - cumsum(c(0, h1$counts)), 
                              After_1992 = n2 - cumsum(c(0, h2$counts)))
# Now do the plot
g4 <- ggplot(exceedance_data, aes(x = Exceedance_Days)) +
  geom_line(aes(y = Before_1992, color = "1960-1991"), size = 1) +
  geom_line(aes(y = After_1992, color = "1992-2023"), size = 1) +
  scale_color_manual(values = c("blue", "red")) +
  labs(x = "Number of exceedance days per year", 
       y = "Number of years with exceedances on more days",
       colour = "Period") + 
  scale_x_continuous(breaks = seq(0, 50, by = 5),
                     limits = c(0, 50)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5),
                   limits = c(0, 33))
  # theme(legend.position = "None")
ggsave("duration_ggplot.jpeg", plot = g4,
       width = std_width, height = std_height)
