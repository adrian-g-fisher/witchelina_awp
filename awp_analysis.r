library(dplyr)
library(ggplot2)
library(patchwork)
library(kSamples)
library(prettyunits)


#####################
# Get rainfall data #
#####################

rain_df <- read.csv("C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\witchelina_rainfall.csv")
rain_df$season_month <- rain_df$Month
rain_df$season_month[rain_df$Month %in% c(12, 1, 2)] = 12
rain_df$season_month[rain_df$Month %in% c(3, 4, 5)] = 3
rain_df$season_month[rain_df$Month %in% c(6, 7, 8)] = 6
rain_df$season_month[rain_df$Month %in% c(9, 10 ,11)] = 9
rain_df$season_year <- rain_df$Year
rain_df$season_year[rain_df$Month %in% c(1, 2)] <- rain_df$season_year[rain_df$Month %in% c(1, 2)] - 1
rain_df$date <- as.Date(paste(rain_df$season_year, rain_df$season_month, 15, sep = "-")) + 30
rain_df <- setNames(aggregate(rain_df$Precipitation, by=list(rain_df$date), FUN=sum), c('date', 'season_rain'))

###########################################
# Analyse the model with random intercept #
###########################################

data_dir <- "C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\model_results_random_intercept"
out_dir <- "C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\figures"

# Read in model output (season, r2_marginal, r2_conditional, correlation_coefficient)
model_df <- read.csv(paste0(data_dir, "/model_output_all_seasons.csv"))
model_df$n <- NA

# For each season, read in all the fixed and random effect values
fixed_list <- list()
random_list <- list()
i <- 1
for (season in model_df$season) {
  
  # Fixed effects
  # term,value,se,z,p_value,lower_2.5,upper_97.5
  fixed_df <- read.csv(paste0(data_dir, "/fixed_effects_", season, ".csv"))
  fixed_df$season <- season
  fixed_list[i] <- list(fixed_df)
  
  # Random effects
  # Paddock,group_var,effect,value,se,lower_2.5,upper_97.5,n
  rand_df <- read.csv(paste0(data_dir, "/random_effects_", season, ".csv"))
  rand_df$season <- season
  rand_df <- subset(rand_df, select = -group_var)
  random_list[i] <- list(rand_df)
  i <- i + 1
  
  # Also add total n to model_df  
  clean_df <- na.omit(rand_df)
  model_df$n[model_df$season == season] <- sum(clean_df$n[clean_df$effect == 'Intercept'])
}
fixed_df <- bind_rows(fixed_list)
random_df <- bind_rows(random_list)

# Change seasons to dates
y <- strtoi(substr(model_df$season, 0, 4))
m <- strtoi(sub("^0+", "", substr(model_df$season, 5, 6)))
model_df$date <- as.Date(paste(y, m, 15, sep = "-")) + 30

y <- strtoi(substr(fixed_df$season, 0, 4))
m <- strtoi(sub("^0+", "", substr(fixed_df$season, 5, 6)))
fixed_df$date <- as.Date(paste(y, m, 15, sep = "-")) + 30

y <- strtoi(substr(random_df$season, 0, 4))
m <- strtoi(sub("^0+", "", substr(random_df$season, 5, 6)))
random_df$date <- as.Date(paste(y, m, 15, sep = "-")) + 30

# Make graph of model rainfall, slope, mean intercept, r2_marginal, r2_conditional, and n over time
fixed_df = merge(fixed_df, rain_df, by = "date")
df <- filter(fixed_df, term == "Distance")
graph_1 <- ggplot(df, aes(x = date, y = season_rain)) +
  geom_col(fill="lightblue") +
  labs(x = '', y = 'Seasonal\n rainfall (mm)') +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank())

df <- filter(fixed_df, term == "Distance")
graph_2 <- ggplot(df, aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = 'grey80') +
  geom_line() +
  labs(x = '', y = 'Slope') +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

df <- filter(fixed_df, term == "Intercept")
graph_3 <- ggplot(df, aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = 'grey80') +
  geom_line() +
  labs(x = '', y = 'Mean\nintercept') +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

graph_4 <- ggplot(model_df, aes(x = date, y = r2_marginal)) +
  geom_line() +
  labs(x = '', y = expression(paste("Marginal ", italic("r"^"2")))) +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

graph_5 <- ggplot(model_df, aes(x = date, y = r2_conditional)) +
  geom_line() +
  labs(x = '', y = expression(paste("Conditional ", italic("r"^"2")))) +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

graph_6 <- ggplot(model_df, aes(x = date, y = n)) +
  geom_line() +
  labs(x = '', y = "Sample\n(pixels)") +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

combined_1 <- (graph_1 + graph_2 + graph_3 + graph_4 + graph_5 + graph_6) +
  plot_layout(ncol = 1)

ggsave(paste0(out_dir, "/timeseries_random_intercept.png"), combined_1, height = 8, width = 6, dpi = 600)

# Make graph of each paddock's intercept over time
df <- filter(fixed_df, term == "Intercept")
graph <- ggplot() +
  geom_ribbon(data=df, aes(x = date, ymin = lower_2.5, ymax = upper_97.5, fill = 'Mean intercept'), alpha = 0.5) +
  geom_ribbon(data=random_df, aes(x = date, ymin = lower_2.5, ymax = upper_97.5, fill = 'Paddock intercept'), alpha = 0.5) +
  scale_fill_manual(values=c("black", "red")) +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  labs(x = '', y = "Intercept", legend = "") +
  facet_wrap("Paddock", ncol=2, axes="all", axis.labels="margins") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  theme(legend.position.inside = c(1, 0), legend.justification = c(2, 0), legend.title = element_blank(), legend.key.size=unit(0.3, "cm"))
ggsave(paste0(out_dir, "/intercept_timeseries_by_paddock_random_intercept.png"), graph, height = 8, width = 8, dpi = 600)

# Create box plots of before/after conservation for rainfall, slope, mean intercept
# Use the Anderson-Darling test to see if distributions are statistically similar
df1 <- filter(fixed_df, term == "Distance")
df1 <- setNames(subset(df1, select = c("date", "season_rain", "value")), c("date", "Rainfall", "Slope"))
df2 <- filter(fixed_df, term == "Intercept")
df2 <- setNames(subset(df2, select = c("date", "value")), c("date", "Intercept"))

df <- merge(df1, df2, by = "date")
df$Conservation <- NA
df$Conservation[df$date < as.Date("2010-01-01")] <- "Before"
df$Conservation[df$date >= as.Date("2010-01-01")] <- "After"
df$Conservation <- factor(df$Conservation , levels=c("Before", "After"))

ad_rain <- ad.test(df$Rainfall[df$Conservation == "Before"], df$Rainfall[df$Conservation == "After"], method="exact")
p_rain <- ad_rain$ad[1, 3]

box_1 <- ggplot(df, aes(x = Conservation, y = Rainfall, fill = Conservation)) +
  geom_boxplot(fill = "grey80", show.legend = FALSE) +
  labs(title = expr(paste(italic('p ='), !!pretty_p_value(p_rain), sep=""))) +
  theme_classic() +
  theme(plot.title = element_text(size=8, hjust = 0.5))

ad_slope <- ad.test(df$Slope[df$Conservation == "Before"], df$Slope[df$Conservation == "After"], method="exact")
p_slope <- ad_slope$ad[1, 3]

box_2 <- ggplot(df, aes(x = Conservation, y = Slope, fill = Conservation)) +
  geom_boxplot(fill = "grey80", show.legend = FALSE) +
  labs(title = expr(paste(italic('p '), !!pretty_p_value(p_slope), sep=""))) +
  theme_classic() +
  theme(plot.title = element_text(size=8, hjust = 0.5))

ad_intercept <- ad.test(df$Intercept[df$Conservation == "Before"], df$Intercept[df$Conservation == "After"], method="exact")
p_intercept <- ad_intercept$ad[1, 3]

box_3 <- ggplot(df, aes(x = Conservation, y = Intercept, fill = Conservation)) +
  geom_boxplot(fill = "grey80", show.legend = FALSE) +
  labs(title = expr(paste(italic('p '), !!pretty_p_value(p_intercept), sep=""))) +
  theme_classic() +
  theme(plot.title = element_text(size=8, hjust = 0.5))

combined_2 <- (box_1 + box_2 + box_3) +
  plot_layout(ncol = 3, axis_titles = "collect")

ggsave(paste0(out_dir, "/boxplot_random_intercept.png"), combined_2, height = 2, width = 6, dpi = 600)

#####################################################
# Analyse the model with random intercept and slope #
#####################################################

data_dir <- "C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\model_results_random_intercept_and_slope"
out_dir <- "C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\figures"

# Read in model output (season, r2_marginal, r2_conditional, correlation_coefficient)
model_df <- read.csv(paste0(data_dir, "/model_output_all_seasons.csv"))
model_df$n <- NA

# For each season, read in all the model values (Paddock, effect, value, lower_2.5, upper_97.5, n)
value_list <- list()
i <- 1
for (season in model_df$season) {
  season_df <- read.csv(paste0(data_dir, "/model_values_", season, ".csv"))
  season_df$season <- season
  season_df <- subset(season_df, select = -group_var)
  value_list[i] <- list(season_df)
  i <- i + 1
  
  # Also add total n to model_df  
  clean_df <- na.omit(season_df)
  model_df$n[model_df$season == season] <- sum(clean_df$n[clean_df$effect == 'Intercept'])
  }
values_df <- bind_rows(value_list)

# Change seasons to dates
y <- strtoi(substr(model_df$season, 0, 4))
m <- strtoi(sub("^0+", "", substr(model_df$season, 5, 6)))
model_df$date <- as.Date(paste(y, m, 15, sep = "-")) + 30

y <- strtoi(substr(values_df$season, 0, 4))
m <- strtoi(sub("^0+", "", substr(values_df$season, 5, 6)))
values_df$date <- as.Date(paste(y, m, 15, sep = "-")) + 30

# Make graph of model r2_marginal, r2_conditional, and n over time
graph_1 <- ggplot(model_df, aes(x = date, y = r2_marginal)) +
  geom_line() +
  labs(x = '', y = expression(paste("Marginal ", italic("r"^"2")))) +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

graph_2 <- ggplot(model_df, aes(x = date, y = r2_conditional)) +
  geom_line() +
  labs(x = '', y = expression(paste("Conditional ", italic("r"^"2")))) +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(axis.text.x=element_blank()) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

graph_3 <- ggplot(model_df, aes(x = date, y = n)) +
  geom_line() +
  labs(x = '', y = "Sample\n(pixels)") +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  theme_classic() +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))

combined_1 <- (graph_1 + graph_2 + graph_3) +
  plot_layout(ncol = 1)

ggsave(paste0(out_dir, "/timeseries_random_intercept_and_slope.png"), combined_1, height = 4, width = 6, dpi = 600)

# Make graph of each paddock's slope over time
df <- filter(values_df, effect == "Distance")
graph <- ggplot(df, aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = 'grey80') +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 0) +  
  geom_line(colour = 'grey50') +
  labs(x = '', y = "Slope") +
  facet_wrap("Paddock", ncol=2, scales="free_y") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(paste0(out_dir, "/slope_timeseries_by_paddock_random_intercept_and_slope.png"), graph, height = 8, width = 8, dpi = 600)

# Make graph of each paddock's intercept over time
df <- filter(values_df, effect == "Intercept")
graph <- ggplot(df, aes(x = date, y = value)) +
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = 'grey80') +
  geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype = "dashed", colour = "red") +
  geom_line(colour = 'grey50') +
  labs(x = '', y = "Intercept") +
  facet_wrap("Paddock", ncol=2, axes="all", axis.labels="margins") +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(paste0(out_dir, "/intercept_timeseries_by_paddock_random_intercept_and_slope.png"), graph, height = 8, width = 8, dpi = 600)

# Create box plots of before/after conservation for slope and intercept by paddock
# Use the Anderson-Darling test to see if distributions are statistically similar
df1 <- filter(values_df, effect == "Distance")
df1 <- setNames(subset(df1, select = c("date", "Paddock", "value")), c("date", "Paddock", "Slope"))
df2 <- filter(values_df, effect == "Intercept")
df2 <- setNames(subset(df2, select = c("date", "Paddock", "value")), c("date", "Paddock", "Intercept"))
df <- merge(df1, df2, by = c("date", "Paddock"))
df$Conservation <- NA
df$Conservation[df$date < as.Date("2010-01-01")] <- "Before"
df$Conservation[df$date >= as.Date("2010-01-01")] <- "After"
df$Conservation <- factor(df$Conservation , levels=c("Before", "After"))

paddock_list <- unique(df$Paddock)

p_df <- data.frame(Paddock = character(), p_value = numeric())
for (paddock in paddock_list){
  ad_slope <- ad.test(df$Slope[df$Conservation == "Before" & df$Paddock == as.symbol(paddock)],
                     df$Slope[df$Conservation == "After" & df$Paddock == as.symbol(paddock)], method="exact")
  p <- ad_slope$ad[1, 3]
  p_df <- rbind(p_df, data.frame(Paddock = paddock, p_value = pretty_p_value(p)))
}

p_labs <- paste0(p_df$Paddock, "\n(", p_df$p_value, ")")
names(p_labs) <- p_df$Paddock

box <- ggplot() +
  geom_boxplot(aes(x = Conservation, y = Slope, fill = Conservation), data = df, fill = "grey80", show.legend = FALSE) +
  facet_wrap("Paddock", ncol=5, axes="all", axis.labels="margins", scales="free_y", labeller = labeller(Paddock = p_labs)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(paste0(out_dir, "/boxplot_slope_by_paddock.png"), box, height = 6, width = 8, dpi = 600)

p_df <- data.frame(Paddock = character(), p_value = numeric())
for (paddock in paddock_list){
  ad_intercept <- ad.test(df$Intercept[df$Conservation == "Before" & df$Paddock == as.symbol(paddock)],
                          df$Intercept[df$Conservation == "After" & df$Paddock == as.symbol(paddock)], method="exact")
  p <- ad_intercept$ad[1, 3]
  p_df <- rbind(p_df, data.frame(Paddock = paddock, p_value = pretty_p_value(p)))
}

p_labs <- paste0(p_df$Paddock, "\n(", p_df$p_value, ")")
names(p_labs) <- p_df$Paddock

box <- ggplot(df, aes(x = Conservation, y = Intercept, fill = Conservation)) +
  geom_boxplot(fill = "grey80", show.legend = FALSE) +
  facet_wrap("Paddock", ncol=5, axes="all", axis.labels="margins", scales="free_y", labeller = labeller(Paddock = p_labs)) +
  theme_classic() +
  theme(strip.background = element_blank())
ggsave(paste0(out_dir, "/boxplot_intercept_by_paddock.png"), box, height = 6, width = 8, dpi = 600)
