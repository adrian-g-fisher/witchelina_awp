library(ggplot2)
library(tidyverse)
library(glmmTMB)
library(easystats)
library(performance)
library(lme4)
library(mixedup)
library(readr)


csvfile <- 'C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\awp_seasonal_analysis_epsg3577_1987_2024.csv'
awp_data <- read.csv(csvfile)

# Change distance to kilometres
awp_data$Distance <- awp_data$Distance / 1000

# Get a list of the columns of interest (values of bare for every season)
columns <- colnames(awp_data)[grepl("B", colnames(awp_data))]

# Get a list of paddocks
paddock_list <- unique(awp_data$Paddock)

# Initialize the results data frame
results_df <- data.frame(
  season = integer(),
  r2_marginal = numeric(),
  r2_conditional = numeric(),
  correlation_coefficient = numeric(),
  assumption_check = character(),
  stringsAsFactors = FALSE
)

# Output directory
plot_dir <- "C:\\Users\\Adrian\\OneDrive - UNSW\\Documents\\publications\\preparation\\witchelina_awp_grazing\\model_results_random_intercept_and_slope" # Directory to save the plots

for (colname in columns) {
    
	season <- as.numeric(str_sub(colname, 6, -1))
	
	print(season)
	
	# Remove 255 values
	df <- filter(awp_data, !!as.symbol(colname) != 255)
	summary_df <- df %>% group_by(Paddock) %>% tally()
	
  # fit model with random intercept nd slope
  model <- try(glmmTMB(formula=as.formula(paste(colname, " ~ Distance + (1 + Distance | Paddock)")), data=df), silent=TRUE)
	df$predicted_values <- predict(model)
	
  # Save slopes and intercepts for each paddock as CSV
  model_values <- extract_random_coefs(model)
  colnames(model_values)[colnames(model_values) == "group"] <- "Paddock" 
  model_values <- merge(model_values, summary_df, by="Paddock")
  
  missing <- setdiff(paddock_list, summary_df$Paddock)
  
  for (p in missing) {
    new_1 <- add_row(df, Paddock = p)
    new_2 <- add_row(model_values, Paddock = p, n = 0)
    df <- new_1
    model_values <- new_2
  }
  
  write_csv(model_values, paste0(plot_dir, "/model_values_", season, ".csv"))
  
  y1 <- substr(season, 0, 4)
  m1 <- substr(season, 5, 6)
  y2 <- substr(season, 7, 10)
  m2 <- substr(season, 11, 12)
  season_label <- paste0(y1, '/', m1, ' - ', y2, '/', m2)
  
  scatterplot <- ggplot(df, aes(x = Distance, y = !!as.symbol(colname))) +
    geom_hex(show.legend = FALSE) +
    geom_line(aes(x = Distance, y = predicted_values), linetype = 2, color='red') +
    xlim(0, 10) +
    ylim(0, 100) +
    labs(title = season_label, x = "Distance from artificial water (km)", y = "Bare (%)") +
    geom_text(aes(label=paste('n=', n)), x=1, y=1, data=summary_df, size=3) +
    facet_wrap("Paddock", axes="all", axis.labels="margins") +
    theme_classic() +
    theme(strip.background = element_blank())
  
  ggsave(paste0(plot_dir, "/scatter_", colname, ".png"), scatterplot, height = 8, width = 10, dpi = 600)
  
  # handle any errors in fitting the model
  if (!inherits(model, "try-error")) {
	  
	  # Calculate R2 values using r2_nakagawa
		r2_values <- r2_nakagawa(model)
		r2_marginal <- format(r2_values$R2_marginal, scientific=FALSE)
		r2_conditional <- format(r2_values$R2_conditional, scientific=FALSE)
		
		# calculate Pearson correlation between observed and predicted values
		correlation_coefficient <- cor(df[[colname]], df$predicted_values, use="complete.obs")
		  
		# check model assumptions
		assumption_check <- tryCatch({
		  # use check_model to check all assumptions
			check_results <- check_model(model, check = c("pp_check", "linearity", "homogeneity", "normality", "qq", "reqq"))
			
			# visualize and save the check results
			plot_file_name <- paste0(plot_dir, "/check_", colname, ".png")
			ggsave(plot_file_name, plot(check_results), width=10, height=8)
			
			# return a summary or a flag if assumptions are met
			if (all(summary(check_results)$OK)) "Assumptions OK" else "Issues Detected"
		   }, error = function(e) {
			"Assumption Check Error"
		   })
		  
		# store the results
		results_df <- rbind(results_df, data.frame(
			season = season,
			r2_marginal = r2_marginal,
			r2_conditional = r2_conditional,
			correlation_coefficient = correlation_coefficient,
			assumption_check = assumption_check
		  ))
  }
}

write_csv(results_df, paste0(plot_dir, "/model_output_all_seasons.csv"))
