library(googlesheets4)
library(openxlsx)
library(tidyverse)
library(devtools)
library(stats)
library(lubridate)
library(dplyr)





###load all data from the data folder ----


load_all_csvs_to_env <- function(folder_path, env = .GlobalEnv) {
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  for (file in files) {
    df_name <- tools::file_path_sans_ext(basename(file))  # remove .csv
    assign(df_name, read.csv(file), envir = env)
  }
}

load_all_csvs_to_env("data")

#define colors for treatments, here controllers
controller_hofw_colors <- c("Adrian's Farm-House 2" = "#EEB38C", 
                            "Adrian's Farm-House 3" = "#92664A", 
                            "Adrian's Farm-House 4" = "#2F1B12")

#define labels
labelsbulk<-c("high bulk density","medium bulk density","low bulk density")



# Fungal and Bacterial abundance ---- 

#Fungi 18S
abundance_18S %>%
  ggplot(aes(x = duration_d, y = Abs_Cpy_g_Wet/10^9, group = factor(Controller))) +
  
  stat_summary(aes(color = Controller, fill = Controller), fun = "mean", geom = "bar", size = 0.4, width = 0.6, position = position_dodge(width = 0.6),show.legend = FALSE) +

  stat_summary(aes(color = "white"), fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar",size=1, width = 0.6, position = position_dodge(width = 0.6),show.legend = FALSE) +
  
  geom_point(aes(color = "white"), size = 3, position = position_dodge(width = 0.6), stroke = 1) +  # Set show.legend to FALSE
  
  labs(x = "Rearing day", y = expression("18S copies x " * 10^9 * "(g"^-1 * "substrate)"))+
  scale_color_manual(values = controller_hofw_colors) +
  scale_fill_manual(values = controller_hofw_colors) +  
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  guides(color = guide_legend(override.aes = list(size = 7.3)))




#Bacteria 16S

abundance_16S%>%
  ggplot(aes(x = duration_d, y = Abs_Cpy_g_Wet/10^9, group = factor(Controller))) +
  
  stat_summary(aes(color = Controller, fill = Controller), fun = "mean", geom = "bar", size = 0.4, width = 0.6, position = position_dodge(width = 0.6),show.legend = FALSE) +
  #add erroror bars
  stat_summary(aes(color = "white"), fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar",size=1, width = 0.6, position = position_dodge(width = 0.6),show.legend = FALSE) +
  
  geom_point(aes(color = "white"), size = 3, position = position_dodge(width = 0.6), stroke = 1) +  # Set show.legend to FALSE
  
  
  labs(x = "Rearing day", y = expression("16S copies x " * 10^9 * "(g"^-1 * "substrate)"))+
  scale_color_manual(values = controller_hofw_colors, name = "Cocopeat fiber addition\n to canteen food waste", labels = c("2% fiber addition\n ","4% fiber addition\n","6% fiber addition\n")) +
  scale_fill_manual(values = controller_hofw_colors) +  
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  guides(color = guide_legend(override.aes = list(size = 7.3)))




# Substrate temperature ----

subs_temp %>%
ggplot(aes(duration_d, value, color=Controller)) + 
  
  geom_point(alpha=0.1,size=0.1)+
  stat_summary(fun = "mean", geom = "line", linewidth = 2)+
  
  
  ylim(27.3,52)+
  
  
  
  theme_bw(base_size = 12)+
  
  
  ylab("Substrate temperature (C°)") +
  xlab("Rearing day")+
  labs(color = "Treatment") +
  
  scale_color_manual(values = controller_hofw_colors ) +

  theme(legend.position = "none") +
  
  guides(color = guide_legend(
    override.aes = list(size = 7.3) , byrow = TRUE # Adjust the size of the legend dots
  ))



# pH----
subs_ph %>%
  
  ggplot(aes(x = duration_d.y, y = pH_value, color = Controller, group = Controller)) +
  geom_point(size=3) +
  
  # Adding error bars using stat_summary
  stat_summary(
    fun.data = "mean_sdl",
    fun.args = list(mult = 1),
    geom = "errorbar",
    size = 1.4,
    width = 0.2,
    aes(color = Controller)
  ) +
  
  # Adding lines for the mean using stat_summary
  stat_summary(
    fun = "mean",
    geom = "line",
    size = 1.4,
    aes(color = Controller)
  ) +
  
  theme_bw(base_size = 12) +
  xlab("Rearing day") +
  ylab("pH value") +
  
  # Customizing the color scale
  scale_color_manual(
    values = controller_hofw_colors,
    name = "bulk density",
    labels = labelsbulk) +
  
  scale_color_manual(values = controller_hofw_colors, name = "Food waste", labels = labelsbulk) +
  scale_fill_manual(values = controller_hofw_colors) +  
  theme_bw(base_size = 12) +
  theme(
    legend.spacing.y = unit(1, "lines"),
    legend.text = element_text(margin = margin(r = 10)),
    legend.box.spacing = unit(0.5, "lines"),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  guides(color = guide_legend(override.aes = list(size = 7.3)))




# NH3----
overall_NH3%>%
  ggplot(aes(x = duration_d.y, y = respiration.NH3_mg_total_min, group = Controller)) +
  geom_point(aes(color = Controller),size=3) +
  
  # Adding lines for the mean using stat_summary
  stat_summary(aes(color = Controller), fun = "mean", geom = "line", size = 1.4) +
  
  # Adding error bars using stat_summary
  stat_summary(
    aes(color = Controller),
    fun.data = "mean_sdl",
    fun.args = 1,# Standard deviation limits
    geom = "errorbar",
    size = 1.4,
    width = 0.2
  ) +
  
  theme_bw(base_size = 12) +  # Adjust the base font size as needed
  xlab("Rearing day") +
  ylab(expression("NH"[3]*" production (mg min"^"-1"*") per crate")) +
  scale_x_continuous(limits = c(0,6.2))+
  
  # Customizing the color scale
  scale_color_manual(
    values = controller_hofw_colors,
    name = "Bulk density",
    labels = c("2% fiber addition\n", "4% fiber addition\n", "6% fiber addition\n")
  ) +
  
  # Customizing the legend
  theme(
    legend.spacing.y = unit(1, "lines"),
    legend.text = element_text(margin = margin(r = 10)),
    legend.box.spacing = unit(0.5, "lines"),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  
  guides(color = guide_legend(
    override.aes = list(size = 7.3),
    byrow = TRUE  # Arrange legend items by rows
  ))



#### total NH3 production per crate ---- 
#mean values for each Controller
mean_values <- overall_NH3 %>%
  group_by(duration_d.y, Controller) %>%
  summarize(mean_resp_NH3_mg_min = mean(respiration.NH3_mg_total_min, na.rm = TRUE))%>%
  #convert from mg/min to mg/day
  mutate(mean_resp_NH3_mg_day = mean_resp_NH3_mg_min * 1440)

# function for integration 
trapezoidal_integration <- function(x, y) {
  # Ensure the x values are ordered
  ordered_indices <- order(x)
  x <- x[ordered_indices]
  y <- y[ordered_indices]
  
  # Calculate the area 
  area <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  return(area)
}

# Step 3: Apply the integration function for each Controller - mg/min x days  per crate

total_NH3_production <- mean_values %>%
  group_by(Controller) %>%
  summarize(total_NH3_production_mg = trapezoidal_integration(duration_d.y, mean_resp_NH3_mg_day)) %>%
  mutate(total_NH3_production_mg = round(total_NH3_production_mg, 2)) %>%
  mutate(total_NH3_production_g = total_NH3_production_mg / 1000) # convert to g




# CO2 emissions  from larvae, microbes and overall ----

#respiration.CO2_mg_total_min_larvae is the CO2 produced by the sample of larvae
#n is the number of larvae in the sample from one crate
#larvae survived is the estimated number (based on survival rate) of larvae in one crate
#respiration.CO2_mg_total_min_crate is the CO2 produced by one crate (larvae + microbes in the crate)
#CO2_total_gh is the the total CO2 production of three crates


overall_CO2 %>%
  ggplot() + 
  geom_point(aes(duration_d, (CO2_total_gh * 1000 / 60 / 3), color = Controller),size = 0.2) +
  #microbial respiration bars
  stat_summary(data = microbe_larvae_CO2, aes(x = duration_d, y = ifelse(n == 0, respiration.CO2_mg_total_min_crate, respiration.CO2_mg_total_min_crate - (respiration.CO2_mg_total_min_larvae / n * larvae_survived) ),group= Controller, fill=Controller, alpha=1), 
               fun = "mean", geom = "bar",shape=5, size = 0.5,width=0.2, position = position_dodge(width = 0.2)) + 
  
  #total respiration bars
  stat_summary(data = microbe_larvae_CO2, aes(x = duration_d, y = ifelse(n == 0, respiration.CO2_mg_total_min_crate, respiration.CO2_mg_total_min_crate),group= Controller, fill=Controller, alpha=0.8), 
               fun = "mean", geom = "bar",shape=5, size = 0.5,width=0.2, position = position_dodge(width = 0.2)) +
  
  #total respiration points 
  geom_point(data = microbe_larvae_CO2, aes( x = duration_d,
                                                          y = ifelse(n == 0, respiration.CO2_mg_total_min_crate, respiration.CO2_mg_total_min_crate), group=Controller, alpha=0.8), 
             size = 2, position = position_dodge(width = 0.2)) +
  
 
  #substrate respiration points 
  geom_point(data = microbe_larvae_CO2, aes( x = duration_d,
                                                          y = ifelse(n == 0, respiration.CO2_mg_total_min_crate, respiration.CO2_mg_total_min_crate - (respiration.CO2_mg_total_min_larvae / n * larvae_survived)), group=Controller, color="white"), 
             size = 2, position = position_dodge(width = 0.2)) +
  
  #substrate respiration error bars
  stat_summary(data = microbe_larvae_CO2, aes(x = duration_d, y = ifelse(n == 0, respiration.CO2_mg_total_min_crate, respiration.CO2_mg_total_min_crate - (respiration.CO2_mg_total_min_larvae / n * larvae_survived)),group = Controller), 
               fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar", size = 1, width=0.2, position = position_dodge(width = 0.2))+ 
  
  
 


ylab(expression("Average CO"[2]*" production (mg min"^{-1}*") per crate")) +
  xlab("Rearing day") +
  labs(color = "Treatment") +
 
  ylim(c(-50, 400)) +
  
  scale_color_manual(values = controller_hofw_colors,
                     name = " food waste", 
                     labels = c( 
                       "high \n","medium\n ","low\n") ) +
  scale_fill_manual(values = controller_hofw_colors,
                    name = "food waste", 
                    labels = c( 
                      "high \n","medium\n ","low\n") ) +
  
  
theme_bw()+
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))









# Larval weight ----
larval_mass %>%

ggplot(aes(duration_d.y,value, group=Controller)) + 
  
  stat_summary(fun.data = "mean_sdl",fun.args = 1, geom = "errorbar", size = 0.7, width=0.1 )+ 
  geom_point(aes(color=Controller)) +
  stat_summary(aes(color=Controller),fun = "mean", geom = "line", size = 0.7)+
  
  
  theme_bw(base_size = 12)+
  

  ylab("Larval mass (mg DM)") +
  xlab("Rearing day") +
  
  scale_color_manual(values = controller_hofw_colors ) +
  
  theme(legend.position = "none") +
  
 
  guides(color = guide_legend(
    override.aes = list(size = 7.3) , byrow = TRUE # Adjust the size of the legend dots
  ))

# Rearing performance parameters----
#Print results

simple_variances <- performance_param %>%
  
  mutate(value = as.numeric(value)) %>%  
  group_by(parameter, Controller) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    variance = var(value, na.rm = TRUE),
    sd= sd(value, na.rm = TRUE)
  ) 

print(simple_variances, n=100)
