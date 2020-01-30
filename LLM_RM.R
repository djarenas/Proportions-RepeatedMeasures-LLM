#Filename: LLM_RM.R
library(car)
library(lme4)

#===================================================================
#Sourcing additional files
#===================================================================
#Set the directory
setwd("C:/Users/arenasd/Desktop/FullCompositional_Updated013120")
print(getwd())
#Source the file containing the functions
source("functions.R")
source("llm_functions.R")

#===================================================================
#Experiment-specific parameters
#===================================================================
#Define resolution, so that you can replace zeros with this value
cell_num <- 10000           #Order of magnitude of counted cells
resolution <<- 1/cell_num   #the smallest possible value is 1/cell_num, will be used to replaced zeros in the data
tpoints <- c(0,30,60,120)             #Number of time points

#===================================================================
#Input-files information
#===================================================================
a_file <- "Flow_pS6_CD14_iMCD.csv"
b_file <- "Flow_pS6_CD14_controls.csv"
#Set a few global variables
percentage <<- FALSE #Is the input data in percentage? If not, it should be in proportions
negative_values_allowed <<- FALSE
paired_boolean <<- FALSE #Are you comparing paired data?
t_test_alternative <<- "two.sided"   #two.sided, less, or equal

#===================================================================
#File clean up and organization
#===================================================================
group_name <- "iMCD"
a_label_organized <- organize_by_label(a_file, group_name, tpoints)
group_name <- "HD"
b_label_organized <- organize_by_label(b_file, group_name, tpoints)

#===================================================================
#Plot each patient dependence over time
#===================================================================
cell_type = "CD8"
ylimits <- c(0,0.50)
#----------------------------------------------------------------------------------------
#Plot for subjects
#----------------------------------------------------------------------------------------
a_data <- a_label_organized; counter = 1
#JPEG CALL
jpegtitle <- paste0("C:/Users/arenasd/Desktop/output_plot_subjects", cell_type, ".jpg")
jpeg(jpegtitle, width = 4, height=8, units = "in", res = 1200)
c = 1
s_data = a_data[a_data$patient == c, ]
plot(s_data$time, s_data$proportion, ylim = ylimits, xlim = c(0,125), col = "royalblue3", type = "o", lty = 3, ylab = "", xlab = "", cex = 1.5, cex.lab = 1.5, cex.axis = 2)
abline(lm(s_data$proportion ~ s_data$time), lwd = 3, col = "royalblue3")
for (c in 2:8){
  counter = counter + 1
  s_data = a_data[a_data$patient == c, ]
  points(s_data$time, s_data$proportion, cex = 1.5, type = "o", lty = 3, col = "royalblue3")
  abline(lm(s_data$proportion ~ s_data$time), col = "royalblue3", lwd = 3)
}
dev.off()#JPEG off
#----------------------------------------------------------------------------------------
#Plot for controls
#----------------------------------------------------------------------------------------
b_data <- b_label_organized; counter = 1
#JPEG CALL
jpegtitle <- paste0("C:/Users/arenasd/Desktop/output_plot_controls", cell_type, ".jpg")
jpeg(jpegtitle, width = 4, height=8, units = "in", res = 1200)
c = 1
s_data = b_data[b_data$patient == c, ]
plot(s_data$time, s_data$proportion, ylim = ylimits, xlim = c(0,125), col = "black", type = "o", lty = 3, ylab = "", xlab = "", cex = 1.5, cex.lab = 1.5, cex.axis = 2)
abline(lm(s_data$proportion ~ s_data$time), lwd = 3, col = "black")
for (c in 2:8){
  counter = counter + 1
  s_data = b_data[b_data$patient == c, ]
  points(s_data$time, s_data$proportion, cex = 1.5, type = "o", lty = 3, col = "black")
  abline(lm(s_data$proportion ~ s_data$time), col = "black", lwd = 5)
}
dev.off()#JPEG off

#===========================================================================
#Linear Mixed Model (LLM) Analysis
#===========================================================================
#------------
#iMCD
#------------
print("iMCD")
rma = lmer(proportion ~ time + (1 | patient), a_label_organized)
print(rma)  
print(confint(rma))
#------------
#HD
#------------
print("HD")
rma = lmer(proportion ~ time + (1 | patient), b_label_organized)
print(rma)  
print(confint(rma))