# To use any of the scripts from this file, select the text of the script 
# and press the "Run" button at the top of the script window. In some cases
# you will select multiple lines of script at one time. If the line contains
# a hash (#), all text to the right of the hash is "comment" and will not run
# as script. The script will run in a separate window, called "Console", which 
# should a visible window in RStudio. The console will show you which scripts have 
# been run and will sometimes monitor progress (depending on the script) and will
# display error messages, if any. 

# This tutorial assumes you have already installed the R, RStudio, ggplot2, and 
# Weathering Trends packages. If not, see supplementary material for Fisher et al., 
# 2017 or ////insert link to starting with WT info on GitHub///

# To open (or "load") Weathering Trends package select the line of script below. 
library(WeatheringTrends)

# To open ggplot2, required by Weathering Trends
library(ggplot2)

# Import data file. You may import your own by modifying the command below
# or using File > Import Dataset > From CSV to select your data file. Both command 
# lines can be selected together before clicking run. Using the menu to import files
# will generate code for you and offers most of the import options in R,
# inlcuding renaming the data set, skipping the first N rows, using the header 
# row for column names, changing column data type (numeric, character, categorical,
# logical (true/fasle), etc.).
WTexampleLP <- read.csv("~/yourdirectorypath/WeatheringTrends/Data/WTexampleLP.csv")
   View(WTexampleLP)

# Subset data to work with one site at a time using WTexampleLP.csv. 
well1 <- subset(WTexampleLP, Site=="LP_Well1")
well2 <- subset(WTexampleLP, Site=="LP_Well2")

# An example plot to test data import. Select both lines to capture full command.
# In RStudio the plot will display in a window called "Plots", which is separate from
# the Console or the script quadrant.
ggplot(well1) + scale_y_reverse() +
  geom_point(aes(x=CaO, y=voBottom/100, colour=Site))


################ FitElementRatios Function ################ 
# FitElementRatios is a function to find the best fit trendline for element ratios 
# over all depth intervals measured. This function will output the depth to bedrock,
# or the depth to a consistent concentration of element ratios, with confidence
# intervals on that depth. In the function definition, mobile elements are assigned as "mm" 
# in a concatenated list first. The items in this list must exactly match the column
# headers in your data. Immobile elements are assigned as "ii" in a separate list. 
# These two lists are required in the function, followed by which column to use for depth 
# which will automatically plot with the smallest value at the top, meaning that 
# the model expects depth to be a positive number such as 450 cm instead of -450 cm. The next
# input is the name of the data set. In the example case we use the subset
# data for Well 1, called "well1". We assign the whole function result a new variable name,
# which we will use to display the results. 
# To run this function, first assign the mobile elements by selecting all lines or by 
# inserting your curser after the closing parenthesis and pressing Run.
# This function will require a few minutes to process if you use the full list of elements, 
# and the function will display its progress and make a note of any missing data.

mm <- c("Na2O", "MgO", "Al2O3", "SiO2", "K2O",
  "CaO", "MnO", "Fe2O3", "P2O5",
  "Ba_ppm", "Ce_ppm", "Co_ppm", "Cr_ppm",
  "Cu_ppm", "La_ppm",
  "Nd_ppm", "Ni_ppm", "Pb_ppm", "Rb_ppm",
  "Sc_ppm", "Sr_ppm", "Th_ppm", "V_ppm",
  "Y_ppm", "Zn_ppm")
ii <- c("Zr_ppm", "TiO2", "Hf_ppm", "Nb_ppm")
well1rock <- FitElementRatios(mm, ii, "voBottom", well1)

################ WT Plot to PDF ################ 
# Plot Weathering Trends model results to a pdf file. Select all three lines and press
# Run. You may adjust the height and width to your preference. The example is 60 x 8 
# (inches), which worked well to plot 25 mobile elements (height) by 4 immobile elements 
# (width). You will find the pdf in your current working directory. To find your working
# directory at any point use the follwoing command:
getwd()

# The horizontal scale is set by the user according to the following:
# plot(fits, scales="sliced") applies the same horizontal scale range to all plots
# plot(fits, scales="free") makes scales fit each individual plot

pdf("well1rock_todaysdate.pdf", height=60, width=8)
plot(well1rock, scales="sliced")
dev.off()

################ WT Numerical Outputs ################ 
# To output a table of numerical values from the Weathering Trends model use the 
# "coef" function to receive depth1, depth2, logratio1, logratio2, s1, and s2.
# To print (or list) directly in the console window:
coef(well1rock, type="output") 
# To print a csv to your working directory (edit the file name):  
write.csv(as.data.frame(coef(well1rock,type="output")),file="well1rockoutput_todaysdate.csv") 

# Confidence intervals
# To print (or list) directly in the console window:
coef(well1rock, type="par.long")
# To print a csv to your working directory (edit the file name):  
write.csv(as.data.frame(coef(well1rock,type="par.long")),file="well1rockCI_todaysdate.csv") ## to csv

# Output of p, d, c, s1, s2, r
# To print (or list) directly in the console window:
coef(well1rock, type="par") 
# To print a csv to your working directory (edit the file name):  
write.csv(as.data.frame(coef(well1rock,type="par")),file="well1rockPARtodaysdate.csv") ## to csv



################ FitTaus Function ################ 
# FitTaus is a function that plots fractional mass change to the data. 
# Mobile elements are preseneted in a concatenated list first, followed
# by immobile elements. After the two concatenated element lists, the 
# function requests which column to use for depth, which will automatically
# plot with the smallest value at the top, meaning that the model expects  
# depths to be positive numbers such as 450 cm instead of -450 cm. The next
# input is the name of the data. In the example case we use the subset data for 
# Well 1, which we already assigned "well1". Finally, we specify the depth to bedrock
# for Tau as the "cutoff" value. All element concentrations below the "cutoff" 
# depth will be averaged and the average value is defined as the parent material
# for the Tau calculations at each interval. 

# Using Weathering Trends we determined that the depth to bedrock for our sample 
# dataset were: well 1 at 12.3 m (1230 cm) and well 2 at 7.2 m (720 cm)
# and we will use these depths in the fractional mass change (tau) function
# that has been coded in the Weathering Trends model. 

taus1 <- FitTaus(c("Na2O", "MgO", "Al2O3", "SiO2", "K2O",
                   "CaO", "MnO", "Fe2O3", "P2O5",
                   "Ba_ppm", "Ce_ppm", "Co_ppm", "Cr_ppm",
                   "Cu_ppm", "La_ppm",
                   "Nd_ppm", "Ni_ppm", "Pb_ppm", "Rb_ppm",
                   "Sc_ppm", "Sr_ppm", "Th_ppm", "V_ppm",
                   "Y_ppm", "Zn_ppm"),
                 c("Zr_ppm", "TiO2", "Hf_ppm", "Nb_ppm"),
                 "voBottom", well1, cutoff=c(1230))

# You may also use the previously defined lists of mobile (mm) and immobile (ii)
# elements within the FitTaus function. 

taus1 <- FitTaus(mm, ii, "voBottom", well1, cutoff=c(1230))

# Select the four lines below to print the plots to a PDF. You may adjust the 
# height and width to your preference. The example set is 60 x 8 (inches), which 
# worked well to plot 25 mobile elements (height) by 4 immobile elements (width).

pdf("taus1printAll2.pdf", height=60, width=8)
plot(taus1, scales="free")
dev.off()


################ Statistics of element concentrations ################ 
# The functions below are not part of Weathering Trends. They are either native
# to R or contained in an installed R package. We include these to demonstrate
# how we examined the correlation between elements in Fisher et al., 2017.

##subset data
well1cor <- subset(WTexampleLP, Site=="LP_Well1")
well2cor <- subset(WTexampleLP, Site=="LP_Well2")

## for Pearson correlation, r
plot(well1cor$Nb_ppm, well1cor$TiO2)
y=well1cor$TiO2
x=well1cor$Nb_ppm
abline(lm(y~x))
cor(well1cor$Nb_ppm, well1cor$TiO2)
##for p value
cor.test(well1cor$Nb_ppm, well1cor$TiO2)$p.value
##to get n
install.packages("pastecs") # install only once
library(pastecs) # open package once per R session
stat.desc(well1cor$Nb_ppm)

plot(well1cor$Zr_ppm, well1cor$Hf_ppm)
x=well1cor$Zr_ppm
y=well1cor$Hf_ppm
abline(lm(y~x))
cor(well1cor$Zr_ppm, well1cor$Hf_ppm)
cor.test(well1cor$Zr_ppm, well1cor$Hf_ppm)$p.value
stat.desc(well1cor$Zr_ppm)

## immobile element Coefficient of Variation (CV)
# Well 1:
well1immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well1cor)
print(well1immobile$s.overall)
mean(well1cor$Nb_ppm)
## coefficient of variation of immobile
well1immobile$s.overall/mean(well1cor$Nb_ppm)*100

## immobile element Coefficient of Variation (CV)
# Well 2:
well2immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well2cor)
print(well2immobile$s.overall)
mean(well2cor$Nb_ppm)
## coefficient of variation of immobile
well2immobile$s.overall/mean(well2cor$Nb_ppm)*100



