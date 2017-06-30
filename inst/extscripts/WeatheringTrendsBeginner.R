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
# 2017 or explore the Weathering Trends documentation at
# https://github.com/fisherba/WeatheringTrends/tree/master/inst
# or if you have R and Weathering Trends installed, by running this command.
help(package="WeatheringTrends")

################ Load Necessary Packages ################
# To open (or "load") Weathering Trends package you have a few options:
# 1) select the line of script below and press the "Run" button at the top of the script window
# 2) or place your cursor at the end of the script line and press the "Run" button
# 3) or do either of the above and instead of pressing the "Run" button you may
# press Command-Enter (Macs) or Control-Enter (Windows).
library(WeatheringTrends) # open package once per R session
# This will not produce a response in the console window. In this case, no news is good news.
# If you have a new empty prompt ">", then the package loaded correctly.

# As above, load ggplot2, which is a package that helps make nice plots.
library(ggplot2) # open package once per R session

################ Import Data File ################
# If your working directory is ~/WeatheringTrends/, you may import the data file by selecting
# the command below.
# To find (or confirm) your working directory use the following command:
getwd()

# This command will import the data file into R.
WTexampleLP <- read.csv("~/WeatheringTrends/inst/extdata/WTexampleLP.csv")
# This command will open the data set for viewing; by default it will appear in the same
# pane as this script. After you view the file, use the tabs to navigate back to this file.
View(WTexampleLP)

# If your working directory is not ~/WeatheringTrends/, you can modify the path above or
# import data from the menu: File > Import Dataset > From CSV to select your data file.
# This will open a dialogue box where you select "Browse" and navigate through your file system
# to select the data file. In the dialogue box, check the following options:
# First Row as Names, Trim Spaces, Open Data Viewer. Also select Delimiter: Comma.
# In the box that says "Name:" be sure it says "dataset". In the "Skip:" box, enter "0".
# Finally, select the "Import" button and the data file will be viewable in a tab beside
# this ".R" file.

# Note: using the menu to import files will generate code for you and offers most of the import
# options in R, inlcuding renaming the data set, skipping the first N rows, using the header
# row for column names, changing column data type (numeric, character, categorical,
# logical (true/false), etc.).

################ Subset Data ################
# Subset data to work with one site at a time using WTexampleLP.csv.
well1 <- subset(WTexampleLP, Site=="LP_Well1")
well2 <- subset(WTexampleLP, Site=="LP_Well2")

################ Plot Data ################
# An example plot to test data import. Select both lines before pressing "Run" to capture
# the full command.
# In RStudio the plot will display in a window called "Plots", which is separate from
# the Console or the script windows. You may resize all RStudio windows to your preference.
ggplot(well1) + scale_y_reverse() +
  geom_point(aes(x=CaO, y=voBottom/100, colour=Site))


################ FitElementRatios Function ################
# FitElementRatios is a function to find the best fit trendline for element ratios
# over all depth intervals measured. This function will output the depth to bedrock,
# or the depth to a consistent concentration of element ratios, with confidence
# intervals on that depth. In the function definition, mobile elements are assigned as "mm"
# in a concatenated list. The items in this list must exactly match the column
# headers in your data. Immobile elements are assigned as "ii" in a separate list.
# These two lists are required in the function, followed by which column to use for depth
# which will automatically plot with the smallest value at the top, meaning that
# the model expects depth to be a positive number such as 450 cm instead of -450 cm. The next
# input is the name of the data set. In the example case we use the subset
# data for Well 1, called "well1". We assign the whole function result to a new variable name,
# which we will use to display the results.

# To run FitElementRatios function, first assign the mobile elements as "mm".
mm <- c("Na2O", "MgO", "CaO", "Fe2O3")

# Next assign the immobile elements as "ii".
ii <- c("Zr_ppm", "Nb_ppm")

# This function will require a few minutes to process and the function will display its progress
# and note of any missing data or zero values, which will be set to the lowest value of it's type
# because we cannot have log(0). The best practice is to set all zero values to the detection limit
# for each element. We left them as zero values so you can see how the model handles this "error".
well1rock <- FitElementRatios(mm, ii, "voBottom", well1)

# Here's the full version used for Fisher et al., 2017:
mm_full <- c("Na2O", "MgO", "Al2O3", "SiO2", "K2O",
        "CaO", "MnO", "Fe2O3", "P2O5",
        "Ba_ppm", "Ce_ppm", "Co_ppm", "Cr_ppm",
        "Cu_ppm", "La_ppm", "Nd_ppm", "Ni_ppm",
        "Pb_ppm", "Rb_ppm", "Sc_ppm", "Sr_ppm",
        "Th_ppm", "V_ppm", "Y_ppm", "Zn_ppm")
ii_full <- c("Zr_ppm", "TiO2", "Hf_ppm", "Nb_ppm")
well1rock_full <- FitElementRatios(mm_full, ii_full, "voBottom", well1)

################ WT Plot to window in RStudio ################
# To plot the results of the WT model run, select the command below and select "Run".
plot(well1rock, scales="sliced")

# The horizontal scale is set by the user according to the following:
# plot(well1rock, scales="sliced") # applies the same horizontal scale range to all plots
# plot(well1rock, scales="free") # makes scales fit each individual plot

# Note, if the plot is too large for your plot window, you will receive an error message.
# To overcome this error, one solution is to plot directly to a PDF file, as instructed below.

################ WT Plot to PDF ################
# Because the Plot view window is limited by your screen dimensions, we overcome this
# by creating a PDF vector graphic file of the model output. This plot is editable in
# vector graphics software (e.g. Adobe Illustrator).
# To plot Weathering Trends model results to a PDF file in your current working directory,
# recall that you find your working directory use the following command:
getwd()

# This plots 8.5 x 4 (inches), which works well to plot 4 mobile elements (height)
# by 2 immobile elements (width).
pdf("well1rock_todaysdate.pdf", height=8.5, width=4)
plot(well1rock, scales="sliced")
dev.off()

# The full version example is 60 x 8 (inches), which worked well to plot 25 mobile
# elements (height) by 4 immobile elements (width).
pdf("well1rock_full_todaysdate.pdf", height=60, width=8)
plot(well1rock_full, scales="sliced")
dev.off()

################ WT Numerical Outputs ################
# To output a table of numerical values from the Weathering Trends model use the
# "coef" function to receive depth1, depth2, logratio1, logratio2, s1, and s2.
# To print (or list) directly in the console window:
coef(well1rock, type="output")
# To print a csv to your working directory (edit the file name):
write.csv(coef(well1rock, type="output"), file="well1rockoutput_todaysdate.csv", row.names=FALSE)

# Confidence intervals
# To print (or list) directly in the console window:
coef(well1rock, type="par.long")
# To print a csv to your working directory (edit the file name):
write.csv(coef(well1rock,type="par.long"), file="well1rockCI_todaysdate.csv", row.names=FALSE) ## to csv

# Output of p, d, c, s1, s2, r
# To print (or list) directly in the console window:
coef(well1rock, type="par")
# To print a csv to your working directory (edit the file name):
write.csv(coef(well1rock,type="par"), file="well1rockPAR_todaysdate.csv", row.names=FALSE) ## to csv

################ FitTaus Function ################
# FitTaus is a function that plots fractional mass change to the data.
# Mobile elements are presented in a concatenated list first, followed
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

taus1 <- FitTaus(c("Na2O", "MgO", "CaO", "Fe2O3"),
                 c("Zr_ppm", "Nb_ppm"),
                 "voBottom", well1, cutoff=1230)

# You may also use the previously defined lists of mobile (mm) and immobile (ii)
# elements within the FitTaus function.

taus1 <- FitTaus(mm, ii, "voBottom", well1, cutoff=1230)

# To plot within RStudio:
plot(taus1, scales="free")

# Select the three lines below to print the plots to a PDF. You may adjust the
# height and width to your preference. The example set is 8.5 x 4 (inches), which
# was sufficient to plot 4 mobile elements (height) by 2 immobile elements (width).

pdf("taus1_todaysdate.pdf", height=8.5, width=4)
plot(taus1, scales="free")
dev.off()

################ Statistics of element concentrations ################
# The code below demonstrates how we examined the correlation between
# elements in Fisher et al., 2017.
# For these examples, "Run" one line of code at a time, in the sequence shown because
# successive computations build on previous definitions.

# for Pearson correlation, r
# plot with regression line added
plot(TiO2 ~ Nb_ppm, data=well1)
abline(lm(TiO2 ~ Nb_ppm, data=well1))
# output correlation and p value
cor.test(~ TiO2 + Nb_ppm, data=well1)
# to get n, this removes any missing values before counting
sum(complete.cases(well1[c("TiO2", "Nb_ppm")]))

plot(Zr_ppm ~ Hf_ppm, data=well1)
abline(lm(Zr_ppm ~ Hf_ppm, data=well1))
cor.test(~ Zr_ppm + Hf_ppm, data=well1)
sum(complete.cases(well1[,c("Zr_ppm", "Hf_ppm")]))

## immobile element Coefficient of Variation (CV) or relative standard deviation
# Well 1, element Nb
well1immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well1)
well1immobile$s.overall # display standard deviation for the element Nb, you may skip this step
mean(well1$Nb_ppm) # mean of element Nb, you may skip this step
# coefficient of variation of element Nb is a computation using two codes above
well1immobile$s.overall/mean(well1$Nb_ppm)*100

## immobile element Coefficient of Variation (CV)
# Well 2, element Nb
well2immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well2)
well2immobile$s.overall
mean(well2$Nb_ppm)
## coefficient of variation of immobile
well2immobile$s.overall/mean(well2$Nb_ppm)*100
