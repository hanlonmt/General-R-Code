# Extract multiple comparison letters and automatically bind them to to the dataframe that you use for plotting
# I use the summary data function (second sheet) to make a dataframe that has means, SD, and SE all calculated. I then use this for plotting

# this code is generic and assumes d=origninal data, d.sum=summarized data, and variable of interest = var, Trmt is the treatment - can be expanded as needed

# this top portion is a boxcox transformation - skip if not needed

if(!require(multcomp)) { 
install.packages("multcomp"); require(multcomp)}
require(MASS)



# run multiple comparison test using glht in the multcomp package - this is a Tukey Test
var.t <- summary(glht(d.lm2, linfct=mcp(Trmt="Tukey")))


# get the multiple comparison letters - cld gives a list
var.l <- cld(var.t)


# extract letters as a vector to recombine with original dataframe
new <- as.data.frame(var.l[[10]]$Letters)
var.letters <- new[,1]

# Combine with summary data frame to be used for plotting
# I use the summary data function to create a data frame and then ammend to it. In this case the data frame is d.sum
d.sum$var.letters <- var.letters


################## To add to a ggplot graphic (I use with barplots, though works with anything)
# add this to your plot
# no need to to have the data and aes_string arguments if they're defined in your master plot call (ggplot())
# I generally don't do that since I plot in layers, thus I have to redefine.  
# If you have groups that aren't defined in that master call BE SURE to add group="" to place them correctly. Vjust is just vertical position

geom_text(data=d.sum, position= position_dodge(0.9), aes_string(x="Trmt", y=m, label="var.letters", group="day"), vjust=-2.2)
