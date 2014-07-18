if(!require(plyr)) { 
  install.packages("plyr"); require(plyr)}

### this function will take a data frame and return a data frame with the mean, standard deviation, and standard error 
### calculated for all the NUMERIC columns split by all the FACTOR columns
### I would first recommend running:

# colwise(class)(dataframe)

### to determine the class of each column.  
### If you have columns of class integer that you would like as factors, the function will automatically change this. If
### you have integers that you want calculations on, change their class to numeric.

### Convert anything not labeled as a factor or as numeric to the class of your choice:

# data$column_name <- as.numeric(data$column_name)
### or
# data$column_name <- as.factor(data$column_name)
### or you can use the column indicies to do the same thing. For example, if the variable in the first column needs to be converted to a factor:
# data[,1] <- as.numeric(data$column_name)

### Once you have the class of everything as you wish, run the summarydata function code.
### When you want to run it on your data, run this and supply additional arguments
### The base is this: sum_df <- summarydata(df) 
### It will split over ALL FACTOR VARIABLES and calculate mean, standard deviation, and standard error for all numeric (or integer) variables

### To split over only some factors:
# sum_df <- summarydata(df, splittingfactors=c("v1","v2", etc.)) where v1 and v2 are your variables of choice

### It will not automatically convert integer columns to factors, but you can do this. An integer will remain an integer and be calculated as a numeric variable
### To convert these:
# sum_df <- summarydata(df, convert_integers=T)

### mean, SD, and SE are all automatically calculated. To only calculate some, change the logical arguments of calc_mean, calc_sd, and calc_se
### to only get mean and se:
# sum_df <- summarudata(df,calc_sd=F)


require(plyr)

summarydata <- function (df,splittingfactors=NULL, convert_integers=F, calc_mean=T, calc_sd=T, calc_se=T){
  
  if((convert_integers)){
    for (i in 1:length(df)) if (is.integer(df[,i]))(df[,i]<-as.factor(df[,i]))
  }
  
  #this section gets an index of factors and numeric variables to use later
  v.numeric <- (rep(0,length(df)))
  v.factor <- (rep(0,length(df)))
  for (i in 1:length(df)) if (is.factor(df[,i])) (v.factor[i]<-as.vector((names(df[i]))))
  for (i in 1:length(df)) if (is.numeric(df[,i])) (v.numeric[i]<-as.vector((names(df[i]))))
  v.factor<-v.factor[!v.factor==0]
  v.numeric<-v.numeric[!v.numeric==0]
  
  if(!is.null(splittingfactors)){
    v.factor <- as.vector(c(splittingfactors))
  }
  
  # each of these three if loops calculate mean, sd, and se for each column (numcolwise) and then combines into a results df
  if(calc_mean){
    mean.res <- ddply(df, c(v.factor), numcolwise(mean, na.rm=TRUE))
    mean.names <- (rep(0,length(v.numeric)))
    for (i in 1:length(v.numeric)) (mean.names[i]<-as.vector(paste(v.numeric[i],"mean",sep=".")))
    column.names <- c(v.factor,mean.names)
    colnames(mean.res) <- column.names
  }
  
  if(calc_sd){
    sd.res <- ddply(df, c(v.factor), numcolwise(sd, na.rm=TRUE))
    sd.names <- (rep(0,length(v.numeric)))
    for (i in 1:length(v.numeric)) (sd.names[i]<-as.vector(paste(v.numeric[i],"sd",sep=".")))
    column.names <- c(v.factor,sd.names)
    colnames(sd.res) <- column.names
    ifelse(calc_mean,ifelse(calc_sd,mean.sd.res <- merge(mean.res,sd.res), mean.sd.res<-mean.res),mean.sd.res<-sd.res)
  }
  
  
  if(calc_se){
    se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    se.res <- ddply(df, c(v.factor), numcolwise(se))
    se.names <- (rep(0,length(v.numeric)))
    for (i in 1:length(v.numeric)) (se.names[i]<-as.vector(paste(v.numeric[i],"se",sep=".")))
    column.names <- c(v.factor,se.names)
    colnames(se.res) <- column.names
  }
  ifelse(calc_mean, ifelse(calc_sd, ifelse(calc_se, results <- merge(mean.sd.res,se.res), results <- merge(mean.res, se.res)), ifelse(calc_se, results<- merge(mean.res,se.res), results <- mean.res)), ifelse(calc_sd, ifelse(calc_se, results <- merge(sd.res,se.res), results <- sd.res), results <- se.res))
  
  
  return(results)
}