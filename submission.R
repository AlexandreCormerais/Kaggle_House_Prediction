#setup
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load livrary
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)
library(scales)
rm(load.libraries, install.lib, libs)

#load data
train = read.csv('train.csv')
test = read.csv('test.csv')

train <- data.table(train)
test <- data.table(test)

#size of the data.
dim(train)
dim(test)

#structure
str(train)
summary(train)

#############################################################################################################################################################
#classification of features
#############################################################################################################################################################
types <- (sapply(train, class))
typeof(types)
unique(sapply(train, class))

cat_var <- names(train)[which(sapply(train, is.character))]
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

cat_var <- names(train)[which(sapply(train, is.factor))]
numeric_var <- names(train)[which(sapply(train, is.numeric))]

#are all the features classed?
dim(train)[2]
length(union(numeric_var,cat_var))
names(train)[which(!union(numeric_var,cat_var) %in% names(train))]
if (length(names(train)[which(!union(numeric_var,cat_var) %in% names(train))])>0) {print("ATTENTION, toutes les variables ne sont pas classées") } 
#cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')

#############################################################################################################################################################
#null values
#############################################################################################################################################################
colSums(sapply(train, is.na))
colSums(sapply(train[,..cat_var], is.na))
colSums(sapply(train[,..numeric_var], is.na))


#Viz
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

train <- data.table(train)

#############################################################################################################################################################
# remodelled houses
#############################################################################################################################################################
sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])
cat('Percentage of houses remodeled',sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])/ dim(train)[1])

train %>% 
  select(YearBuilt, YearRemodAdd) %>%
  mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>% 
  ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) +
      geom_bar() + 
      xlab('Remodeled') +
      theme_light()


summary(train[,.SD, .SDcols =numeric_var])
unique(train)


#############################################################################################################################################################
#Summarize the numeric values and the structure of the data.
#############################################################################################################################################################
cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')
cat('Train has ',sum(is.na(train)) / (nrow(train) *ncol(train)),'% of missing values')
cat('TEst has ',sum(is.na(test)) / (nrow(test) *ncol(test)),'% of missing values')
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

#############################################################################################################################################################
# Barplots for the categorical features
#############################################################################################################################################################
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope %in% c('Sev', 'Mod')) %>% 
  arrange(train$Neighborhood) %>% 
  group_by(Neighborhood, LandSlope) %>%
  summarize(Count = n()) %>% 
  ggplot(aes(Neighborhood, Count)) + 
    geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + 
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))

train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')


#############################################################################################################################################################
#Density plots for numeric variables.
#############################################################################################################################################################
doPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 7:12, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 13:17, ncol = 2)
doPlots(train_cont, fun = plotHist, ii = 18:23, ncol = 2)

#############################################################################################################################################################
#Explore the correlation
#############################################################################################################################################################
correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))
# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
?apply
row_indic
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

#############################################################################################################################################################
#Plot scatter plot for variables that have high correlation.
#############################################################################################################################################################
plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}


highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)]
              , names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

data_corr <- train[,highcorr, with = FALSE]


doPlots(data_corr, fun = plotCorr, ii = 1:6)
doPlots(data_corr, fun = plotCorr, ii = 6:11)

#############################################################################################################################################################
#target variable
#############################################################################################################################################################

ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)
#Normalize distribution
ggplot(train, aes(x=log(SalePrice+1))) + geom_histogram(col = 'white') + theme_light()

correlations.m <- melt(correlations)
SalePrice_cor <- correlations.m %>% subset(Var1=='SalePrice') %>% select(Var2,value) %>% arrange(desc(value))
head(SalePrice_cor)
tail(SalePrice_cor)


library(sklearn)
install.packages('sklearn')
