library(ggplot2)
library(readr)
library(dplyr)
#install.packages("readr")
runners <- read_csv("Data/runners.csv")
odds <- read_csv("Data/odds.csv")
conditions <-read_csv("Data/conditions.csv")
forms <- read_csv("Data/forms.csv")
horses <- read_csv("Data/horses.csv")
markets <- read_csv("Data/markets.csv")
riders <- read_csv("Data/riders.csv")
weathers <- read_csv("Data/weathers.csv")


master.df <- merge(runners,markets,by.x='market_id',by.y='id',all=TRUE)




numeric_features = c('position','market_id','barrier','handicap_weight')
categorical_features = c('rider_id')




ConvertToFactors <- function(in.column.names, in.dat, missing.value = "xxx", use.dynamics = FALSE) {
  
  column.names <- colnames(in.dat)
  l <- which(column.names %in% in.column.names)
  
  cat("use.dynamics = ", use.dynamics, "\n")
  for(i in 1:length(l)) {
    if(is.factor(in.dat[, l[i]])) {
      cat("\t\tColumn", column.names[l[i]], "is already a factor, skipping\n")
    } else {
      cat("\t\tConverting", column.names[l[i]], "to a factor...")
      if(!use.dynamics) {
        
        in.dat[, l[i]] <-  factor((xf <- factor(in.dat[, l[i]])),
                                  levels = c(levels(xf), NA), exclude = NULL)
      } else {
        cat("Use the levels already determined...")
        wanted.levels <- levels(in.sample[, which(colnames(in.sample) == column.names[l[i]])])
        in.dat[, l[i]] <- factor(in.dat[, l[i]], levels = wanted.levels, exclude = NULL)
      }
      cat("Done!\n")
    }
  }
  return(in.dat)
}







dat <- ConvertToFactors(categorical_features, master.df, use.dynamics = FALSE)

df_features = dat[numeric_features]
df_features$win = FALSE
df_features$win[df_features$position==1]=TRUE















