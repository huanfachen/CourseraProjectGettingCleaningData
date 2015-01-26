dataManipulate <- function(dir = "C:\\Users\\ShawnChan\\Downloads\\UCI HAR Dataset")
{# featureInfoTest.R
  #dir <- normalizePath("C:\\Users\\ShawnChan\\Downloads\\UCI HAR Dataset")
  fileName.Fea <- "features.txt"
  filePath.Fea <- paste(dir, "\\",fileName.Fea, sep = "")
  Feature <- read.table(file = filePath.Fea, stringsAsFactors = FALSE, header = FALSE)
  vec.feature <- Feature[,2]
  vec.feature <- make.names(vec.feature, unique = TRUE)
  
  logc.select.feature <- grepl(pattern = "mean", x = vec.feature) | grepl(pattern = "std", x = vec.feature)
  select.feature <- vec.feature[logc.select.feature]
  #select.Feature <- Feature[grepl("mean", Feature) | grepl("std", Feature)]
  
  dir.train <- paste(dir, "\\train", sep = "")
  dir.test <- paste(dir, "\\test", sep = "")
  filep.subject.train <- paste(dir.train, "\\subject_train.txt", sep = "")
  filep.subject.test <- paste(dir.test, "\\subject_test.txt", sep = "")
  
  subject.train <- read.table(filep.subject.train, header = FALSE)
  subject.test <- read.table(filep.subject.test, header = FALSE)
  subject.all <- rbind(subject.train, subject.test)
  subject.all <- rename(subject.all, subject = V1)
  
  filep.y.train <- paste(dir.train, "\\y_train.txt", sep = "")
  filep.y.test <- paste(dir.test, "\\y_test.txt", sep = "")
  
  y.train <- read.table(filep.y.train, header = FALSE)
  y.test <- read.table(filep.y.test, header = FALSE)
  y.all <- rbind(y.train, y.test)
  y.all <- rename(y.all, label = V1)
  
  filep.X.train <- paste(dir.train, "\\X_train.txt", sep = "")
  filep.X.test <- paste(dir.test, "\\X_test.txt", sep = "")
  
  X.train <- read.table(filep.X.train, header = FALSE, colClasses = "numeric")
  X.test <- read.table(filep.X.test, header = FALSE, colClasses = "numeric")
  X.all <- rbind(X.train, X.test)
  X.all <- X.all[,logc.select.feature]
  
  change.feature <- function(testStr)
  {
    vec.str <- unlist(strsplit(testStr, split = "[.]+"))
    vec.logi <- grepl(pattern = "mean", x = vec.str) | grepl(pattern = "std", x = vec.str)
    stopifnot(length(which(vec.logi)) == 1)
    paste(c(vec.str[!vec.logi], vec.str[vec.logi]), sep = ".", collapse = ".")
  }
  
  
  new.select.feature <- sapply(X = select.feature, FUN = change.feature,USE.NAMES = FALSE)
  names(X.all) <- new.select.feature
  final.df <- cbind(subject.all, y.all, X.all)
  
  write.table(x = final.df, file = "data1.csv", row.names = FALSE)
  
  #dt <- as.data.table(final.df)
  
  new.df <- NULL
  factor.subject <- as.factor(final.df$subject)
  factor.label <- as.factor(final.df$label)
  
  for(i in 3:81)
  {
    temp <- tapply(X = final.df[,i], INDEX = interaction(factor.subject, factor.label), FUN = mean)
    new.df <- cbind(new.df, temp)
  }
  
  names(new.df) <- new.select.feature
  write.table(x = new.df, file = "data2.csv", row.names = FALSE)
}
