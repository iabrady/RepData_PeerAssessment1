process-data <- function() {
      ## Code to do this work of getting the data and processing it
      library("lubridate")
      library("dplyr")
      library("ggplot2")
      ## load and process the data
      data.file <- "activity.csv"
      data <- tbl_df(read.table(data.file, header = TRUE))
      data$steps <- as.numeric(data$steps)
      data$date <- ymd(data$date) # ensure we have a date format
      
      ## create the histogram
      png(filename = "hist-of-steps.png",width = 480, height = 480, units = "px",bg = "transparent")
      hist(data$steps, xlab = "No of steps", col="red", main="Frequency of Daily No of Steps")
      dev.off()
      
      ## output the mean and median
      print(paste("The mean number of steps per day is:", 
                  round(mean(data$steps, na.rm=TRUE), 2), "after removing NA values" 
      ))
      print(paste("The median of number of steps per day is:", 
                  round(median(data$steps, na.rm=TRUE), 2), "after removing NA values" 
      ))
      qplot(date, mean(steps), data=data, geom="path")
      ## 
      ## get a list of means grouped by date
      x <- data %>% 
            group_by(date) %>% 
            summarise(average.steps = mean(steps)) 
      ## plot the average steps by day
      ggplot(x, aes(date, average.steps)) + 
            geom_line(aes(colour = average.steps, col="Avg Steps")) +
            labs(title="Average Steps per Day", x="Average Steps", y="Days")
      ## Count the number of NAs we have in the data
      na.rows <- is.na(data$steps)  # find the rows where there are NAs
      num.nas <- sum(na.rows)       # use sum on the logical vector to get number
      
      ## devise a strategy for filling in the NA number
      ## use the mean for that day for the other measurements
      ##mutate(data)

new.steps <- data$steps # get a list of the steps
with(new.steps, replace(, is.na(x), mean(y, na.rm=TRUE)))
      
      
}




replace.nas <- function(data, new.steps, na.rows) {
      #'    for all rows where there is an NA
      #'    if the mean for that date is not NA then replace with the mean
      #'    else get the mean for the month then replace steps with it 
      #' 
      #'   get the means of the days and the months (in case we need it)  
      mean.by.days <- data %>% 
                  group_by(date) %>% 
                  summarise(average.steps = mean(data$steps, rna.rm=TRUE)) 
      #'
      #'    Need to allow for the data which, when assessed, has days that have 
      #'    a value of NA so lets get the means for the month and if the day
      #'    has an NA value, use the mean for the month
      
      mean.by.months <- data %>% 
                  group_by(month(date)) %>% 
                  summarise(average.steps.month = mean(data$steps, na.rm=TRUE))
      #'    firstly, ensure that mean.days does not have any NAs itself
      #'
      for(i in seq_along(mean.by.days)) {
            if(is.na(mean.days[i])) { # then we need to replace it
                  if(is.na(mean.days[i])) mean.days[i] <- mean.month[i]
                  new.val <- filter(mean.by.months, 
                         month(mean.by.days$month[i]) == month(mean.by.months$month) 
                         && year(mean.by.days$month[i]) == year(mean.by.months$month) 
                        ) 
                  is.na(mean.days[i] <- new.val
                  , na.rm=TRUE)d
                  
            }
      }
      # if any NA value then we need to replace it with mean of days
      for(i in seq_along(new.steps)) {
            if(is.na(new.steps[i])) new.steps[i] <- mean.days[i]
      }

      x <- data %>% group_by(month(date)) %>% summarise(mmean = mean(data$steps, na.rm=TRUE))  
      x <- data %>% 
            filter(data, is.na(steps)==FALSE) %>%
            group_by(as.character(month(date))) %>% 
            summarise(mmean = mean(steps, na.rm=TRUE))
}
      
datax <-data %>%  
      + group_by(as.character(month(date)))     
