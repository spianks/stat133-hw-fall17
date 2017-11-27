# ==================================================================
# title: functions
# description: list of codes of functions
# ==================================================================
remove_missing <- #takes a vector and returns the input vector without missign values
  function(v){
  v[!is.na(v)]
}



get_minimum <-  #takes a numeric vector to find the minimum value
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    new <- remove_missing(v)
  }
  sort(v)[1] 
}


get_maximum <- #takes a numeric vector to find the maximum value
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    new <- remove_missing(v)
  }
  sort(v, decreasing = TRUE)[1] 
}


get_range <- #takes a numeric vector to compute the overall range of the input vecctor
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    new <- remove_missing(v)
  }
  get_maximum(new) - get_minimum(new)
}


get_median <- #takes a numeric vector to find the median
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    v <- remove_missing(v)
  }
  if(length(v) %% 2 == 0){
    (sort(v)[length(v)/2] + sort(v)[length(v)/2 + 1])/2
  }
  else{
    sort(v)[length(v)%/%2 + 1]
  }
}
 
   
get_percentile10 <- #takes a numeric vector to find the 10th percentile of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){v <- remove_missing(v)}
  quantile(v, 0.1)[[1]]
}


get_percentile90 <- #takes a numeric vector to find the 90th percentile of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){v <- remove_missing(v)}
  quantile(v, 0.9)[[1]]
}


get_quartile1 <- #takes a numeric vector to find the first quartile of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){v <- remove_missing(v)}
  quantile(v, 0.25)[[1]]
}


get_quartile3 <- #takes a numeric vector to find the third quartile of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){v <- remove_missing(v)}
  quantile(v, 0.75)[[1]]
}


get_average <- #takes a numeric vector to find the average of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    v <- remove_missing(v)
  }
  total <- 0
  for (i in 1:length(v)){
    total <- total + v[i]
  }
  total/length(v)
}


get_stdev <- #takes a numeric vector to find the standard deviation of the input vector
  function(v, na.rm = TRUE){
  if(mode(v) != 'numeric') stop("non-numeric argument")
  if(na.rm == TRUE){
    v <- remove_missing(v)
  }
  avg <- get_average(v)
  total <- 0
  for (i in 1:length(v)){
    total <- total+(v[i]-avg)**2
  }
  sqrt(total/(length(v)-1))
}


count_missing <- #takes a numeric vector and calculates the number of missing values NA
  function(v){
  total <- 0
  for (i in 1:length(v)){
    total <- total + anyNA(v[i])
  }
  total
}


summary_stats <- #takes a numeric vector and returns a list of summary statistics
  function(v){
  list(minimum = get_minimum(v), percent10 = get_percentile10(v), quartile1 = get_quartile1(v),
       mean = get_average(v), median = get_median(v), quartile3 = get_quartile3(v), percent90 = get_percentile90(v),
       maximum = get_maximum(v), range = get_range(v), stdev = get_stdev(v), missing = count_missing(v)
       )
}


formulate <- #takes a number and converts it to four decimal places
  function(num){
sprintf('%0.4f', num)
}


print_stats <- #takes a list of summary statistics and prints the values in a nice format
  function(lst){
  name <- names(lst)
  outcome <-c()
  for (i in 1:length(lst)){
    if (nchar(name[i])==max(nchar(names(stats)))){
      cat(name[i], ": ", formulate(lst[[i]]), "\n", sep = "")
    }
      else{
    cat(name[i], c(rep(" ", max(nchar(names(stats)))-nchar(name[i]))), ": ", formulate(lst[[i]]), "\n", sep = "")}
    
  }
}


drop_lowest <- #takes a numeric vector of length n and returns a vector of length n-1 by 
  #dropping the lowest value
  function(n){
    for (i in 1:length(n)){
      if (n[i] == get_minimum(n)){
#        return(i)
        return(n[-i])
        }
    }
}


rescale100 <- #takes three arguments, a numeric vector x, a minimum xmin, and a maximum xmax, 
  #to find a rescaled vector with a potential scale from 0 to 100
  function(x, xmin, xmax){
  100*(x-xmin)/(xmax-xmin)
}


score_homework <- #takes a numeric vector of homework scores and an optional logical argumwnt drop
  #to find a single homework value. If drop=TRUE, the lowest HW score must be dropped. 
  #Returns the avereage of the hw scores 
  function(n, drop = TRUE){
  if(drop==TRUE){n <- drop_lowest(n)}
  get_average(n)
}


score_quiz <- #takes a numeric vector of homework scores and an optional logical argumwnt drop
  #to find a single quiz value. If drop=TRUE, the lowest quiz score must be dropped
  #Returns the avereage of the quiz scores
  function(n, drop = TRUE){
  if(drop==TRUE){n <- drop_lowest(n)}
  get_average(n) 
}


score_lab <- #takes a numeric value of lab attendance and returns the lab score
  function(n){
  if(n==11 | n==12){100}
  else if(n==10){80}
  else if(n==9){60}
  else if(n==8){40}
  else if(n==7){20}
  else{0}
}