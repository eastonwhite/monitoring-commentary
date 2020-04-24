# Created by Easton R. White
# Last edited 24-Apr-2020
# Extra functions for building figures

calculate_slope = function(x){
  return(as.numeric(coef(lm(c(x)~c(1:length(x))))[2]))
}

create_subsamples = function(pop){
  subsamples=list()
  for (n in 1:(length(pop)-1)){
    subsamples[[n]] = ldply(1:length(pop), function(x){pop[x:(x+n)]})
  }
  return(subsamples)
}

require(plyr)
calculate_average_slopes = function(pop){
  subset_times=create_subsamples(pop)
  average_slopes=matrix(0,nrow=(length(pop)-1),1)
  for (n in 1:(length(pop)-1)){
    average_slopes[n,1]=mean(apply(na.omit(subset_times[[n]]),1,calculate_slope))
  }
  return(average_slopes)
}  

calculate_prob_correct = function(pop){
  
  true_trend = sign(calculate_slope(pop))
  
  subset_times=create_subsamples(pop)
  prob_correct=matrix(0,nrow=(length(pop)-1),1)
  for (n in 1:(length(pop)-1)){
    prob_correct[n,1]=sum(sign(apply(na.omit(subset_times[[n]]),1,calculate_slope))==true_trend)/nrow(na.omit(subset_times[[n]]))
  }
  return(prob_correct)
}  





min_time_needed = function(pop, threshold){
  average_slopes = calculate_average_slopes(pop)
  min_value = tail(which(abs((average_slopes - as.numeric(tail(average_slopes,1)))/as.numeric(tail(average_slopes,1)))>threshold),1)+1
  
  if (length(min_value)==0){
    return(2)
  }else if (min_value ==length(pop)){
    return(-999)
  }else{
    return(min_value)
  }
}
