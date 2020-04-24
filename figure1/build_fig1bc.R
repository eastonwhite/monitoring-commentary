# Created by Easton R. White
# Last edited 24-Apr-2020
# Build figure 1 (parts b and c) for monitoring commentary piece


# Part b (long-term time series question)

source('figure1/scripts.R')
load("figure1/cleaned_timeseries_database.Rdata")

time_series <- long_dat$popvalue[1:35]
par(mfrow=c(1,1),oma=c(4,4,0.5,0.5),mar=c(0.5,0.5,0.5,0.5))

pdf(file = 'figures/whole_timeseries.pdf',width=5,height=5)
    years <- 1973:2007
    plot(years,time_series,pch=16,ylim=c(0,1.2),ylab='Population size',xlab='Time (years)',las=1,cex.lab=1.3)
    points(years,fitted(lm(time_series~years)),type='l',col='red',lwd=2)
dev.off()


build_subsample_plot <- function(time_series,subsample_length){
  slopes = vector(mode = 'numeric',length = 15)
  par(mfrow=c(1,1),oma=c(4,4,0.5,0.5),mar=c(0.5,0.5,0.5,0.5))
  
  pdf(file = paste('figures/subsample_timeseries',subsample_length,'.pdf',sep =''),width=3,height=3)
  plot(time_series,pch=16,ylim=c(0,1.2),ylab='Population size',xlab='Time (years)',las=1,cex.lab=1.3)
  
  starting_points <- sample(x = 1:(length(time_series)-subsample_length),size = length(slopes))
  
  for (i in 1:length(slopes)){
    years = (1+starting_points[i]):(subsample_length+starting_points[i])
    subsample <- time_series[years]
    
    points(years,fitted(lm(subsample~years)),type='l',col='red',lwd=2)
    slopes[i] <- calculate_slope(subsample)
  }
  dev.off()
  return(slopes)
}

l5 <- build_subsample_plot(time_series,5)
l10 <- build_subsample_plot(time_series,10)
l15 <- build_subsample_plot(time_series,15)



# Panels (c) and (d)
pdf(file = 'figure1/figures/slope_and_percent_correct.pdf',width=5,height=8)
par(mfrow=c(2,1),oma=c(4.5,6,0.5,0.5),mar=c(0.5,0.5,0.5,0))
plot(calculate_average_slopes(time_series),pch=16,ylab='Average slope',xlab='',las=1,cex.lab=1.3,xaxt='n',ylim=c(-0.025,-0.015))
mtext(text = 'Average slope',side = 2,line = 4,outer = F,cex=1.3)
abline(h=-0.021,lty=2,lwd=2,col='red')
plot(calculate_prob_correct(time_series),pch=16,ylim=c(0,1),ylab='Pr(correct trend)',xlab='Length of time series (years)',las=1,cex.lab=1.3)
mtext(text = 'Length of time series (years)',side = 1,line = 3,outer = T,cex=1.3)
mtext(text = 'Pr(correct trend)',side = 2,line = 4,outer = F,cex=1.3)
dev.off()

