# Created by Easton White
# Last edited: 1-Oct-2019

# Build figure 2 regarding data-poor fisheries 

# Code comes from Github repo for following paper: https://github.com/brianstock/spatial-bycatch
# Stock BC, Ward EJ, Eguchi T, Jannot JE, Thorson JT, Feist BE, and Semmens BX. "Comparing predictions of fisheries bycatch using multiple spatiotemporal species distribution model frameworks."

# Use other script to build map


set.seed(12345)
require(viridis)
require(arm)

load("wcann_processed.RData")
#head(dat)

pdf(file = 'figure2bc.pdf',width = 7,height = 10)
par(mfrow=c(2,1),oma=c(0,2,0,0))

# Figure 2c: removing deep trawls

dat_shallow <- dat[dat$DEPTH<=230,];nrow(dat_shallow)
dat_deep <- dat[dat$DEPTH>230,];nrow(dat_deep)

glm1 <- glm(data = dat[sample(1:nrow(dat),size = round(data_frac*nrow(dat_shallow))),],formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY)
glm2 <- glm(data = dat_shallow,formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY)
glm3 <- glm(data = dat_deep,formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY)

#glm_all <- summary(glm(data = dat,formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY))

#glm_shallow <- summary(glm(data = dat_shallow,formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY))

longnames <- c("(Intercept)", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "log (depth)", "log (depth²)" , "log (sst²)", "log (sst^2)", "inRCA", "day")
colors <- viridis(3,begin=0.2,end=0.7)

coefplot(glm3, vertical=TRUE, mar=c(0.5,0.5,4,2),xlim=c(-20,30),main='',col.pts=colors[1],varnames = longnames)
coefplot(glm2, vertical=TRUE, mar=c(5.5,0.5,2,2),xlim=c(-20,30),main='',col.pts=colors[2],varnames = longnames,add=TRUE)
coefplot(glm1, vertical=TRUE, mar=c(5.5,0.5,2,2),xlim=c(-20,30),main='',add=TRUE,col.pts=colors[3],varnames = longnames)
legend('bottomright',legend = c('Subset of deep data','Subset of shallow data','All data ("true estimates")'),col = colors,pch=15,cex=1.1)
mtext(text = '(c)',side = 3,line = -0.3,at = 33.5,cex = 1.3)
mtext(text = 'Regression estimates',side = 3,line = 1,padj = 0.5,cex = 1.3)






# Figure 2c: removing percent of data randomly 
param <- "inRCA1"
param_name <- "Model estimate for effect of being in rockfish conservation area"

plot(1,1,ylim=c(0,20),xlim=c(0,1),type='n',ylab='',xlab='Fraction of samples included',las=1,cex.lab=1.3)
for (data_frac in seq(0.01,1,0.01)){
  
  # Take fraction of data and run glm
  glm1 <- summary(glm(data = dat[sample(1:nrow(dat),size = round(data_frac*nrow(dat))),],formula = DBRK~YEAR + logDEPTH + logDEPTH2 + sst + sst2 + inRCA + DAY))
  #print(glm1$coefficients[13,1])
  # Add parameter estimate
  points(data_frac,glm1$coefficients[param,1],pch=16)
  
  #Add
  arrows(x0 =data_frac,y0 = glm1$coefficients[param,1],x1 =data_frac,y1=glm1$coefficients[param,1]+glm1$coefficients[param,2],angle = 90,length = 0.0)
  arrows(x0 =data_frac,y0 = glm1$coefficients[param,1],x1 =data_frac,y1=glm1$coefficients[param,1]-glm1$coefficients[param,2],angle = 90,length = 0.0)
  
}
abline(h=glm1$coefficients[param,1],lty=2,lwd=2.5,col='red')
mtext(text = '(b)',side = 3,line = -2,at = 1.08,cex = 1.3)
mtext(text = 'Model estimate for effect of being',side = 2,line = 3.5,padj  = 0.5,cex = 1.2)
mtext(text = 'in rockfish conservation area',side = 2,line = 2.5,padj = 0.5,cex = 1.2)

dev.off()


