# growth rate - biomass turnover to estimate % dormant
# SEJ
# 5-27-14

biomassTurnover=seq(-4,4,0.1)		#log10 days
microGrowth=seq(-3,1.7,0.1)			#  1000 day (r=0.0007) to 20 minute (r=50) doubling time

estFracDorm<-function(bt,mg){
	activeTurnover=log(2)/(10^mg)
	fractionDormant=1-(10^bt)/activeTurnover
	return(fractionDormant)
}

fracDorm=matrix(NA,length(biomassTurnover),length(microGrowth))
for(i in 1:length(biomassTurnover)){
	for(j in 1:length(microGrowth)){
		fracDorm[i,j]=estFracDorm(biomassTurnover[i],microGrowth[j])
	}
}
fracDorm[fracDorm<0]=NA

filled.contour(biomassTurnover,microGrowth,fracDorm,xlab="log10 Biomass Turnover",ylab="log10 Growth Rate",color.palette=colorRampPalette(c('purple','blue','green','yellow','orange','red')),main="Fraction Dormant")
#abline(a=-1.6,b=-1.25,lwd=3)