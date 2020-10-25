secher<-read.table("secher.txt",header=T)
model1<-lm(bwt~bpd,data=secher) 
sum.model1<-summary(model1)
R2<-sum.model1$r.squared
f<-sum.model1$fstatistic 
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-model1$coefficients[1] 
slope1<-model1$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope1, intercept)
cat(output)
png("bwt_bpd.png")
plot(bwt~bpd,data=secher) 
abline(model1)
dev.off()


model2<-lm(bwt~ad,data=secher) 
sum.model2<-summary(model2)
R2<-sum.model2$r.squared
f<-sum.model2$fstatistic 
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-model2$coefficients[1] 
slope2<-model2$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope2, intercept)
cat(output)
png("bwt_ad.png")
plot(bwt~ad,data=secher) 
abline(model2)
dev.off()


model3<-lm(bwt~bpd+ad,data=secher) 
sum.model3<-summary(model3)
R2<-sum.model3$r.squared
f<-sum.model3$fstatistic 
p.value<-pf(f[1],f[2],f[3],lower.tail=F)
output<-sprintf("R2 = %f and p-value=%f", R2, p.value)
cat(output)
intercept<-model3$coefficients[1] 
slope3<-model3$coefficients[2]
output<-sprintf("slope=%f  intercept=%f",slope3, intercept)
cat(output)
png("bwt~bpd+ad.png")
plot(bwt~bpd+ad,data=secher) 
abline(model3)
dev.off()