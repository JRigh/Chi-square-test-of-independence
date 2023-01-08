#---------------------------------------------
# Tests of independence for contingency tables
#---------------------------------------------

# Visualizing the Chi-squared distribution

set.seed(100)
# generate a sequence of 100
x <- seq(1, 30, length.out=100)

# plot density chi-2 
plot(dchisq(x,1),lwd=2, type="l", xlab="x", 
     ylab = "PDF P (X=x)", 
     main=expression(paste("Different ", chi^2, " distributions")),
     cex.sub=0.9)

# add density lines
lines(dchisq(x,5),lwd=2, col="firebrick1")
lines(dchisq(x,10),lwd=2, col="firebrick3")
lines(dchisq(x,20),lwd=2, col="firebrick4")

# add a legend
text=c('1', '5', '10', '20')
legend( "topright",title="Degrees of freedom", legend=text, lwd=2, lty=c(1, 1, 1, 1),
        fill = c("black", "firebrick1","firebrick3","firebrick4"), 
        cex = 0.85 )

# create dataset and visualize the contingency table using mosaic plot
data<-matrix(c(8,  37,  36, 13, 49,	30, 
               10,	28,	10, 28,	43,	9), nrow=4, byrow=TRUE)
rownames(data)<-c("25-39 years", "40-54 years", "55-64 years", "65 years and older")
colnames(data)<-c("Primary education", "Secondary education", "University education")
data
#                    Primary education Secondary education University education
# 25-39 years                        8                  37                   36
# 40-54 years                       13                  49                   30
# 55-64 years                       10                  28                   10
# 65 years and older                28                  43                    9

# plot
mosaicplot(data, col=c("indianred1","indianred3","indianred4"))

# chi-2 independence test
chisq.test(data)
# Pearson's Chi-squared test
# 
# data:  data
# X-squared = 33.355, df = 6, p-value = 8.961e-06

# chi-2 independence test, p-value computed by bootstrap
chisq.test(data, simulate.p.value=TRUE, B = 10000)
# Pearson's Chi-squared test with simulated p-value (based on 10000 replicates)
# 
# data:  data
# X-squared = 33.355, df = NA, p-value = 9.999e-05

# Chi-2 test manually
sum(((data - chisq.test(data,simulate.p.value=FALSE)$expected)^2) / chisq.test(data,simulate.p.value=FALSE)$expected)
# critical probability
qchisq(.95,df=6)

# association plot - analysis of the standardized residuals - vcd package
library(vcd)
assoc(data, shade=TRUE, residuals_type = "Pearson")

round(chisq.test(data, simulate.p.value=FALSE)$residuals, 2)
#                    Primary education Secondary education University education
# 25-39 years                    -1.98               -0.81                 2.74
# 40-54 years                    -1.19                0.15                 0.79
# 55-64 years                     0.19                0.59                -0.97
# 65 years and older              3.11                0.20                -2.86

#######
# end #
#######
