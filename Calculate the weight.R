library(ggplot2)
library(car)
library(stringr)

setwd("C:/Users/Administrator/Desktop/SPL")

######################################################################################
## Load dataset##
data = read.csv("data_reg.csv", 
                header = TRUE, sep = ",")

## Select 1992.1993.2005.2006 data##
data = subset(data,
              year %in% c(1992, 1993, 2005, 2006),
              select = c(Code,realGDP, lightarea, year))
names(data) = c("Code", "GDP", "light", "year")

## Reshape data##
wide = reshape(data, 
               v.names = c('light', "GDP"), 
               idvar = "Code",
               timevar = "year", 
               direction = "wide")

## Calculate GDP & Lights growth##
log.GDP.9293  = log(apply(wide[, c("GDP.1992", "GDP.1993")], 1, mean))
log.GDP.0506  = log(apply(wide[, c("GDP.2005", "GDP.2006")], 1, mean))
wide$GDP.grow = 100*((log.GDP.0506)-(log.GDP.9293)) / (2006-1993)

log.lights.9293 = log(apply(wide[, c("light.1992", "light.1993")], 1, mean))
log.lights.0506 = log(apply(wide[, c("light.2005", "light.2006")], 1, mean))
wide$light.grow = 100*((log.lights.0506)-(log.lights.9293)) / (2006-1993)


## Get final dataset##
data.total = wide[, c("Code", "GDP.grow", "light.grow")]
data.total = data.total [complete.cases(data.total ), ]
data.total = data.total [is.infinite(data.total$GDP.grow)== F &
                           is.infinite(data.total$light.grow) == F, ]
row.names (data.total) = data.total$Code

## Regress growth of lights on growth of gdp##

model = lm(GDP.grow~light.grow, data = data.total)
summary (model)

## Test Regression Model##
avPlots (model, ask = FALSE)
influencePlot(model, 
              main = "Influence Plot",
              sub  = "Circle size is proportional to Cook's distance")

##omit high influencial point##
data.total = data.total[data.total$Code!="GNQ" &
                          data.total$Code!="ATA" &
                          data.total$Code!="TUV", ]

model = lm(GDP.grow~light.grow, data = data.total)
summary(model)


#############
SCI = read.csv("SCI1.csv", header = TRUE, sep = ",")
SCI = SCI[complete.cases(SCI), ]
SCI = SCI[, c(4,5)]
names(SCI) = c('Code', "SCI")

bad.code = read.csv("code.csv", header = FALSE, sep = ",")
gb.data = merge(data.total, SCI, by="Code")
gb.data$bad = ifelse(gb.data$Code %in% bad.code$V1,1,0)

bad.ctry = gb.data[which(gb.data$bad == 1), ]
good.ctry = gb.data[which(gb.data$bad == 0), ]

###########
## Solving the statistical model
A = c(1, 0.9, 0.8, 0.7, 0.6)
m = matrix(nrow = 5, ncol =5)
m[, 1] = A
for (i in 1:5){
  CalculateLambda = function(phi_g){ 
    zg       = var(good.ctry$GDP.grow)
    y2       = phi_g * zg
    zg2      = (1 - phi_g) * zg
    zb2      = var(bad.ctry$GDP.grow) - y2
    phi_b    = y2 / (y2 + zb2)
    beta     = cov(data.total$GDP.grow,data.total$light.grow) / y2
    x2       = var(data.total$light.grow) - beta ^ 2 * y2
    lambda_g = (x2 * y2) / (zg2 * (beta ^ 2 * y2 + x2) + x2 * y2)
    lambda_b = (x2 * y2) / (zb2 * (beta ^ 2 * y2 + x2) + x2 * y2)
    result   = list(phi_b, beta, lambda_g, lambda_b)
    names(result) = c("phi_b", "beta", "lambda_g", "lambda_b")
    return(result)
  }
  temp    = CalculateLambda(phi_g = A[i])
  m[i, 2] = temp['phi_b']$phi_b
  m[i, 3] = temp['beta']$beta
  m[i, 4] = temp['lambda_g']$lambda_g
  m[i, 5] = temp['lambda_b']$lambda_b
}  

colnames(m) = c("phi_g", "phi_b", "beta", "lambda_g", "lambda_b")

write.csv(bad.ctry, 'bad_ctry.csv')
write.csv(data.total, 'data_total.csv')
write.csv(m, 'Solving the statistical model.csv')