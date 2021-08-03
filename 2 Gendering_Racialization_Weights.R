library(Matching)
library(randomForest)
library(CBPS)

rm(list=ls())

set.seed(08540)
load("Parsedata.RData")

#################################################
#
# TRIMMING AND WEIGHTS
#
#################################################
var.list <- c("modern_sexism.s", "heterosexism.s", 
              "pid", "ideology", 
              "white_woman", "man_of_color", "woman_of_color",
              "income", "education")
exact.v.rm <- c(F, F, 
                F, F, 
                T, T, T, 
                F, F)
outcomes <- c("treat_thermometer", "photo_block", "obama_photo", "photo_treatment", 
              "obama_thermometer", "policy_support", "policy_treatment")
other.vars <- c("racial_resentment_bin", "racial_resentment", "racial_resentment.s",
                "race.lim", "gender", "gender.lim", "race.bin", 'lgbt', 'white_man',
                'racial_resentment_groups', 'modern_sexism_groups', 'heterosexism_groups',
                "position_threat", "position_threat.s", "rr.quantile", "ms.quantile", "hs.quantile")

data <- boxes_data[, c(outcomes, var.list, other.vars)]
data <- subset(data, subset = complete.cases(data[,c(outcomes, var.list)]) == T)

f.m <- formula(paste0('racial_resentment_bin ~ ', paste(var.list, collapse=' + ')))
########################
# Coarsened Exact Matching
########################
match.out <- Match(Y=data$treat_thermometer, Tr=data$racial_resentment_bin, X=data[,var.list],
                   exact=exact.v.rm, estimand="ATE", M=1)
summary(match.out)
# summary( data[c(match.out$index.control, match.out$index.treatment),])
# summary( data[-c(match.out$index.control, match.out$index.treatment),])
data$w.match <- rep(0, len=nrow(data))
data$w.match[match.out$index.treated] <- match.out$weights
data$w.match[match.out$index.control] <- match.out$weights

#MatchBalance(f.m, match.out = match.out, data=data)

rm(match.out)
########################
# Genetic Matching
########################
match.out.gen <- GenMatch(Tr=data$racial_resentment_bin, X=data[,var.list],
                          pop.size = 1000, estimand="ATE", 
                          paired=F)
gen.out <- Match(Y=data$treat_therm, Tr=data$racial_resentment_bin, X=data[,var.list],
                 Weight.matrix = match.out.gen$Weight.matrix,
                 )
summary(gen.out)
data$w.gen <- rep(0, len=nrow(data))
data$w.gen[gen.out$index.treated] <- gen.out$weights
data$w.gen[gen.out$index.control] <- gen.out$weights

#MatchBalance(f.m, match.out = gen.out, data=data)

rm(gen.out)
rm(match.out.gen)
########################
# Random Forests
########################
data$treat <- as.factor(data$racial_resentment_bin)
form <- formula(paste0('treat ~ ', paste(var.list, collapse=' + ')))
mw = randomForest(form, data=data, ntree=10000)
rf.match <- Match(Y=data$treat_therm, Tr=data$racial_resentment_bin, X=data[,var.list],
                 weights = predict(mw, type='prob')[,2],
)
summary(rf.match)
data$w.rf <- rep(0, len=nrow(data))
data$w.rf[rf.match$index.treated] <- rf.match$weights
data$w.rf[rf.match$index.control] <- rf.match$weights

#plot(density(data$w.rf[data$treat == '1'], cut=2))
#lines(density(data$w.rf[data$treat == '0'], cut=2), col='red')

rm(mw)
rm(form)
rm(rf.match)
########################
# Logistic Propensity
########################
p.out <- glm(f.m, data=data, family='binomial')
p.match <- Match(Y=data$treat_therm, Tr=data$racial_resentment_bin, X=data[,var.list],
                    weights = predict(p.out, type='response'),
)
summary(p.match)
data$w.ps <- rep(0, len=nrow(data))
data$w.ps[p.match$index.treated] <- p.match$weights
data$w.ps[p.match$index.control] <- p.match$weights

#plot(density(data$p.match[data$treat == '1'], cut=2))
#lines(density(data$p.match[data$treat == '0'], cut=2), col='red')

rm(p.out)
rm(p.match)
########################
# CBPS
########################
cbps.out <- CBPS(f.m, data, method='over')
cbps.match <- Match(Y=data$treat_therm, Tr=data$racial_resentment_bin, X=data[,var.list],
                    weights = cbps.out$fitted.values,
)
summary(cbps.match)
data$w.cbps <- rep(0, len=nrow(data))
data$w.cbps[cbps.match$index.treated] <- cbps.match$weights
data$w.cbps[cbps.match$index.control] <- cbps.match$weights

#plot(density(data$w.cbps[data$treat == '1'], cut=2))
#lines(density(data$w.cbps[data$treat == '0'], cut=2), col='red')

rm(cbps.out)
rm(cbps.match)
########################
# Save New Data
########################
boxes_trim <- data
rm(data)
rm(var.list)
rm(outcomes)
rm(other.vars)
rm(f.m)
rm(exact.v.rm)
save.image("Parsedata_Weights.RData")
