library(plm)
library(survey)
library(foreign)
library(memisc)
library(stargazer)
library(weights)

rm(list=ls())
load("Parsedata_Weights.RData")
design <- svydesign(ids=1:nrow(boxes_trim), data=boxes_trim)
boxes_trim$photo_treatment2 <- relevel(boxes_trim$photo_treatment, 'Clinton with Obama')

# boxes_data: The full parsed data set (without weights)
# boxes_trim: The data that only includes variables in analysis and 
#  the weights from matching.

##########################################################################
#
# Functions
#
##########################################################################
add.bars <- function(svyby.object, bar.plot){
    ind <- length(svyby.object) - 1
    means <- svyby.object[ind]
    means <- means[1:nrow(means), names(means)[1]]
    lower.ci <- svyby.object[ind] - (svyby.object[ind + 1] * 1.96)
    lower.ci <- lower.ci[1:nrow(lower.ci), names(lower.ci)[1]]
    upper.ci <- svyby.object[ind] + (svyby.object[ind + 1] * 1.96)
    upper.ci <- upper.ci[1:nrow(upper.ci), names(upper.ci)[1]]
    arrows(x0=bar.plot,
             y0=lower.ci,
             y1=upper.ci, 
             lwd=2, col='red', code=3, angle=90, length=.05)
}


GetModel <- function(f, data=boxes_data){
    out <- lm(f, data=data)
    return(out)
}


GetEstimates <- function(reg.out){
    # model_sum <- summary(reg.out, vcov=reg.out$vcov)$coefficients
    model_sum <- summary(reg.out)$coefficients
    estimates <- model_sum[,1]
    lower <- estimates - 1.96 * model_sum[,2]
    upper <- estimates + 1.96 * model_sum[,2]
    reg.data <- cbind(estimates, lower, upper)
    dat.index <- unique(c(grep("_groupsLow:", rownames(reg.data)),  grep("photo_treatment", rownames(reg.data))))
    reg.data.ordered <- reg.data[dat.index, ]
    if("photo_treatmentBiden with Obama" %in% rownames(reg.data.ordered)){
        swm <- matrix(0, nrow=1, ncol=3)
        colnames(swm) <- colnames(reg.data.ordered)
        rownames(swm) <- c("Biden")
        start <- which(rownames(reg.data.ordered) == "photo_treatmentBiden with Obama")
        reg.data.ordered <- rbind(reg.data.ordered[1:start-1,], swm, reg.data.ordered[start:nrow(reg.data.ordered),])
        if("racial_resentment_groupsLow:photo_treatmentBiden with Obama" %in% rownames(reg.data.ordered)){
            start <- which(rownames(reg.data.ordered) == "racial_resentment_groupsLow:photo_treatmentBiden with Obama")
            reg.data.ordered <- rbind(reg.data.ordered[1:start-1,], swm, reg.data.ordered[start:nrow(reg.data.ordered),])
        } else if(length(grep("_groupsLow:photo_treatmentBiden with Obama", rownames(reg.data.ordered))) > 0){
            start <- grep("_groupsLow:photo_treatmentBiden with Obama", rownames(reg.data.ordered))
            reg.data.ordered <- rbind(reg.data.ordered[1:start-1,], swm, reg.data.ordered[start:nrow(reg.data.ordered),])
            
        }
    }
    return(reg.data.ordered)
}


CoefficientPlot <- function(f, data, base=TRUE, compare=FALSE, lab.names=NULL, x.limits=NULL, ref.group="Biden", main="", mar=c(2,7,2,.75)){
    if("treat_thermometer" %in% all.vars(f)){
        x.label = "Change in Thermometer Rating"
    } else {
        x.label = "Change in Policy Support"
    }
    if(compare == TRUE){
        base = FALSE
    }
    par(mar=mar, mgp=c(1, .05, 0), family='sans', las=3, tck=-.001, xaxs='i', yaxs='i', 
        cex.axis=.7, cex.lab=.75, lend=2)
    if(base == TRUE){
        out.base <- GetModel(f, data=data)
        base.est <- GetEstimates(out.base)
        x.limits <- c(min(base.est[,2]) - .1 * min(base.est[,2]), 
                      max(base.est[,3]) + .1 * max(base.est[,3]))
        if(is.null(lab.names) == TRUE){
            lab.names = row.names(base.est)
            lab.names = gsub("cand_association", "", lab.names)
            lab.names = gsub("sexual_orientation", "", lab.names)
            lab.names = gsub("cand_profile", "", lab.names)
            lab.names = gsub("photo_treatment", "", lab.names)
            lab.names = gsub("_full", "", lab.names)
        }
        plot(NULL, axes=FALSE, xlim=x.limits, ylim=c(0, nrow(base.est) + .25), main="",
             xlab=x.label, ylab="")
        axis(2, at=seq(1:nrow(base.est)), labels=lab.names[nrow(base.est):1], las=2, pos=c(x.limits[1], 0))
        axis(1, at=c(x.limits[1], 0, x.limits[2]), las=1)
        abline(v=0)
        points(c(nrow(base.est):1)~ base.est[,1])
        segments(x0=base.est[,2], x1=base.est[,3], y0=c(nrow(base.est):1))
    }  
    if(compare == TRUE){
        out.base <- GetModel(f, data = data)
        
        estimates <- GetEstimates(out.base)
        split <- nrow(estimates)/2
        base.est <- estimates[1:split,]
        dif.est <- estimates[(split + 1):nrow(estimates),]
        if(is.null(x.limits) == TRUE){
            x.limits <- c(min(base.est[,2]) + .5 * min(base.est[,2]), 
                          max(base.est[,3]) + .5 * max(base.est[,3]))
        }
        if(is.null(lab.names) == TRUE){
            lab.names = row.names(base.est)
            lab.names = gsub("cand_association", "", lab.names)
            lab.names = gsub("sexual_orientation", "", lab.names)
            lab.names = gsub("cand_profile", "", lab.names)
            lab.names = gsub("racial_resentment_groupsLow:", "", lab.names)
            lab.names = gsub("heterosexism_groupsLow:", "", lab.names)
            lab.names = gsub("modern_sexism_groupsLow:", "", lab.names)
            lab.names = gsub("photo_treatment", "", lab.names)
            lab.names = gsub("cand_profile_association", "", lab.names)
            lab.names = gsub("_association", "", lab.names)
            lab.names = gsub("_full", "", lab.names)
            lab.names = gsub("gender", "Gender", lab.names)
            lab.names = gsub("race", "Race", lab.names)
            lab.names = gsub("sexuality", "Sexuality", lab.names)
        }
        plot(NULL, axes=FALSE, xlim=x.limits, ylim=c(.75, nrow(base.est) + .25),
             xlab=x.label, ylab="", main=main)
        axis(2, at=seq(1, nrow(base.est), 1), labels=lab.names[nrow(base.est):1], las=2, pos=c(x.limits[1], 0))
        axis(1, at=c(x.limits[1], 0, x.limits[2]), las=1)
        abline(v=0)
        y.points <- seq(nrow(base.est), 1, by=-1)
        points(y.points ~ base.est[,1], col='red')
        segments(x0=base.est[,2], x1=base.est[,3], y0=y.points, col='red')
        
        points(y.points ~ dif.est[,1], pch=2, col='blue')
        segments(x0=dif.est[,2], x1=dif.est[,3], y0=y.points, col='blue')
        legend('topleft', c("High", "Low"), col=c('red', 'blue'), pch=c(1, 2), lty=1, bty='n', cex=.75)
    }
}


ScoreCor <- function(data){
    scores <- cbind(data$racial_resentment, data$modern_sexism, data$heterosexism, data$pid, data$ideology)
    score.cor <- cor(scores)
    rownames(score.cor) <- c("Racial Resentment", "Modern Sexism", "Heterosexism", "Party ID", "Ideology")
    colnames(score.cor) <- c("Racial Resentment", "Modern Sexism", "Heterosexism", "Party ID", "Ideology")
    score.cor <- round(score.cor, 3)
    return(score.cor)
}


GetTreatBalance <- function(variable, treatment){
  treat_levels <- levels(boxes_trim[,treatment])
  v <- boxes_trim[,variable]
  tr <- boxes_trim[,treatment]
  out1 <- t.test(x=v[tr == treat_levels[1]],
                 y=v[tr == treat_levels[2]])
  out2 <- t.test(x=v[tr == treat_levels[1]],
                 y=v[tr == treat_levels[3]])
  out3 <- t.test(x=v[tr == treat_levels[1]],
                 y=v[tr == treat_levels[4]])
  out.m <- c(out1$estimate[1], out1$estimate[2], out2$estimate[2], out3$estimate[2])
  out.m <- as.character(round(out.m, 3))
  ps <- c(out1$p.value, out2$p.value, out3$p.value)
  ps <- round(ps, 4)
  out <- c(out.m[1], paste(out.m[2:4], ' (', ps, ')', sep=''))
  names(out) <- paste("Mean: ", treat_levels, sep='')
  return(out)
}


GetBalance <- function(variable, dta){
  none <- t.test(x=dta[dta$racial_resentment_bin == 1, variable],
                 y=dta[dta$racial_resentment_bin == 0, variable])
  none.out <- c((none$estimate[1] - none$estimate[2]), none$estimate, none$p.value)
  m <- wtd.t.test(x=dta[dta$racial_resentment_bin == 1 & dta$w.match > 0, variable],
                  y=dta[dta$racial_resentment_bin == 0 & dta$w.match > 0, variable],
                  weight=dta$w.match[dta$racial_resentment_bin == 1 & dta$w.match > 0],
                  weighty=dta$w.match[dta$racial_resentment_bin == 0 & dta$w.match > 0],
                  bootse=T, bootp=T, bootn=1000, samedata = F)
  m.out <- c(m$additional[1:3], m$coefficients[3])
  rf <- wtd.t.test(x=dta[dta$racial_resentment_bin == 1 & dta$w.rf > 0, variable],
                   y=dta[dta$racial_resentment_bin == 0 & dta$w.rf > 0, variable],
                   weight=dta$w.rf[dta$racial_resentment_bin == 1 & dta$w.rf > 0],
                   weighty=dta$w.rf[dta$racial_resentment_bin == 0 & dta$w.rf > 0],
                   bootse=T, bootp=T, bootn=1000, samedata = F)
  rf.out <- c(rf$additional[1:3], rf$coefficients[3])
  ps <- wtd.t.test(x=dta[dta$racial_resentment_bin == 1 & dta$w.ps > 0, variable],
                   y=dta[dta$racial_resentment_bin == 0 & dta$w.ps > 0, variable],
                   weight=dta$w.ps[dta$racial_resentment_bin == 1 & dta$w.ps > 0],
                   weighty=dta$w.ps[dta$racial_resentment_bin == 0 & dta$w.ps > 0],
                   bootse=T, bootp=T, bootn=1000, samedata = F)
  ps.out <- c(ps$additional[1:3], ps$coefficients[3])
  cbps <- wtd.t.test(x=dta[dta$racial_resentment_bin == 1 & dta$w.cbps > 0, variable],
                     y=dta[dta$racial_resentment_bin == 0 & dta$w.cbps > 0, variable],
                     weight=dta$w.cbps[dta$racial_resentment_bin == 1 & dta$w.cbps > 0],
                     weighty=dta$w.cbps[dta$racial_resentment_bin == 0 & dta$w.cbps > 0],
                     bootse=T, bootp=T, bootn=1000, samedata = F)
  cbps.out <- c(cbps$additional[1:3], cbps$coefficients[3])
  gen <- wtd.t.test(x=dta[dta$racial_resentment_bin == 1 & dta$w.gen > 0, variable],
                    y=dta[dta$racial_resentment_bin == 0 & dta$w.gen > 0, variable],
                    weight=dta$w.gen[dta$racial_resentment_bin == 1 & dta$w.gen > 0],
                    weighty=dta$w.gen[dta$racial_resentment_bin == 0 & dta$w.gen > 0],
                    bootse=T, bootp=T, bootn=1000, samedata = F)
  gen.out <- c(gen$additional[1:3], gen$coefficients[3])
  out <- cbind(none.out, m.out, rf.out, ps.out, cbps.out, gen.out)
  out[4,] <- round(out[4, ], 4)
  out[1:3,] <- round(out[1:3, ], 3)
  colnames(out) <- c("No Weights", "Coarsened", "Random Forest", "Propensity", "CBPS", "Genetic")
  rownames(out) <- c("Difference", "Mean >= Avg. Resentment", "Mean < Avg Resentment", "P Value")
  return(out)
}


ReturnDescriptive <- function(var, label.names=NULL){
  tab <- table(boxes_trim[, var]) / sum(table(boxes_trim[, var]))
  if(is.null(label.names) == FALSE){
    names(tab) <- label.names
  }
  tab <- tab * 100
  tab <- round(tab, 2)
  stargazer(t(as.matrix(tab)))
  return(tab)
}

##########################################################################
#
# TABLES
#
##########################################################################
# The Following tables are numbered using the original submission numbers
#  and may not reflect the numbering in online materials. However, the names
#  of the tables provided in this code match the text.
# Tables and figures can only be created after parsing the raw data (1 Parse file)  
#  and then generating the weights for the data (2 Weights file)
#################################################
# TABLE 1: Social Attitudes, Party ID, and Ideology Correlations
#################################################
stargazer(ScoreCor(boxes_data))
#################################################
# TABLE 2: Feeling Thermometer Evaluations of Clinton
#################################################
# Table should have included LGBT variable; however, it does not affect 
#  Substantive results. 
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s   + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education + lgbt, data=boxes_trim)
f.2 <- formula(treat_thermometer ~obama_photo * racial_resentment.s  + 
                 modern_sexism.s  * obama_photo + 
                 heterosexism.s  * obama_photo + 
                 ideology  * obama_photo + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education + lgbt, data=boxes_trim)
# Models used in paper
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s   + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
f.2 <- formula(treat_thermometer ~obama_photo * racial_resentment.s  + 
                 modern_sexism.s  * obama_photo + 
                 heterosexism.s  * obama_photo + 
                 ideology  * obama_photo + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
stargazer(lm(f.1, data=boxes_trim, subset = photo_block == 1),
          lm(f.1, data=boxes_trim, subset = photo_block == 1, weights=boxes_trim$w.gen),
          lm(f.2, data=boxes_trim, subset = photo_block == 1),
          lm(f.2, data=boxes_trim, subset = photo_block == 1, weights=boxes_trim$w.gen),
          style = 'ajps', type='text', 
          omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid','income', 'education', 'lgbt'),
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Clinton", 
          label="tab:1a", 
          column.labels = c("Unmatched, Only Resentment",  "Matched, Only Resentment",  "Unmatched, All Attitudes",  "Matched,  All Attitudes"),
          covariate.labels = c("With Obama", 
                               "Resentment", "Sexism", 'Heterosexism', "Ideology",
                               "With Obama: Resentment", "With Obama: Sexism", 'With Obama: Heterosexism', "With Obama: Ideology"))
#################################################
# TABLE 3: Feeling Thermometer Evaluations of Biden and Clinton by Attitude
#################################################
# Table should have included LGBT variable; however, it does not affect 
#  Substantive results. 
f.ate <- formula(treat_thermometer ~ I(relevel(photo_treatment, "Clinton with Obama")))
f.only.resent <- formula(treat_thermometer ~ I(relevel(photo_treatment, "Clinton with Obama")) * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education + lgbt)
f.all <- formula(treat_thermometer ~ 
                 I(relevel(photo_treatment, "Clinton with Obama")) * racial_resentment.s + 
                 I(relevel(photo_treatment, "Clinton with Obama")) * modern_sexism.s + 
                 I(relevel(photo_treatment, "Clinton with Obama")) * heterosexism.s + 
                 I(relevel(photo_treatment, "Clinton with Obama")) * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education + lgbt)
# Models used in table. Results in Matched model may depend on the seed
#  used, which was not included in the model in the paper but was included
#  in the replication data.  Results substantively the same, but some 
#  slight shifts in estimates.
f.only.resent <- formula(treat_thermometer ~ I(relevel(photo_treatment, "Clinton with Obama")) * racial_resentment.s + 
                           modern_sexism.s + heterosexism.s + 
                           white_woman + man_of_color + woman_of_color + 
                           pid + ideology + income + education)
f.all <- formula(treat_thermometer ~ 
                   I(relevel(photo_treatment, "Clinton with Obama")) * racial_resentment.s + 
                   I(relevel(photo_treatment, "Clinton with Obama")) * modern_sexism.s + 
                   I(relevel(photo_treatment, "Clinton with Obama")) * heterosexism.s + 
                   I(relevel(photo_treatment, "Clinton with Obama")) * ideology + 
                   white_woman + man_of_color + woman_of_color + 
                   pid + ideology + income + education )
out.ate <- lm(f.ate, data=boxes_trim)
out.only.resent <- lm(f.only.resent, data=boxes_trim)
out.all <- lm(f.all, data=boxes_trim)
out.all.match <- lm(f.all, data=boxes_trim, weights=boxes_trim$w.gen)

stargazer(out.ate, out.only.resent, out.all, out.all.match,   style = 'ajps',  type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          column.labels = c("ATE",  "Unmatched, Only Resentment", "Unmatched", "Matched"),
          title="Evaluations of Biden and Clinton",
          label="tab:reg exp 1", omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid', 'income', 'education', 'lgbt'),
          covariate.labels = c("Biden", "Biden, Obama", "Clinton", 
                               "Resentment", "Sexism", 'Heterosexism', 
                               "Ideology", 
                               "Biden: Resentment", "Biden, Obama: Resentment", "Clinton: Resentment",
                               "Biden: Sexism", "Biden, Obama: Sexism", "Clinton: Sexism",
                               "Biden: Heteroexism", "Biden, Obama: Heteroexism", "Clinton: Heteroexism",
                               "Biden: Ideology", "Biden, Obama: Ideology", "Clinton: Ideology"
          ))
#################################################
# Supplemental Materials, Table 4: Balance by Treatment Condition
#################################################
exp_bal <- rbind(
  GetTreatBalance('racial_resentment.s', 'photo_treatment'),
  GetTreatBalance('modern_sexism.s', 'photo_treatment'),
  GetTreatBalance('heterosexism.s', 'photo_treatment'),
  GetTreatBalance('pid', 'photo_treatment'),
  GetTreatBalance('ideology', 'photo_treatment'),
  GetTreatBalance('education', 'photo_treatment'),
  GetTreatBalance('income', 'photo_treatment'),
  GetTreatBalance('white_man', 'photo_treatment'),
  GetTreatBalance('white_woman', 'photo_treatment'),
  GetTreatBalance('man_of_color', 'photo_treatment'),
  GetTreatBalance('woman_of_color', 'photo_treatment'),
  GetTreatBalance('lgbt', 'photo_treatment')
)
rownames(exp_bal) <- c("Resentment", "Sexism", 'Heterosexism', 
                         "Party ID", "Ideology", "Education", "Income", 
                         'white Man',  'White Woman', "Man of Color", 'Women of Color', 'LGBT')
stargazer(exp_bal, title ='Balance by Treatment Condition', type='text')

#################################################
# Supplemental Materials, TABLES 5-14: Treatment Balance with Weights
#################################################
stargazer(GetBalance('modern_sexism.s', boxes_trim), title='Modern Sexism', type='text')
stargazer(GetBalance('heterosexism.s', boxes_trim), title='Heterosexism', type='text')
stargazer(GetBalance('pid', boxes_trim), title='Party ID', type='text')
stargazer(GetBalance('ideology', boxes_trim), title='Ideology', type='text')
stargazer(GetBalance('income', boxes_trim), title='Income', type='text')
stargazer(GetBalance('education', boxes_trim), title='Education', type='text')
stargazer(GetBalance('white_man', boxes_trim), title='White Men', type='text')
stargazer(GetBalance('white_woman', boxes_trim), title='White Women', type='text')
stargazer(GetBalance('man_of_color', boxes_trim), title='Men of Color', type='text')
stargazer(GetBalance('woman_of_color', boxes_trim), title='Women of Color', type='text')

#################################################
# APPENDIX TABLE 15: Sample Demographics
#################################################
ReturnDescriptive('race.lim')
ReturnDescriptive('gender.lim', label.names=c("Not Identify as a Man", "Identify as a Man"))
ReturnDescriptive('lgbt', label.names=c("Non-LGBT", "LGBT"))
ReturnDescriptive('education', label.names = c("Some H.S", "H.S. Diploma", "Some College", "College Dergree", "Advanced Degree"))
ReturnDescriptive('income', label.names = c("<30K", "30K-69K", "70K-99K", "100K-200K", ">200K"))
ReturnDescriptive('pid', label.names = c("Strong Dem", "Weak Dem", "Lean Dem", "Indep", "lean Rep", "weak Rep", "Strong Rep"))
ReturnDescriptive('ideology', label.names = c("Very Lib", "Lib", "Slight Lib", "Moderate", "Slight Con", "Con", "Very Con"))
#################################################
# APPENDIX TABLE 16 Evaluations of Biden
#################################################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s   + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
f.2 <- formula(treat_thermometer ~obama_photo * racial_resentment.s  + 
                 modern_sexism.s  * obama_photo + 
                 heterosexism.s  * obama_photo + 
                 ideology  * obama_photo + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
stargazer(lm(f.1, data=boxes_trim, subset = photo_block == 0),
          lm(f.1, data=boxes_trim, subset = photo_block == 0, weights=boxes_trim$w.gen),
          lm(f.2, data=boxes_trim, subset = photo_block == 0),
          lm(f.2, data=boxes_trim, subset = photo_block == 0, weights=boxes_trim$w.gen),
          style = 'ajps', type='text', 
          omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid','income', 'education', 'lgbt'),
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden", 
          label="tab:1ab", 
          column.labels = c("Unmatched, Only Resentment",  "Unmatched, Only Resentment",  "Unmatched, All Attitudes",  "Matched,  All Attitudes"),
          covariate.labels = c("With Obama", 
                               "Resentment", "Sexism", 'Heterosexism', "Ideology",
                               "With Obama: Resentment", "With Obama: Sexism", 'With Obama: Heterosexism', "With Obama: Ideology"))
#################################################
# APPENDIX TABLE 17 Evaluations of Biden and Clinton with Abs. Value of Resentment
#################################################
f.1.abs <- formula(treat_thermometer ~ 
                     I(relevel(photo_treatment, "Clinton with Obama")) * abs(racial_resentment.s) + 
                     I(relevel(photo_treatment, "Clinton with Obama")) * modern_sexism.s + 
                     I(relevel(photo_treatment, "Clinton with Obama")) * heterosexism.s + 
                     I(relevel(photo_treatment, "Clinton with Obama")) * ideology + 
                     white_woman + man_of_color + woman_of_color + 
                     pid + ideology + income + education)
f.2d <- formula(treat_thermometer ~ I(relevel(photo_treatment, "Clinton with Obama")) * abs(racial_resentment.s) + 
                  modern_sexism.s + heterosexism.s + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)
out.1.abs <- lm(f.1.abs, data=boxes_trim)
out.2.abs <- lm(f.2d, data=boxes_trim)

stargazer(out.2.abs, out.1.abs,   style = 'ajps',  type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          column.labels = c("Unmatched, Only Resentment", "Unmatched"),
          title="Evaluations of Biden and Clinton",
          label="tab:reg exp 1", omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid', 'income', 'education', 'lgbt'),
          covariate.labels = c("Biden", "Biden, Obama", "Clinton", 
                               "|Resentment|", "Sexism", 'Heterosexism', 
                               "Ideology", 
                               "Biden: |Resentment|", "Biden, Obama: |Resentment|", "Clinton: |Resentment|",
                               "Biden: Sexism", "Biden, Obama: Sexism", "Clinton: Sexism",
                               "Biden: Heteroexism", "Biden, Obama: Heteroexism", "Clinton: Heteroexism",
                               "Biden: Ideology", "Biden, Obama: Ideology", "Clinton: Ideology"
          ))
#################################################
# APPENDIX TABLE 18 Evaluations of Biden and Clinton by Sexism Score
#################################################
f.2 <- formula(treat_thermometer ~ I(relevel(photo_treatment, "Clinton with Obama")) * racial_resentment.s + 
                 heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
stargazer(lm(f.2, data=boxes_trim[boxes_trim$modern_sexism_groups == "Low",]),
          lm(f.2, data=boxes_trim[boxes_trim$modern_sexism.s < 0,]),
          lm(f.2, data=boxes_trim[boxes_trim$modern_sexism.s > 0,]),
          lm(f.2, data=boxes_trim[boxes_trim$modern_sexism_groups == "High",]),
          style = 'ajps',  type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          column.labels = c("Bottom 30th",  "Below Average", "Above Average", "Top 30th"),
          title="Evaluations of Biden and Clinton by Sexism Score",
          label="tab:reg exp 1 sexism groups", omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid', 'ideology', 'income', 'education', "Ideology"),
          covariate.labels = c("Biden", "Biden, Obama", "Clinton", 
                               "Resentment",'Heterosexism', 
                               "Biden: Resentment", "Biden, Obama: Resentment", "Clinton: Resentment"
          ))
#################################################
# APPENDIX TABLE 19 Evaluations of Biden and Clinton by Ideology
#################################################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + race.lim + lgbt + gender.lim + income + education)
out.1 <- lm(f.1, data=boxes_data, subset = photo_block == 0 & ideology < 4)
out.2 <- lm(f.1, data=boxes_data, subset = photo_block == 0 & ideology > 4)
out.3 <- lm(f.1, data=boxes_data, subset = photo_block == 1 & ideology < 4)
out.4 <- lm(f.1, data=boxes_data, subset = photo_block == 1 & ideology > 4)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', type='text',
    dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
    title="Evaluations of Biden and Clinton by Ideology",
    label="tab:1a clinton id",
    column.labels = c("Biden X Lib", "Biden X Cons", "Clinton X Lib", "Clinton X Cons"),
    covariate.labels =  c("With Obama", 
                          "Resentment", "Sexism", 'Heterosexism',
                          "Party ID", "Other", "Asian", "Black", "Latinx", "Man", "LGBT", "Income", "Education",
                          "With Obama: Resentment"))
#################################################
# APPENDIX TABLE 20 Evaluations of Biden and Clinton by Group
#################################################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + ideology + gender.lim + lgbt + income + education)

out.1 <- lm(f.1, data=boxes_trim[boxes_trim$race.bin == 1 & boxes_trim$photo_block == 0,])
out.2 <- lm(f.1, data=boxes_trim[boxes_trim$race.bin != 1 & boxes_trim$photo_block == 0,])
out.3 <- lm(f.1, data=boxes_trim[boxes_trim$race.bin == 1 & boxes_trim$photo_block == 1,])
out.4 <- lm(f.1, data=boxes_trim[boxes_trim$race.bin != 1 & boxes_trim$photo_block == 1,])

f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + ideology + gender.lim + lgbt + income + education)
out.1 <- lm(f.1, data=boxes_trim[boxes_trim$race.lim == 'White' & boxes_trim$photo_block == 0,])
out.2 <- lm(f.1, data=boxes_trim[boxes_trim$race.lim == 'White' & boxes_trim$photo_block == 1,])
out.3 <- lm(f.1, data=boxes_trim[boxes_trim$race.lim != 'White' & boxes_trim$photo_block == 0,])
out.4 <- lm(f.1, data=boxes_trim[boxes_trim$race.lim != 'White' & boxes_trim$photo_block == 1,])
stargazer(out.1, out.3, out.2, out.4,  style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by Group",
          label="tab:reg exp 1a groups 1 unweight",
          column.labels = c("Biden X White", "Biden X POC", "Clinton X White", "Clinton X POC"),
          covariate.labels = c("With Obama", 
                               "Resentment", "Sexism", 'Heterosexism',
                               "Party ID", "Ideology", "Man", "LGBT", "Income", "Education",
                               "With Obama: Resentment"))

########################
# Gender (Not included)
########################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 obama_photo * modern_sexism.s + obama_photo * heterosexism.s + obama_photo *  ideology + 
                 man_of_color + woman_of_color + 
                 pid + ideology + income + education)
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 1)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 1, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 0)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 0, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Clinton by Gender",
          label="tab:1a clinton gender",
          column.labels = c("Men", "Matched", "Women", "Matched"),
          covariate.labels = c("Obama", 
                               "Resentment", "Sexism", 'Heterosexism', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Obama: Resentment"))

out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & gender.lim == 1)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & gender.lim == 1, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & gender.lim == 0)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & gender.lim == 0, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden by Gender",
          label="tab:1a biden gender",
          column.labels = c("Men", "Matched", "Women", "Matched"),
          covariate.labels = c("Obama", 
                               "Resentment", "Sexism", 'Heterosexism', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Obama: Resentment"))

########################
# Race (Not Included)
########################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 obama_photo * modern_sexism.s + obama_photo * heterosexism.s + obama_photo *  ideology + 
                 white_woman + woman_of_color + 
                 pid + ideology + income + education)
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 1)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 1, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 0)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 0, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Clinton by Race",
          label="tab:1a clinton race",
          column.labels = c("White", "Matched", "POC", "Matched"),
          covariate.labels = c("Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Obama: Resentment"))
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & race.bin == 1)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & race.bin == 1, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & race.bin == 0)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & race.bin == 0, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden by Race",
          label="tab:1a biden race",
          column.labels = c("White", "Matched", "POC", "Matched"),
          covariate.labels = c("Obama", 
                               "Resentment", "Sexism", 'Heterosexism',"Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Obama: Resentment"))
########################
# Party (Not Included)
########################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 obama_photo * modern_sexism.s + obama_photo * heterosexism.s + obama_photo *  ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid < 4)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid < 4, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid > 4)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid > 4, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', type='text',
dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
title="Evaluations of Clinton by Party",
label="tab:1a clinton pid",
column.labels = c("Dem.", "Matched", "Rep.", "Matched"),
covariate.labels = c("Obama", 
                     "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                     "Party ID", "Ideology", "Income", "Education",
                     "Obama: Resentment"))

########################
# Alt Group Models (Not Included)
########################
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & pid < 4)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & pid < 4, weights=boxes_trim$w.gen)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & pid > 4)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 0 & pid > 4, weights=boxes_trim$w.gen)
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden by Party",
          label="tab:1a biden pid",
          column.labels = c("Dem.", "Matched", "Rep.", "Matched"),
          covariate.labels = c("Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Obama: Resentment"))
f.1 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
f.2 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 man_of_color + woman_of_color + 
                 pid + ideology + income + education)
f.3 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + woman_of_color + 
                 pid + ideology + income + education)
f.1a <- formula(treat_thermometer ~ photo_treatment2 * racial_resentment.s + 
                  modern_sexism.s + heterosexism.s + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)
f.2a <- formula(treat_thermometer ~ photo_treatment2 * racial_resentment.s + 
                  modern_sexism.s + heterosexism.s + 
                  man_of_color + woman_of_color + 
                  pid + ideology + income + education)
f.3a <- formula(treat_thermometer ~ photo_treatment2 * racial_resentment.s + 
                  modern_sexism.s + heterosexism.s + 
                  white_woman + woman_of_color + 
                  pid + ideology + income + education)
out <- lm(f.1, data=boxes_trim)
out.1 <- lm(f.2, data=boxes_trim[boxes_trim$gender.lim == 1,])
out.2 <- lm(f.2, data=boxes_trim[boxes_trim$gender.lim == 0,])
out.3 <- lm(f.3, data=boxes_trim[boxes_trim$race.bin == 1,])
out.4 <- lm(f.3, data=boxes_trim[boxes_trim$race.bin != 1,])
out.5 <- lm(f.1, data=boxes_trim[boxes_trim$ideology < 4,])
out.6 <- lm(f.1, data=boxes_trim[boxes_trim$ideology > 4,])
out.7 <- lm(f.1, data=boxes_trim[boxes_trim$pid < 4,])
out.8 <- lm(f.1, data=boxes_trim[boxes_trim$pid > 4,])
out.1a <- lm(f.2a, data=boxes_trim[boxes_trim$gender.lim == 1,])
out.2a <- lm(f.2a, data=boxes_trim[boxes_trim$gender.lim == 0,])
out.3a <- lm(f.3a, data=boxes_trim[boxes_trim$race.bin == 1,])
out.4a <- lm(f.3a, data=boxes_trim[boxes_trim$race.bin != 1,])
out.5a <- lm(f.1a, data=boxes_trim[boxes_trim$ideology < 4,])
out.6a <- lm(f.1a, data=boxes_trim[boxes_trim$ideology > 4,])
out.7a <- lm(f.1a, data=boxes_trim[boxes_trim$pid < 4,])
out.8a <- lm(f.1a, data=boxes_trim[boxes_trim$pid > 4,])
stargazer(out.1, out.2, out.3, out.4,  style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by Group",
          label="tab:reg exp 1a groups 1 unweight",
          column.labels = c("Men", "Women", "White", "POC"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))
stargazer(out.1a, out.2a, out.3a, out.4a,  style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by Group (Matched)",
          label="tab:reg exp 1a groups 1 weight",
          column.labels = c("Men", "Women", "White", "POC"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))
stargazer(out.5, out.6, out.7, out.8,  style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by PID and IDEO ",
          label="tab:reg exp 1a PID and IDEO  1 unweight",
          column.labels = c("Lib", "Consv", "Dem", "Rep"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))
stargazer(out.5a, out.6a, out.7a, out.8a,  style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by PID and IDEO (Matched)",
          label="tab:reg exp 1a PID and IDEO  1 weight",
          column.labels = c("Lib", "Consv", "Dem", "Rep"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))


f.1 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_data[boxes_data$race.lim == 'White' & boxes_data$gender.lim == 1,])
out.2 <- lm(f.1, data=boxes_data[boxes_data$race.lim == 'White' & boxes_data$gender.lim == 0,])
out.3 <- lm(f.1, data=boxes_data[boxes_data$race.lim != 'White' & boxes_data$gender.lim == 1,])
out.4 <- lm(f.1, data=boxes_data[boxes_data$race.lim != 'White' & boxes_data$gender.lim == 0,])
stargazer(out.1, out.2, out.3, out.4, style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton by Intersection of Race and Gender",
          label="tab:reg exp 1a wm and ww",
          column.labels = c("White Men", "White Women", "Men of Color", "Women of Color"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 
                               "Party ID", "Ideology", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))
#################################################
# APPENDIX TABLE 21 Evaluations of Biden and Clinton
#################################################
f.4 <- formula(treat_thermometer ~ photo_treatment * (racial_resentment.s) + modern_sexism.s + heterosexism.s +
                 pid + ideology + race.lim + gender.lim + lgbt + income + education)
f.5 <- formula(treat_thermometer ~ photo_treatment * (modern_sexism.s) +  racial_resentment.s + heterosexism.s +
                 pid + ideology + race.lim + gender.lim + lgbt + income + education)
f.6 <- formula(treat_thermometer ~ photo_treatment * (heterosexism.s) +  racial_resentment.s + modern_sexism.s +
                 pid + ideology + race.lim + gender.lim + lgbt + income + education)
out.1 <- lm(f.4, data=boxes_data)
out.4 <- lm(f.5, data=boxes_data)
out.5 <- lm(f.6, data=boxes_data)
stargazer(out.1, out.4, out.5,  style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton",
          label="tab:reg exp 1 other attitudes",
          column.labels = c("Resentmetn Alone", "Sexism Alone","Heterosexism Alone"),
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 
                               "Party ID", "Ideology", "Other", "Asian", "Black", "LAtinx", "Man", "LGBT", "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton,Obama: Resentment",
                               "Biden, Obama: Sexism", "Clinton: Sexism", "Clinton,Obama: Sexism",
                               "Biden, Obama: Heterosexism", "Clinton: Heterosexism", "Clinton,Obama: Heterosexism"))

#################################################
# APPENDIX TABLE 22 Evaluations of Biden and Clinton with Different Reference Groups
#################################################
f.1 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.1a <- lm(f.1, data=boxes_trim, weights=boxes_trim$w.gen)
boxes_trim$photo_treatment <- relevel(boxes_trim$photo_treatment, "Clinton")
out.2 <- lm(f.1, data=boxes_trim)
out.2a <- lm(f.1, data=boxes_trim, weights=boxes_trim$w.gen)
boxes_trim$photo_treatment <- relevel(boxes_trim$photo_treatment, "Biden with Obama")
out.3 <- lm(f.1, data=boxes_trim)
out.3a <- lm(f.1, data=boxes_trim, weights=boxes_trim$w.gen)
boxes_trim$photo_treatment <- relevel(boxes_trim$photo_treatment, "Biden")

stargazer(out.1,out.2, out.3, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton with Different Reference Groups (Unmatched)",
          label="tab:reg exp 1 baseline unmatched",
          covariate.labels = c("Biden","Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Modern Sexism", "Heterosexism", 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Resentment:Biden", "Resentment:Biden, Obama","Resentment:Clinton", "Resentment:Clinton, Obama"))
# Not included
stargazer(out.1a, out.2a, out.3, style = 'ajps', #type='text')
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Biden and Clinton with Different Reference Groups (Matched)",
          label="tab:reg exp 1 baseline matched",
          column.labels = c("", "Matched","", "Matched","", "Matched"),
          covariate.labels = c("Biden","Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Modern Sexism", "Heterosexism", 'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", "Income", "Education",
                               "Resentment:Biden", "Resentment:Biden, Obama","Resentment:Clinton", "Resentment:Clinton, Obama"))

#################################################
# APPENDIX TABLE 23 Evaluations of Obama and Racial Resentment
#################################################
f.1 <- formula(obama_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + ideology + race.lim + gender.lim + lgbt +
                 income + education)
f.2 <- formula(obama_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s)
f.3 <- formula(obama_thermometer ~ photo_treatment + racial_resentment.s + 
                 modern_sexism.s + heterosexism.s)
out.1 <- lm(f.1, data=boxes_trim)
out.2 <- lm(f.2, data=boxes_trim)
out.3 <- lm(f.3, data=boxes_trim)
stargazer(out.1, out.2, out.3, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Obama",
          label="tab:reg exp 1a",
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 
                               'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", 
                               "Other", "Asian", "Black", "Latinx", "Man", "LGBT",
                               "Income", "Education",
                               "Biden, Obama: Resentment", "Clinton: Resentment", "Clinton, Obama: Resentment"))
#################################################
# APPENDIX TABLE 24 Evaluations of Obama and Sexism
#################################################
f.1 <- formula(obama_thermometer ~ photo_treatment + racial_resentment.s + 
                 photo_treatment * modern_sexism.s + heterosexism.s + 
                 pid + ideology + race.lim + gender.lim + lgbt +
                 income + education)
f.2 <- formula(obama_thermometer ~ photo_treatment + racial_resentment.s + 
                 photo_treatment * modern_sexism.s + heterosexism.sn)
f.3 <- formula(obama_thermometer ~ photo_treatment + racial_resentment.s + 
                 modern_sexism.s + heterosexism.s)
out.1 <- lm(f.1, data=boxes_trim)
out.2 <- lm(f.2, data=boxes_trim)
out.3 <- lm(f.3, data=boxes_trim)
stargazer(out.1, out.2, out.3, style = 'ajps', type='text',
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Obama",
          label="tab:reg exp 1a",
          covariate.labels = c("Biden, Obama", "Clinton", "Clinton, Obama", 
                               "Resentment", "Sexism", 'Heterosexism', 
                               'White Woman', "Man of Color", 'Women of Color',
                               "Party ID", "Ideology", 
                               "Other", "Asian", "Black", "Latinx", "Man", "LGBT",
                               "Income", "Education",
                               "Biden, Obama: Sexism", "Clinton: Sexism", "Clinton, Obama: Sexism"))
#################################################
# APPENDIX TABLE 25 Evaluations of Obama By Photo Treatment
#################################################
f <- formula(obama_thermometer ~ photo_block)
f.1 <- formula(obama_thermometer ~ photo_block * racial_resentment.s   + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
f.2 <- formula(obama_thermometer ~photo_block * racial_resentment.s  + 
                 modern_sexism.s  * photo_block + 
                 heterosexism.s  * photo_block + 
                 ideology  * photo_block + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
f.3 <- formula(obama_thermometer ~ racial_resentment.s  + 
                 modern_sexism.s  * photo_block + 
                 heterosexism.s  +
                 ideology +
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education, data=boxes_trim)
stargazer(lm(f.1, data=boxes_trim),
          lm(f.3, data=boxes_trim),
          lm(f.2, data=boxes_trim),
          style = 'ajps', type='text', 
          omit = c('white_woman', 'man_of_color', 'woman_of_color', 'pid','income', 'education'),
          dep.var.labels = "Feeling Thermometer", model.numbers = FALSE,
          title="Evaluations of Obama By Photo Treatment", 
          label="tab:1a", 
          column.labels = c("Only Resentment",  "Only Sexism",  "All Attitudes"),
          covariate.labels = c("Clinton Treatment", 
                               "Resentment", "Sexism", 'Heterosexism', "Ideology",
                               "Clinton Treatment: Resentment", "Clinton Treatment: Sexism", 'Clinton Treatment: Heterosexism', "Clinton Treatment: Ideology"))
##########################################################################
#
# FIGURES
#
##########################################################################
#################################################
# FIGURE 1: Sample statistic
#################################################
#png('BarPlot.png', height = 6.5, width = 6.5, units='in', res=300)
layout(matrix(ncol=2, nrow=3, c(1, 3, 5, 2, 4, 5)), heights = c(.75, 1.25, .25))
par(mar=c(2.5, 2, 1.5, .5), mgp=c(1.1, .05, 0), family='sans', las=1, tck=-.001,
    cex.main=.9, cex.axis=.75)
plot(density(boxes_trim$racial_resentment.s, cut=0), main='Distribution of Standardized Racial Resentment Score', xlab='', lwd=2, ylim=c(0, 1))
tab.pt <- svyby(~treat_thermometer, ~racial_resentment_groups+photo_treatment, design, svymean, na.rm=TRUE)

plot(density(boxes_trim$modern_sexism.s, cut=0), main='Distribution of Standardized Modern Sexism Score', xlab='', lwd=2, ylim=c(0, 1))
tab.pt <- svyby(~treat_thermometer, ~modern_sexism_groups+photo_treatment, design, svymean, na.rm=TRUE)

x.vals <- (seq(0, .8, .2)+.1)
tab.pt <- svyby(~treat_thermometer, ~rr.quantile* photo_treatment, design, svymean, na.rm=TRUE)
plot(tab.pt[1:5,3] ~ x.vals, type='b', ylim=c(0, 100), lwd=2, xlim=c(0, 1), xlab='Racial Resentment Score Quintile', ylab='Average Feeling Thermometer', main='Candidate Evaluations by Resentment Quintile')
lines(tab.pt[6:10,3] ~ x.vals, lty=2, pch=2, type='b', lwd=2)
lines(tab.pt[11:15,3] ~ x.vals, lty=3, pch=3, type='b', lwd=2)
lines(tab.pt[16:20,3] ~ x.vals, lty=4, pch=4, type='b', lwd=2)
#text(x=.8, y=65, '[', srt=45, font=2, cex=4)
segments(x0=c(.7, .7, .9), x1=c(.7, .9, .9), y0=c(65, 65, 65), y1=c(64, 65, 64), lty=2)
segments(x0=c(.7, .7, .9), x1=c(.7, .9, .9), y0=c(15, 15, 15), y1=c(16, 15, 16), lty=4)
segments(x0=c(.91, .92, .91), x1=c(.92, .92, .92), y0=c(67, 67, 13), y1=c(67, 13, 13))
text(x=c(.8, .8, .97), y=c(13, 67, 39), c('-36.13', '-18.07', '-18.01*'))


x.vals <- (seq(0, .8, .2)+.1)
tab.pt <- svyby(~treat_thermometer, ~ms.quantile* photo_treatment, design, svymean, na.rm=TRUE)
plot(tab.pt[1:5,3] ~ x.vals, type='b', lwd=2, ylim=c(0, 100), xlim=c(0, 1), xlab='Modern Sexism Score Quintile', ylab='Average Feeling Thermometer', main='Candidate Evaluations by Sexism Quintile')
lines(tab.pt[6:10,3] ~ x.vals, lty=2, pch=2, type='b', lwd=2)
lines(tab.pt[11:15,3] ~ x.vals, lty=3, pch=3, type='b', lwd=2)
lines(tab.pt[16:20,3] ~ x.vals, lty=4, pch=4, type='b', lwd=2)

plot(NA, type='n', axes=F, xlim=c(0,1), ylim=c(0,10), xlab='', ylab='')
legend('top', levels(boxes_data$photo_treatment), lty=1:4, pch=1:4, bty='n', ncol=2, xpd=T)
#dev.off()
#################################################
# FIGURE 2: Predicted feeling thermometer scores of Clinton and Obama when depicted with and without President Obama.
#################################################
f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:racial_resentment.s", out.1,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Pred_Attitude_Only_Resentment_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()
#################################################
# FIGURE 3: Predicted feeling thermometer scores broken out by respondent demographics and treatment
#################################################
f.1 <- formula(treat_thermometer ~ photo_treatment * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_data[boxes_data$race.lim == 'White' & boxes_data$gender.lim == 1,])
out.2 <- lm(f.1, data=boxes_data[boxes_data$race.lim == 'White' & boxes_data$gender.lim == 0,])
out.3 <- lm(f.1, data=boxes_data[boxes_data$race.lim != 'White' & boxes_data$gender.lim == 1,])
out.4 <- lm(f.1, data=boxes_data[boxes_data$race.lim != 'White' & boxes_data$gender.lim == 0,])
out.ef.wm <- effect("photo_treatment:racial_resentment.s", out.1, 
                    xlevels=list(racial_resentment.s=quantile(boxes_data$racial_resentment.s, probs=seq(0,1,.2))))
out.ef.ww <- effect("photo_treatment:racial_resentment.s", out.2, 
                    xlevels=list(racial_resentment.s=quantile(boxes_data$racial_resentment.s, probs=seq(0,1,.2))))
out.ef.moc <- effect("photo_treatment:racial_resentment.s", out.3, 
                     xlevels=list(racial_resentment.s=quantile(boxes_data$racial_resentment.s, probs=seq(0,1,.2))))
out.ef.woc <- effect("photo_treatment:racial_resentment.s", out.4, 
                     xlevels=list(racial_resentment.s=quantile(boxes_data$racial_resentment.s, probs=seq(0,1,.2))))

#png('../Exp_1_Pred_Prod_white_women.png', height = 8, width = 8, units='in', res=300)
par(mfrow=c(2, 2), mar=c(2.25, 2.25, 2, .5), mgp=c(1.1, .05, 0), family='sans', las=3, tck=-.001,
    cex.main=.9)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", ylab="Predicted Feeling Thermometer",
     main="White Men")
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind <- seq(1, 22, 4)
lines(out.ef.wm$fit[ind] ~ c(0:5), 
      lty=1, pch=1, type="b", col='black')
lines(out.ef.wm$fit[ind+1] ~ c(0:5), 
      lty=2, pch=2, type="b", col='firebrick3')
lines(out.ef.wm$fit[ind+2] ~ c(0:5), 
      lty=3, pch=3, type="b", col='royalblue4')
lines(out.ef.wm$fit[ind+3] ~ c(0:5), 
      lty=4, pch=4, type="b", col='palevioletred4')
box()
legend("bottomleft", c("Biden", "Biden, Obama", "Clinton", "Clinton, Obama"),
       lty=1:5, pch=1:5, col=c("black", "firebrick3",'royalblue4', "palevioletred4"),
       cex=.75, bty="n", ncol=1)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", ylab="Predicted Feeling Thermometer",
     main="White Women")
abline(h=50)
axis(1, at=c(0:4), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind <- seq(1, 22, 4)
lines(out.ef.ww$fit[ind] ~ c(0:5), 
      lty=1, pch=1, type="b", col='black')
lines(out.ef.ww$fit[ind+1] ~ c(0:5), 
      lty=2, pch=2, type="b", col='firebrick3')
lines(out.ef.ww$fit[ind+2] ~ c(0:5), 
      lty=3, pch=3, type="b", col='royalblue4')
lines(out.ef.ww$fit[ind+3] ~ c(0:5), 
      lty=4, pch=4, type="b", col='palevioletred4')
box()
legend("bottomleft", c("Biden", "Biden, Obama", "Clinton", "Clinton, Obama"),
       lty=1:5, pch=1:5, col=c("black", "firebrick3",'royalblue4', "palevioletred4"),
       cex=.75, bty="n", ncol=1)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", ylab="Predicted Feeling Thermometer",
     main="Men of Color")
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind <- seq(1, 22, 4)
lines(out.ef.moc$fit[ind] ~ c(0:5), 
      lty=1, pch=1, type="b", col='black')
lines(out.ef.moc$fit[ind+1] ~ c(0:5), 
      lty=2, pch=2, type="b", col='firebrick3')
lines(out.ef.moc$fit[ind+2] ~ c(0:5), 
      lty=3, pch=3, type="b", col='royalblue4')
lines(out.ef.moc$fit[ind+3] ~ c(0:5), 
      lty=4, pch=4, type="b", col='palevioletred4')
box()
legend("bottomleft", c("Biden", "Biden, Obama", "Clinton", "Clinton, Obama"),
       lty=1:5, pch=1:5, col=c("black", "firebrick3",'royalblue4', "palevioletred4"),
       cex=.75, bty="n", ncol=1)
plot(NA, type="n", xlim=c(0, 4), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", ylab="Predicted Feeling Thermometer",
     main="Women of Color")
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind <- seq(1, 22, 4)
lines(out.ef.woc$fit[ind] ~ c(0:5), 
      lty=1, pch=1, type="b", col='black')
lines(out.ef.woc$fit[ind+1] ~ c(0:5), 
      lty=2, pch=2, type="b", col='firebrick3')
lines(out.ef.woc$fit[ind+2] ~ c(0:5), 
      lty=3, pch=3, type="b", col='royalblue4')
lines(out.ef.woc$fit[ind+3] ~ c(0:5), 
      lty=4, pch=4, type="b", col='palevioletred4')
box()
legend("bottomleft", c("Biden", "Biden, Obama", "Clinton", "Clinton, Obama"),
       lty=1:5, pch=1:5, col=c("black", "firebrick3",'royalblue4', "palevioletred4"),
       cex=.75, bty="n", ncol=1)
#dev.off()
#################################################
# FIGURE 4: Predicted feeling thermometer scores by racial resentment score by respondent sexism.
#################################################
f.1 <- formula(treat_thermometer ~ 
                 photo_treatment2 * racial_resentment.s + 
                 photo_treatment2 * modern_sexism.s + 
                 photo_treatment2 * heterosexism.s + 
                 photo_treatment2 * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
f.1a <- formula(treat_thermometer ~ 
                  photo_treatment2 * racial_resentment.s + 
                  + heterosexism.s + 
                  + ideology + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)
f.1a <- formula(treat_thermometer ~ 
                  photo_treatment2 * racial_resentment.s + 
                  photo_treatment2 * heterosexism.s + 
                  photo_treatment2 * ideology + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)
f.1b <- formula(treat_thermometer ~ 
                  photo_treatment * racial_resentment + 
                  modern_sexism.s + heterosexism.s + 
                  ideology + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)
f.1b <- formula(treat_thermometer ~ 
                  obama_photo * photo_block * racial_resentment.s + 
                  modern_sexism.s + heterosexism.s + 
                  ideology + 
                  white_woman + man_of_color + woman_of_color + 
                  pid + ideology + income + education)

out.1 <- lm(f.1, data=boxes_trim)
out.1a <- lm(f.1a, data=boxes_trim[boxes_trim$modern_sexism_groups == 'Low',])
out.1b <- lm(f.1a, data=boxes_trim[boxes_trim$modern_sexism.s < .28,])
out.1c <- lm(f.1a, data=boxes_trim[boxes_trim$modern_sexism.s > .28,])
out.1d <- lm(f.1a, data=boxes_trim[boxes_trim$modern_sexism_groups == 'High',])
out.ef.1 <- summary(effect("photo_treatment2:racial_resentment.s", out.1,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
out.ef.1a <- summary(effect("photo_treatment2:racial_resentment.s", out.1a,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
out.ef.1b <- summary(effect("photo_treatment2:racial_resentment.s", out.1b,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
out.ef.1c <- summary(effect("photo_treatment2:racial_resentment.s", out.1c,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
out.ef.1d <- summary(effect("photo_treatment2:racial_resentment.s", out.1d,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
out.ef.2 <- summary(effect("photo_treatment2:modern_sexism.s", out.1,  xlevels=list(modern_sexism.s=quantile(boxes_trim$modern_sexism.s, probs=seq(0,1,.2)))))
out.ef.3 <- summary(effect("photo_treatment2:heterosexism.s", out.1,  xlevels=list(heterosexism.s=quantile(boxes_trim$heterosexism.s, probs=seq(0,1,.2)))))
out.ef.4 <- summary(effect("photo_treatment2:ideology", out.1,  xlevels=list(ideology=quantile(boxes_trim$ideology, probs=seq(0,1,.2)))))

#png('../Exp_1_Pred_Attitude_sexism.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.7)
i=1
cols = grey.colors(5, start=0, end=.5)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer",
     main="Clinton with Obama")
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
lines((out.ef.1b$effect[i,])~ c(0:5), 
      lty=3, pch=3, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1b$upper[i, ], y1=out.ef.1b$lower[i, ], lwd=1, col='black', code=3, angle=90, length=.05)
lines((out.ef.1c$effect[i,])~ c(0:5), 
      lty=4, pch=4, type="b", col=cols[2], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1c$upper[i, ], y1=out.ef.1c$lower[i, ], lwd=1, col='black', code=3, angle=90, length=.05)
i=3
cols = grey.colors(5, start=0, end=.5)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer",
     main="Biden with Obama")
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
lines((out.ef.1b$effect[i,])~ c(0:5), 
      lty=3, pch=3, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1b$upper[i, ], y1=out.ef.1b$lower[i, ], lwd=1, col='black', code=3, angle=90, length=.05)
lines((out.ef.1c$effect[i,])~ c(0:5), 
      lty=4, pch=4, type="b", col=cols[2], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1c$upper[i, ], y1=out.ef.1c$lower[i, ], lwd=1, col='black', code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=.75, y=2.5,  c("Below Average", "Above Average"),
       lty=3:4, pch=3:4, col=cols[1:2], title="Sexism Subset",
       bty="n", ncol=2, xpd=T)

#dev.off()
#################################################
# FIGURE A1: Photo Manipulations
#################################################
# Treatment Photos from AP, NYT
#################################################
# FIGURE A2: Predicted Feeling thermometer using the absolute value of respondents’ racial resentment scores
#################################################
f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * abs(racial_resentment.s) + 
                 modern_sexism.s + 
                 heterosexism.s + 
                 ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:abs(racial_resentment.s)", out.1,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Pred_Attitude_Only_Resentment_abs_value.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()

#################################################
# FIGURE A3: Predicted feeling thermometer score broken out by treatment and attitude
#################################################
f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:modern_sexism.s", out.1,  xlevels=list(modern_sexism.s=quantile(boxes_trim$modern_sexism.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Pred_Attitude_Only_Sexism_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Modern Sexism Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Modern Sexism Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()

f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:heterosexism.s", out.1,  xlevels=list(heterosexism.s=quantile(boxes_trim$heterosexism.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Pred_Attitude_Only_Heterosexism_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Heterosexism Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Heterosexism Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()

f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:ideology", out.1,  xlevels=list(ideology=quantile(boxes_trim$ideology, probs=seq(0,1,.2)))))
#png('../Exp_1_Pred_Attitude_Only_Ideology_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Ideology Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Ideology Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()

#################################################
# FIGURE A4: Predicted feeling thermometer scores of Obama broken out by treatment and respondent sexism score percentile
#################################################
f.1 <- formula(obama_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:modern_sexism.s", out.1,  xlevels=list(modern_sexism.s=quantile(boxes_trim$modern_sexism.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Obama_Pred_Sexism_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Sexism Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Clinton')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Sexism Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Biden')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, c('Without Obama', 'With Obama'),
       lty=2:1, pch=1:2, col=cols[2:1], title="",
       bty="n", ncol=2, xpd=T)
#dev.off()
#################################################
# FIGURE A5: Predicted feeling thermometer scores of Obama broken out by treatment and respondent racial resentment score percentile
#################################################
f.1 <- formula(obama_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 photo_treatment * modern_sexism.s + 
                 photo_treatment * heterosexism.s + 
                 photo_treatment * ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:racial_resentment.s", out.1,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
#png('../Exp_1_Obama_Pred_Attitude_Only_Resentment_Control_Others.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Clinton')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Biden')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, c('Without Obama', 'With Obama'),
       lty=2:1, pch=1:2, col=cols[2:1], title="",
       bty="n", ncol=2, xpd=T)
#dev.off()
#################################################
# FIGURE A6: Predicted feeling thermometer scores of Clinton and Obama when depicted with and without President Obama
#################################################
f.1 <- formula(treat_thermometer ~ 
                 photo_treatment * racial_resentment.s + 
                 modern_sexism.s + 
                 heterosexism.s + 
                 ideology + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out.1 <- lm(f.1, data=boxes_trim)
out.ef.1 <- summary(effect("photo_treatment:racial_resentment.s", out.1,  xlevels=list(racial_resentment.s=quantile(boxes_trim$racial_resentment.s, probs=seq(0,1,.2)))))
#png('Exp_1_Pred_Attitude_Only_Resentment.png', height = 8, width = 6, units='in', res=300)
par(oma=c(3, .5, 2, .5),mar=c(2.5,2.5,.5,.05), mfrow=c(1, 2), mgp=c(1.2, .15, 0), family='sans',
    cex.main=.75, las=1, tck=-.001, yaxs='i', cex.axis=.75)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='Without Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[1,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[1, ], y1=out.ef.1$lower[1, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
lines((out.ef.1$effect[3,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[3, ], y1=out.ef.1$lower[3, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
plot(NA, type="n", xlim=c(0, 5), ylim=c(0,100), axes=FALSE, 
     xlab="Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer", main='With Obama')
abline(h=50)
axis(1, at=c(0:5), labels=seq(0, 1, .2), las=1)
axis(2, at=seq(0, 100, 10), las=1)
cols = grey.colors(4, start=0)
lines((out.ef.1$effect[2,])~ c(0:5), 
      lty=2, pch=1, type="b", col=cols[3], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[2, ], y1=out.ef.1$lower[2, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)

lines((out.ef.1$effect[4,])~ c(0:5), 
      lty=1, pch=2, type="b", col=cols[1], lwd=2)
arrows(x0=c(0:5), y0=out.ef.1$upper[4, ], y1=out.ef.1$lower[4, ], lwd=1, col=cols[1], code=3, angle=90, length=.05)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)
legend(x=1.25, y=3, rownames(out.ef.1$effect)[c(3,1)],
       lty=1:4, pch=2:1, col=cols, title="",
       bty="n", ncol=2, xpd=T)
#dev.off()






#################################################
# FIGURE (excluded): Within-Candidated Pred Prob
#################################################
f.1 <- formula(treat_thermometer ~ obama_photo * racial_resentment.s + 
                 modern_sexism.s + heterosexism.s + 
                 white_woman + man_of_color + woman_of_color + 
                 pid + ideology + income + education)
out <- lm(f.1, data=boxes_trim, subset = photo_block == 1)
out.1 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 1)
out.2 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & gender.lim == 0)
out.3 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 1)
out.4 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & race.bin == 0)
out.5 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & ideology < 4)
out.6 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & ideology > 4)
out.7 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid < 4)
out.8 <- lm(f.1, data=boxes_trim, subset = photo_block == 1 & pid > 4)


out.ef <- effect("obama_photo:racial_resentment.s", out,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.1 <- effect("obama_photo:racial_resentment.s", out.1,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.2 <- effect("obama_photo:racial_resentment.s", out.2,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.3 <- effect("obama_photo:racial_resentment.s", out.3,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.4 <- effect("obama_photo:racial_resentment.s", out.4,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.5 <- effect("obama_photo:racial_resentment.s", out.5,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.6 <- effect("obama_photo:racial_resentment.s", out.6,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.7 <- effect("obama_photo:racial_resentment.s", out.7,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))
out.ef.8 <- effect("obama_photo:racial_resentment.s", out.8,  xlevels=list(racial_resentment=quantile(boxes_trim$racial_resentment.s)))

#png('Exp_1a_Pred_Prod_groups.png', height = 6, width = 6, units='in', res=300)
par(mfrow=c(1, 2), mar=c(2.25, 2, 2, .5), mgp=c(1.1, .05, 0), family='sans', las=3, tck=-.001,
    cex.main=.9)
plot(NA, type="n", xlim=c(0, 4), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer",
     main="Demographics")
abline(h=50)
axis(1, at=c(0:4), labels=seq(0, 1, .25), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind = seq(1, 10, 2)
lines((out.ef$fit[ind+1])~ c(0:4), 
      lty=1, pch=1, type="b", col='black')
arrows(x0=c(0:4), y0=out.ef$upper[ind+1], y1=out.ef$lower[ind+1], lwd=.25, col='black', code=3, angle=90, length=.05)
lines((out.ef.1$fit[ind+1])~ c(0:4), 
      lty=2, pch=2, type="b", col='firebrick3')
arrows(x0=c(0:4), y0=out.ef.1$upper[ind+1], y1=out.ef.1$lower[ind+1], lwd=.25, col='firebrick3', code=3, angle=90, length=.05)
lines((out.ef.2$fit[ind+1])~ c(0:4), 
      lty=3, pch=3, type="b", col='firebrick3')
arrows(x0=c(0:4), y0=out.ef.2$upper[ind+1], y1=out.ef.2$lower[ind+1], lwd=.25, col='firebrick3', code=3, angle=90, length=.05)
lines((out.ef.3$fit[ind+1])~ c(0:4), 
      lty=4, pch=4, type="b", col='royalblue4')
arrows(x0=c(0:4), y0=out.ef.3$upper[ind+1], y1=out.ef.3$lower[ind+1], lwd=.25, col='royalblue4', code=3, angle=90, length=.05)
lines((out.ef.4$fit[ind+1])~ c(0:4), 
      lty=5, pch=5, type="b", col='royalblue4')
arrows(x0=c(0:4), y0=out.ef.4$upper[ind+1], y1=out.ef.4$lower[ind+1], lwd=.25, col='royalblue4', code=3, angle=90, length=.05)
legend("topright", c("Full", "Men", "Women", "White", "POC"),
       lty=1:5, pch=1:6, col=c('black', 'firebrick3', 'firebrick3', "royalblue4", "royalblue4"),
       cex=.75, bty="n", ncol=1)

plot(NA, type="n", xlim=c(0, 4), ylim=c(0,100), axes=FALSE, 
     xlab="Racial Resentment Score Percentile", 
     ylab="Predicted Feeling Thermometer",
     main="Ideology")
abline(h=50)
axis(1, at=c(0:4), labels=seq(0, 1, .25), las=1)
axis(2, at=seq(0, 100, 10), las=1)
ind = seq(1, 10, 2)
lines((out.ef$fit[ind+1])~ c(0:4), 
      lty=1, pch=1, type="b", col='black')
arrows(x0=c(0:4), y0=out.ef$upper[ind+1], y1=out.ef$lower[ind+1], lwd=.25, col='black', code=3, angle=90, length=.05)
lines((out.ef.5$fit[ind+1])~ c(0:4), 
      lty=2, pch=2, type="b", col='firebrick3')
arrows(x0=c(0:4), y0=out.ef.5$upper[ind+1], y1=out.ef.5$lower[ind+1], lwd=.25, col='firebrick3', code=3, angle=90, length=.05)
lines((out.ef.6$fit[ind+1])~ c(0:4), 
      lty=3, pch=3, type="b", col='firebrick3')
arrows(x0=c(0:4), y0=out.ef.6$upper[ind+1], y1=out.ef.6$lower[ind+1], lwd=.25, col='firebrick3', code=3, angle=90, length=.05)
lines((out.ef.7$fit[ind+1])~ c(0:4), 
      lty=4, pch=4, type="b", col='royalblue4')
arrows(x0=c(0:4), y0=out.ef.7$upper[ind+1], y1=out.ef.7$lower[ind+1], lwd=.25, col='royalblue4', code=3, angle=90, length=.05)
lines((out.ef.8$fit[ind+1])~ c(0:4), 
      lty=5, pch=5, type="b", col='royalblue4')
arrows(x0=c(0:4), y0=out.ef.8$upper[ind+1], y1=out.ef.8$lower[ind+1], lwd=.25, col='royalblue4', code=3, angle=90, length=.05)
legend("topright", c("Full", "Lib.", "Consv.", "Dem.", "Rep."),
       lty=1:5, pch=1:6, col=c('black', 'firebrick3', 'firebrick3', "royalblue4", "royalblue4"),
       cex=.75, bty="n", ncol=1)
#dev.off()