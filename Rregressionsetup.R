
library(survival)

panel = read.csv("/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Jiayi_Protest_Investment/Analysis/SurvivalPanel/paneladdvariable01012025.csv", sep =',', header= TRUE)
#75780

'''   X          portfoliocompany Year_Established year month       date statefip
1 0 Cadent Therapeutics, Inc.             2017 2017     1 2017-01-01       25
  countyfips portfoliocompanyid social_focus      city priordeal_number
1      25017              65677    0.0146808 Cambridge                0
  priordeal_amount currentdeal_num currentdeal_amount firstdeal colocation
1                0               0                  0         0          1
  protest_l issues_homogeneity monthpast totalcity averagesocialfocus
1         0                  0        25         0                  0
  competition otherinvestors rankofsocialfocus countyfip protestissues_1m
1           0              0         0.0146808        17                0
  protestactivist_1m protestconcrete_1m numprotest_1m protestissues_6m
1                  0                  0             0                0
  protestactivist_6m protestconcrete_6m numprotest_6m
1                  0                  0             0'''






panel$scope = ( panel$numprotest_6m+1)*(panel$protestconcrete_6m+1)/1000
panel$averagescale = panel$protestactivist_6m/(panel$protestissues_6m*1000000+1)
panel$protestnumber = panel$protestissues_1m/1000

panel$otherinvestors= (panel$totalcity-panel$colocation)
panel$rankofsocialfocus = (panel$social_focus)*(panel$protestissues_6m)


panel$gdpaverage = panel$irs130208d/(panel$totalpop+1)
panel$unemployeerate = panel$unemploymentrate/(panel$totalpop+1)
panel$totalpop= panel$totalpop/1000000

panel$investindustry = panel$industryinvestmentdeals/(panel$irs130208d+1)


'age' 'early_stage' 'Sub_Industries'
 'cityinvestmentdeals' 'industryinvestmentdeals' 'accelerator' 'statefips'
 'countyfips' 'totalpop' 'irs130208d' 'unemploymentrate' 'diversity'





listname<-c( #DV
            'monthpast',
            'scope',
            'rankofsocialfocus',
            'otherinvestors',
            


            'protestnumber',
            'averagescale',
            'age',
            'early_stage',
            'investindustry',
             'gdpaverage',
            'totalpop',
               'diversity'
            )

mcor<-format(round(cor(panel[,listname]),3),nsmall=3)
mcor[upper.tri(mcor, diag=TRUE)]<-""

library(pastecs)
mm<-stat.desc(panel[,listname])
library(psych)
mm<-describe(panel[,listname])


summarystat<-format(round(cbind(mm$mean, mm$sd),digit =3),nsmall=3)
summarystat<-data.frame(cbind(summarystat,mcor))
colnames(summarystat)<-c("mean","sd",seq(1,nrow(summarystat)))
summarystat$label<-rownames(summarystat)

write.table(summarystat$label,
            file="/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Jiayi_Protest_Investment/TableFigure/variablenames_adj.csv",
            sep=",",
            row.names=FALSE)

    
varname<-read.table("/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Jiayi_Protest_Investment/TableFigure/variablenames_summary.csv",sep=",",header=TRUE)

printmodel<-merge(varname, summarystat,by.y = "label", 
                  by.x = "x",  all.x=TRUE, sort = F)   

printmodel$x<- rownames(printmodel)

colnames(printmodel)[1]<-" "
colnames(printmodel)[2]<-" "

test = xtable(printmodel)
print(test,
      type = "latex",
      file = "/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Jiayi_Protest_Investment/TableFigure/table_only.tex",  # This creates a file with just the table code
      include.rownames = FALSE,
      latex.environments = "center",
      sanitize.text.function = function(x) x)


##### write survival tables

library(survival)

# use only one dimension of measuring the actionabel requests
results <- coxph(Surv(monthpast, firstdeal) ~ 
protestconcrete_6m+ 
protestconcrete_6m*otherinvestors+
protestconcrete_6m*rankofsocialfocus+ 
protestnumber+
averagescale+
investindustry+
age+
totalpop+
gdpaverage+
diversity

, data = panel)
summary(results)

scope+
scope*social_focus+
scope*social_focus*otherinvestors+

protestconcrete_6m+ 
protestconcrete_6m* numprotest_6m+
protestconcrete_6m*social_focus+ 
numprotest_6m
protestconcrete_6m

scope*rankofsocialfocus+
otherinvestors+
rankofsocialfocus*otherinvestors+

results <- coxph(Surv(monthpast, firstdeal) ~ 
rankofsocialfocus+
scope*rankofsocialfocus+

otherinvestors+
scope*otherinvestors+

investindustry+
age+
early_stage+
protestnumber+
averagescale+
totalpop+
gdpaverage+
diversity+

scope 

, data = panel)
summary(results)



results<- coxph(Surv(monthpast, firstdeal) ~ 
rankofsocialfocus+
scope*rankofsocialfocus+

otherinvestors+
scope*otherinvestors+

investindustry+
age+
early_stage+
protestnumber+
averagescale+
totalpop+
gdpaverage+
diversity+

scope 
, data = panel)

library(survminer)

# main effect
sex_df <- with(panel,
data.frame(
scope = c(0.05,0.2),
otherinvestors = rep(mean(otherinvestors, na.rm = TRUE), 2), 
rankofsocialfocus = rep(mean(rankofsocialfocus, na.rm = TRUE),2),
protestnumber=rep(mean(protestnumber, na.rm = TRUE), 2),
averagescale=rep(mean(averagescale, na.rm = TRUE), 2),
investindustry = rep(mean(investindustry, na.rm = TRUE), 2),
age= rep(mean(age, na.rm = TRUE), 2),
early_stage= rep(mean(early_stage ,na.rm = TRUE), 2),

totalpop= rep(mean(totalpop ,na.rm = TRUE), 2),
gdpaverage= rep(mean(gdpaverage, na.rm = TRUE), 2),
diversity= rep(mean(diversity, na.rm = TRUE), 2)

                          )
               )
          
fit <- survfit(results, newdata = sex_df)


ggsurvplot(fit,data= sex_df,conf.int = FALSE, 
censor = FALSE,
 legend.title = "Protest scope",
 legend.labs = c("Mean-1SD", "Mean+1SD"),
 palette = c("#000000", "#C0C0C0"),

 ggtheme = theme_minimal())



# interaction 1
sex_df <- with(panel,
data.frame(otherinvestors = rep(mean(otherinvestors, na.rm = TRUE), 4), 
scope = c(0.05,0.2,0.05,0.2),
protestnumber=rep(mean(protestnumber, na.rm = TRUE), 4),
averagescale=rep(mean(averagescale, na.rm = TRUE), 4),
rankofsocialfocus = c(0.3,0.3,1.6,1.6),
investindustry = rep(mean(investindustry, na.rm = TRUE), 4),
age= rep(mean(age, na.rm = TRUE), 4),
early_stage= rep(mean(early_stage ,na.rm = TRUE), 4),

totalpop= rep(mean(totalpop ,na.rm = TRUE), 4),
gdpaverage= rep(mean(gdpaverage, na.rm = TRUE), 4),
diversity= rep(mean(diversity, na.rm = TRUE), 4)

                          )
               )
          
fit <- survfit(results, newdata = sex_df)


ggsurvplot(fit,data= sex_df,conf.int = FALSE, 
censor = FALSE,
 legend.title = "Prosocial narratives alignment * Protest scope",
 legend.labs = c("LL", "LH","HL", "HH"),
 palette = c("#000000","#C0C0C0", "#000000", "#C0C0C0"),
linetype = c(1,1,2,2),
 ggtheme = theme_minimal())


# interaction 1
sex_df <- with(panel,
data.frame(otherinvestors = c(-1,-1, 3, 3), 
scope = c(0.05,0.2,0.05,0.2),
protestnumber=rep(mean(protestnumber, na.rm = TRUE), 2),
averagescale=rep(mean(averagescale, na.rm = TRUE), 2),
rankofsocialfocus = rep(mean(rankofsocialfocus, na.rm = TRUE),2),
investindustry = rep(mean(investindustry, na.rm = TRUE), 2),
age= rep(mean(age, na.rm = TRUE), 2),
early_stage= rep(mean(early_stage ,na.rm = TRUE), 2),

totalpop= rep(mean(totalpop ,na.rm = TRUE), 2),
gdpaverage= rep(mean(gdpaverage, na.rm = TRUE), 2),
diversity= rep(mean(diversity, na.rm = TRUE), 2)

                          )
               )
          
fit <- survfit(results, newdata = sex_df)



 ggsurvplot(fit,data= sex_df,conf.int = FALSE, 
censor = FALSE,
 legend.title = "Investor origin cities * Protest scope",
 legend.labs = c("LL", "LH","HL", "HH"),
 palette = c("#000000","#C0C0C0", "#000000", "#C0C0C0"),
linetype = c(1,1,2,2),
 ggtheme = theme_minimal())



write.csv(panel,"/Users/katewang/Library/CloudStorage/Dropbox/WangjuekateDropbox/Jiayi_Protest_Investment/Analysis/SurvivalPanel/paneladdvariable01012025.csv", row.names= FALSE )

