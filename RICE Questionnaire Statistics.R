###
### RiCE QUESTIONNAIRE STATISTICS CODE
### By Corey Ford (c.j.ford@qmul.ac.uk)
###

#=SETUP=========================================================================

# Install packages 
install.packages("rstudioapi")
install.packages("readxl")
install.packages("psych")
install.packages("REdaS")
install.packages("GPArotation")
install.packages("Hmisc", dependencies=TRUE)
install.packages("lavaan")
install.packages("lavaanPlot")

# Load packages
library("rstudioapi")
library("readxl")
library("psych")
library("REdaS")
library("Hmisc")
library("lavaan")
library("lavaanPlot")

# Also setup R to read current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)





#=ITEM DEVELOPMENT==============================================================

## ::: Preliminary... Authors Round 1 ::: 
round1 = read_excel("PHASE 1 - ITEM DEVELOPMENT//Item Development Analysis.xlsx",
                    sheet = "Authors Round 1") 
result = psych::alpha(round1[, c("Author_A", "Author_B")])$total$raw_alpha
signif(result,2)

## ::: Preliminary... Authors Round 2 ::: 
round2 = read_excel("PHASE 1 - ITEM DEVELOPMENT//Item Development Analysis.xlsx",
                    sheet = "Authors Round 2") 
result = psych::alpha(round2[, c("Author_A", "Author_B")])$total$raw_alpha
signif(result,2)

## ::: Expert Review :::
experts = read_excel("PHASE 1 - ITEM DEVELOPMENT//Item Development Analysis.xlsx",
                    sheet = "Experts Final") 
psych::alpha(experts[,-1])








#=SCALE DEVELOPMENT=============================================================
# Credit to this video which helped with the code:
# https://www.youtube.com/watch?v=VCpVcXf_wOk&t=40s

# Read data
scale_dev = read_excel("PHASE 2 - SCALE DEVELOPMENT//Scale Development Analysis.xlsx",
                        sheet = "Cleaned Data (n=300)") 

## ::: Demographics ::: 
table(scale_dev$Gender)
summary(scale_dev$Age)
sd(scale_dev$Age)
table(scale_dev$Country)

## ::: SRIS Scores ::: 
sris_scores = scale_dev[, c("SRIS_Engagement_Score", "SRIS_Need_Score",	
                            "SRIS_Insight_Score", "SRIS_Score")]
summary(sris_scores)
signif(sd(sris_scores$SRIS_Engagement_Score),2)
signif(sd(sris_scores$SRIS_Need_Score),2)
signif(sd(sris_scores$SRIS_Insight_Score),2)
signif(sd(sris_scores$SRIS_Score),2)


## ::: Factor Analysis ::: 
scale_dev = scale_dev[, c("RICE_Q11", "RICE_Q23", "RICE_Q30", "RICE_Q12", 
                          "RICE_Q29", "RICE_Q35", "RICE_Q13", "RICE_Q7", 
                          "RICE_Q1", "RICE_Q22", "RICE_Q25", "RICE_Q5",
                          "RICE_Q2", "RICE_Q14", "RICE_Q19", "RICE_Q21")]
# test assumptions
bart_spher(scale_dev)
KMO(scale_dev)       

# count factors with eigenvalues > 1, for 16 factors (16 statements)
fa(scale_dev, nfactors = 16, rotate =  "oblimin" )  

# repeat and reduce until all factors eigenvalues > 1... nfactors=4
fa(scale_dev, nfactors = 4, rotate =  "oblimin" )


# repeat and reduce until all factors eigenvalues > 1... nfactors=4
fa(scale_dev, nfactors = 1, rotate =  "oblimin" )


## ::: Inter-item Reliability ::: 
# for whole scale
scale_dev = read_excel("PHASE 2 - SCALE DEVELOPMENT//Scale Development Analysis.xlsx",
                       sheet = "Cleaned Data") 
selectedItems = scale_dev[, c("RICE_Q13", "RICE_Q35",
                              "RICE_Q19", "RICE_Q14",
                              "RICE_Q11", "RICE_Q23",
                              "RICE_Q22", "RICE_Q25")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)

# for factor 1
selectedItems = scale_dev[, c("RICE_Q13", "RICE_Q35")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)
cor.test(scale_dev$RICE_Q13, scale_dev$RICE_Q35, method=c("spearman"))

# for factor 2
selectedItems = scale_dev[, c("RICE_Q19", "RICE_Q14")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)
cor.test(scale_dev$RICE_Q19, scale_dev$RICE_Q14, method=c("spearman"))

# for factor 3
selectedItems = scale_dev[, c("RICE_Q11", "RICE_Q23")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)
cor.test(scale_dev$RICE_Q11, scale_dev$RICE_Q23, method=c("spearman"))

# for factor 4
selectedItems = scale_dev[, c("RICE_Q22", "RICE_Q25")]
result = psych::alpha(selectedItems)$total$raw_alpha
signif(result,2)
cor.test(scale_dev$RICE_Q22, scale_dev$RICE_Q25, method=c("spearman"))








#=SCALE EVALUATION==============================================================


## ::: Demographics ::: 
scaleEvalTest = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx",
                        sheet = "Cleaned Data - Small") 
table(scaleEvalTest$Gender)
summary(scaleEvalTest$Age)
sd(scaleEvalTest$Age)
table(scaleEvalTest$Country)

scaleEvalTest = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis ReTest.xlsx",
                           sheet = "Cleaned Data - Small") 
table(scaleEvalTest$Gender)
summary(scaleEvalTest$Age)
sd(scaleEvalTest$Age)
table(scaleEvalTest$Country)





##---
## ::: Confirmatory Factor Analysis ::: 
##---

# load data
scaleEvalTestBig = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx",
                           sheet = "Cleaned Data") 
scaleEvalReTestBig = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis ReTest.xlsx",
                           sheet = "Cleaned Data") 

# Do CFA (MODIFY BELOW FOR VARIATIONS on Writing vs Drawing)
myModel <- 'ft1 =~ Writing_RICE_Pa1 + Writing_RICE_Pa2
            ft2 =~ Writing_RICE_Ex1 + Writing_RICE_Ex2
            ft3 =~ Writing_RICE_Cp1 + Writing_RICE_Cp2
            ft4 =~ Writing_RICE_Se1 + Writing_RICE_Se2'
fit <- cfa(myModel, data = scaleEvalReTestBig, estimator = "MLR") #<- change data for test/retest
lavResiduals(fit, type = "cor.bentler")
summary(fit, standardized=TRUE, ci=TRUE, fit.measures=TRUE)






##---
## ::: Test-Retest Reliability ::: 
##---


beforeAfter = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx",
                           sheet = "Test Re-Test") 

ICCTest = beforeAfter[, c("Before_Writing_RICE", "After_Writing_RICE")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Writing_RICE_Se", "After_Writing_RICE_Se")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Writing_RICE_Ex", "After_Writing_RICE_Ex")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Writing_RICE_Pa", "After_Writing_RICE_Pa")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Writing_RICE_Cp", "After_Writing_RICE_Cp")]
ICC(ICCTest, alpha=0.5, missing=TRUE)

ICCTest = beforeAfter[, c("Before_Drawing_RICE", "After_Drawing_RICE")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Drawing_RICE_Se", "After_Drawing_RICE_Se")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Drawing_RICE_Ex", "After_Drawing_RICE_Ex")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Drawing_RICE_Pa", "After_Drawing_RICE_Pa")]
ICC(ICCTest, alpha=0.5, missing=TRUE)
ICCTest = beforeAfter[, c("Before_Drawing_RICE_Cp", "After_Drawing_RICE_Cp")]
ICC(ICCTest, alpha=0.5, missing=TRUE)


##---
## ::: Differentiation by Know-Groups :::
##---

# Reload simplified data & retest
scaleEvalTest = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx", 
                           sheet = "Cleaned Data - Small") 
beforeAfter = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx", 
                         sheet = "Test Re-Test") 

# Wilcoxon signed-rank tests
wilcox.test(scaleEvalTest$Writing_RICE_Cp, scaleEvalTest$Drawing_RICE_Cp, 
            alt="two.sided", paired=TRUE) #not sig
median(scaleEvalTest$Writing_RICE_Cp)
median(scaleEvalTest$Drawing_RICE_Cp)
wilcox.test(scaleEvalTest$Writing_RICE_Ex, scaleEvalTest$Drawing_RICE_Ex, 
            alt="two.sided", paired=TRUE) #sig  p = 0.038
median(scaleEvalTest$Writing_RICE_Ex)
median(scaleEvalTest$Drawing_RICE_Ex)
wilcox.test(scaleEvalTest$Writing_RICE_Pa, scaleEvalTest$Drawing_RICE_Pa, 
            alt="two.sided", paired=TRUE) #not sig
median(scaleEvalTest$Writing_RICE_Pa)
median(scaleEvalTest$Drawing_RICE_Pa)
wilcox.test(scaleEvalTest$Writing_RICE_Se, scaleEvalTest$Drawing_RICE_Se, 
            alt="two.sided", paired=TRUE) #sig p = 0.046
median(scaleEvalTest$Writing_RICE_Se)
median(scaleEvalTest$Drawing_RICE_Se)
wilcox.test(scaleEvalTest$Writing_RICE, scaleEvalTest$Drawing_RICE, 
            alt="two.sided", paired=TRUE) #not sig 
median(scaleEvalTest$Writing_RICE)
median(scaleEvalTest$Drawing_RICE)

#----

wilcox.test(beforeAfter$After_Writing_RICE_Cp, beforeAfter$After_Drawing_RICE_Cp, 
            alt="two.sided", paired=TRUE) #not sig
median(beforeAfter$After_Writing_RICE_Cp)
median(beforeAfter$After_Drawing_RICE_Cp)
wilcox.test(beforeAfter$After_Writing_RICE_Ex, beforeAfter$After_Drawing_RICE_Ex, 
            alt="two.sided", paired=TRUE) #sig  p = 0.000118
median(beforeAfter$After_Writing_RICE_Ex)
median(beforeAfter$After_Drawing_RICE_Ex)
wilcox.test(beforeAfter$After_Writing_RICE_Pa, beforeAfter$After_Drawing_RICE_Pa, 
            alt="two.sided", paired=TRUE) #not sig
median(beforeAfter$After_Writing_RICE_Pa)
median(beforeAfter$After_Drawing_RICE_Pa)
wilcox.test(beforeAfter$After_Writing_RICE_Se, beforeAfter$After_Drawing_RICE_Se, 
            alt="two.sided", paired=TRUE) #p-value = 0.002162
median(beforeAfter$After_Writing_RICE_Se)
median(beforeAfter$After_Drawing_RICE_Se)
wilcox.test(beforeAfter$After_Writing_RICE, beforeAfter$After_Drawing_RICE, 
            alt="two.sided", paired=TRUE) #not sig 
median(beforeAfter$After_Writing_RICE)
median(beforeAfter$After_Drawing_RICE)






## ---
## ::: Comparison between RICE and SRIS ::: 
## ---

# Load necessary datas
scaleEvalTest = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis.xlsx", 
                           sheet = "Cleaned Data - Small") 
beforeAfter = read_excel("PHASE 3 - RICE USER STUDY/RiCE User Study Analysis ReTest.xlsx", 
                         sheet = "Cleaned Data - Small") 

# For test... 
Writing_SRIS_RICE = scaleEvalTest[, c("SRIS_Score",	
                                      "Writing_RICE")]

Drawing_SRIS_RICE = scaleEvalTest[, c("SRIS_Score",	
                                      "Drawing_RICE")]

matrix1 = rcorr(as.matrix(Writing_SRIS_RICE), type="spearman")
print(matrix1)


matrix2 = rcorr(as.matrix(Drawing_SRIS_RICE), type="spearman")
print(matrix2)


# For re-test... 
Writing_SRIS_RICE = beforeAfter[, c("SRIS_Score",	
                                    "Writing_RICE")]

Drawing_SRIS_RICE = beforeAfter[, c("SRIS_Score",	
                                    "Drawing_RICE")]

matrix1a = rcorr(as.matrix(Writing_SRIS_RICE), type="spearman")
print(matrix1a)


matrix2a = rcorr(as.matrix(Drawing_SRIS_RICE), type="spearman")
print(matrix2a)





## ---
## ::: Comparison between RICE and CSI :::
## --- 

# For test... 
Writing_CSI_RICE = scaleEvalTest[, c("Writing_CSI_Total",
                                     "Writing_RICE")]



Drawing_CSI_RICE = scaleEvalTest[, c("Drawing_CSI_Total",
                                     "Drawing_RICE")]


matrix3 = rcorr(as.matrix(Writing_CSI_RICE), type="spearman")
print(matrix3)


matrix4 = rcorr(as.matrix(Drawing_CSI_RICE), type="spearman")
print(matrix4)


# For re-test... 

Writing_CSI_RICE = beforeAfter[, c("Writing_CSI_Total",
                                   "Writing_RICE")]


Drawing_CSI_RICE = beforeAfter[, c("Drawing_CSI_Total",
                                   "Drawing_RICE")]


matrix3b = rcorr(as.matrix(Writing_CSI_RICE), type="spearman")
print(matrix3b)


matrix4b = rcorr(as.matrix(Drawing_CSI_RICE), type="spearman")
print(matrix4b)
