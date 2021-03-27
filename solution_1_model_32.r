#Test mit TURNOUT, VOTRPOP, ESM und MIGRANT als Konditionen
#und ELECPERF als Outcome



#Laden der benötigten Packages

library(tidyverse)
library(QCA)
library(venn)
library(SetMethods)
library(kableExtra)



#Laden der Rohtabelle und der kalibrierten Tabelle

Excel_RAW_M32 <- read.csv("Excel_RAW_M32.csv")

has_rownames(Excel_RAW_M32)
Excel_RAW_M32 <- Excel_RAW_M32 %>% 
  column_to_rownames("X")
has_rownames(Excel_RAW_M32)


Excel_FS_M32 <- read.csv("Excel_FS_M32.csv")
has_rownames(Excel_FS_M32)
Excel_FS_M32 <- Excel_FS_M32 %>% 
  column_to_rownames("X")
has_rownames(Excel_FS_M32)


#Necessity

#conjunctions
Nec <- superSubset(Excel_FS_M32, 
                   conditions = "TURNOUT, VOTRPOP, ESM, MIGRANT",
                   outcome = "ELECPERF", incl.cut = 0.9)
Nec

#single conditions
conds <- Excel_FS_M32[, 1:5]
conds_specific <- subset(Excel_FS_M32, select = c("TURNOUT", "VOTRPOP", "ESM", "MIGRANT"))
conds_nec <- pof(conds_specific, Excel_FS_M32$ELECPERF)
conds_nec
neg_conds_nec <- pof(1-conds_specific, Excel_FS_M32$ELECPERF)
neg_conds_nec


clabels <- logical(nrow(Excel_FS_M32))
clabels[c(3,6,7,8,9,12,14,15)] <- T

#plot necessary conjunction
XYplot("~VOTRPOP+ESM, ELECPERF", data = Excel_FS_M32,
       relation = "necessity",
       jitter = T, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)



# TruthTable - Sufficiency

#Sufficiency single conditions
conds_suf <- pof(conds_specific, Excel_FS_M32$ELECPERF, relation = "sufficiency")
conds_suf
neg_conds_suf <- pof(1-conds_specific, Excel_FS_M32$ELECPERF, relation = "sufficiency")
neg_conds_suf



TTable <- truthTable(Excel_FS_M32, conditions = "TURNOUT, VOTRPOP, ESM, MIGRANT",
                     outcome = "ELECPERF", incl.cut = c(0.75, 0.5),
                     show.cases = T, complete = F, sort.by = "incl") 

TTable

#deviant cases consistency
TTabledcc <- truthTable(Excel_FS_M32, conditions = "TURNOUT, VOTRPOP, ESM, MIGRANT",
                        outcome = "ELECPERF", incl.cut = c(0.75,0.5),
                        show.cases = T, complete = F, sort.by = "incl,n",
                        dcc = T) 
TTabledcc

#-> zum Glück keine bei Outcome 1

#Standard Analysis

#conservative solution
conservative_sol <- minimize(TTable, details= T)
conservative_sol


#parsimonious solution
parsimonious_sol <- minimize(TTable, include="?",
                             details= T)
parsimonious_sol


#intermediate solution


#directional expectations
dir_exp <- c(1,0,1,1)

#solution
intermed_sol <-  minimize(TTable, include = "?",
                          dir.exp = dir_exp,
                          details = T)
#rows filtered out by directional expectations
filtered_out <- intermed_sol$i.sol$C1P1$EC 
filtered_out

#result
intermed_sol




#Enhanced Standard Analysis


#enhanced parsimonious solution

#contradictory simplifying assumptions for solution

CSA <- findRows(obj = TTable, type = 2)
CSA

#solution

enhanced_pars_sol <- minimize(TTable, include = "?",
                              exclude = CSA, details = T)
#result
enhanced_pars_sol


#enhanced intermediate solution


#removing observed configuration to have a consistency score
#above the threshold for both the presence and the absence
#of the outcome

cons_score_both <- findRows(obj = TTable, type = 3)
cons_score_both

#incoherent configurations 
Nec

incoherent_config <- findRows("~VOTRPOP+ESM" , TTable,
                              type = 1)
incoherent_config


#solution
#weglasssen von CSA, cons score both und incoherent configurations
enhanced_intermed_sol <-  minimize(TTable, include = "?",
                                   exlude = c(CSA, cons_score_both, incoherent_config),
                                   dir.exp = dir_exp,
                                   details = T)

#result
enhanced_intermed_sol





# Plot solution terms

# Solution term conservative
XYplot("TURNOUT*ESM*MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term conservative
XYplot("VOTRPOP*ESM*MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term conservative
XYplot("~TURNOUT*~VOTRPOP*~ESM*~MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term parsimonious, intermediate, enhanced parsimonious, enhanced intermediate
XYplot("ESM*MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term parsimonious, intermediate, enhanced intermediate 
XYplot("~VOTRPOP*~MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = F, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term parsimonious, intermediate, enhanced intermediate
XYplot("~TURNOUT*~VOTRPOP, ELECPERF", data = Excel_FS_M32,
       jitter = F, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

# Solution term enhanced parsimonious
XYplot("~TURNOUT*~VOTRPOP*~MIGRANT, ELECPERF", data = Excel_FS_M32,
       jitter = F, 
       enhance = T,
       cex = 0.7, 
       clabels = clabels)

#Negating the Outcome




#Necessity Negation

NecNeg <- superSubset(Excel_FS_M32, conditions = "VOTRPOP, MIGRANT, ESM, TURNOUT",
                      outcome = "~ELECPERF", incl.cut = 0.9, ron.cut = 0.5)
NecNeg

conds_nec_neg <- pof(conds_specific,1- Excel_FS_M32$ELECPERF)
conds_nec_neg
neg_conds_nec_neg <- pof(1-conds_specific,1- Excel_FS_M32$ELECPERF)
neg_conds_nec_neg




#Truth Table - Sufficiency Negation

TTableNeg <- truthTable(Excel_FS_M32, conditions = "VOTRPOP, MIGRANT, ESM, TURNOUT",
                        outcome = "~ELECPERF", incl.cut = c(0.9,0.6),
                        show.cases = T, complete = F, sort.by = "incl,n") 

TTableNeg

#Standard Analysis Negation


#conservative solution negation
conservative_sol_neg <- minimize(TTableNeg, details= T)
conservative_sol_neg


#parsimonious solution negation
parsimonious_sol_neg <- minimize(TTableNeg, include="?",
                                 details= T)
parsimonious_sol_neg


#intermediate solution negation

#directional expectations
dir_exp_neg <- c(1,0,0,1)

intermed_sol_neg <- minimize(TTableNeg, include = "?",
                             dir.exp = dir_exp_neg,
                             details = T)
intermed_sol_neg

#####################################################################


#enhanced parsimonious solution negation

#contradictory simplifying assumptions for solution
CSA_neg <- findRows(obj = TTableNeg, type = 2)
CSA_neg

#result
enhanced_pars_sol_neg <- minimize(TTableNeg, include = "?",
                                  exclude = CSA_neg, details = T)
enhanced_pars_sol_neg



#enhanced intermediate solution negation


#removing observed configuration to have a consistency score
#above the threshold for both the presence and the absence
#of the outcome

cons_score_both <- findRows(obj = TTableNeg, type = 3)
cons_score_both

#incoherent configurations
NecNeg
# -> relevence of necessity (RoN) score zu gering um notwendige conjunction
# zu bestimmen, deshalb keine incoherent configurations


#solution
#cons score both bleibt auch weg, da Vektor mit Länge 0
enhanced_intermed_sol_neg <-  minimize(TTableNeg, include = "?",
                                       exclude = CSA_neg, 
                                       dir.exp = dir_exp_neg,
                                       details = T)

#result
enhanced_intermed_sol_neg



#################################################################


# Plot solution term negation

clabels <- logical(nrow(Excel_FS_M32))
clabels[c(1,2,4,5,10,11,13)] <- T

#Solution term enhanced parsimonious
XYplot("VOTRPOP*~MIGRANT, ~ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       cex = 0.7, 
       clabels = clabels)

#Solution term conservative, intermediate, enhanced intermediate, enhanced parsimonious
XYplot("VOTRPOP*~ESM, ~ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       cex = 0.7, 
       clabels = clabels)

#Solution term conservative, intermediate, enhanced intermediate
XYplot("VOTRPOP*~MIGRANT*TURNOUT, ~ELECPERF", data = Excel_FS_M32,
       jitter = T, 
       cex = 0.7, 
       clabels = clabels)




#Venn-Diagramm (schöne Blumen)

#positives Outcome
TTable
venn(TTable, opacity = TTable$incl, counts = T, ilabels = T, ellipse = T)

#negatives Outcome
TTableNeg
venn(TTableNeg, opacity = TTable$incl, counts = T, ilabels = T, ellipse = T)
