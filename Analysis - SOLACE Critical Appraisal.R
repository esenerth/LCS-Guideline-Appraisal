### LOAD PACKAGES ###

library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(gtsummary)
library(reactable)
library(MetBrewer)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rmapshaper)
library(sf)

### READ IN DATA ###

AGREEdat <- read_excel("Dataset for R - LC Guidelines SR - SOLACE.xlsx")
AGREEcat <- read_excel("AGREEbyCat.xlsx")
AGREEcont <- read_excel("AGREEbyCont.xlsx")
View(AGREEdat) 
View(AGREEcat) 
View(AGREEcont)

### DESCRIPTIVE ANALYSIS ###

# Summarize guideline characteristics
CharSum <- AGREEdat %>%
  tbl_summary(include = c(DevType,DocType,Fund,SR,Certainty),
  label = list(DevType ~ "Developer Type", DocType ~ "Document Type",
               Fund ~ "Funding Source", SR ~ "Based on SR",
               Certainty ~ "Certainty Assessment"),
  sort = all_categorical(FALSE) ~ "frequency")
  bold_labels(CharSum)
  theme_gtsummary_journal(journal = "jama")
  theme_gtsummary_compact()
CharSum

# Summarize agree scores
reset_gtsummary_theme()
AGREEMed <- AGREEdat %>%
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
  statistic = everything() ~ "{median} ({p25}, {p75})",
  label = list(AGREE1 ~ "Scope & Purpose",AGREE2 ~ "Stakeholder Involvement",
               AGREE3 ~ "Rigor of Development",
               AGREE4 ~ "Clarity of Presentation", AGREE5 ~ "Applicability",
               AGREE6 ~ "Editorial Independence"))
  modify_header(AGREEMed, label = "**AGREE Domain**")
AGREEMed

# AGREE score distributions
AGREEI <- hist(AGREEdat$AGREE1)
plot(AGREEI, xlab = "Scores for AGREE II Domain 1 - Scope and Purpose", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

AGREE2 <- hist(AGREEdat$AGREE2)
plot(AGREE2, xlab = "Scores for AGREE II Domain 2 - Stakeholder Involvement", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

AGREE3 <- hist(AGREEdat$AGREE3)
plot(AGREE3, xlab = "Scores for AGREE II Domain 3 - Rigor", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

AGREE4 <- hist(AGREEdat$AGREE4)
plot(AGREE4, xlab = "Scores for AGREE II Domain 4 - Clarity", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

AGREE5 <- hist(AGREEdat$AGREE5)
plot(AGREE5, xlab = "Scores for AGREE II Domain 5 - Applicability", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

AGREE6 <- hist(AGREEdat$AGREE6)
plot(AGREE6, xlab = "Scores for AGREE II Domain 6 - Editorial Independence", 
     ylab = "Frequency", main = "", xlim = c(0, 1), ylim = c(0, 50),
     cex.lab=1.5, col = met.brewer("Cassatt1", n=10))

# Summarize recommendations per category
Recdat <- AGREEdat %>%
  summarise(across(DX:Surveil, sum))
Recdat2 <- pivot_longer(Recdat, cols = DX:Surveil)
names(Recdat2)[names(Recdat2) == 'name'] <- 'Recommendation Category'
names(Recdat2)[names(Recdat2) == 'value'] <- 'Count of Recommendations'
Recdat2[Recdat2 == 'DX'] <- 'Diagnosis'
Recdat2[Recdat2 == 'Eligible'] <- 'Eligibilty for Screening'
Recdat2[Recdat2 == 'Protocol'] <- 'Screening Protocols'
Recdat2[Recdat2 == 'Report'] <- 'Reporting Findings'
Recdat2[Recdat2 == 'Incident'] <- 'Management of Incidental Findings'
Recdat2[Recdat2 == 'Nodule'] <- 'Nodule Management'
Recdat2[Recdat2 == 'Path'] <- 'Pathology'
Recdat2[Recdat2 == 'Mode'] <- 'Screening Modality'
Recdat2[Recdat2 == 'Program'] <- 'Program Requirements'
Recdat2[Recdat2 == 'SDM'] <- 'Shared Decision-Making'
Recdat2[Recdat2 == 'Smoke'] <- 'Smoking Cessation'
Recdat2[Recdat2 == 'Stage'] <- 'Staging'
Recdat2[Recdat2 == 'Surveil'] <- 'Surveillance'

# Tabulate recommendations per category
reactable(Recdat2,
          defaultSorted = list('Count of Recommendations'= "desc"),
          showSortIcon = FALSE,
          defaultPageSize = 13,
          style = list(fontFamily = "Work Sans, sans-serif", 
                       fontSize = "0.875rem"))

# Summarize documents per continent - MAP
world <- ne_countries(scale = "medium", returnclass = "sf")
country_names <- world %>% group_by(name) %>% tally()
ggplot(data = world) +
  geom_sf()

AGREEdat5 <- AGREEdat %>% group_by(Country) %>% tally()
AGREEdat6 <- AGREEdat5[c(1:13,16,17,1,19,20,21,22,23,24,25),]
AGREEdat7 <- AGREEdat6 %>%
  mutate(Country = recode(Country, 'UAE'='United Arab Emirates',
                          'UK'='United Kingdom',
                          'USA'='United States of America'))

world_joined <- left_join(world, AGREEdat7, by = c("name" = "Country"))
world_joined <- world_joined[-c(241),]
summary(world_joined)

docjoin <- aggregate(x = world_joined['n']
                     , by = list(continent = world_joined[["continent"]])
                     , FUN = sum, na.rm = TRUE)

docjoin <- subset(docjoin,n != 0)
print(docjoin)
docjoin <- cbind(docjoin, 
                 label=c('Africa (1)','Asia (13)','Europe (29)',
                         'North America (60)','South America (5)'))
print(docjoin)

Docs_map <- ggplot(data=docjoin, aes(fill=n)) +
  geom_sf(color = "white",linewidth = 0.3) +
  geom_sf_label(aes(label=label),fill = "white") +
  scale_fill_gradientn(name='Count of\nDocuments',
                       colors = met.brewer("Hokusai2",n=5,direction=1)) +
  theme_void() +
  theme(legend.position = 'bottom')
Docs_map

# Summarize AGREE scores by continent
AGREEtab = 
  subset(AGREEdat, select = -c(ID,Author,Year,Title,Country,DevType,
                             DevName,DocType,Fund,SR,Certainty))
AGREEcontsum <- AGREEtab %>%
  group_by(Continent) %>% 
  summarize(AGREE1Med = median(AGREE1),
            AGREE1IQR = IQR(AGREE1),
            AGREE2Med = median(AGREE2),
            AGREE2IQR = IQR(AGREE2),
            AGREE3Med = median(AGREE3),
            AGREE3IQR = IQR(AGREE3),
            AGREE4Med = median(AGREE4),
            AGREE4IQR = IQR(AGREE4),
            AGREE5Med = median(AGREE5),
            AGREE5IQR = IQR(AGREE5),
            AGREE6Med = median(AGREE6),
            AGREE6IQR = IQR(AGREE6)) 

# Tabulate AGREE scores by continent
reactable(AGREEcontsum,
          defaultSorted = list('Continent'= "asc"),
          showSortIcon = FALSE,
          columns = list(
            AGREE1Med = colDef(name = 'AGREE 1: Median Score',
                               format = colFormat(digits = 2)),
            AGREE1IQR = colDef(name = 'AGREE 1: IQR',
                               format = colFormat(digits = 2)),
            AGREE2Med = colDef(name = 'AGREE 2: Median Score',
                               format = colFormat(digits = 2)),
            AGREE2IQR = colDef(name = 'AGREE 2: IQR',
                               format = colFormat(digits = 2)),
            AGREE3Med = colDef(name = 'AGREE 3: Median Score',
                               format = colFormat(digits = 2)),
            AGREE3IQR = colDef(name = 'AGREE 3: IQR',
                               format = colFormat(digits = 2)),
            AGREE4Med = colDef(name = 'AGREE 4: Median Score',
                               format = colFormat(digits = 2)),
            AGREE4IQR = colDef(name = 'AGREE 4: IQR',
                               format = colFormat(digits = 2)),
            AGREE5Med = colDef(name = 'AGREE 5: Median Score',
                               format = colFormat(digits = 2)),
            AGREE5IQR = colDef(name = 'AGREE 5: IQR',
                               format = colFormat(digits = 2)),
            AGREE6Med = colDef(name = 'AGREE 6: Median Score',
                               format = colFormat(digits = 2)),
            AGREE6IQR = colDef(name = 'AGREE 6: IQR',
                               format = colFormat(digits = 2))),
          striped = TRUE,
          style = list(fontFamily = "Work Sans, sans-serif", 
                       fontSize = "0.875rem"))

# Tabulate AGREE scores by continent - BETTER OPTION
AGREEbyCont <- AGREEdat %>%
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              by='Continent',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEbyCont, label = "**AGREE Domain**")

# Heat map of AGREE scores by continent
ggplot(AGREEcont) + 
  geom_tile(aes(x=Domain,y=fct_relevel(
    Continent,"NR","International","South America","North America","Europe",
    "Asia","Africa"),fill=Score)) +
  labs(x="AGREE II Domain",
       y="Continent") +
  scale_fill_gradientn(name='Median Score',
                       colors = met.brewer("Tam",n=6),
                       limits=c(0,1),
                       breaks = c(0,.2,.4,.6,.8,1)) +
  scale_x_continuous(n.breaks = 6) +
  theme_minimal()

# Summary of count of recommendations + AGREE median score by continent
# This table is interactive and doens't export well, but cool?
AGREEsort <- reactable(AGREEtab,
                       groupBy = "Continent",
                       columns = list(
                         AGREE1 = colDef(name = "AGREE Domain 1 Scores", 
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         AGREE2 = colDef(name = "AGREE Domain 2 Scores",
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         AGREE3 = colDef(name = "AGREE Domain 3 Scores",
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         AGREE4 = colDef(name = "AGREE Domain 4 Scores",
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         AGREE5 = colDef(name = "AGREE Domain 5 Scores",
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         AGREE6 = colDef(name = "AGREE Domain 6 Scores",
                                         aggregate = "median",
                                         format = colFormat(digits = 2)),
                         DX = colDef(name = 'Diagnosis', aggregate = "sum"),
                         Eligible = colDef(name = 'Eligibilty for Screening',
                                           aggregate = "sum"),
                         Protocol = colDef(name = 'Screening Protocols',
                                           aggregate = "sum"),
                         Report = colDef(name = 'Reporting Findings',
                                         aggregate = "sum"),
                         Incident = colDef(name = 'Management of 
                                           Incidental Findings',
                                           aggregate = "sum"),
                         Nodule = colDef(name = 'Nodule Management',
                                         aggregate = "sum"),
                         Path = colDef(name = 'Pathology', aggregate = "sum"),
                         Mode = colDef(name = 'Screening Modality',
                                       aggregate = "sum"),
                         Program = colDef(name = 'Program Requirements',
                                          aggregate = "sum"),
                         SDM = colDef(name = 'Shared Decision-Making',
                                      aggregate = "sum"),
                         Smoke = colDef(name = 'Smoking Cessation',
                                        aggregate = "sum"),
                         Stage = colDef(name = 'Staging', aggregate = "sum"),
                         Surveil = colDef(name = 'Surveillance', 
                                          aggregate = "sum")),
                       bordered = TRUE,
                       striped = TRUE,
                       style = list(fontFamily = "Work Sans, sans-serif", 
                                    fontSize = "0.875rem"))
AGREEsort

# AGREE scores by category of recommendations
AGREEDX <- AGREEdat %>% 
  filter(DX != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEDX, label = "**AGREE Domain**")

AGREEElig <- AGREEdat %>% 
  filter(Eligible != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEElig, label = "**AGREE Domain**")

AGREEProt <- AGREEdat %>% 
  filter(Protocol != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEProt, label = "**AGREE Domain**")

AGREERep <- AGREEdat %>% 
  filter(Report != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREERep, label = "**AGREE Domain**")

AGREEInc <- AGREEdat %>% 
  filter(Incident != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEInc, label = "**AGREE Domain**")

AGREENod <- AGREEdat %>% 
  filter(Nodule != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREENod, label = "**AGREE Domain**")

AGREEPath <- AGREEdat %>% 
  filter(Path != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEPath, label = "**AGREE Domain**")

AGREEMode <- AGREEdat %>% 
  filter(Mode != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEMode, label = "**AGREE Domain**")

AGREEProg <- AGREEdat %>% 
  filter(Program != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEProg, label = "**AGREE Domain**")

AGREEsdm <- AGREEdat %>% 
  filter(SDM != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREEsdm, label = "**AGREE Domain**")

AGREESmo <- AGREEdat %>% 
  filter(Smoke != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREESmo, label = "**AGREE Domain**")

AGREESta <- AGREEdat %>% 
  filter(Stage != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREESta, label = "**AGREE Domain**")

AGREESur <- AGREEdat %>% 
  filter(Surveil != 0) %>% 
  tbl_summary(include = c(AGREE1,AGREE2,AGREE3,AGREE4,AGREE5,AGREE6),
              type = everything() ~ 'continuous',
              statistic = everything() ~ "{median} ({p25}, {p75})",
              label = list(AGREE1 ~ "Scope & Purpose",
                           AGREE2 ~ "Stakeholder Involvement",
                           AGREE3 ~ "Rigor of Development",
                           AGREE4 ~ "Clarity of Presentation", 
                           AGREE5 ~ "Applicability",
                           AGREE6 ~ "Editorial Independence"))
modify_header(AGREESur, label = "**AGREE Domain**")

# Heat map of AGREE scores by recommendation category
ggplot(AGREEcat, aes(Domain, fct_relevel(Category,rev))) + 
  geom_tile(aes(fill=Score)) +
  labs(x="AGREE II Domain",
       y="Recommendation Category") +
  scale_fill_gradientn(name='Median Score',
                       colors = met.brewer("Tam",n=6),
                       limits=c(0,1),
                       breaks = c(0,.2,.4,.6,.8,1)) +
  scale_x_continuous(n.breaks = 6) +
  theme_minimal()

#### CORRELATION ANALYSIS ####

# System for evidence appraisal (GRADE vs. other)
# Kruskal-Wallis (dependent ~ independent variable)

AGREEKW = subset(AGREEdat, select = c(Certainty,AGREE1,AGREE2,AGREE3,AGREE4,
                                      AGREE5,AGREE6))
AGREEKW <- AGREEKW %>% 
  mutate(CertaintyKW = case_when(Certainty %in% 
                                   c('NR','ADAPTE','SIGN','Oxford','ACC/AHA') 
                                 ~ 'Other',
                                 TRUE ~ 'GRADE'))

kruskal.test(AGREE1~CertaintyKW, data=AGREEKW)
kruskal.test(AGREE2~CertaintyKW, data=AGREEKW)
kruskal.test(AGREE3~CertaintyKW, data=AGREEKW)
kruskal.test(AGREE4~CertaintyKW, data=AGREEKW)
kruskal.test(AGREE5~CertaintyKW, data=AGREEKW)
kruskal.test(AGREE6~CertaintyKW, data=AGREEKW)

# Year of publication (trends over time)
# Kendall's coefficient
# (ties = identical observations in one of the variables)
# Both tests are non-parametric
# 0 = no correlation; closer to 1 = stronger correlation
# positive correlation means that high values of one variable are 
# associated with high values of the other variable

AGREESP = subset(AGREEdat, select = c(Year,AGREE1,AGREE2,AGREE3,AGREE4,
                                      AGREE5,AGREE6))


AGREE1SP <- cor.test(formula = ~ Year + AGREE1,
                     data = AGREESP,
                     method = "kendall")

AGREE2SP <- cor.test(formula = ~ Year + AGREE2,
                     data = AGREESP,
                     method = "kendall")

AGREE3SP <- cor.test(formula = ~ Year + AGREE3,
                     data = AGREESP,
                     method = "kendall")

AGREE4SP <- cor.test(formula = ~ Year + AGREE4,
                     data = AGREESP,
                     method = "kendall")

AGREE5SP <- cor.test(formula = ~ Year + AGREE5,
                     data = AGREESP,
                     method = "kendall")

AGREE6SP <- cor.test(formula = ~ Year + AGREE6,
                     data = AGREESP,
                     method = "kendall")

# Scatter plot of trends over time
# To filter for specific domains, add # in front of all geom_point functions
# except for the relevant domain

dots <- c("Scope & Purpose"="#c969a1",
          "Stakeholder Involvement"="#ee8577",
          "Rigor of Development"="#ffbb44", 
          "Clarity of Presentation"="#859b6c",
          "Applicability"="#62929a",
          "Editorial Independence"="#122451")

AGREEPlot <-
ggplot(AGREEdat, mapping = aes(x = Year)) +
  labs(x = 'Year', y = 'AGREE Domain Score') +
  geom_point(aes(y = AGREE1,color="Scope & Purpose"),size=3) +
  geom_point(aes(y = AGREE2,color="Stakeholder Involvement"),size=3) +
  geom_point(aes(y = AGREE3,color="Rigor of Development"),size=3) +
  geom_point(aes(y = AGREE4,color="Clarity of Presentation"),size=3) +
  geom_point(aes(y = AGREE5,color="Applicability"),size=3) +
  geom_point(aes(y = AGREE6,color="Editorial Independence"),size=3) +
  geom_vline(xintercept = 2011,
             linetype='longdash',size=0.5,color="#41485f") +
  annotate("text", x=2010.7, y=0.95, label="IOM Standards published", 
           size=3.5,fontface = "bold",color="#41485f",angle=90) +
  scale_x_continuous(n.breaks = 16) +
  ylim(0,1) +
  scale_colour_manual(name="AGREE Domains",values=dots) +
  scale_fill_discrete(breaks=c("Scope & Purpose","Stakeholder Involvement",
                               "Rigor of Development","Clarity of Presentation",
                               "Applicability","Editorial Independence")) +
  theme_minimal()

#### CITATIONS ####
version$version.string
citation("readxl")
citation("gtsummary")
citation("tidyverse")
citation("tidyr")
citation("dplyr")
citation("ggplot2")
citation("MetBrewer")
citation("rworldmap")
citation("maps")
citation("rnaturalearth")
citation("rnaturalearthdata")
citation("rmapshaper")
citation("sf")