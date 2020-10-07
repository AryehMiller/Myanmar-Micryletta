---
title: "Micryletta Morphological Analysis"
author: "Aryeh H. Miller"
date: "10/07/2020"
---
  
  
#Load relevant packages
library(viridis)
library(RColorBrewer)
library(ggsci)
library(tidyverse)
library(viridis)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(car)

# Load raw data of all mature individuals
dataraw<- SupplementalTable2_MicrylettaMature_6Oct20_Code

#Mann-Whitney U Test on sexual dimorphism in raw data
wilcox.test(SVL~Sex,data=dataraw)

#Subset and make new dataframes-- males and females for seperate treatments
MatureMales <- subset(dataraw, select=c(1:26), subset=(Sex=="Male"))
MatureFemales <- subset(dataraw, select=c(1:26), subset=(Sex=="Female"))

#Sample sizes
table(MatureFemales$Species)
table(MatureMales$Species)

#Male Summary Statistics
minMales <- aggregate(MatureMales[,9:26], by=list(MatureMales$Species),min)
maxMales <- aggregate(MatureMales[,9:26], by=list(MatureMales$Species),max)
meanMales <- aggregate(MatureMales[,9:26], by=list(MatureMales$Species),mean)
sdMales <- aggregate(MatureMales[,9:26], by=list(MatureMales$Species),sd)
bindedMales <- cbind(minMales,maxMales,meanMales,sdMales)

#Female Summary Statistics
minFemales <- aggregate(MatureFemales[,9:26], by=list(MatureFemales$Species),min)
maxFemales <- aggregate(MatureFemales[,9:26], by=list(MatureFemales$Species),max)
meanFemales <- aggregate(MatureFemales[,9:26], by=list(MatureFemales$Species),mean)
sdFemales <- aggregate(MatureFemales[,9:26], by=list(MatureFemales$Species),sd)
bindedFemales <- cbind(minFemales,maxFemales,meanFemales,sdFemales)

#Log transform data females
TlogFemaleSVL <- log(MatureFemales$SVL)
TlogFemaleHeadL <- log(MatureFemales$HeadL)
TlogFemaleHeadWP <- log(MatureFemales$HeadWP) 
TlogFemaleSnEye <- log(MatureFemales$SnEye) 
TlogFemaleNarEye <- log(MatureFemales$NarEye) 
TlogFemaleEyeD <- log(MatureFemales$EyeD)
TlogFemaleIntNar <- log(MatureFemales$Intnar)
TlogFemaleInterOrb <- log(MatureFemales$Interorb)
TlogFemaleTrunkL <- log(MatureFemales$TrunkL)
TlogFemaleForarmL <- log(MatureFemales$ForarmL)
TlogFemaleHandL <- log(MatureFemales$HandL)
TlogFemaleThreeFingL <- log(MatureFemales$ThreeFingL)
TlogFemaleThighL <- log(MatureFemales$ThighL)
TlogFemaleCrusL <- log(MatureFemales$CrusL)
TlogFemaleTarsL <- log(MatureFemales$TarsL)
TlogFemaleFootL <- log(MatureFemales$FootL)
TlogFemaleHindlL <- log(MatureFemales$HindlL)
TlogFemaleFootL <- log(MatureFemales$FootL)
TlogFemaleFourToeL <- log(MatureFemales$FourToeL)

#Correct for SVL and generate residuals
TlogFemaleHeadLc <- residuals(lm(TlogFemaleHeadL ~ TlogFemaleSVL))
TlogFemaleHeadWPc <- residuals(lm(TlogFemaleHeadWP ~ TlogFemaleSVL))
TlogFemaleSnEyec <- residuals(lm(TlogFemaleSnEye ~ TlogFemaleSVL))
TlogFemaleNarEyec <- residuals(lm(TlogFemaleNarEye ~ TlogFemaleSVL))
TlogFemaleEyeDc <- residuals(lm(TlogFemaleEyeD ~ TlogFemaleSVL))
TlogFemaleInterOrbc <- residuals(lm(TlogFemaleInterOrb ~ TlogFemaleSVL))
TlogFemaleIntNarc <- residuals(lm(TlogFemaleIntNar ~ TlogFemaleSVL))
TlogFemaleTrunkLc <- residuals(lm(TlogFemaleTrunkL ~ TlogFemaleSVL))
TlogFemaleForarmLc <- residuals(lm(TlogFemaleForarmL ~ TlogFemaleSVL))
TlogFemaleHandLc <- residuals(lm(TlogFemaleHandL ~ TlogFemaleSVL))
TlogFemaleThreeFingLc <- residuals(lm(TlogFemaleThreeFingL ~ TlogFemaleSVL))
TlogFemaleThighLc <- residuals(lm(TlogFemaleThighL ~ TlogFemaleSVL))
TlogFemaleCrusLc <- residuals(lm(TlogFemaleCrusL ~ TlogFemaleSVL))
TlogFemaleTarsLc <- residuals(lm(TlogFemaleTarsL ~ TlogFemaleSVL))
TlogFemaleFootLc <- residuals(lm(TlogFemaleFootL ~ TlogFemaleSVL))
TlogFemaleHindlLc <- residuals(lm(TlogFemaleHindlL ~ TlogFemaleSVL))
TlogFemaleFourToeLc <- residuals(lm(TlogFemaleFourToeL ~ TlogFemaleSVL))

#Combine females into new dataframe
datameasurementsFemales = droplevels(subset(MatureFemales, select=c(Museum, Species,Country,Province,	Latitude,	Longitude,Sex,Mature,HeadL,	HeadWP, SnEye,	NarEye,	EyeD,	Interorb,	Intnar,	SVL,	TrunkL,	ForarmL,	HandL,	ThreeFingL,	ThighL,	CrusL,	TarsL,	FootL,	HindlL,	FourToeL)))
TTableFemales = cbind(datameasurementsFemales,TlogFemaleHeadL,TlogFemaleHeadWP,TlogFemaleSnEye,TlogFemaleNarEye, TlogFemaleEyeD, TlogFemaleInterOrb, TlogFemaleIntNar, TlogFemaleSVL, TlogFemaleTrunkL, TlogFemaleForarmL, TlogFemaleHandL, TlogFemaleThreeFingL, TlogFemaleThighL, TlogFemaleCrusL, TlogFemaleTarsL, TlogFemaleFootL, TlogFemaleHindlL, TlogFemaleFourToeL, TlogFemaleHeadLc ,TlogFemaleHeadWPc,TlogFemaleSnEyec,TlogFemaleNarEyec, TlogFemaleEyeDc, TlogFemaleInterOrbc, TlogFemaleIntNarc, TlogFemaleTrunkLc, TlogFemaleForarmLc, TlogFemaleHandLc, TlogFemaleThreeFingLc, TlogFemaleThighLc, TlogFemaleCrusLc, TlogFemaleTarsLc, TlogFemaleFootLc, TlogFemaleHindlLc, TlogFemaleFourToeLc)

#Log transform males
TlogMaleSVL <- log(MatureMales$SVL)
TlogMaleHeadL <- log(MatureMales$HeadL)
TlogMaleHeadWP <- log(MatureMales$HeadWP) 
TlogMaleSnEye <- log(MatureMales$SnEye) 
TlogMaleNarEye <- log(MatureMales$NarEye) 
TlogMaleEyeD <- log(MatureMales$EyeD)
TlogMaleIntNar <- log(MatureMales$Intnar)
TlogMaleInterOrb <- log(MatureMales$Interorb)
TlogMaleTrunkL <- log(MatureMales$TrunkL)
TlogMaleForarmL <- log(MatureMales$ForarmL)
TlogMaleHandL <- log(MatureMales$HandL)
TlogMaleThreeFingL <- log(MatureMales$ThreeFingL)
TlogMaleThighL <- log(MatureMales$ThighL)
TlogMaleCrusL <- log(MatureMales$CrusL)
TlogMaleTarsL <- log(MatureMales$TarsL)
TlogMaleFootL <- log(MatureMales$FootL)
TlogMaleHindlL <- log(MatureMales$HindlL)
TlogMaleFootL <- log(MatureMales$FootL)
TlogMaleFourToeL <- log(MatureMales$FourToeL)

#Correct for SVL and generate residuals
TlogMaleHeadLc <- residuals(lm(TlogMaleHeadL ~ TlogMaleSVL))
TlogMaleHeadWPc <- residuals(lm(TlogMaleHeadWP ~ TlogMaleSVL))
TlogMaleSnEyec <- residuals(lm(TlogMaleSnEye ~ TlogMaleSVL))
TlogMaleNarEyec <- residuals(lm(TlogMaleNarEye ~ TlogMaleSVL))
TlogMaleEyeDc <- residuals(lm(TlogMaleEyeD ~ TlogMaleSVL))
TlogMaleInterOrbc <- residuals(lm(TlogMaleInterOrb ~ TlogMaleSVL))
TlogMaleIntNarc <- residuals(lm(TlogMaleIntNar ~ TlogMaleSVL))
TlogMaleTrunkLc <- residuals(lm(TlogMaleTrunkL ~ TlogMaleSVL))
TlogMaleForarmLc <- residuals(lm(TlogMaleForarmL ~ TlogMaleSVL))
TlogMaleHandLc <- residuals(lm(TlogMaleHandL ~ TlogMaleSVL))
TlogMaleThreeFingLc <- residuals(lm(TlogMaleThreeFingL ~ TlogMaleSVL))
TlogMaleThighLc <- residuals(lm(TlogMaleThighL ~ TlogMaleSVL))
TlogMaleCrusLc <- residuals(lm(TlogMaleCrusL ~ TlogMaleSVL))
TlogMaleTarsLc <- residuals(lm(TlogMaleTarsL ~ TlogMaleSVL))
TlogMaleFootLc <- residuals(lm(TlogMaleFootL ~ TlogMaleSVL))
TlogMaleHindlLc <- residuals(lm(TlogMaleHindlL ~ TlogMaleSVL))
TlogMaleFourToeLc <- residuals(lm(TlogMaleFourToeL ~ TlogMaleSVL))

#Combine males into new data frame
datameasurementsMales = droplevels(subset(MatureMales, select=c(Museum, Species,Country,Province,	Latitude,	Longitude,	Sex,	Mature,HeadL,	HeadWP, SnEye,	NarEye,	EyeD,	Interorb,	Intnar,	SVL,	TrunkL,	ForarmL,	HandL,	ThreeFingL,	ThighL,	CrusL,	TarsL,	FootL,	HindlL,	FourToeL)))
TTableMales = cbind(datameasurementsMales,TlogMaleHeadL,TlogMaleHeadWP,TlogMaleSnEye,TlogMaleNarEye, TlogMaleEyeD, TlogMaleInterOrb, TlogMaleIntNar, TlogMaleSVL, TlogMaleTrunkL, TlogMaleForarmL, TlogMaleHandL, TlogMaleThreeFingL, TlogMaleThighL, TlogMaleCrusL, TlogMaleTarsL, TlogMaleFootL, TlogMaleHindlL, TlogMaleFourToeL,TlogMaleHeadLc ,TlogMaleHeadWPc,TlogMaleSnEyec,TlogMaleNarEyec, TlogMaleEyeDc, TlogMaleInterOrbc, TlogMaleIntNarc, TlogMaleTrunkLc, TlogMaleForarmLc, TlogMaleHandLc, TlogMaleThreeFingLc, TlogMaleThighLc, TlogMaleCrusLc, TlogMaleTarsLc, TlogMaleFootLc, TlogMaleHindlLc, TlogMaleFourToeLc)

#Set up  SVL boxplots
#Change font and italicize 
black.italic.5 <- element_text(face = "italic", color = "black", size = 10)
#Add species labels
SpeciesLabels <- c("M. aishani", "M. inornata", "M. lineata")
#Males
mylabelsMales <- c(expression(paste(italic("M. aishani"), " (n=11)")),
                   expression(paste(italic("M. cf. inornata"), " (n=43)")),
                   expression(paste(italic("M. lineata"), " (n=17)")))
#Females
mylabelsFemales <- c(expression(paste(italic("M. aishani"), " (n=3)")),
                     expression(paste(italic("M. cf. inornata"), " (n=29)")),
                     expression(paste(italic("M. lineata"), " (n=13)")))

#Plot SVL for males and females

#Males
p10<-ggplot(TTableMales, aes(x=Species, y=SVL, fill=Species)) + geom_boxplot() + theme_grey() + theme(legend.position = "none") + scale_fill_brewer(palette="Dark2") + theme(axis.text = black.italic.5)+ 
  labs( y = "SVL (mm)") + scale_x_discrete(labels= mylabelsMales)+labs(x=NULL)

#Females
BoxplotFemales <- TTableFemales[-c(33),] #Remove M. inornata sensu stricto from the dataset for this boxplot
p11<-ggplot(BoxplotFemales, aes(x=Species, y=SVL, fill=Species)) + geom_boxplot() + theme_grey() + theme(legend.position = "none") + scale_fill_brewer(palette="Dark2") + theme(axis.text = black.italic.5)+ 
  labs( y = "SVL (mm)") + scale_x_discrete(labels= mylabelsFemales)+labs(x=NULL)

#Principal Components Analysis 

#PCA Males
data = TTableMales
species <- data[,2] #Isolate species
data3 <- cbind(species, TTableMales[45:61]) #Dataframe with OTU residual data and species identities
pca <- prcomp(data3[,2:ncol(data3)], scale=T) #Perform PCA
scores <- data.frame(species, pca$x[,1:2]) #Dataframe for PC1 and PC2
summary(pca)

#Assemble single table
loadings <- pca$rotation
temp <- summary(pca) 
sum <- temp$importance 
eigen <- pca$sdev^2 
sumM <- rbind(sum, eigen, loadings) #Combine into single table
View(sumM) #View table

#Extract PC1 and PC2 scores for plot
PC1 = scores$PC1
PC2 = scores$PC2

# Males PCA plot
pmainM <- ggplot(scores, aes(x = PC1, y = PC2, color = species))+
  geom_point(aes(shape=species), size=4) + scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+ theme_grey()+
  theme(legend.position="none")+labs(x = "PC1 (26.8%)", y = "PC2 (17.6%)")

#Females PCA
dataF = TTableFemales
speciesF <- dataF[,2] #Isolate species
data4 <- cbind(speciesF, TTableFemales[45:61]) #Dataframe with OTU residual data and species identities
pcaF <- prcomp(data4[,2:ncol(data4)], scale=T) #Perform PCA
scoresF <- data.frame(speciesF, pcaF$x[,1:2]) #Dataframe for PC1 and PC2

#Extract PC1 and PC2 scores for plot
PC1F = scoresF$PC1
PC2F = scoresF$PC2

#Assemble single table
loadingsF <- pcaF$rotation
tempF <- summary(pcaF) 
sumF <- tempF$importance
eigenF <- pcaF$sdev^2
sumF <- rbind(sumF, eigenF, loadingsF) #Combine into single table
View(sumF) #View table

# Main plot
pmainF <- ggplot(scoresF, aes(x = PC1F, y = PC2F, color = speciesF))+
  geom_point(aes(shape=speciesF), size=4) + scale_color_manual(values = c("#1B9E77", "#D95F02","skyblue1", "#7570B3"))+ theme_grey()+labs(x = "PC1 (31.4%)", y = "PC2 (13.6%)")+
  theme(legend.position="none")+ scale_shape_manual(values = c(19, 17, 17,15))

#Plot the boxplots and PCAs together
plot_grid(p10, p11, pmainM, pmainF, labels = "AUTO")
