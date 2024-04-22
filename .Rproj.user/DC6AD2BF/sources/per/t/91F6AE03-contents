################################################################
# Title: Otolith shape analyses (PCA, CVA, disparity)
# Author: Lydia Walton
# Last updated: 19-March-2024
################################################################


dev.off() #Shut down open graphics devices
rm(list = ls(all=T)) #Clear environment

#Setting your working directory
library(here) 
#Data manipulation and analysis
library(geomorph)
library(dplyr)
library(dispRity)
library(Morpho)
#Plotting and visualization
library(rgl)
library(ggplot2)
library(ggfortify)
library(viridis)


#Import the tps file with the landmark data
myct_landmarks <- readland.tps("Data/myct_landmarks.txt", specID = "ID", readcurves = FALSE, warnmsg = TRUE)

#Import the classifier data
classifier <- read.csv("Data/Myct_classifiers_SUBSET.csv", header=T)
as.factor(classifier$Species) #Change species to a factor
#remove specimens that do not meet inclusion criteria
classifier <- classifier %>% 
  filter(Include == "Yes")

## Code for this analysis adapted from Maucieri et al. 2021

#----------------- Procrustes Analysis

myct_gpa <- gpagen(myct_landmarks, curves = NULL, surfaces = NULL, PrinAxes = TRUE,
                   max.iter = NULL, ProcD = TRUE, Proj = TRUE, print.progress = TRUE) #GPA code

#Procrustes
myct.crusties <- gpagen(myct_gpa$coords, Proj = TRUE)
plot(myct.crusties)

centroids <- myct.crusties$Csize

crusties.coords <- myct.crusties$coords
x <- two.d.array(crusties.coords)

#Examining effect of allometry 

#Make a gdf for use in the static allometry model
gdf.allo <- geomorph.data.frame(coords=crusties.coords, Csize=centroids)

#Allometry
reg.res <- procD.lm(coords~Csize, data = gdf.allo, RRPP = TRUE, logsz = TRUE, iter = 999)
plot(reg.res, type = "regression", predictor = log(centroids))

#Check results of static allometric regression
summary(reg.res)  #Csize p-value = 0.001 **
par(mfrow=c(2,2))
plot(reg.res)
par(mfrow=c(1,1))

#Extract residual shape coordinates because there is a significant result
corrected_coords_myct <- reg.res$GM$residuals + replicate(length(centroids), mshape(crusties.coords))

#Plot and rename coords
coords_myct<-corrected_coords_myct
plotAllSpecimens(corrected_coords_myct)

#to save procrustes figure (not included in MS)
#tiff("Figures/Procrustes.tiff", width=12, height = 10, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
# plotAllSpecimens(corrected_coords_myct)
# dev.off() 

#Check specimens
mshape_myct<-mshape(corrected_coords_myct)

#MPlot all specimens
##par(mfrow=c(3,3))
##for (i in 1:205){
##  plotRefToTarget(mshape_myct,corrected_coords_myct[,,i],mag=1.5,method="TPS")
##  mtext(as.character(i),side=3)
##}
##dev.off()


#---------------Principal Coordinate Analysis (PCA)

#Reformat two-d-array into 3D array like uncorrected coordinates
twoD_coords_myct<- two.d.array(coords_myct)

#Remove specimens that do not meet inclusion criteria (not barcode compliant; less than 3 specimens per species)
twoD.sub <- twoD_coords_myct[-c(52, 68, 79, 94, 95, 96, 101, 133, 135, 136, 137, 138, 139, 144, 146, 147, 148, 149), ]

myct.pca<-prcomp(twoD.sub)
summary(myct.pca)

#scree plot of varation
barplot(myct.pca$sdev) #right-skewed

#Corrected
coords_myct_wSpecies <- cbind(twoD.sub, classifier)
Species.df <- as.data.frame(coords_myct_wSpecies)

#Plot for removed centroid correction

unique(Species.df$Species)

#Sample size
sample.size.table <- Species.df %>% 
  group_by(Species) %>% 
  summarise("Sample size" = length(ID))

sample.size.table

#Plot
Figure_PCA <- autoplot(myct.pca,data=Species.df,colour="Species", shape = "Species", frame = TRUE)+
  geom_point(aes(colour=Species.df$Species, shape = Species.df$Species),size=2) +
  scale_shape_manual(values=c(15,16,17,18,20,6,8,0,1,13,9,12,10,5), 
                     breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis" )) +
  
  scale_color_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, 
                      breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                 "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                 "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                 "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  scale_fill_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, 
                     breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +

  scale_shape_manual(values=c(15,16,17,18,20,6,8,0,1,13,9,12,10,5), breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                               "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                               "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                               "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis" )) +
  
  scale_color_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                               "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                               "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                               "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  scale_fill_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                              "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                              "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                              "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  theme_classic()+
  theme(legend.title = element_text(size=16, face="bold"), axis.title = element_text(size=16, face = "bold"), legend.text = element_text(face="italic"))

Figure_PCA

################################
# Export Figure 3
################################

#Save figure 
 #tiff("Figures/Fig.3_PCA.tiff", width=9, height = 6, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
 #Figure_PCA
 #dev.off()

#--------------------------------------------------------------------------------------------------------------------------------------------

#Warp grid generation
gm_pca_myct <-gm.prcomp(coords_myct)
par(mfrow = c(2,2))
plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp1$min, mag=-0.5,method="TPS")
plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp1$max, mag=-0.5,method="TPS")
plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp2$min, mag=-0.5,method="TPS")
plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp2$max, mag=-0.5,method="TPS")
dev.off()

#Save the warp grids which were added to Figure 3

#tiff("Figures/PC1_min.tiff", width=4, height = 4, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
#plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp1$min, mag=-0.5,method="TPS")
#dev.off()
 
#tiff("Figures/PC1_max.tiff", width=4, height = 4, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
#plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp1$max, mag=-0.5,method="TPS")
#dev.off()
 
#tiff("Figures/PC2_min.tiff", width=4, height = 4, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
#plotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp2$min, mag=-0.5,method="TPS")
#dev.off()
 
#tiff("Figures/PC2_max.tiff", width=4, height = 4, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
#lotRefToTarget(mshape_myct, gm_pca_myct$shapes$shapes.comp2$max, mag=-0.5,method="TPS")
#dev.off()

#Save PCA results to .RDATA file
 #save(myct.pca, file = "Data/PCA.RData")


#--------------Disparity Analysis
#Find PC scores
Raw_PCSCORES <- as.data.frame(myct.pca$x)
PCSCORES <- as.data.frame(matrix(data = NA, ncol = 4, nrow = length(Raw_PCSCORES$PC1)))
colnames(PCSCORES) <- c("ID", "PC1", "PC2", "Species")
PCSCORES$ID <- rownames(Raw_PCSCORES)
PCSCORES$PC1 <- Raw_PCSCORES$PC1
PCSCORES$PC2 <- Raw_PCSCORES$PC2
PCSCORES$Species <- Species.df$Species

#Save PC scores to .RDATA file
# save(PCSCORES, file = "Data/PCSCORES.RData")

#PC 1 --> 6 explains 95% of the morphological variation
summary(myct.pca)

Multivariate_Analysis <- geomorph.data.frame(fine = myct.pca$x[,c(1:5)], species = PCSCORES$Species)

#Procrustes ANOVA shows species p = 0.001 
Procrustes_ANOVA <- procD.lm ((fine ~ species), RRPP=TRUE,iter=999, data = Multivariate_Analysis)
summary(Procrustes_ANOVA)

disparity_data<-cbind(myct.pca$x[,c(1:6)], species = PCSCORES$Species)
disparity_data_df <- as.data.frame(disparity_data)
cols.num <- c(1:6)
disparity_data_df[cols.num] <- sapply(disparity_data_df[cols.num],as.numeric)

Species_list<-list(
  A.risso=which(disparity_data_df$species == "Arctozenus risso"),
  B.pac=which(disparity_data_df$species == "Bathylagus pacificus"),
  C.mac=which(disparity_data_df$species == "Chauliodus macouni"),
  D.theta=which(disparity_data_df$species == "Diaphus theta"),
  L.reg=which(disparity_data_df$species == "Lampanyctus regalis"),
  L.ritt=which(disparity_data_df$species == "Lampanyctus ritteri"),
  L.mand=which(disparity_data_df$species == "Lycodapus mandibularis"),
  N.can=which(disparity_data_df$species == "Nansenia candida"),
  S.leu=which(disparity_data_df$species == "Stenobrachius leucopsarus"),
  S.cal=which(disparity_data_df$species == "Symbolophorus californiensis"),
  T.mac=which(disparity_data_df$species == "Tactostoma macropus"),
  T.cren=which(disparity_data_df$species == "Tarletonbeania crenularis")
)

#Run  disparity analyses
#NOTE - When running the disparity analysis you will get differing values as the dispRity.per.group() function uses bootstrapping
# which is a randomized function. We have set our seed so you can get the same results we did in our study. 
set.seed(589)
disparity_myct <- dispRity.per.group(disparity_data_df[,c(1:6)],Species_list,metric = pairwise.dist)
summary(disparity_myct)
plot(disparity_myct)

#Run disparity t-tests w bonferroni correction
myct_post_HOC<-test.dispRity(disparity_myct, test = t.test, comparisons = "pairwise",
                             concatenate = FALSE, correction = "bonferroni")
myct_post_HOC[[3]]


#-----------------Canonical Variate Analysis (CVA)
#CVA
myct_cva <- CVA(myct.pca$x[,c(1:6)], Species.df$Species, plot = TRUE)
myct_cva 

myct_CVAscores <- as.data.frame(myct_cva$CVscores)

CVAscores_myct <- as.data.frame(matrix(data = NA, ncol = 4, nrow = length(Species.df$Species)))
colnames(CVAscores_myct) <- c("ID", "CV1", "CV2", "Species")
CVAscores_myct$ID <- PCSCORES$ID
CVAscores_myct$CV1 <- myct_CVAscores$`CV 1`
CVAscores_myct$CV2 <- myct_CVAscores$`CV 2`
CVAscores_myct$Species <- Species.df$Species

CVA_hull <- CVAscores_myct %>% group_by(Species) %>%  slice(chull(CV1, CV2))
CVA_hull$Species <- factor(CVA_hull$Species, 
                           levels = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                      "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                      "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                      "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis"))
CVAscores_myct$Species <- factor(CVAscores_myct$Species, 
                                 levels = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                            "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                            "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                            "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis"))

CVA_hull$Species <- factor(CVA_hull$Species, levels = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                        "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                        "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                        "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis"))
CVAscores_myct$Species <- factor(CVAscores_myct$Species, levels = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                    "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                    "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                    "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis"))


#turn it into a plot

myct_cva_plot <- ggplot(CVAscores_myct, aes(x = CV1, y = CV2)) + labs(x="CV1", y="CV2", title="", colour="Species", size=2)+
  geom_point(aes(shape=Species,colour=Species),size=3)+ scale_shape_manual(values=c(15,16,17,18,20,6,8,0,1,13,9,12,10), breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni","Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", "Lycodapus mandibularis", "Nansenia candida", "Stenobrachius leucopsarus", "Stenobrachius nannochir", "Symbolophorus californiensis","Tactostoma macropus", "Tarletonbeania crenularis" )) +

  scale_color_viridis(option = "turbo", 
                      alpha = 0.7, 
                      discrete=TRUE, 
                      breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                 "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                 "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                 "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  scale_fill_viridis(option = "turbo", 
                     alpha = 0.7, 
                     discrete=TRUE, 
                     breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +

  scale_color_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                               "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                               "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                               "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  scale_fill_viridis(option = "turbo", alpha = 0.7, discrete=TRUE, breaks = c("Arctozenus risso","Bathylagus pacificus","Chauliodus macouni",
                                                                              "Diaphus theta","Lampanyctus regalis", "Lampanyctus ritteri", 
                                                                              "Lycodapus mandibularis", "Nansenia candida","Stenobrachius leucopsarus", 
                                                                              "Symbolophorus californiensis", "Tactostoma macropus", "Tarletonbeania crenularis")) +
  
  aes(fill = factor(Species), color = factor(Species)) + 
  geom_polygon(data = CVA_hull, alpha = 0.2) +
  guides(shape = FALSE, fill = FALSE, colour = guide_legend(override.aes = list(shape = c(15,16,17,18,20,6,8,0,1,13,9,12)))) + theme_classic()+ 
  theme(legend.title = element_text(size=16, face="bold"), axis.title = element_text(size=16, face = "bold"), legend.text = element_text(face="italic"))+
  labs(color="Species")

myct_cva_plot

###########################
# Export Figure 4
###########################

 #tiff("Figures/Fig.4_CVA_with hull outlines.tiff", width=9, height = 6, units = "in", pointsize = "14", compression = "none", type = "cairo", res = 300)
 #myct_cva_plot
 #dev.off()


