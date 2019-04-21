setwd("/Users/Lauren/Desktop/")
# install.packages('factoextra')
library("factoextra")

ALD <- read.csv("Ordered Team4-ALD_4cohort_dataset.csv")
ALD
control <- ALD[1:25, 3:152]
control
control_avg <- colMeans(control)
options(max.print = 99999)
control_AC <- ALD[26:45, 3:152]
options(max.print = 99999)
control_AC_avg <- colMeans(control_AC)
mild_AH <- ALD[46:55, 3:152]
options(max.print = 99999)
mild_AH_avg <- colMeans(mild_AH)
severe_AH <- ALD[56:78, 3:152]
options(max.print = 99999)
severe_AH_avg <- colMeans(severe_AH)

all <- rbind(control_avg, control_AC_avg, mild_AH_avg, severe_AH_avg)
colnames(all)

all_pca <- prcomp(all, scale=TRUE)
summary(all_pca)
fviz_pca_biplot(all_pca, label = "none",
                title = "Bacterial Species in Four Cohorts ALD",
                habillage =  c("control_avg", "control_AC_avg", "mild_AH_avg", "severe_AH_avg"),
                addElipses = TRUE, elipse.level=1,
                ggtheme=theme_minimal())

fviz_pca_var(all_pca,                 
             title = "Bacterial Species in Four Cohorts ALD",
             col.ind = colnames(all), 
             label="none", addElipse=TRUE)
fviz_pca_ind(all_pca,                 
             title = "Bacterial Species in Four Cohorts ALD",
             col.ind = c("control_avg", "control_AC_avg", "mild_AH_avg", "severe_AH_avg"), 
             label="none", addElipse=TRUE)

t_all <- t(all)
cov_all <- cov(t_all)
cov_all
det(cov_all)
# determinant is 0 which means the cohorts are linearly dependent on each other. This means that they can be defined as linear combinations of the others
ALDpca <- prcomp(t_all, scale=TRUE)
summary(ALDpca)
(eigen_ALD <- ALDpca$sdev^2)
fviz_pca_biplot(ALDpca, 
                title = "Bacterial Species foo",
                pallette = "jco")

fviz_pca_biplot(ALDpca, label = "var",
                title = "Bacterial Species in Four Cohorts ALD",
                addElipses = TRUE, elipse.level=1,
                ggtheme=theme_minimal())

fviz_pca_var(ALDpca,                 
             title = "Bacterial Species in Four Cohorts ALD",
             col.ind = colnames(all), 
             label="none", addElipse=TRUE)
fviz_pca_ind(ALDpca,                 
             title = "Bacterial Species in Four Cohorts ALD",
             label="none", addElipse=TRUE)
