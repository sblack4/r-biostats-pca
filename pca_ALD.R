setwd("/Users/Lauren/Desktop/")
ALD <- read.csv("Ordered Team4-ALD_4cohort_dataset.csv")
ALD
control <- ALD[1:25, 3:152]
control
options(max.print = 99999)
control
control_AC <- ALD[26:45, 3:152]
options(max.print = 99999)
control_AC
mild_AH <- ALD[46:55, 3:152]
options(max.print = 99999)
mild_AH
severe_AH <- ALD[56:78, 3:152]
options(max.print = 99999)
severe_AH
control_matrix <- as.matrix(control)
control_matrix
t_control <- t(control_matrix)
control_AC_matrix <- as.matrix(control_AC)
control_AC_matrix
t_control_AC <- t(control_AC_matrix)
mild_AH_matrix <- as.matrix(mild_AH)
mild_AH_matrix
t_mild_AH <- t(mild_AH_matrix)
severe_AH_matrix <- as.matrix(severe_AH)
severe_AH_matrix
t_severe_AH <- t(severe_AH_matrix)
c_cAC_mAH_sAH <- cbind(t_control, t_control_AC, t_mild_AH, t_severe_AH)
colnames(c_cAC_mAH_sAH) <- c(replicate(25, "control"), replicate(20, "control_AC"), replicate(10, "mild_AH"), replicate(23, "severe_AH"))
c_cAC_mAH_sAH
cov_all <- cov(c_cAC_mAH_sAH)
cov_all
det(cov_all)
# determinant is 0 which means the cohorts are linearly dependent on each other. This means that they can be defined as linear combinations of the others
ALDpca <- prcomp(c_cAC_mAH_sAH, scale=TRUE)
summary(ALDpca)
(eigen_ALD <- ALDpca$sdev^2)
names(eigen_ALD) <- c(replicate(25, "control"), replicate(20, "control_AC"), replicate(10, "mild_AH"), replicate(23, "severe_AH"))
eigen_ALD
install.packages('factoextra')
library("factoextra")
fviz_pca_biplot(ALDpca, row.ind = c(1:78))
