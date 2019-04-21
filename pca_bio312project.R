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
ALD_matrix <- as.matrix(read.csv("Ordered Team4-ALD_4cohort_dataset.csv"))
ALD_matrix
cov_ALD_matrix <- cov(ALD_matrix)
cohort_vs_species <- read.csv("cohorts_species.csv")
dist.cohort_vs_species <- dist(cohort_vs_species, method="euclidean")
dist.cohort_vs_species
dist.control_AC <- dist(control_AC, method="euclidean")
dist.control_AC
dist.mild_AH <- dist(mild_AH, method="euclidean")
dist.mild_AH
dist.severe_AH <- dist(severe_AH, method="euclidean")
dist.severe_AH
dist.control <- dist(control, method="euclidean")
dist.control
clustcontrol_AC <- hclust(dist.control_AC, method="single")
plot(as.dendrogram(clustcontrol_AC), horiz = TRUE, xlab="Distance between Species for control_AC", main="Dendrogram based on control_AC cohort of 152 bacteria samples")
clustcontrol <- hclust(dist.control, method="single")
plot(as.dendrogram(clustcontrol), horiz = TRUE, xlab="Distance between Species for control", main="Dendrogram based on control cohort of 152 bacteria samples")
clustmild_AH <- hclust(dist.mild_AH, method="single")
plot(as.dendrogram(clustmild_AH), horiz = TRUE, xlab="Distance between Species for mild_AH", main="Dendrogram based on mild_AH cohort of 152 bacteria samples")
dist.severe_AH <- dist(severe_AH, method="euclidean")
dist.severe_AH
clustsevere_AH <- hclust(dist.severe_AH, method="single")
plot(as.dendrogram(clustsevere_AH), horiz = TRUE, xlab="Distance between Species for severe_AH", main="Dendrogram based on severe_AH cohort of 152 bacteria samples")

