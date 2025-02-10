## Data analysis

### 1.Differential expression analysis
library(limma)

df_1 <- equalMedianNormalization(df)

df_1 %>%
  mutate(Accession = rownames(df_1)) %>%
  melt("Accession") %>%
  ggplot(aes(variable, value)) +
  geom_boxplot()

workmatrix <- model.matrix(~0+factor(c(1,1,1,2,2,2)))
colnames(workmatrix) <- c("Wet","Dry")
fit1 <- lmFit(df_1,workmatrix)
contrast.matrix <- makeContrasts(Wet-Dry,levels = workmatrix)
fit2 <- contrasts.fit(fit1,contrast.matrix)
fit2 <- eBayes(fit2)
df_DE <- topTable(fit2,number=Inf,adjust="BH")
df_DE <- df_DE %>%
  mutate(Accession = rownames(df_DE)) %>%
  arrange(desc(logFC))

df_DE$"Sig" <- "N.S."
df_DE[df_DE$logFC > 0.58 & df_DE$P.Value < 0.05, "Sig"] <- "Wet"
df_DE[df_DE$logFC < -0.58 & df_DE$P.Value < 0.05, "Sig"] <- "Dry"


### 2. Co-regulatory analysis
library(Hmisc)
coexp.df <- rcor(t(df), type = "pearson")

coexp.df.p <- coexp_gene$P %>%
  melt() %>%
  rename("Pvalue" = "value")

coexp.df.r <- coexp_gene$r %>%
  melt() %>%
  rename("Rho" = "value") %>%
  mutate(Pvalue = coexp.df.p$Pvalue)

coexp.data <- coexp.df.r %>%
  select(V1, V2, Rho) %>%
  spread(key = Accession_2, value = Rho, fill = 0)

fviz_nbclust(coexp.data, FUNcluster = hcut, method = "wss", k.max = 30)

coexp.data_hc <- hclust(dist(coexp.data, method = "euclidean"), method = "complete" )

coexp.data_cl <- coexp.data_hc %>%
  cutree(30)


### 3. Enrichment analysis
library(clusterProfiler)

GO <- enricher(
  gene = ptn_list,
  universe = ptn_bg,
  pvalueCutoff = 0.05,
  pAdjustMethod = "BH",
  TERM2GENE = term2gene, 
  TERM2NAME = term2name)

GO.df <- GO@result





