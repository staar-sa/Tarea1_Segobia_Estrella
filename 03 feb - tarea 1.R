# Verificar e instalar paquetes necesarios
to_install <- c("GEOquery", "limma", "umap", "Biobase", "maptools")
new_packages <- to_install[!(to_install %in% installed.packages()[,"Package"])]
if(length(new_packages)) BiocManager::install(new_packages)

# Cargar librerías
library(GEOquery)
library(limma)
library(umap)
library(Biobase)
library(maptools)

# Cargar datos de GEO
gset <- getGEO("GSE68849", GSEMatrix = TRUE, AnnotGPL = TRUE)
if (length(gset) > 1) idx <- grep("GPL10558", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# Ajustar nombres de columnas
fvarLabels(gset) <- make.names(fvarLabels(gset))

# Definir grupos
gsms <- "1010101010"
sml <- strsplit(gsms, split = "")[[1]]

gs <- factor(sml)
groups <- c("GRIPE_A", "SIN_CONTROL")
levels(gs) <- groups
gset$group <- gs

design <- model.matrix(~group + 0, gset)
colnames(design) <- levels(gs)

# Transformación log2
ex <- exprs(gset)
if (max(ex, na.rm = TRUE) > 100) {
  ex[ex <= 0] <- NA
  exprs(gset) <- log2(ex)
}

gset <- gset[complete.cases(exprs(gset)), ]

# Análisis con limma
fit <- lmFit(gset, design)
cont.matrix <- makeContrasts(contrasts = "GRIPE_A-SIN_CONTROL", levels = design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)

tT <- topTable(fit2, adjust = "fdr", sort.by = "B", number = 250)
write.table(tT, file = "resultado.tsv", row.names = FALSE, sep = "\t")

# Visualización
hist(tT$adj.P.Val, col = "grey", border = "white", xlab = "P-adj", ylab = "Genes", main = "Distribución de P-adj")
dT <- decideTests(fit2, adjust.method = "fdr", p.value = 0.05, lfc = 0)
vennDiagram(dT)
plotMD(fit2, column = 1, status = dT[,1], legend = FALSE, pch = 20)
abline(h = 0)

# UMAP
ex <- exprs(gset)
ump <- umap(t(ex), n_neighbors = 5, random_state = 123)
plot(ump$layout, main = "UMAP", xlab = "", ylab = "", col = gs, pch = 20)

mi_volcano_plot <- volcanoplot(fit2, coef = 1, main = "Volcano Plot", pch = 20, highlight = length(which(dT[,1] != 0)), names = rep("+", nrow(fit2)))
mi_volcano_plot


# Número de genes diferencialmente expresados
summary(dT)

# Visualización de histogramas de p-values
hist(tT$adj.P.Val, col = "grey", border = "white", xlab = "P-adj", ylab = "Genes", main = "Distribución de P-adj")

# Venn diagram para comparar métodos
vennDiagram(dT)

# MD plot
plotMD(fit2, column = 1, status = dT[,1], legend = FALSE, pch = 20)
abline(h = 0)

# UMAP
ex <- exprs(gset)
ump <- umap(t(ex), n_neighbors = 5, random_state = 123)
plot(ump$layout, main = "UMAP", xlab = "", ylab = "", col = gs, pch = 20)

# Incluir referencia del artículo original en el análisis
title("Referencia: Estudio original GSE68849")