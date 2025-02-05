# Instalar paquetes si no están disponibles
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("GEOquery", "org.Hs.eg.db", "clusterProfiler", "enrichplot"))

# Cargar librerías
library(GEOquery)
library(org.Hs.eg.db)
library(clusterProfiler)
library(enrichplot)

# Descargar los datos de GEO
gds <- getGEO("GDS6063", GSEMatrix = TRUE)
gds
str (gds)

# Ver los nombres de las ranuras (slots) en el objeto GDS
names(gds)

# Acceder a los datos de expresión
expression_data <- gds@dataTable@table

# Ver las primeras filas para explorar los datos
head(expression_data)

# Si no tienes GEOquery instalado, instálalo
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("GEOquery")

# Cargar la librería
library(GEOquery)

# Filtrar columnas relevantes
genes <- expression_data$ID_REF
expression_values <- expression_data[, 3:ncol(expression_data)]  # Las columnas de expresión, excluyendo las primeras dos

# Ver las primeras filas para comprobar
head(expression_values)

# Instalar clusterProfiler si no lo tienes
# Instalar Bioconductor para R 4.4
install.packages("BiocManager")
BiocManager::install(version = "3.20")
# Instalar clusterProfiler
BiocManager::install("clusterProfiler")



