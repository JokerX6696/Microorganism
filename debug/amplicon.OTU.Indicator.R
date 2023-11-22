#!/usr/bin/env Rscript
setwd('D:/desk/github/Microorganism/debug')
rm(list = ls())

args <- commandArgs(trailingOnly = T)

if (0) {
    cat(
        "============== Indicator ===========
	Usage:
	Rscript amplicon.OTU.Indicator.r otufile groupfile type Threshold N outputpath
		parameters ->
			  otufile: [file -> always otu.tax.0.03.xls];
			groupfile: [file -> always sample.groups with header and must have #SampleID and Group columns];
			     Type: [character -> P or FDR, always P];
			Threshold: [number -> significance, always 0.05];
			        N: [number -> N most abundant taxa to plot bubble, always 100];
		       outputpath: [path -> path for output]; \n"
    )
    options("show.error.messages" = F)
    stop()
}

# https://github.com/tidyverse/ggplot2
if (packageVersion("ggplot2") != "2.2.0") {
    # devtools::install_github("tidyverse/ggplot2")
}

library(labdsv)
library(ggplot2)
library(gtools)

otufile <- 'indicator.xls'
groupfile <- 'mapping.txt'
Type <- 'P'
Threshold <- 0.05
N <- 100
outputpath <- './'

# otufile = normalizePath("D:/Softwares/group4/otu.tax.0.03.xls")
# groupfile = normalizePath("D:/Softwares/group4/sample.groups")
# Type = "P"
# Threshold = 0.05
# N = 100
# outputpath = normalizePath("D:/Softwares/group4")

setwd(outputpath)

# ====================== prepare data =======================
SampleInfo <- read.table(groupfile, header = T, sep = "\t", comment.char = "", na.strings = "")
SampleInfo <- SampleInfo[, c("X.SampleID", "Group")]
colnames(SampleInfo) <- c("Sample", "Group")
rownames(SampleInfo) <- SampleInfo[, 1]
Group <- as.character(SampleInfo[, 2])

OTUFile <- read.table(otufile, header = T, sep = "\t", row.names = 1, check.name = F, comment.char = "", quote = "")
otu_data <- OTUFile[, rownames(SampleInfo)]

otu_data <- otu_data[rowSums(otu_data) != 0, ]

loc <- grep("All", rownames(otu_data))
if (length(loc) > 0) otu_data <- otu_data[-loc, ]

# decreasing sort by OTU Abundance
A <- apply(otu_data, 1, function(x) sum(x))
otu_data <- otu_data[order(A / sum(A), decreasing = T), ]

set.seed(0)
# indval
indval.out <- indval(t(otu_data), Group, numitr = 999, digits = 3)

indval <- indval.out$indval # the indicator value for each species
pval <- indval.out$pval # significance of identified as indicator species

fdr <- p.adjust(pval, method = "BH")
if (toupper(Type) == "P") {
    sig <- names(pval)[which(pval < Threshold)]

    if (length(sig) < 2) {
        cat("ERROR: We can not get significance result with pvalue and Threshold you give, so you can modify Threshold!\n")
        q()
    }
} else {
    sig <- names(fdr)[which(fdr < Threshold)]

    if (length(sig) < 2) {
        cat("ERROR: We can not get significance result with FDR and Threshold you give, so you can turn to use P or modify Threshold!\n")
        q()
    }
}

indicator <- cbind(indval[sig, ], pval[sig], fdr[sig])
colnames(indicator)[ncol(indicator) - 1] <- "p-value"
colnames(indicator)[ncol(indicator)] <- "FDR"

# plot Abundance top 50
if (length(sig) > as.numeric(N)) {
    sig <- sig[1:as.numeric(N)]
} else {
      sig <- sig
  }

plot_relabun <- (A / sum(A))[sig] * 100
plot_indval <- indval[sig, ]

indicator <- cbind(rownames(indicator), indicator)
colnames(indicator)[1] <- "Species"

write.table(indicator, "indicators_indicator_value.xls", sep = "\t", row.names = F, quote = F)

# Add annotation
plot_anno1 <- OTUFile[sig, c("phylum", "genus")]

###########################
##### yuchuan.liu 2020.11.30
x <- plot_anno1
x <- as.matrix(x)
x[which(x[, 1] == "")] <- "Bacteria"
x[which(x == "")] <- "unclassified_genus"
plot_anno <- as.data.frame(x)
### yuchuan.liu 2020.11.30
###########################
# sort
order <- order(plot_anno$genus, decreasing = T)
plot_anno <- plot_anno[order, ]
plot_indva <- plot_indval[order, ]
plot_relabun <- plot_relabun[order]

# prepare data
data <- stack(as.data.frame(plot_indva))
data$ylabel <- as.factor(rep(rownames(plot_indva), times = ncol(plot_indva)))
data$groups <- rep(unique(as.character(SampleInfo[, 2])), each = nrow(plot_indva))
colnames(data) <- c("Indicator.values", "xlabel", "ylabel", "Groups")
data$Groups <- factor(data$Groups, levels = unique(as.character(SampleInfo[, 2])))

# sort
# data$xlabel = factor(data$xlabel, levels = mixedsort(levels(data$xlabel)))
data$ylabel <- factor(data$ylabel, levels = rownames(plot_indva))

data$xlabel <- factor(data$xlabel, levels = levels(data$Groups))

nm <- 0.09 * apply(plot_anno, 2, function(p) max(nchar(p)))
ng <- ifelse(0.8 * ncol(plot_indva) > 5, 0.8 * ncol(plot_indva), 5) # groups num

width <- sum(nm) + 3 + ng
height <- 0.2 * nrow(plot_indva)

# plot
# mycol = c(119,132,147,454,89,404,123,529,463,461,128,139,552,28,54,84,100,258,558,376,43,652,165,31,610,477,256,588,99,632,81,503,104,562,76,96,495,598,645,507,657,33,179,107,62)
# mycol = colors()[mycol[1:length(unique(data$Groups))]]

# # 31colors_excel
# mycol <- c("#0B5FD1","#C83406","#008039","#226ED4","#CC4925","#378E4C","#397DD8","#D05F45","#6F9C5F","#508CDB","#D47564","#A6AA72","#679BDF","#D88B84","#DEB885","#7FAAE2","#DB9E9D","#FAC397","#96B9E6","#DDAEB1","#FBCDA7","#ADC8E9","#DFBEC4","#FBD6B8","#C4D7ED","#E1CED8","#FCDFC8","#DCE6F1","#E4DFEC","#FDE9D9","#3466A8")
# singler
mycol <- c(
    "#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17",
    "#666666", "#1B9E77", "#7570B3", "#66A61E", "#E6AB02", "#A6761D",
    "#A6CEE3", "#B2DF8A", "#FB9A99", "#E31A1C", "#FF7F00", "#6A3D9A",
    "#8DA0CB", "#4DAF4A", "#984EA3", "#c6c386", "#999999", "#66C2A5",
    "#FC8D62", "#A6D854", "#FFD92F", "#BEBADA", "#FB8072", "#80B1D3",
    "#FDB462", "#BC80BD", "#B3B3B3", "#33A02C", "#B3DE69", "#4038b0",
    "#ee7576", "#e94749", "#E78AC3", "#ff0000", "#A65628", "#d80172",
    "#F781BF", "#D95F02", "#E7298A", "#1F78B4", "#FDBF6F", "#CAB2D6",
    "#B15928", "#FBB4AE", "#B3CDE3", "#0173b2", "#de8f05", "#029e73",
    "#d55e00", "#cc78bc", "#ca9161", "#fbafe4", "#949494", "#ece133",
    "#56b4e9", "#00AFBB", "#E7B800", "#FC4E07", "#FFDB6D", "#C4961A",
    "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"
)
# # cold
# mycol -> c("#1660A7","#FF6A00","#219418","#CD0C18","#814BB2","#794339",
#          "#5ddb53","#5066d1","#FF0000","#F0E442","#4836be","#EE6677",
#          "#DC59B6","#CC79A7","#982f29","#AFB400","#11B3C6","#292344",
#          "#E69F00","#AA3377","#009E73","#576e8b","#0072B2","#D55E00",
#          "#00AFBB","#00FFFF","#4477AA","#228833","#CCBB44","#66CCEE",
#           "#56B4E9","#BBBBBB")

library(gridExtra)

# pdf("indicatorsNew.pdf", height = height+2, width = width)

# phylum
# p1 = ggplot() + annotate("text", x = rep(1, nrow(plot_indva)), y = 0.15*c(1:nrow(plot_indva)), size = 3.5, label = as.character(plot_anno[,1])) + theme_bw() +
# theme_void() + 	# with no axes or grid
# scale_x_continuous(expand = c(0, 0)) + 		# 原点 0,0
# theme(plot.margin = unit(c(0.2, 0.2, 1.15+0.15*(max(nchar(as.character(data$Groups)))), 0), "cm"))		# 上、右、下、左

# genus
# p2 = ggplot() + annotate("text", x = rep(1, nrow(plot_indva)), y = 0.15*c(1:nrow(plot_indva)), size = 3.5, label = as.character(plot_anno[,2])) + theme_bw() +
# theme_void() + 	# with no axes or grid
# scale_x_continuous(expand = c(0, 0)) +
# theme(plot.margin = unit(c(0.2, 0.2, 1.15+0.15*(max(nchar(as.character(data$Groups)))), 0), "cm"))

###########################
##### yuchuan 2020.11.30 ######
##############################
# barplot
# mofdified: DataForPlot = data.frame(Species = names(plot_relabun), rela = plot_relabun)

a <- plot_anno
b <- cbind(rownames(a), a)
colnames(b) <- c("OTU", "phylum", "genus")
library(tidyr)
c <- tidyr::unite(b, "OTU_phylum_genus", OTU, phylum, genus, sep = ".")
# d<-cbind(c,as.data.frame(plot_relabun))
d <- merge(c, as.data.frame(plot_relabun), by.x = "row.names", by.y = "row.names", sort = F)
e <- d[, -1]
row.names(e) <- e[, 1]
colnames(e) <- c("Species", "rela")
DataForPlot <- e
##### yuchuan 2020.11.30 ######
###############################

# DataForPlot$Species = factor(DataForPlot$Species, levels = names(plot_relabun))
DataForPlot$Species <- factor(DataForPlot$Species, levels = row.names(DataForPlot))

# 20231103 liujiang: Replace Taxonomy information with species names
split_Species <- strsplit(as.character(DataForPlot$Species), split = "\\.")
first_elements <- sapply(split_Species, function(x) x[1])
DataForPlot$Species <- factor(first_elements, levels = unique(first_elements))

p3 <- ggplot(DataForPlot, aes(x = Species, y = rela)) +
    geom_bar(position = position_dodge(), stat = "identity", size = 0.3, colour = "black") + # Use black outlines,
    coord_flip() + # 横向绘图
    theme_bw() +
    ylab("Relative.Abundance (%)") +
    xlab("") +
    # theme(panel.border = element_blank()) +		# 无边框
    theme(axis.title.x = element_text(size = 10), plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_y_continuous(position = "left", expand = c(0, 0), limits = c(0, max(plot_relabun) * 1.2)) # x 轴刻度线在图形上方; 原点 0,0   # ggplot2 version 2.2.0

# Bubble plot
p4 <- ggplot(data, aes(x = xlabel, y = ylabel, size = Indicator.values, colour = xlabel)) + #
    guides(color = FALSE) + # 不用颜色标签
    # guides(colour = guide_legend()) +		 # combine size and color legend
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # x轴标签倾斜
    scale_size(range = c(0, 6)) +
    labs(x = "Groups", y = "") +
    theme(plot.margin = unit(c(-0.2, 0.5, 0.2, 0), "cm")) +
    scale_color_manual(limits = levels(data$xlabel), values = mycol[1:length(levels(data$xlabel))]) + # custom colors
    theme(axis.title.x = element_text(size = 10), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "top") # 去除 y轴标签及刻度线；标签位置

# combine plots
library(patchwork)

if (length(levels(data$xlabel)) < 30) {
    p4w <- 4
} else if (length(levels(data$xlabel)) < 60) {
    p4w <- 6
} else {
    p4w <- 8
}

p <- p3 + p4 + plot_layout(widths = c(4, p4w))
ggsave("indicator.pdf", p, height = 16, width = 16)
