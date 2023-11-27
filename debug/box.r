# input=/storge1/automation/projects/DNA/dna/2bRAD-M/2023/10/DZOE2023090692-b1/Suppl_5/RefSeq/meta_statistic/ANOVA/species/diff_species_top10_boxplot.txt
# group=/storge1/automation/projects/DNA/dna/2bRAD-M/2023/10/DZOE2023090692-b1/Suppl_5/RefSeq/meta_statistic/ANOVA/species/species.ANOVA.Group.xls

# #   %s %s/taxon_diff_boxplot.r -i %s/diff_%s_top10_boxplot.txt --output %s -l %s

# /usr/local/envs/qiime2-2020.11/bin/Rscript ./plot_box.r -i $input --output ./ -l species
suppressPackageStartupMessages(library("optparse"))
option_list <- list(
    make_option(c("-i", "--infile"),
        type = "character", default = NULL,
        help = "input file name", metavar = "character"
    ),
    make_option(c("-g", "--grouporder"),
        type = "character", default = "NA",
        help = "group order file name", metavar = "character"
    ),
    make_option(c("-l", "--level"),
        type = "character", default = NULL,
        help = "level", metavar = "character"
    ),
    make_option(c("-o", "--output"),
        type = "character", default = NULL,
        help = "output directory name", metavar = "character"
    )
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
infile <- opt$infile
grouporder <- opt$grouporder
level <- opt$level
output <- opt$output

library("ggplot2")
library("dplyr")
library("ggsignif")


# agricolae
lastC <- function(x) {
    y <- sub(" +$", "", x)
    p1 <- nchar(y)
    cc <- substr(y, p1, p1)
    return(cc)
}

orderPvalue <- function(treatment, means, alpha, pvalue, console) {
    n <- length(means)
    z <- data.frame(treatment, means)
    letras <- c(letters[1:26], LETTERS[1:26], 1:9, c(
        ".", "+",
        "-", "*", "/", "#", "$", "%", "&", "^", "[", "]", ":",
        "@", ";", "_", "?", "!", "=", "#", rep(" ", 2000)
    ))
    w <- z[order(z[, 2], decreasing = TRUE), ]
    M <- rep("", n)
    k <- 1
    k1 <- 0
    j <- 1
    i <- 1
    cambio <- n
    cambio1 <- 0
    chequeo <- 0
    M[1] <- letras[k]
    q <- as.numeric(rownames(w))
    while (j < n) {
        chequeo <- chequeo + 1
        if (chequeo > n) {
              break
          }
        for (i in j:n) {
            s <- pvalue[q[i], q[j]] > alpha
            if (s) {
                if (lastC(M[i]) != letras[k]) {
                      M[i] <- paste(M[i], letras[k], sep = "")
                  }
            } else {
                k <- k + 1
                cambio <- i
                cambio1 <- 0
                ja <- j
                for (jj in cambio:n) {
                    M[jj] <- paste(M[jj], "",
                        sep = ""
                    )
                }
                M[cambio] <- paste(M[cambio], letras[k], sep = "")
                for (v in ja:cambio) {
                    if (pvalue[q[v], q[cambio]] <= alpha) {
                        j <- j + 1
                        cambio1 <- 1
                    } else {
                        break
                    }
                }
                break
            }
        }
        if (cambio1 == 0) {
              j <- j + 1
          }
    }
    w <- data.frame(w, stat = M)
    trt <- as.character(w$treatment)
    means <- as.numeric(w$means)
    output <- data.frame(means, groups = M)
    rownames(output) <- trt
    if (k > 81) {
          cat("\n", k, "groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
      }
    invisible(output)
}

data <- read.table(
    infile,
    sep = "\t", header = T, check.name = FALSE, quote = ""
)

data$Group <- factor(data$Group, levels = unique(data$Group))

if (grouporder != "NA") {
    custom_level <- unlist(strsplit(grouporder, split = ","))
} else {
    custom_level <- levels(data$Group)
}

group_num <- length(levels(data$Group))
group_nc <- max(nchar(levels(data$Group)))


taxon <- as.character(unique(data$id))

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

method <- basename(dirname(output))
# taxon 长度影响出图数量
for (i in 1:length(taxon)) {
    plotdata <- data.frame(
        Group = data[data$id %in% taxon[i], ]$Group,
        abundance = data[data$id %in% taxon[i], ]$Abundance
    )
    tongji <- plotdata %>%
        group_by(Group) %>%
        summarise(value = sum(abundance))
    # if (length(tongji$value[tongji$value %in% 0]) > 0) next # 一整组数值为零，跳过
    pValue <- c()
    if (length(unique(plotdata$Group)) > 2) {
        if (method == "Wilcoxon") {
            res <- pairwise.wilcox.test(
                log(plotdata$abundance * 100, 10), plotdata$Group,
                p.adjust.method = "none"
            )
        } else {
            res <- pairwise.t.test(
                plotdata$abundance, plotdata$Group,
                p.adjust.method = "none"
            )
        }
        pValue <- res$p.value
        pValue[is.nan(pValue)] <- 1
        pValue <- pValue[lower.tri(pValue, diag=TRUE)]
        # pValue[is.na(pValue)] <- 1
    } else {
        pValue <- as.numeric(unique(data[data$id %in% taxon[i], ][, "p-value"]))
    }
    my_annotations <- as.character(
        symnum(
            pValue,
            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
            symbols = c("***", "**", "*", "")
        )
    )
    my_annotations1 <- my_annotations[my_annotations != ""]

    wtq <- levels(plotdata$Group)
    lis <- combn(wtq, 2)
    my_comparisons <- tapply(
        lis, rep(1:ncol(lis), each = nrow(lis)), function(i) i
    )
    my_comparisons1 <- my_comparisons[my_annotations != ""]

    if (group_num < 3) {
        width <- 1.4 + 1.0 * group_num + (max(group_nc, 10) - 10) * 0.1
    } else {
        width <- 3 + 0.4 * group_num + (max(group_nc, 10) - 10) * 0.1
    }
    if (nchar(taxon[i]) >= 70 & width <= 7) {
        width_ <- 7 + (max(group_nc, 10) - 10) * 0.1
    } else if (nchar(taxon[i]) >= 55 & width <= 6.2) {
        width_ <- 6.2 + (max(group_nc, 10) - 10) * 0.1
    } else if (nchar(taxon[i]) >= 40 & width <= 4.6) {
        width_ <- 4.6 + (max(group_nc, 10) - 10) * 0.1
    } else {
        width_ <- width
    }
    width <- max(width, width_)
    height <- 4 + 0.2 * length(my_annotations1)

    my_levels <- custom_level

    p <- ggplot(
        data = plotdata, aes(x = Group, y = abundance * 100, colour = Group)
    ) +
        geom_boxplot(
            alpha = 0.5,
            size = 0.8,
            width = 0.3,
            outlier.alpha = 0
        ) +
        geom_jitter(
            alpha = 0.3,
            size = 1
        ) +
        scale_color_manual(
            limits = my_levels,
            values = mycol[1:length(my_levels)]
        ) +
        theme_classic(base_line_size = 1) +
        labs(title = taxon[i], x = "", y = "Relative_Abundance(%)") +
        theme(
            plot.title = element_text(
                size = 12,
                colour = "black",
                face = "bold",
                hjust = 0
            ),
            axis.title.y = element_text(
                size = 12,
                color = "black",
                face = "bold",
                vjust = 1.9,
                hjust = 0.5,
                angle = 90
            ),
            legend.title = element_text(
                color = "black", # 修改图例的标题
                size = 13,
                face = "bold"
            ),
            legend.text = element_text(
                color = "black", # 设置图例标签文字
                size = 11
            ),
            axis.text.x = element_text(
                size = 11, # 修改X轴上字体大小，
                color = "black", # 颜色
                vjust = 1, # 位置
                hjust = 1,
                angle = 45
            ), # 角度
            axis.text.y = element_text(
                size = 11,
                color = "black",
                vjust = 0.5,
                hjust = 0.5,
                angle = 0
            ),
            axis.ticks.x = element_line(size = 0.5),
            axis.ticks.y = element_line(size = 0.5),
            axis.line.x = element_line(size = 0.8),
            axis.line.y = element_line(size = 0.8)
        )

    if (length(my_annotations1) > 0) {
        if (length(levels(plotdata$Group)) <= 5) {
            p <- p +
                geom_signif(
                    annotations = my_annotations1,
                    comparisons = my_comparisons1,
                    step_increase = 0.1,
                    vjust = .2,
                    colour = "gray20",
                    tip_length = 0.015
                )
        } else {
            x_axis_order <- levels(plotdata$Group)
            y_max <- aggregate(abundance ~ Group, data = plotdata, max)
            y_sep <- diff(range(plotdata$abundance)) * 0.05
            y_start_use <- (y_max$abundance + y_sep) * 100
            treatments <- as.character(y_max$Group)
            means <- aggregate(
                abundance ~ Group,
                data = plotdata, mean
            )$abundance
            pvalue <- matrix(
                1,
                nrow = length(treatments), ncol = length(treatments)
            )
            rownames(pvalue) <- colnames(pvalue) <- treatments
            for (c in my_comparisons1) {
                pvalue[c[1], c[2]] <- 0
                pvalue[c[2], c[1]] <- 0
            }
            grps <- orderPvalue(
                treatments, means, 0.05, pvalue, console = TRUE)
            add_sig_label <- grps[x_axis_order, "groups",  drop = T]

            textdf <- data.frame(
                x = x_axis_order,
                y = y_start_use,
                add = add_sig_label,
                stringsAsFactors = FALSE
            )

            p <- p +
                geom_text(
                    aes(x = x, y = y, label = add),
                    data = textdf, inherit.aes = FALSE
                )
            height <- 4
        }
    }
    pdf(
        file = paste0(
            output, "/diff_", level, "_top10_boxplot_each", i, ".pdf"
        ),
        width = width, height = height
    )
    print(p)
    dev.off()
}
