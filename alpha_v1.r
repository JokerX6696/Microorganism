#!/usr/bin/env Rscript
# 微生物组计算阿尔法多样性
calcChao1Coverage <- function(x) {
    x <- x[x >= 1]
    n <- sum(x)
    f1 <- sum(x == 1)
    f2 <- sum(x == 2)
    
    if (f2 > 0) {
        rC1 <- 1 - (f1 / n) * (((n - 1) * f1) / ((n - 1) * f1 + 2 * f2))
    } else {
        rC1 <- 1 - (f1 / n) * (((n - 1) * (f1 - 1)) / ((n - 1) * (f1 - 1) + 2))
    }
    
    return(rC1)
}

# library(docopt)

# "Usage: alpha_diversity_ace.r  -i <file> -s <file> --output <file>
# Options:
#    -i , --otufile <file>          otu table
#    -s , --summary <file>          alpha estimator summary
#    --output <file>                output file" -> doc
library("optparse")
option_list <- list(
    make_option(
        c("-i", "--otufile"),
        type = "character", default = NULL,
        help = "otu table", metavar = "character"
    ),
    make_option(
        c("-o", "--output"),
        type = "character", default = NULL,
        help = "output file", metavar = "character"
    )
)
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

# opts <- docopt(doc, version = "Alpha多样性指数 ACE计算\n")
otufile <- opts$otufile
output <- opts$output


library(vegan)

otufile <- normalizePath(otufile)

otudata <- read.table(
    otufile,
    header = TRUE, sep = "\t", row.names = 1, check.names = FALSE,
    fill = TRUE, comment.char = "", quote = ""
)
otudata <- otudata[which(rowSums(otudata) != 0), , drop = FALSE]

chao1_out <- t(
    data.frame(
        estimateR(
            t(sapply(otudata, as.integer))
        ),
        check.names = FALSE
    )
)[, 2, drop = FALSE]

cov_out <-
    data.frame(
        goods_coverage = apply(t(sapply(otudata, as.integer)), 1, calcChao1Coverage),
        check.names = FALSE
    )

shannon_out <-
    data.frame(
        shannon = diversity(t(sapply(otudata, as.integer)), index = "shannon"),
        check.names = FALSE
    )

simpson_out <-
    data.frame(
        simpson = diversity(t(sapply(otudata, as.integer)), index = "simpson"),
        check.names = FALSE
    )

ace_out <- t(
    data.frame(
        estimateR(
            t(sapply(otudata, as.integer))
        ),
        check.names = FALSE
    )
)[, 4, drop = FALSE]

obs_out <- 
    data.frame(
        obs = rowSums(t(sapply(otudata, as.integer)) > 0),
        check.names = FALSE
    )

out <- data.frame(
    Sample = rownames(ace_out),
    chao1 = chao1_out[, 1],
    goods_coverage = cov_out,
    shannon = shannon_out,
    simpson = simpson_out,
    ACE = ace_out[, 1],
    obs = obs_out
)

write.table(
    out, output,
    col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t"
)
