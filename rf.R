# author wenbo.kou, jiang.liu

library(getopt)

spec <- matrix(c(
        'abd',   'a', '1', "character", "abd file, like L7.Top30_species.xls",
        'group', 'g', '1', "character", "group file, sample <tab> group, like group.list",
        'outdir','o', '1', "character", "outdir",
        'help',  'h', '0', 'character', "help"
),byrow=TRUE,ncol=5)

opt <- getopt(spec)

if(is.null(opt$abd) & is.null(opt$group)){
cat(getopt(spec,usage=TRUE))
q(status=1)
}

if(!dir.exists(opt$outdir)){
        dir.create(opt$outdir)
}

library(randomForest)
library(pROC)
library(ggpubr)

train.x <- opt$abd
train.y <- opt$group
cv.fold <- 10
cv.step <- 0.9
cv.time <- 10
marker.num <- 0
prefix <- paste0(opt$outdir,"/RF")

rfcv1 <- function(trainx, trainy, cv.fold = 5, scale = "log", step = 0.5,
                  mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE,
                  ipt = NULL, ...) {
    classRF <- is.factor(trainy)
    n <- nrow(trainx)
    p <- ncol(trainx)
    if (scale == "log") {
        k <- floor(log(p, base = 1 / step))
        n.var <- round(p * step^(0:(k - 1)))
        same <- diff(n.var) == 0
        if (any(same)) {
            n.var <- n.var[-which(same)]
        }
        if (!1 %in% n.var) {
            n.var <- c(n.var, 1)
        }
    } else {
        n.var <- seq(from = p, to = 1, by = step)
    }
    k <- length(n.var)
    cv.pred <- vector(k, mode = "list")
    for (i in 1:k) cv.pred[[i]] <- trainy
    if (classRF) {
        f <- trainy
        if (is.null(ipt)) {
            ipt <- nlevels(trainy) + 1
        }
    } else {
        f <- factor(rep(1:5, length = length(trainy))[order(order(trainy))])
        if (is.null(ipt)) {
            ipt <- 1
        }
    }
    nlvl <- table(f)
    idx <- numeric(n)
    for (i in 1:length(nlvl)) {
        idx[which(f == levels(f)[i])] <- sample(
            rep(1:cv.fold, length = nlvl[i])
        )
    }
    res <- list()
    for (i in 1:cv.fold) {
        all.rf <- randomForest(
            trainx[idx != i, , drop = FALSE], trainy[idx != i],
            trainx[idx == i, , drop = FALSE], trainy[idx == i],
            mtry = mtry(p), importance = TRUE, ...
        )
        cv.pred[[1]][idx == i] <- all.rf$test$predicted
        impvar <- (1:p)[order(all.rf$importance[, ipt], decreasing = TRUE)]
        res[[i]] <- impvar
        for (j in 2:k) {
            imp.idx <- impvar[1:n.var[j]]
            sub.rf <- randomForest(
                trainx[idx != i, imp.idx, drop = FALSE],
                trainy[idx != i],
                trainx[idx == i, imp.idx, drop = FALSE],
                trainy[idx == i],
                mtry = mtry(n.var[j]), importance = recursive, ...
            )
            cv.pred[[j]][idx == i] <- sub.rf$test$predicted
            if (recursive) {
                impvar <- (1:length(imp.idx))[
                    order(sub.rf$importance[, ipt], decreasing = TRUE)
                ]
            }
            NULL
        }
        NULL
    }
    if (classRF) {
        error.cv <- sapply(cv.pred, function(x) mean(trainy != x))
    } else {
        error.cv <- sapply(cv.pred, function(x) mean((trainy - x)^2))
    }
    names(error.cv) <- names(cv.pred) <- n.var
    list(n.var = n.var, error.cv = error.cv, predicted = cv.pred, res = res)
}

remove_rare <- function(table, cutoff_pro) {
    row2keep <- c()
    cutoff <- ceiling(cutoff_pro * ncol(table))
    for (i in 1:nrow(table)) {
        row_nonzero <- length(which(table[i, ] > 0))
        if (row_nonzero > cutoff) {
            row2keep <- c(row2keep, i)
        }
    }
    return(table[row2keep, , drop = F])
}

otu_table_rare_removed <- remove_rare(
    data.frame(
        read.table(
            train.x,
            sep = "\t", header = T, row.names = 1,
            stringsAsFactors = FALSE
        )
    ),
    cutoff_pro = 0
)
otu_table_rare_removed_norm <- sweep(
    otu_table_rare_removed, 2, colSums(otu_table_rare_removed), "/"
) * 100
otu_table_scaled <- scale(
    otu_table_rare_removed_norm,
    center = TRUE, scale = TRUE
)
otu_table_scaled_state <- data.frame(t(otu_table_scaled))
colnames(otu_table_scaled_state) <- rownames(otu_table_scaled)
train.x <- otu_table_scaled_state
train.y <- as.factor(
    read.table(
        train.y,
        sep = "\t", header = F, row.names = 1, stringsAsFactors = FALSE
    )[, 1]
)
train.l <- levels(train.y)
levels(train.y) <- 0:1

# cross validation
##################################
##################################
set.seed(123)
train.cv <- replicate(
    cv.time,
    rfcv1(train.x, train.y, cv.fold = cv.fold, recursive = T, step = cv.step),
    simplify = F
)
error.cv <- sapply(train.cv, "[[", "error.cv")
error.cv.rm <- rowMeans(error.cv)
id <- error.cv.rm < min(error.cv.rm) + sd(error.cv.rm)

if (marker.num == 0) {
    marker.num <- min(as.numeric(names(error.cv.rm)[id]))
}
print(train.cv)
q()
pdf.dir1 <- paste0(prefix, "_vars.pdf")
# pdf.dir2 <- paste0(prefix, "_train_possibility_boxplot.pdf")
pdf.dir3.1 <- paste0(prefix, "_train_roc.pdf")


pdf(pdf.dir1)

matplot(
    train.cv[[1]]$n.var, error.cv,
    type = "l", log = "x", col = rep("#bebebe", cv.time),
    main = paste("select", marker.num, "Vars"),
    xlab = "Number of vars", ylab = "CV Error", lty = 2
)
lines(train.cv[[1]]$n.var, error.cv.rm, lwd = 2)
abline(v = marker.num, col = "pink", lwd = 2)
dev.off()

marker.t <- table(unlist(lapply(train.cv, function(x) {
    lapply(x$res, "[", 1:marker.num)
})))
marker.t <- sort(marker.t, d = T)
names(marker.t) <- colnames(train.x)[as.numeric(names(marker.t))]
marker.dir <- paste0(prefix, "_marker.txt")
write.table(marker.t, marker.dir, col.names = F, sep = "\t", quote = F)
marker.p <- names(marker.t)[1:marker.num]


set.seed(123)
train.rf <- randomForest(
    train.x[, marker.p], train.y,
    ntree = 10001, importance = TRUE, proximities = TRUE
)
train.p <- predict(train.rf, type = "prob")
pr.dir <- paste0(prefix, "_train_probability.txt")
write.table(train.p[, 2], pr.dir, sep = "\t", quote = F, col.names = F)

# boxplot(
#     train.p[, 2] ~ train.y, col = 2:3, main = "Probability", names = train.l)

y <- as.factor(
    read.table(
        'group.list',
        sep = "\t", header = F, row.names = 1, stringsAsFactors = FALSE
    )[, 1]
)
l <- levels(y)

plot_dta <- as.data.frame(train.p)
colnames(plot_dta) <- l

write.table(plot_dta, pr.dir, sep = "\t", quote = F, col.names = NA)

plot_dta[["grp"]] <- y

bplot <- ggboxplot(plot_dta,
    x = "grp", y = colnames(plot_dta)[2],
    fill = "grp", line.color = NA, line.size = 0,
    palette = c("#ED000080", "#00468B80")
) +
    stat_compare_means(paired = FALSE) +
    ylab("POD") +
    ggtitle("Probability") +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = .5)
    )

pdf(
    paste0(prefix, "_train_possibility_boxplot.pdf"),
    width = 4, height = 5, bg = "white"
)
print(bplot)
dev.off()

# train ROC
pdf(pdf.dir3.1)
rocobj <- plot.roc(train.y, as.numeric(train.p[, 2]),
    main = "",
    of = "se",
    #  of="sp",
    percent = TRUE,
    ci = TRUE, # compute AUC (of AUC by default)
    print.auc = TRUE, # print the AUC (will contain the CI)
    print.auc.x = 40,
    print.auc.y = 10,
    specificities = seq(0, 100, 5), # on a select set of specificities
    sensitivity = seq(0, 100, 5), # on a select set of specificities
    ci.type = "shape",
    ci.col = "lightgreen"
)
lines.roc(rocobj, col = "red")
dev.off()
