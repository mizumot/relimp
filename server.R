library(shiny)
library(shinyAce)
library(psych)
library(car)
library(rpsychi)
library(boot)
library(plyr)
library(ggplot2)
library(Boruta)
library(relaimpo)
library(MASS)
library(yhat)


shinyServer(function(input, output, session) {
    observe({
        if (input$do > 0) {
            #-----------------------------------------------------------------
            # Dominance Analysis (Relative Importance Analysis) Using Raw Data
            #-----------------------------------------------------------------

            # Basic statistics
            bs <- reactive({
                x <- read.csv(text = input$text, sep = "\t")
                describe(x)[2:13]
            })

            output$textarea.out <- renderPrint({
                bs()
            })


            # Correlation
            makecorPlot <- function() {
                x <- read.csv(text = input$text, sep = "\t")
                pairs.panels(x)
            }

            output$corPlot <- renderPlot({
                print(makecorPlot())
            })


            # Regression
            reg <- reactive({
                dat <- read.csv(text = input$text, sep = "\t")

                colnames(dat) <- c("Criterion", c(colnames(dat)[2:ncol(dat)]))
                result <- lm(Criterion ~ ., dat)

                reslt <- summary(result)
                print(reslt)

                z <- scale(dat) # standardize the data
                z <- data.frame(z)
                z.res <- summary(lm(Criterion ~ ., z))

                stdb <- data.frame(round((z.res$coefficients[, 1][-1]), 3))
                colnames(stdb)[1] <- "Standardized beta"

                cat("\n", "---", "\n", "Standardized beta estimates:", "\n")
                print(stdb)

                if (ncol(dat) >= 3) {
                    VIF <- vif(result)
                    Tolerance <- 1 / VIF

                    vif.res <- round(data.frame(VIF, Tolerance), 3)

                    cat("\n", "---", "\n", "VIF and tolerance statistic (1/VIF):", "\n")
                    print(vif.res)
                    cat(
                        "\n", "VIF should be smaller than 10 (clozer to 1 better);", "\n",
                        "tolerance statistic (1/VIF) should be greater than 0.2.", "\n"
                    )
                }
            })

            output$reg.out <- renderPrint({
                reg()
            })


            # Dominance Analysis
            dominance <- reactive({
                dat <- read.csv(text = input$text, sep = "\t")
                colnames(dat) <- c("Criterion", c(colnames(dat)[2:ncol(dat)]))

                lm.out <- lm(Criterion ~ ., dat)

                regrOut <- yhat::calc.yhat(lm.out)

                # Set seed for reproducibility
                set.seed(8888)

                boot.out <- NULL
                tryCatch(
                    {
                        boot.out <- boot::boot(dat, yhat::boot.yhat, 100, # Reduced from 1000 to 100
                            lmOut = lm.out,
                            regrout0 = regrOut
                        )
                    },
                    error = function(e) {
                        # More detailed error message
                        message("Bootstrap failed with error: ", e$message)
                    }
                )

                # Generate bootstrap results
                if (!is.null(boot.out)) {
                    result <- yhat::booteval.yhat(regrOut, bty = "perc", boot.out)

                    # safely extract GenDom column if it exists
                    if (is.null(result$combCIpm) || !("GenDom" %in% colnames(result$combCIpm))) {
                        # Fallback if bootstrap results don't have expected structure
                        da <- relaimpo::calc.relimp(lm.out)
                        da_res <- as.data.frame(round(da@lmg, 4))
                        colnames(da_res) <- "Weight"
                        da_res$CI_lower <- NA
                        da_res$CI_upper <- NA

                        return(list(
                            da_ci = da_res,
                            pv_comparisons = data.frame(
                                Comparison = "Bootstrap failed to produce expected structure",
                                Diff = NA,
                                CI_lower = NA,
                                CI_upper = NA,
                                Significant = ""
                            )
                        ))
                    }

                    da_weights <- result$combCIpm[, "GenDom", drop = FALSE]

                    # Create a data frame with separate columns for weight, lower CI, and upper CI
                    # Parse the string format "0.176(0.097,0.260)"
                    weights_df <- data.frame(
                        Variable = rownames(da_weights),
                        GenDom_string = da_weights[, 1],
                        stringsAsFactors = FALSE
                    )

                    # Extract values with better parsing
                    parse_value_with_ci <- function(str) {
                        # Remove any * at the end
                        str <- sub("\\*$", "", str)

                        # Extract main value (before parenthesis)
                        main_val <- as.numeric(sub("^([0-9.-]+)\\(.*", "\\1", str))

                        # Extract lower CI (between parenthesis and comma)
                        lower_val <- as.numeric(sub(".*\\(([0-9.-]+),.*", "\\1", str))

                        # Extract upper CI (between comma and closing parenthesis)
                        upper_val <- as.numeric(sub(".*,([0-9.-]+)\\).*", "\\1", str))

                        # Check if original string had * at the end
                        significant <- grepl("\\*$", weights_df$GenDom_string[1])

                        return(list(
                            main = main_val,
                            lower = lower_val,
                            upper = upper_val,
                            significant = significant
                        ))
                    }

                    # Apply parsing to each row
                    parsed_values <- lapply(weights_df$GenDom_string, parse_value_with_ci)

                    # Extract values
                    weights_df$Weight <- sapply(parsed_values, function(x) x$main)
                    weights_df$CI_lower <- sapply(parsed_values, function(x) x$lower)
                    weights_df$CI_upper <- sapply(parsed_values, function(x) x$upper)

                    # Create final data frame with weight and CI columns
                    da_ci <- weights_df[, c("Weight", "CI_lower", "CI_upper")]
                    rownames(da_ci) <- weights_df$Variable

                    # Comparisons of PVs - Format as separate columns
                    if (is.null(result$combCIpmDiff) || !("GenDom" %in% colnames(result$combCIpmDiff))) {
                        # If comparison data is not available in expected format
                        return(list(
                            da_ci = round(da_ci, 4),
                            pv_comparisons = data.frame(
                                Comparison = "Comparisons not available in expected format",
                                Diff = NA,
                                CI_lower = NA,
                                CI_upper = NA,
                                Significant = ""
                            )
                        ))
                    }

                    pv_raw_comparisons <- result$combCIpmDiff[, "GenDom", drop = FALSE]

                    formatted_comparisons <- data.frame(
                        Comparison = rownames(pv_raw_comparisons),
                        Diff = NA_real_,
                        CI_lower = NA_real_,
                        CI_upper = NA_real_,
                        Significant = "",
                        stringsAsFactors = FALSE
                    )

                    for (i in 1:nrow(pv_raw_comparisons)) {
                        comp_str <- as.character(pv_raw_comparisons[i, 1])

                        is_significant <- grepl("\\*$", comp_str)

                        clean_str <- sub("\\*$", "", comp_str)

                        main_val <- as.numeric(sub("^([0-9.-]+)\\(.*", "\\1", clean_str))
                        lower_val <- as.numeric(sub(".*\\(([0-9.-]+),.*", "\\1", clean_str))
                        upper_val <- as.numeric(sub(".*,([0-9.-]+)\\).*", "\\1", clean_str))

                        formatted_comparisons$Diff[i] <- main_val
                        formatted_comparisons$CI_lower[i] <- lower_val
                        formatted_comparisons$CI_upper[i] <- upper_val

                        if (is_significant) {
                            formatted_comparisons$Significant[i] <- "*"
                        }
                    }

                    formatted_comparisons$Diff <- round(formatted_comparisons$Diff, 4)
                    formatted_comparisons$CI_lower <- round(formatted_comparisons$CI_lower, 4)
                    formatted_comparisons$CI_upper <- round(formatted_comparisons$CI_upper, 4)

                    # Return as a list
                    return(list(
                        da_ci = round(da_ci, 4),
                        pv_comparisons = formatted_comparisons
                    ))
                } else {
                    # Fallback to simple dominance analysis if bootstrap fails
                    da <- relaimpo::calc.relimp(lm.out)
                    da_res <- as.data.frame(round(da@lmg, 4))
                    colnames(da_res) <- "Weight"

                    # Add placeholder CI columns
                    da_res$CI_lower <- NA
                    da_res$CI_upper <- NA

                    return(list(
                        da_ci = da_res,
                        pv_comparisons = data.frame(
                            Comparison = "Bootstrap failed",
                            Diff = NA_real_,
                            CI_lower = NA_real_,
                            CI_upper = NA_real_,
                            Significant = ""
                        )
                    ))
                }
            })

            output$dominance.out <- renderPrint({
                dominance()
                dom_results <- dominance()

                # Print weights with CI
                cat("Dominance Analysis Results with 95% CI:\n\n")
                print(dom_results$da_ci)

                cat("\n\n")
                cat("Predictor Variable Comparisons:\n")
                print(dom_results$pv_comparisons)
                cat("\n")
                cat("Positive values indicate greater predictive importance of the first variable;\n")
                cat("negative values indicate greater importance of the second.\n")
                cat("* indicates a statistically significant difference\n")
                cat("(when the confidence interval does not include zero).\n\n")
            })


            # 95% CI Plot
            confPlot <- function() {
                dat <- read.csv(text = input$text, sep = "\t")

                dom_results <- dominance()
                da_ci <- dom_results$da_ci

                Variables <- rownames(da_ci)

                plotdat <- data.frame(
                    Variables = Variables,
                    Weight = da_ci$Weight,
                    CI_lower = da_ci$CI_lower,
                    CI_upper = da_ci$CI_upper
                )

                dat_model <- read.csv(text = input$text, sep = "\t")
                colnames(dat_model) <- c("Criterion", c(colnames(dat_model)[2:ncol(dat_model)]))
                result <- lm(Criterion ~ ., dat_model)
                model_summary <- summary(result)
                RSQ.Results <- model_summary$r.squared

                ggplot(plotdat, aes(reorder(Variables, Weight), Weight)) +
                    geom_bar(stat = "identity", fill = "lightblue", colour = "blue") +
                    coord_flip() +
                    geom_text(aes(label = round(Weight, 3)), hjust = -0.1, vjust = -0.3) +
                    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = .2, position = position_dodge(.9)) +
                    labs(
                        title = "Variable Importance (Dominance Analysis)",
                        x = "Predictor Variables",
                        y = "Dominance Weights",
                        caption = paste0(
                            "Note: R-squared = ",
                            round(RSQ.Results, 3),
                            ". ",
                            "Dominance weights sum to R-squared. n = ",
                            nrow(dat), ". "
                        )
                    ) +
                    theme_classic()
            }

            output$confPlot <- renderPlot({
                print(confPlot())
            })


            # RWA
            rwacalc <- reactive({
                dat <- read.csv(text = input$text, sep = "\t")
                thedata <- dat
                Labels <- names(thedata)[2:length(thedata)]
                multRegress <- function(mydata) {
                    numVar <<- NCOL(mydata)
                    Variables <<- names(mydata)[2:numVar]
                    mydata <- cor(mydata, use = "complete.obs")
                    RXX <- mydata[2:numVar, 2:numVar]
                    RXY <- mydata[2:numVar, 1]
                    RXX.eigen <- eigen(RXX)
                    D <- diag(RXX.eigen$val)
                    delta <- sqrt(D)
                    lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
                    lambdasq <- lambda^2
                    beta <- solve(lambda) %*% RXY
                    rsquare <<- sum(beta^2)
                    RawWgt <- lambdasq %*% beta^2
                    import <- (RawWgt / rsquare) * 100
                    result <<- data.frame(Variables, Raw.RelWeight = RawWgt, Rescaled.RelWeight = import)
                }
                multBootstrap <- function(mydata, indices) {
                    mydata <- mydata[indices, ]
                    multWeights <- multRegress(mydata)
                    return(multWeights$Raw.RelWeight)
                }
                multBootrand <- function(mydata, indices) {
                    mydata <- mydata[indices, ]
                    multRWeights <- multRegress(mydata)
                    multReps <- multRWeights$Raw.RelWeight
                    randWeight <- multReps[length(multReps)]
                    randStat <- multReps[-(length(multReps))] - randWeight
                    return(randStat)
                }
                mybootci <- function(x) {
                    boot.ci(multBoot, conf = 0.95, type = "bca", index = x)
                }
                runBoot <- function(num) {
                    INDEX <- 1:num
                    test <- lapply(INDEX, FUN = mybootci)
                    test2 <- t(sapply(test, "[[", i = 4)) # extracts confidence interval
                    CIresult <<- data.frame(Variables, CI.Lower.Bound = test2[, 4], CI.Upper.Bound = test2[, 5])
                }
                myRbootci <- function(x) {
                    boot.ci(multRBoot, conf = 0.95, type = "bca", index = x)
                }
                runRBoot <- function(num) {
                    INDEX <- 1:num
                    test <- lapply(INDEX, FUN = myRbootci)
                    test2 <- t(sapply(test, "[[", i = 4))
                    CIresult <<- data.frame(Labels, CI.Lower.Bound = test2[, 4], CI.Upper.Bound = test2[, 5])
                }
                myCbootci <- function(x) {
                    boot.ci(multC2Boot, conf = 0.95, type = "bca", index = x)
                }
                runCBoot <- function(num) {
                    INDEX <- 1:num
                    test <- lapply(INDEX, FUN = myCbootci)
                    test2 <- t(sapply(test, "[[", i = 4))
                    CIresult <<- data.frame(Labels2, CI.Lower.Bound = test2[, 4], CI.Upper.Bound = test2[, 5])
                }
                myGbootci <- function(x) {
                    boot.ci(groupBoot, conf = 0.95, type = "bca", index = x)
                }
                runGBoot <- function(num) {
                    INDEX <- 1:num
                    test <- lapply(INDEX, FUN = myGbootci)
                    test2 <- t(sapply(test, "[[", i = 4))
                    CIresult <<- data.frame(Labels, CI.Lower.Bound = test2[, 4], CI.Upper.Bound = test2[, 5])
                }

                multRegress(thedata)
                RW.Results <- result

                RSQ.Results <- rsquare

                # Reduced number of bootstrap iterations for better performance
                multBoot <- boot(thedata, multBootstrap, 1000) # Reduced from 10000
                multci <- boot.ci(multBoot, conf = 0.95, type = "bca")
                runBoot(length(thedata[, 2:numVar]))
                CI.Results <- CIresult

                # Reduced number of bootstrap iterations for better performance
                randVar <- rnorm(length(thedata[, 1]), 0, 1)
                randData <- cbind(thedata, randVar)
                multRBoot <- boot(randData, multBootrand, 1000) # Reduced from 10000
                multRci <- boot.ci(multRBoot, conf = 0.95, type = "bca")
                runRBoot(length(randData[, 2:(numVar - 1)]))
                CI.Significance <- CIresult

                list(RSQ.Results = RSQ.Results, RW.Results = RW.Results, CI.Results = CI.Results, CI.Significance = CI.Significance)
            })


            rwa <- reactive({
                cat("R-squared For the Model:", "\n")
                RSQ.Results <- rwacalc()$RSQ.Results
                print(RSQ.Results)

                cat("\n", "The Raw and Rescaled Weights:", "\n")
                RW.Results <- rwacalc()$RW.Results
                print(RW.Results)

                cat("\n", "BCa Confidence Intervals around the raw weights:", "\n")
                CI.Results <- rwacalc()$CI.Results
                print(CI.Results)

                cat("\n", "BCa Confidence Interval Tests of significance:", "\n")
                cat("  (If 0 is not included, weight is significant at p < .05)", "\n")
                CI.Significance <- rwacalc()$CI.Significance
                print(CI.Significance)
            })

            output$rwa.out <- renderPrint({
                rwa()
            })

            # Info
            info1 <- reactive({
                info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
                info2 <- paste("It was executed on ", date(), ".", sep = "")
                cat(sprintf(info1), "\n")
                cat(sprintf(info2), "\n")
            })

            output$info1.out <- renderPrint({
                info1()
            })
        }
    })


    # 2. Random Forest Analysis observe block
    observe({
        if (input$run_forest > 0) {
            # Feature Selection: Variable Importance (Random Forest)
            borutaCalc <- function() {
                dat <- read.csv(text = input$text, sep = "\t")
                dat <- na.omit(dat) # listwise deletion
                colnames(dat) <- c("Criterion", c(colnames(dat)[2:ncol(dat)]))
                set.seed(456)
                # Reduced maxRuns for better performance
                boruta <- Boruta(Criterion ~ ., maxRuns = 50, data = dat, doTrace = 1)
                list(boruta = boruta)
            }

            randomForestPlot <- function() {
                boruta <- borutaCalc()$boruta
                plot(boruta)
            }

            output$randomForestPlot <- renderPlot({
                print(randomForestPlot())
            })

            borutaInfo <- function() {
                boruta <- borutaCalc()$boruta
                print(list(boruta, attStats(boruta)))
            }

            output$boruta.out <- renderPrint({
                borutaInfo()
            })
        }
    })




    #-----------------------------------------------------------------
    # Dominance Analysis (Relative Importance Analysis) Using Correlation Matrix
    #-----------------------------------------------------------------

    # Check the Correlation Matrix
    inputData <- reactive({
        dat <- read.csv(text = input$text2, sep = "", na.strings = c("", "NA", "."))
        print(dat)
    })

    output$inputData.out <- renderPrint({
        inputData()
    })


    # Multiple Regression analysis
    mra2.result <- reactive({
        n <- input$n
        dat <- read.csv(text = input$text2, sep = "", na.strings = c("", "NA", "."))
        dat <- as.data.frame(dat)
        # source("https://mizumot.com/code/multreg.second2.R", encoding="UTF-8")
        multreg.second2 <-
            function(dat, n, m = NULL, sd = NULL, sig.level = 0.05,
                     digits = 3) {
                depname <- colnames(dat[1])
                indname <- colnames(dat[-1])
                x <- c(depname, indname)
                corr <- dat
                sd <- sd[x]
                m <- m[x]
                corr.lower <- tanh(atanh(corr) + qnorm(sig.level / 2, lower.tail = TRUE) / sqrt(n -
                    3))
                corr.upper <- tanh(atanh(corr) + qnorm(sig.level / 2, lower.tail = FALSE) / sqrt(n -
                    3))
                corr.conf <- corr.lower
                corr.conf[upper.tri(corr.conf)] <- corr.upper[upper.tri(corr.upper)]
                p <- ncol(corr)
                K <- solve(corr)
                a <- 1 / sqrt(diag(K))
                K <- K * outer(a, a)
                partial.corr <- 2 * diag(p) - K
                dimnames(partial.corr) <- dimnames(partial.corr)
                cor.mat <- corr
                cor.mat[upper.tri(cor.mat)] <- partial.corr[upper.tri(partial.corr)]
                num <- which(depname == colnames(corr))
                rxy <- corr[, num][-num]
                Rind <- corr[-num, -num]
                bs <- solve(Rind) %*% rxy
                R.sq <- as.vector(t(as.matrix(rxy)) %*% solve(Rind) %*% as.matrix(rxy))
                k <- nrow(bs)
                bs.sem <- numeric(k)
                for (i in 1:k) {
                    xname <- rownames(bs)[i]
                    xdel <- setdiff(rownames(bs), xname)
                    num <- which(xname == colnames(Rind))
                    rxy <- Rind[, num][-num]
                    Rind2 <- Rind[-num, -num]
                    Ri <- as.vector(t(as.matrix(rxy)) %*% solve(Rind2) %*%
                        as.matrix(rxy))
                    bs.sem[i] <- sqrt((1 - R.sq) / (n - k - 1)) * sqrt(1 / (1 -
                        Ri))
                }
                standardized.estimates <- data.frame(matrix(NA,
                    nrow = k,
                    ncol = 4
                ))
                rownames(standardized.estimates) <- rownames(bs)
                colnames(standardized.estimates) <- c(
                    "estimates", "lower",
                    "upper", "std"
                )
                standardized.estimates[, 1] <- as.numeric(bs)
                standardized.estimates[, 4] <- bs.sem
                standardized.estimates[, 2] <- standardized.estimates$estimates +
                    qnorm(sig.level / 2) * standardized.estimates$std
                standardized.estimates[, 3] <- standardized.estimates$estimates +
                    qnorm(sig.level / 2, lower.tail = FALSE) * standardized.estimates$std
                if (!is.null(sd)) {
                    b <- sd[depname] / sd[rownames(bs)] * standardized.estimates$estimates
                    b.sem <- sd[depname] / sd[rownames(bs)] * standardized.estimates$std
                    b.lower <- b + qt(sig.level / 2, n - k - 1) * b.sem
                    b.upper <- b + qt(sig.level / 2, n - k - 1, lower.tail = FALSE) *
                        b.sem
                    Intercept <- m[depname] - sum(b * m[names(b)])
                    raw.estimates <- data.frame(matrix(NA,
                        nrow = k + 1,
                        ncol = 4
                    ))
                    rownames(raw.estimates) <- c("Intercept", names(b))
                    colnames(raw.estimates) <- c(
                        "estimates", "lower", "upper",
                        "std"
                    )
                    raw.estimates[, 1] <- c(Intercept, b)
                    raw.estimates[, 4] <- c(NA, bs.sem)
                    raw.estimates[, 2] <- c(NA, b.lower)
                    raw.estimates[, 3] <- c(NA, b.upper)
                }
                u <- length(indname)
                nu <- n - u - 1
                f.sq <- R.sq / (1 - R.sq)
                f.value <- f.sq * (nu / u)
                delta.lower <- try(FNONCT(f.value, u, nu, prob = 1 - sig.level / 2))
                delta.upper <- FNONCT(f.value, u, nu, prob = sig.level / 2)
                if (is.character(delta.lower)) {
                    delta.lower <- 0
                }
                R.sq.lower <- delta.lower / (delta.lower + u + nu + 1)
                R.sq.upper <- delta.upper / (delta.upper + u + nu + 1)
                omnibus.es <- c(Rsq = R.sq, lower = R.sq.lower, upper = R.sq.upper)
                criterion.power <- c(small = power.multi(
                    n = n, n.ind = u,
                    delta = 0.02, sig.level = sig.level
                ), medium = power.multi(
                    n = n,
                    n.ind = u, delta = 0.15, sig.level = sig.level
                ), large = power.multi(
                    n = n,
                    n.ind = u, delta = 0.35, sig.level = sig.level
                ))
                output <- list(
                    corr.partial.corr = cor.mat, corr.confidence = corr.conf,
                    omnibus.es = omnibus.es, standardized.estimates = standardized.estimates,
                    power = criterion.power
                )
                if (!is.null(sd)) {
                    output <- list(
                        corr.partial.corr = cor.mat, corr.confidence = corr.conf,
                        omnibus.es = omnibus.es, raw.estimates = raw.estimates,
                        standardized.estimates = standardized.estimates,
                        power = criterion.power
                    )
                }
                output <- sapply(output, round, digits)
                return(output)
            }

        multreg.second2(dat, n = n)
    })

    output$mra2.result.out <- renderPrint({
        mra2.result()
    })


    # Dominance Analysis
    dominance.cor <- reactive({
        dat <- read.csv(text = input$text2, sep = "", na.strings = c("", "NA", "."))
        n <- input$n
        # dat2 <- dat
        # dat <- as.matrix(dat)
        # as.formula(paste(colnames(dat2[1]), " ~ ", paste(colnames(dat2[-1]), collapse="+")))
        # lm.cov <- lmWithCov(as.formula(paste(colnames(dat2[1]), " ~ ", paste(colnames(dat2[-1]), collapse="+"))), dat)
        # da <- dominanceAnalysis(lm.cov)
        # res <- as.data.frame(round(da$contribution.average[[1]], 4))
        # colnames(res) <- "Weight"
        # print(res)

        # Generating simulation dataset because dominanceanalysis package cannot be used with older version of R
        mu <- rep(c(0), times = length(colnames(dat)))
        simdat <- mvrnorm(n = n, mu = mu, Sigma = dat, empirical = TRUE)
        simdat <- as.data.frame(simdat)
        colnames(simdat) <- c("Criterion", c(colnames(simdat)[2:ncol(simdat)]))
        res <- lm(Criterion ~ ., simdat)
        da <- calc.relimp(res)
        da_res <- as.data.frame(round(da@lmg, 4))
        colnames(da_res) <- "Weight"
        da_res
    })

    output$dominance.cor.out <- renderPrint({
        dominance.cor()
    })


    # Relative Weight Analysis
    rwacalc.cor <- reactive({
        dat <- read.csv(text = input$text2, sep = "", na.strings = c("", "NA", "."))
        thedata <- as.data.frame(dat)
        Labels <- names(thedata)[2:length(thedata)]
        multRegress <- function(mydata) {
            numVar <<- ncol(mydata)
            Variables <<- names(mydata)[2:numVar]
            RXX <- mydata[2:numVar, 2:numVar]
            RXY <- mydata[2:numVar, 1]
            RXX.eigen <- eigen(RXX)
            D <- diag(RXX.eigen$val)
            delta <- sqrt(D)
            lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
            lambdasq <- lambda^2
            beta <- solve(lambda) %*% RXY
            rsquare <<- sum(beta^2)
            RawWgt <- lambdasq %*% beta^2
            import <- (RawWgt / rsquare) * 100
            result <<- data.frame(Variables, Raw.RelWeight = RawWgt, Rescaled.RelWeight = import)
        }
        multRegress(thedata)
        RW.Results <- result
        RSQ.Results <- rsquare

        list(RSQ.Results2 = RSQ.Results, RW.Results2 = RW.Results) # 他で使うために定義
    })


    wra.cor <- reactive({
        cat("R-squared For the Model:", "\n")
        RSQ.Results2 <- rwacalc.cor()$RSQ.Results2
        print(RSQ.Results2)

        cat("\n", "The Raw and Rescaled Weights:", "\n")
        RW.Results2 <- rwacalc.cor()$RW.Results2
        print(RW.Results2)
    })

    output$wra.cor.out <- renderPrint({
        wra.cor()
    })


    # Importance Plot
    imPlot <- function() {
        da_res <- dominance.cor()

        Variables <- rownames(da_res)

        plotdat <- data.frame(
            Variables = Variables,
            Weight = da_res$Weight
        )

        dat <- read.csv(text = input$text2, sep = "", na.strings = c("", "NA", "."))
        RSQ.Results <- dat[1, 1]

        ggplot(plotdat, aes(reorder(Variables, Weight), Weight)) +
            geom_bar(stat = "identity", fill = "lightblue", colour = "blue") +
            coord_flip() +
            scale_y_continuous(limits = c(0, max(plotdat$Weight) * 1.1)) +
            geom_text(aes(label = Weight), hjust = -0.1, vjust = -0.3) +
            labs(
                title = "Variable Importance (Dominance Analysis)",
                x = "Predictor Variables",
                y = "Dominance Weights",
                caption = paste0(
                    "Note: R-squared = ",
                    round(RSQ.Results, 3),
                    ". ",
                    "Dominance weights sum to R-squared. n = ",
                    input$n, ". "
                )
            ) +
            theme_classic()
    }

    output$imPlot <- renderPlot({
        print(imPlot())
    })


    # Info
    info2 <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })

    output$info2.out <- renderPrint({
        info2()
    })
})
