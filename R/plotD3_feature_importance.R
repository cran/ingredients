#' @title Plot Feature Importance Objects in D3 with r2d3 Package.
#'
#' @description
#' Function \code{\link{plotD3.feature_importance_explainer}} plots dropouts for variables used in the model.
#' It uses output from \code{\link{feature_importance}} function that corresponds to permutation based measure of feature importance.
#' Variables are sorted in the same order in all panels. The order depends on the average drop out loss.
#' In different panels variable contributions may not look like sorted if variable importance is different in different models.
#'
#' @param x a feature importance explainer produced with the \code{feature_importance()} function
#' @param ... other explainers that shall be plotted together
#' @param max_vars maximum number of variables that shall be presented for for each model.
#' By default \code{NULL} which means all variables
#' @param show_boxplots logical if \code{TRUE} (default) boxplot will be plotted to show permutation data.
#' @param bar_width width of bars in px. By default \code{12px}
#' @param split either "model" or "feature" determines the plot layout
#' @param scale_height a logical. If \code{TRUE}, the height of plot scales with window size. By default it's \code{FALSE}
#' @param margin extend x axis domain range to adjust the plot.
#' Usually value between \code{0.1} and \code{0.3}, by default it's \code{0.15}
#' @param chart_title a character. Set custom title
#'
#' @return a \code{r2d3} object.
#'
#' @references Explanatory Model Analysis. Explore, Explain, and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @examples
#' library("DALEX")
#' library("ingredients")
#'
#' lm_model <- lm(m2.price ~., data = apartments)
#' explainer_lm <- explain(lm_model,
#'                         data = apartments[,-1],
#'                         y = apartments[,1],
#'                         verbose = FALSE)
#'
#' fi_lm <- feature_importance(explainer_lm,
#'       loss_function = DALEX::loss_root_mean_square, B = 1)
#'
#' head(fi_lm)
#' plotD3(fi_lm)
#'
#' \donttest{
#' library("ranger")
#'
#' rf_model <- ranger(m2.price~., data = apartments)
#'
#' explainer_rf <- explain(rf_model,
#'                         data = apartments[,-1],
#'                         y = apartments[,1],
#'                         label = "ranger forest",
#'                         verbose = FALSE)
#'
#' fi_rf <- feature_importance(explainer_rf, loss_function = DALEX::loss_root_mean_square)
#'
#' head(fi_rf)
#' plotD3(fi_lm, fi_rf)
#'
#' plotD3(fi_lm, fi_rf, split = "feature")
#'
#' plotD3(fi_lm, fi_rf, max_vars = 3, bar_width = 16, scale_height = TRUE)
#' plotD3(fi_lm, fi_rf, max_vars = 3, bar_width = 16, split = "feature", scale_height = TRUE)
#' plotD3(fi_lm, margin = 0.2)
#' }
#'
#' @export
#' @rdname plotD3_feature_importance
plotD3.feature_importance_explainer <-  function(x, ...,
                                                 max_vars = NULL,
                                                 show_boxplots = TRUE,
                                                 bar_width = 12,
                                                 split = "model",
                                                 scale_height = FALSE,
                                                 margin = 0.15,
                                                 chart_title = "Feature importance") {

  if (!(split %in% c("model", "feature"))){
    stop("The plotD3.feature_importance_explainer()
         function requires split to be model or feature.")
  }

  permutation <- NULL
  dfl <- c(list(x), list(...))

  # add boxplot data
  if (show_boxplots) {
    dfl <- lapply(dfl, function(x) {
      result <- data.frame(
        min = tapply(x$dropout_loss, x$variable, min, na.rm = TRUE),
        q1 = tapply(x$dropout_loss, x$variable, quantile, 0.25, na.rm = TRUE),
        q3 = tapply(x$dropout_loss, x$variable, quantile, 0.75, na.rm = TRUE),
        max = tapply(x$dropout_loss, x$variable, max, na.rm = TRUE)
      )

      result$min <- as.numeric(result$min)
      result$q1 <- as.numeric(result$q1)
      result$q3 <- as.numeric(result$q3)
      result$max <- as.numeric(result$max)

      merge(x[x$permutation == 0,], cbind(rownames(result),result), by.x = "variable", by.y = "rownames(result)")
    })
  } else {
    dfl <- lapply(dfl, function(x) {
      x[x$permutation == 0,]
    })
  }

  fi_df <- do.call(rbind, dfl)

  # add this so it works as before boxplots
  df <- subset(fi_df, select = -permutation)

  xmax <- max(df[df$variable!="_baseline_",]$dropout_loss)
  xmin <- min(df$dropout_loss)

  xmargin <- abs(xmin-xmax)*margin;

  best_fits <- df[df$variable == "_full_model_", ]
  df <- merge(df, best_fits[,c("label", "dropout_loss")], by = "label", sort = FALSE)

  # remove rows that starts with _
  df <- df[!(substr(df$variable,1,1) == "_"),]

  perm <- aggregate(df$dropout_loss.x, by = list(Category=df$variable), FUN = mean)

  options <- list(showBoxplots = show_boxplots,
                  barWidth = bar_width,
                  xmin = xmin - xmargin,
                  xmax = xmax + xmargin,
                  scaleHeight = scale_height,
                  chartTitle = chart_title)

  if (split == "model") {
    # one plot for each model

    # for each model leave only max_vars
    if (!is.null(max_vars)) {

      trimmed_parts <- lapply(unique(df$label), function(label) {
        tmp <- df[df$label == label, ]
        tmp[tail(order(tmp$dropout_loss.x), max_vars), ]
      })
      df <- do.call(rbind, trimmed_parts)
    }

    # sorting bars in groups
    perm <- as.character(perm$Category[order(perm$x)])
    df$variable <- factor(as.character(df$variable), levels = perm)
    df <- df[order(df$variable),]

    colnames(df)[colnames(df) == "dropout_loss.x"] <- "dropout_loss"
    colnames(df)[colnames(df) == "dropout_loss.y"] <- "full_model"

    label_list <- unique(as.character(df$variable))

    dfl <- split(df, f = df$label)

    temp <- jsonlite::toJSON(list(dfl, label_list))

    # n - number of models, m - number of features
    options["n"] <- length(dfl)
    options$m <- unname(sapply(dfl, function(x) dim(x)[1]))

    r2d3::r2d3(data = temp, script = system.file("d3js/featureImportance.js", package = "ingredients"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "ingredients"),
                 system.file("d3js/d3-tip.js", package = "ingredients"),
                 system.file("d3js/hackHead.js", package = "ingredients")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
         d3_version = 4,
         options = options)

  } else if (split == "feature") {
    # one plot for each feature

    colnames(df)[colnames(df) == "dropout_loss.x"] <- "dropout_loss"
    colnames(df)[colnames(df) == "dropout_loss.y"] <- "full_model"

        label_list <- unique(as.character(df$label))

    dfl <- split(df, f = as.character(df$variable))

    # sorting plots, leave only max_vars of features
    dfl <- dfl[as.character(perm$Category)[order(-perm$x)]]

    if (!is.null(max_vars)) {
      m <- max_vars
      dfl <- dfl[1:max_vars]
    }

    temp <- jsonlite::toJSON(list(dfl, label_list))

    # n - number of plots, m - number of bars
    options["n"] <- length(dfl)
    options$m <- unname(sapply(dfl, function(x) dim(x)[1]))

    r2d3::r2d3(data = temp, script = system.file("d3js/featureImportanceSplit.js", package = "ingredients"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "ingredients"),
                 system.file("d3js/d3-tip.js", package = "ingredients"),
                 system.file("d3js/hackHead.js", package = "ingredients")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "ingredients"),
         d3_version = 4,
         options = options)
  }
}

