#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import patchwork
#' @importFrom scales percent
NULL  # <- this line tells roxygen the imports are not attached to any function

#' Plot a bar graph of the proportion of insects classified as moths vs non-moths. Choose whether to use
#' the moth/non-moth classifier and/or the insect taxonomic order classifier.
#'
#' @param dataframe Results dataframe
#' @param moth_nonmoth_classifier TRUE or FALSE depending on whether users wants plot for the moth vs non-moth classifier
#' @param order_classifier TRUE or FALSE depending on whether users wants plot for the order classifier
#' @param conf_threshold Numeric value between 0 and 1 giving the confidence threshold used to split moth classifications
#'  into high- and low-confidence groups (default 0.8).
#'
#' @return A  barplot showing the proportion of moths to non-moths in the dataset
#'
#' @export
plot_moths_vs_nonmoths <- function(dataframe, moth_nonmoth_classifier = TRUE, order_classifier = TRUE, conf_threshold = 0.8){

  plots <- list()

  if (moth_nonmoth_classifier) { # == TRUE
    # Plot a visualisation of the proportion of detections classified as a moth (class_name == "moth") or a non-moth by the moth_non_moth_classifier.
    # I'd like to see a breakdown in confidence of those classified as a moth e.g., proportion above 0.8 confidence (class_confidence >= 0.8)

    high_label <- paste0("Moth (\u2265 ", conf_threshold, " confidence)")
    low_label  <- paste0("Moth (< ",  conf_threshold, " confidence)")

    moth_nm_summary <- dataframe %>%
      mutate(
        category = case_when(
          class_name == "moth" & class_confidence >= conf_threshold ~ high_label,
          class_name == "moth" & class_confidence <  conf_threshold ~ low_label,
          TRUE                                                      ~ "Non-moth"
        ),
        category = factor(
          category,
          levels = c(
            high_label,
            low_label,
            "Non-moth"
          )
        )
      ) %>%
      count(category) %>%
      mutate(
        proportion = n / sum(n),
        classifier = "Moth / non-moth classifier"
      )

    plots$moth_nomoth <- ggplot(
      moth_nm_summary,
      aes(x = classifier, y = proportion, fill = category)
    ) +
      geom_col(width = 0.6) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = NULL,
        y = "Proportion of detections",
        fill = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18)
      )

  }

  if (order_classifier) { # == TRUE
    # Plot a visualisation of the proportion of detections classified as a moth (order_name %in% c("Lepidoptera Macros", "Lepidoptera Micros")) or a non-moth by the order_classifier.
    # I'd like to see a breakdown in confidence of those classified as a moth e.g., proportion above 0.8 confidence (order_confidence >= 0.8)

    high_label <- paste0("Moth (\u2265 ", conf_threshold, " confidence)")
    low_label  <- paste0("Moth (< ",  conf_threshold, " confidence)")

    order_summary <- dataframe %>%
      mutate(
        category = case_when(
          order_name %in% c("Lepidoptera Macros", "Lepidoptera Micros") &
            order_confidence >= conf_threshold ~ high_label,
          order_name %in% c("Lepidoptera Macros", "Lepidoptera Micros") &
            order_confidence <  conf_threshold ~ low_label,
          TRUE                                 ~ "Non-moth"
        ),
        category = factor(
          category,
          levels = c(
            high_label,
            low_label,
            "Non-moth"
          )
        )
      ) %>%
      count(category) %>%
      mutate(
        proportion = n / sum(n),
        classifier = "Order classifier"
      )

    plots$order <- ggplot(
      order_summary,
      aes(x = classifier, y = proportion, fill = category)
    ) +
      geom_col(width = 0.6) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = NULL,
        y = "Proportion of detections",
        fill = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18)
      )
  }

  # If both true, plot 2 plots next to each other
  # Else, just return the one

  if (length(plots) == 2) {
    return(plots$moth_nomoth + plots$order)
  }

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  stop("At least one classifier must be TRUE")

}

#' Filter the data  to only include predictions where either the moth/non-moth OR order classifier predicts its a moth with relatively high confidence (>0.8)
#' @param dataframe Results dataframe
#' @param conf_threshold Numeric value between 0 and 1 giving the confidence threshold used to filter the data (default 0.8).
#'
#' @return A filtered dataframe containing predictions where classifiers are confident it is a moth
#'
#' @export
moths_only <- function(dataframe, conf_threshold = 0.8){

  filtered_df <- dataframe %>%
    filter((class_name == "moth" & class_confidence >= conf_threshold) |
             (order_name %in% c("Lepidoptera Macros", "Lepidoptera Micros") & order_confidence >= conf_threshold))

  return(filtered_df)

}


