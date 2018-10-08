#' Summarizes a numeric or categorical variable for plotting
#' @param x The input variable to summarize
#' @param type The type of summary statistic to output. This should be one of 'Average', 'Sum' or 'Percentage'.
#' @param subset Option logical vector of the same length as \code{x} indicating whether or not to include
#'   an observation in \code{x}.
#' @param weights Sampling or replication weights. This is a numeric vector of the same length as \code{x}.
#' @param category A comma-seperated list of the name or indices of the categories to include for 'Percentage'.
#' @importFrom flipStatistics Mean Sum WeightedTable
#' @importFrom flipTables SelectEntry
SummarizeVariable <- function(x, type = c("Average", "Sum", "Percentage")[1], weights = NULL, subset = NULL, category = NULL)
{
    if (!is.null(subset) && length(subset) > 1)
    {
        if (length(subset) != length(x))
           stop("Filters should have the same length as the input variable")
        x <- x[subset]
        weights <- weights[weights]
    }

    if (grepl("Average", type))
        return(Mean(x, weights = weights))
    if (grepl("Sum", type))
        return(Sum(x, weights = weights))

    if (is.null(weights))
        weights <- rep(1, length(weights))
    total <- sum(weights, na.rm = TRUE)

    # Pick any questions are encoded as 0 and 1s
    if (is.numeric(x))
        return(sum(weights * x)/total)

    # Categorical variable
    counts <- WeightedTable(x, weights = weights)
    if (is.null(category) || is.na(category))
        stop("Select categories from '", paste(names(counts), collapse="', '"), "'.")

    return(sum(SelectEntry(counts, category))/total)
}
