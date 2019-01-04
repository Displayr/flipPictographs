#' Summarizes a numeric or categorical variable for plotting
#' @param x The input variable to summarize
#' @param type The type of summary statistic to output. This should be one of 'Average', 'Sum', 'Most frequent' or 'Percentage'.
#' @param subset Option logical vector of the same length as \code{x} indicating whether or not to include
#'   an observation in \code{x}.
#' @param weights Sampling or replication weights. This is a numeric vector of the same length as \code{x}.
#' @param category A comma-seperated list of the name or indices of the categories to include for 'Percentage'.
#' @importFrom flipStatistics Mean Sum WeightedTable
#' @importFrom flipTransformations AsNumeric TextAsVector
#' @export
SummarizeVariable <- function(x, type = c("Average", "Sum", "Percentage")[1], weights = NULL, subset = NULL, category = NULL)
{
    if (!is.null(weights) && length(weights) > 1 && length(weights) != length(x))
        stop("Weights should be the same length as the input variable")

    if (!is.null(subset) && length(subset) > 1)
    {
        if (length(subset) != length(x))
           stop("Filters should have the same length as the input variable")
        x <- x[subset]
        if (!is.null(weights))
            weights <- weights[subset]
    }

    if (grepl("Average", type) || grepl("Mean", type))
        return(Mean(AsNumeric(x, binary = FALSE), weights = weights))
    if (grepl("Sum", type))
        return(Sum(AsNumeric(x, binary = FALSE), weights = weights))
    if (grepl("Most frequent", type))
    {
        counts <- sort(WeightedTable(x, weights = weights), decreasing = TRUE)
        tmp <- names(counts)[1]
        if (is.numeric(x))
            return(as.numeric(tmp))
        else
            return(x[x == tmp][1])
    }

    if (is.null(weights))
        weights <- rep(1, length(x))
    total <- sum(weights, na.rm = TRUE)

	if (isTRUE(attr(x, "questiontype") == "PickAny") || is.logical(x))
    {
        if (sum(nchar(category), na.rm = TRUE))
            warning("Showing percentage selected (ignoring Category '",
                    category, "').")
        return(sum(weights * x)/total)

    }

	if (is.numeric(x) && sum(nchar(category) > 0, na.rm = TRUE))
	{
		if (grepl("\\d+\\s*-\\s*\\d+", category))
		{
			cat.range <- as.numeric(trimws(strsplit(category, split = "-")[[1]]))
			if (length(cat.range) == 2 && all(!is.na(cat.range)))
			{
				in.range <- x >= min(cat.range) & x <= max(cat.range)
				return(sum(weights * in.range)/total)
			}
		}
		else if (is.numeric(x) && any(x != round(x, 0)))
			warning("A numeric variable was supplied. Specify a range as the category (e.g. '1-5') or round variables to integers")
	}

    # Categorical variable
    counts <- WeightedTable(x, weights = weights)
    if (sum(nchar(category), na.rm = TRUE) == 0)
        stop("Select one or more categories from '", paste(names(counts), collapse="', '"), "'.")

    return(sum(counts[TextAsVector(as.character(category))], na.rm = TRUE)/total)
}
