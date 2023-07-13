#' Build Table One
#'
#' @param data a data frame to summarize
#' @param pretty.labels a named character vector where the names correspond to all
#'    column names of data except for the strata variable. Values correspond to the
#'    cleaned version of the variable names to be displayed in the summary table.
#' @return a tibble with summary statistics (mean/sd or n/%)
#' @export
#'
#' @examples
#' library(tableOne)
#' data(iris)
#' data <- iris
#' tableOne(
#'     iris,
#'     pretty.labels = c(
#'         Species = "Species",
#'         Sepal.Length = "Sepal Length (cm)",
#'         Sepal.Width = "Sepal Width (cm)",
#'         Petal.Length = "Petal Length (cm)"),
#'         Petal.Width = "Petal Width (cm)"))


tableOne_oneColumn <- function(data, pretty.labels){

	# model.matrix needs to retain nas, so we reset this option
	current.na.action <- options('na.action')
	options(na.action='na.pass')
	# Convert Characters to Factors,
	# Factors to dummy variables
	new.data <-
		lapply(
			data,
			function(variable){
				if(is.character(variable)){
					variable <- factor(variable)
				}
				if(is.factor(variable)){
					save.labels <- levels(variable)
					new.data <- model.matrix(~x-1, data = data.frame(x = variable))
					new.data <- as_tibble(new.data)
					colnames(new.data) <- paste0(save.labels, ", n(%)")
					return(new.data)
				}else{
					return(variable)
				}
			})%>%
		do.call(what = "cbind") %>% as_tibble()

	# reset na.action
	options(na.action=current.na.action)

	# Create Table of summary measures
	tab1 <-
		new.data %>%
		pivot_longer(cols = everything(),
					 names_to = "key",
					 values_to = "value") %>%
		group_by(key) %>%
		summarise(
			Mean = mean(value, na.rm = T),
			sd = sd(value,  na.rm = T),
			n = sum(value, na.rm = T),
			Missing = as.character(sum(is.na(value))),
			bin.var = all((1*value) %in% c(0:1,NA), na.rm=T)) %>%
		mutate(
			`(sd)` = ifelse(
				bin.var,
				paste0("(", 100*round(Mean,3), ")"),
				paste0("(", round(sd,1), ")")
			),
			Mean = as.character(ifelse(
				bin.var, n, round(Mean,1)
			)))%>%
		select(key, Mean, `(sd)`, Missing) %>%
		pivot_longer(Mean:Missing) %>%
		pivot_wider(names_from =c(name), values_from = value) %>%
		mutate_at(vars(ends_with("Missing")), as.numeric) %>%
		select(-ends_with("_Missing"))

	# Get correct ordering
	tab1$order<-
		sapply(
			tab1$key,
			function(name){
				if(name %in% names(pretty.labels)){
					which(name == names(pretty.labels))
				}else{
					NA
				}
			})

	# Get order for variables split by factor levels
	if(any(is.na(tab1$order))){
		tab1$order[is.na(tab1$order)]<-
			sapply(
				tab1$key[is.na(tab1$order)],
				function(name){
					if(any(sapply(names(pretty.labels), grepl, x=name))){
						which(sapply(names(pretty.labels), grepl, x=name))
					}
				})
	}

	tab1$order<- rank(tab1$order,ties.method = "first")
	tab1 <- arrange(tab1, order) %>% select(-order)

	# Re-Label With Pretty Names
	tab1$key<-
		sapply(
			tab1$key,
			function(name){
				if(name %in% names(pretty.labels)){
					pretty.labels[name == names(pretty.labels)]
				}else{
					name
				}
			})


	tab1$key<-
		sapply(
			tab1$key,
			function(name){
				if(any(sapply(names(pretty.labels), grepl, x=name))){
					which.label <- sapply(names(pretty.labels), grepl, x=name)
					prefix <- names(pretty.labels)[which.label]
					gsub(
						paste0(prefix,"\\."),
						paste0(pretty.labels[which.label], ": "),
						name)
				}else{
					name
				}
			})

	# Reformat Column Names to Include Sample Sizes
	colnames(tab1) <- gsub("_"," ", colnames(tab1))
	colnames(tab1)[1] <- ""

	colnames(tab1)[grepl("Mean",colnames(tab1))] <-
		paste0("Overall (n = ",nrow(new.data),") Mean")

	return(tab1)
}
