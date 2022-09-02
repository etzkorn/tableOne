#' Build Table One
#'
#' @param data a data frame to summarize
#' @param strata.variable a single character naming the column in the data frame which contains a character/factor of the strata
#' @param pretty.labels a named character vector where the names correspond to all
#'    column names of data except for the strata variable. Values correspond to the
#'    cleaned version of the variable names to be displayed in the summary table.
#' @return a tibble with summary statistics (mean/sd or n/%)
#' @export
#'
#' @examples
#' data(iris)
#' tableOne(
#'     iris,
#'     strata = "Species",
#'     pretty.labels = c(
#'         Sepal.Length = "Sepal Length (cm)",
#'         Sepal.Width = "Sepal Width (cm)",
#'         Petal.Length = "Petal Length (cm)"),
#'         Petal.Width = "Petal Width (cm)"))

tableOne <- function(data, strata.variable, pretty.labels){
	strata <- data[[strata.variable]]

	new.data <-
	lapply(
		select(data,-matches(strata.variable)),
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
		})
	#names(new.data)[sapply(new.data, is_tibble)]<- ""

	new.data <- do.call(new.data, what = "cbind") %>% as_tibble()
	new.data$strata.variable <- strata

	tab1 <-
	new.data %>%
	pivot_longer(!strata.variable,names_to = "key", values_to = "value") %>%
	group_by(strata.variable,key) %>%
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
	select(strata.variable, key, Mean, `(sd)`, Missing) %>%
	pivot_longer(Mean:Missing) %>%
	pivot_wider(names_from =c(strata.variable, name), values_from = value) %>%
    mutate_at(vars(ends_with("Missing")), as.numeric) %>%
	mutate(missing = rowSums(across(ends_with("Missing")))) %>%
	select(-ends_with("_Missing"))

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

	colnames(tab1) <- gsub("_"," ", colnames(tab1))
	return(tab1)
}
