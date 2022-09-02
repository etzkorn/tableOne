#' Title
#'
#' @return
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

#
# df <- read.csv("~/OneDrive - Johns Hopkins/SPRING_Accelerometry_Analysis_Etzkorn/Data/SPRING_PA_20220817.csv")
# df <- dplyr::select(df, study, n_days:mean_active_bout)
# df$study2 <- df$study
# strata.variable <- "study2"
# data <- df
#
# pretty.labels <-
# 	c(
# 		study = "Study",
# 		n_days = "Recorded Days (n)",
# 		n_valid_days ="Valid Recorded Days (n)",
# 		wear_time_on_valid_days = "Daily Wear Time (m)",
# 		tac="TAC (counts)",
# 		tlac="TLAC (log counts)",
# 		ltac="LTAC (log counts)",
# 		astp="ASTP",
# 		satp="SATP",
# 		time_spent_active="Daily Active Time (m)",
# 		time_spent_nonactive="Daily Inactive Time (m)",
# 		no_of_active_bouts="Daily Active Bouts (n)",
# 		no_of_nonactive_bouts="Daily Inactive Bouts (n)",
# 		mean_active_bout="Duration Active Bouts (m)"
# 	)
#
#
# 	tab1 <- tab1[sapply(tab.order, function(x) which(grepl(x, tab1$key))),]
# 	tab1$key <- tab.names

# # Format Table 1
# gt(tab1, rowname_col = "key") %>%
# tab_header(title = md("**Table 1:** Patient Characteristics by Shunt Status (n = 169)"))%>%
# cols_label(
# 		mean = html("Mean"),
# 		missing = html("Missing (n)")
# 	)%>%
# cols_align(align = "right",columns = c(2)
# 	)
#
#
#
# # Format Table 1
# bind_cols(tab1, select(tab2,-key)) %>%
# mutate(missing = ifelse(missing==0,"",missing)) %>%
# gt(rowname_col = "key") %>%
# 	tab_header(title = md("**Table 1:** Patient Characteristics by Diagnosis of Normal Pressure Hydrocephalus (n = 224)"))%>%
# 	cols_label(
# 		iqr2 = html("(sd)"),
# 		iqr = html("(sd)"),
# 		Shunt = html("NPH (n=71)"),
# 		`No Shunt` = html("No NPH (n=153)"),
# 		missing = html("Missing (n)"),
# 		mean = html("Mean")
# 	)%>%
# 	cols_align(align = "right",columns = c(2, 5,7)
# 	)%>%
# 	cols_align(align = "center",columns = c(4,9)
# 	) %>%
# tab_style(
# 	style = list(
# 		cell_borders(sides = "right")
# 	),
# 	locations = list(
# 		cells_body(
# 			columns = vars(missing)
# 		)
# 	)
# ) %>%
# tab_footnote(footnote = "P-Values were generated using a Wilcoxon test for continuous variables and a chi-square test for binary variables.",
# 	cells_column_labels(9)
# )
