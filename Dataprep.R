df.degree <- read.csv("degrees-that-pay-back.csv", header=T)
colnames(df.degree)[4] <- "Percent from Start to Mid Career"
df.degree[,c(2,3,5:8)] <- apply(df.degree[,c(2,3,5:8)], 2, function(y) gsub(",", "", y))
df.degree[,c(2,3,5:8)] <- apply(df.degree[,c(2,3,5:8)], 2, function(y) gsub("\\$", "", y))
df.degree[,c(2,3,5:8)] <- as.numeric(unlist(df.degree[,c(2,3,5:8)]))
write.csv(df.degree, file="Degrees.csv", row.names=F)

df.type <- read.csv("salaries-by-college-type.csv", header=T)
df.type[,c(3:8)] <- apply(df.type[,c(3:8)], 2, function(y) gsub(",", "", y))
df.type[,c(3:8)] <- apply(df.type[,c(3:8)], 2, function(y) gsub("\\$", "", y))
df.type[,c(3:8)] <- as.numeric(unlist(df.type[,c(3:8)]))
df.type$Mid.Career.Med.Pow3 <- (df.type[,4]/1000)^3
df.type$Mid.Career.Legend <- paste0(df.type[,4]/1000,"K")
df.type$Salary.Growth <- (df.type[,4]^3/df.type[,3]-1)
df.type$Salary.Growth.Pow3 <- df.type$Salary.Growth^1.5
write.csv(df.type, file="Type.csv", row.names=F)

df.region <- read.csv("salaries-by-region.csv", header=T)
df.region[,c(3:8)] <- apply(df.region[,c(3:8)], 2, function(y) gsub(",", "", y))
df.region[,c(3:8)] <- apply(df.region[,c(3:8)], 2, function(y) gsub("\\$", "", y))
df.region[,c(3:8)] <- as.numeric(unlist(df.region[,c(3:8)]))
write.csv(df.region, file="Region.csv", row.names=F)

df.combined <- merge.data.frame(df.type, df.region, by=c(1))
df.combined <- df.combined[c(1,2,13,3:8,11)]
df.combined$Start.Bin <- cut(df.combined[,4], breaks =  quantile(df.combined[,4]),
                             labels=c("Q1 Start","Q2 Start","Q3 Start","Q4 Start"))
df.combined$Mid.Bin <- cut(df.combined[,5], breaks =  quantile(df.combined[,5]) ,
                           labels=c("Q1 Mid-Career","Q2 Mid-Career","Q3 Mid-Career","Q4 Mid-Career"))
write.csv(df.combined, file="Type and Region.csv", row.names = F)

df.type.median <- as.data.frame(matrix(data=c(
                                "Engineering","Ivy League","Liberal Arts", "Party","State",
                                tapply(df.type[,3], df.type$School.Type, median),
                               tapply(df.type[,4], df.type$School.Type, median),
                               tapply(df.type[,6], df.type$School.Type, median),
                               tapply(df.type[,7], df.type$School.Type, median)),
                             ncol=5, nrow=5))
colnames(df.type.median) <- colnames(df.type[,c(2,3,4,6,7)])
write.csv(df.type.median, file="Type Median of Median.csv", row.names=F)



