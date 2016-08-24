
datas <- read.table("box.csv", row.names=1)
datas <- tr(datas)
boxplot(datas[,3])
