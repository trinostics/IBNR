library(excelRio)
x <- pasteFromExcel(header = TRUE, rowheader = TRUE)

library(ChainLadder)
atax <- ata(x)
copyToExcel(atax)
MackChainLadder(x)
