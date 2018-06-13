
library(ChainLadder, lib.loc ="C:\\ReservePrism\\ParallelAndGrid\\library")
library(lattice,lib.loc ="C:\\ReservePrism\\ParallelAndGrid\\library")
library(grid)
library(para)

setGeneric("triAccrCompare", function(A, B, ...)  standardGeneric("triAccrCompare"))
setMethod("triAccrCompare", signature("Triangle", "Triangle"), function(A, B, minimum=10, rowInterested=NULL,ignore_big=10000.0){
	if (!setequal(getAges(A), getAges(B)) && !setequal(getYears(A), getYears(B))) stop("Cannot minus two triangles with different years and ages")
	
	a<-Accumulate(A)
	b<-Accumulate(B)
	
	diff=(a@data-b@data)
	divided<-abs(a@data)

	#one way to deal with the last cells is to ignore them, this will happen to counts
	divided[divided<=1]<-1000000000
	
	diff=diff/divided
	diff[diff>=ignore_big]<-0
	if (!is.null(rowInterested)){diff<-diff[rowInterested,]}
	
	diff
})

SIMULATION_START_DATE<-"2001-01-01"
SIMULATION_END_DATE<-"2010-12-31"
Simulation1048731227<-new("simulation")
setNeedCopula(Simulation1048731227)<-FALSE
setPlanning(Simulation1048731227)<-TRUE
setEvaluationDate(Simulation1048731227)<-"2009-12-31"
Line900225180<-new("lineObj")

########################################################################################################
#Playing and adjusting (1)
#######################################################################################################
AnnualFrequency137599672<-c(new("pois", p1=500, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000),
			
			new("pois", p1=825, min=1, max=1000000),
			new("pois", p1=800, min=1, max=1000000),
			
			new("pois", p1=700, min=1, max=1000000),
			new("pois", p1=600, min=1, max=1000000))
names(AnnualFrequency137599672)<-as.character(c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010))
setObjName(Line900225180)<-"Line900225180"
setDescription(Line900225180)<-"GEN"
setAnnualFrequency(Line900225180)<-AnnualFrequency137599672

setExposure(Line900225180)<-dateHashWithValues(list(c(1, "2001-01-01")))
setTrend(Line900225180)<-dateHashWithValues(list(c(1, "2001-01-01")))
setSeasonality(Line900225180)<-c(1,1,1,1,1,1,1,1,1,1,1,1)
setAcc(Line900225180)<-new("multinom", p1=0, p2=c(1), min=1 , max=1000000)
setTypeRatio(Line900225180)<-list(c(1))
setC0(Line900225180)<-dateHashWithValues(list(c(1, "2001-01-01")))
setIsClaimMade(Line900225180)<-FALSE
Type1285289439<-new("typeObj")
setObjName(Type1285289439)<-"Type1285289439"
setDescription(Type1285289439)<-"LB"
setLine(Type1285289439)<-"Line900225180"

########################################################################################################
#Playing and adjusting (2) and (3)
#######################################################################################################
setReportLag(Type1285289439)<-dateHashWithValues(list(c(new("exp", p1=0.01, min=1, max=3650), "2001-01-01"),
							new("exp", p1=0.005, min=1, max=3650), "2005-01-01"),
							new("exp", p1=0.01, min=1, max=3650), "2006-01-01")))


#setPaymentLag(Type1285289439)<-dateHashWithValues(list(c(new("exp", p1=0.0015, min=1, max=7000), "2001-01-01")))
#weibull, p1=0.993157, p2=658.3868, 750.435800
setPaymentLag(Type1285289439)<-dateHashWithValues(list(c(new("weibull", p1=0.993175, p2=658.3868, min=1, max=7000), "2001-01-01")))


setRecoveryLag(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=3, p2=1, min=1, max=10000000), "2001-01-01")))
setValuationFreq(Type1285289439)<-3
setStartingMonth(Type1285289439)<-6
setNeedRecovery(Type1285289439)<-FALSE

########################################################################################################
#Playing and adjusting (4)
#######################################################################################################
setLossSize(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=8, p2=0.2, min=1, max=1000000), "2001-01-01"),
							c(new("lnorm", p1=7.9, p2=0.8, min=1, max=1000000), "2002-01-01"), 
							c(new("lnorm", p1=7.193911, p2=1.076850, min=1, max=1000000), "2004-01-01"), 
							c(new("lnorm", p1=8.457614, p2=0.579164, min=1, max=1000000), "2005-01-01"), 
							c(new("lnorm", p1=7.9, p2=0.8, min=1, max=1000000), "2006-01-01"), 
							c(new("lnorm", p1=8.568955, p2=0.537057, min=1, max=1000000), "2007-01-01"), 
							c(new("lnorm", p1=7.9, p2=0.8, min=1, max=1000000), "2006-01-01"),
							c(new("lnorm", p1=7.558966, p2=0.935832, min=1, max=1000000), "2009-01-01"), 
							c(new("lnorm", p1=7.697441, p2=0.959989, min=1, max=1000000), "2010-01-01")))

setReserve0(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=0, p2=0, min=0, max=10000000), "2001-01-01")))
setReserve40(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=0, p2=0, min=0, max=10000000), "2001-01-01")))
setReserve70(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=0, p2=0, min=0, max=10000000), "2001-01-01")))
setReserve90(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=0, p2=0, min=0, max=10000000), "2001-01-01")))
setLiability(Type1285289439)<-dateHashWithValues(list(c(0, "2001-01-01")))
Limit1003377047<-list(1)
names(Limit1003377047)<-c(10000000)
Deductible847014462<-new("Deductible", value=c(0,1), limit=new("Limit", hash=Limit1003377047))
LimitAndDeductible1167378488<-new("DL", value=list(Deductible847014462), totalclaim=0, sir=FALSE)
setLimitAndDeductible(Type1285289439)<-LimitAndDeductible1167378488
setTrend(Type1285289439)<-dateHashWithValues(list(c(1, "2001-01-01")))
setRatio(Type1285289439)<-c(1,1,1)
setAlpha(Type1285289439)<-0
setNeedCopula(Type1285289439)<-FALSE
setLossLagCopula(Type1285289439)<-new("copulaObj", family="normal", param=c(0), margins=list(new("lnorm", p1=8, p2=0.2, min=1, max=1000000),new("exp", p1=0.0015, min=1, max=7000) ), dispstr="un", df=5)
setFastTrack(Type1285289439)<-dateHashWithValues(list(c(0, "2001-01-01")))
setThreshold(Type1285289439)<-dateHashWithValues(list(c(0, "2001-01-01")))
setEstP0(Type1285289439)<-dateHashWithValues(list(c(0.4, "2001-01-01")))
setMinChange(Type1285289439)<-dateHashWithValues(list(c(100, "2001-01-01")))
setMinRelChange(Type1285289439)<-dateHashWithValues(list(c(0.01, "2001-01-01")))
setRecoveryLiability(Type1285289439)<-dateHashWithValues(list(c(1, "2001-01-01")))
setInitPaymentAdequacy(Type1285289439)<-dateHashWithValues(list(c(new("lnorm", p1=0.5, p2=0.05, min=1, max=10000000), "2001-01-01")))

########################################################################################################
#Playing and adjusting (5)
#######################################################################################################
setNumberOfPayments(Type1285289439)<-dateHashWithValues(list(c(new("geom", p1=0.4, min=1, max=15), "2001-01-01")))

setPaymentPattern(Type1285289439)<-2
t<-c(Type1285289439)
names(t)<-c("Type1285289439")
setTypes(Line900225180)<-t
t<-c(Line900225180)
names(t)<-c("Line900225180")
setLines(Simulation1048731227)<-t
t<-c(Type1285289439)
names(t)<-c("Type1285289439")
setTypes(Simulation1048731227)<-t
setClaimFile(Simulation1048731227)<-"C:\\temp\\GenIns\\Claims.csv"
setTransactionFile(Simulation1048731227)<-"C:\\temp\\GenIns\\Transactions.csv"
set.seed(16807)
setIteration(Simulation1048731227)<-1
startSimulation(Simulation1048731227, socketConnt=FALSE)

ResultContainer<-new("analysis")
setCovariateValue(ResultContainer)<-"NA"
setLineValue(ResultContainer)<-"NA"
setTypeValue(ResultContainer)<-"NA"
setAccountingDate(ResultContainer)<-"2011-12-31"
setEvaluationDate(ResultContainer)<-"2010-12-31"
setYearsAndAges(ResultContainer)<-list(c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010),c(12,24,36,48,60,72,84,96,108,120))
loadClaimFile(ResultContainer)<-"C:\\temp\\GenIns\\Claims.csv"
loadTransactionFile(ResultContainer)<-"C:\\temp\\GenIns\\Transactions.csv"
getAccidentYear(ResultContainer)
getAnnualFrequencyFromClaim(ResultContainer)
getUltimateLoss(ResultContainer, 1)
getUltimatePayment(ResultContainer, 1)
getReserve(ResultContainer, 1)
getIBNR_IBNER_Calculation(ResultContainer, 1)
getAccidentYear(ResultContainer)
getAnnualFrequencyFromClaim(ResultContainer)

ResultContainer@triangleviewtype=1
t<-generateTriangles(ResultContainer, 1)
triangle1751460428<-t[[1]]
triangle1751460428@data
triangle1751460428@accumulated
triangle1751460428@tagInfo

loss<-Accumulate(triangle1751460428)
mack<- MackChainLadder(loss@data, est.sigma="Mack")
mack


triangle2136249259<-new("Triangle", data=readTriangleCSVFile("C:\\temp\\GenIns\\GenIns.csv"), accumulated=FALSE, point=FALSE, rectangle=FALSE)
GenIns<-Accumulate(triangle2136249259)
mm<- MackChainLadder(GenIns@data, est.sigma="Mack")
mm

triAccrCompare(loss, GenIns)
avgAbsPctError(loss, GenIns)
loss@data
GenIns@data
toTriangleCSVFile(loss, "C:\\temp\\GenIns\\simulated.csv")



ResultContainer@triangleviewtype=2
t<-generateTriangles(ResultContainer, 1)
x<-Accumulate(t[[1]])
x@data
write.csv(x@data, file = "c:\\temp\\GenIns\\rec.csv", row.names = TRUE)

generateAggregatedClaims(ResultContainer, "C:\\temp\\GenIns\\aggregated.csv", 1, FALSE)





