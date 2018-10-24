library(data.table)  
library(Metrics)
library(forecast)
#for ARIMA package 
require(dplyr)

cat("reading the train and test data (with data.table) \n")
train <- fread("~/Downloads/train.csv",stringsAsFactors = T)
test  <- fread("~/Downloads/test.csv",stringsAsFactors = T)

#將train和test的資料載進去

train1 = train[train$Sales>0,]
# delete sales = 0  將所有sales是零的資料都先刪掉
preds=c('Store','DayOfWeek','Promo')
# make a table that only have columns names

mdl = train1 %>% group_by_(.dots=preds) %>% summarise(PredSales=exp(mean(log(Sales)))) %>% ungroup()

pred = train %>% left_join(mdl,by=preds) %>% rename(mSales=PredSales)
pred$mSales[is.na(pred$mSales)]=0
train <- pred

pred = test %>% left_join(mdl,by=preds) %>% rename(mSales=PredSales)
pred$mSales[is.na(pred$mSales)]=0
test <- pred

train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

train[,logSales:=log1p(Sales)]

ar_fit = function(x) {
  Sales <- ts(x$logSales)
  lambda <- BoxCox.lambda(Sales)
  tsclean(Sales, replace.missing = TRUE, lambda=lambda)
  xreg <- cbind(DayOfWeek = x$DayOfWeek , 
                Open = x$Open,
                Promo = x$Promo,
                StateHoliday = x$StateHoliday,
                SchoolHoliday = x$SchoolHoliday,
                mSales = x$mSales
  )
  fit <- auto.arima(Sales, xreg=xreg)
  return(fit)
}


#results <- data.frame(Date=as.Date("01/01/2000", format="%m/%d/%Y"), Store=NA, Sales=0, predSales=0, stringsAsFactors=FALSE)
results <- data.frame(Id=NA, Sales=NA)

for (store in 1:1115) {
  strn <- train[Store == store]
  stst <- test[Store == store]
  stst$Open[is.na(stst$Open)] <- 1
  cat(sprintf("Store %d has training %d rows and test %d rows\n", 
              store, nrow(strn), nrow(stst)  ))
  if (nrow(stst) > 0) { 
    strn <- strn[order(Date)]
    stst <- stst[order(Date)]
    max_sales <- max(strn$Sales, na.rm = TRUE)
    out <- ar_fit(strn)
    xreg1 <- cbind(DayOfWeek = stst$DayOfWeek , 
                   Open = stst$Open,
                   Promo = stst$Promo,
                   StateHoliday = stst$StateHoliday,
                   SchoolHoliday = stst$SchoolHoliday,
                   mSales = stst$mSales
    )
    pred <- data.frame(forecast(out, xreg=xreg1, lambda=out$lambda))
    
    pred$Point.Forecast <- expm1(pred$Point.Forecast)
    pred$Point.Forecast <- ifelse(pred$Point.Forecast < 0, 0, pred$Point.Forecast)
    pred$Point.Forecast <- ifelse(pred$Point.Forecast > max_sales, max_sales, pred$Point.Forecast)

    stst[,predSales:=round(Open * pred$Point.Forecast, digits=0)]
  }
  results <- rbind(results, data.frame(Id=stst$Id, Sales=stst$predSales))
}
results <- na.omit(results)
results <- results[order(results$Id),]
write.csv(results, "rossmann_arima_Sub1_600.csv",row.names=F)



