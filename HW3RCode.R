#load data
	library(data.table)
	intel=read.csv("IntelData.csv",header=TRUE)
	
#Split data into training and testing
	intel=intel[,c(2,3,4)]
	intel.train=intel[1:(nrow(intel)-4),]
	intel.test=intel[(nrow(intel)-3):nrow(intel),]

#	#	#	Question 1	#	#	#	
#Original time series
	plot(intel.train[,1],xlab="Weeks",ylab="Product 1",type="l")
	savePlot("1_original_time_series_product1.png",type="png")
	plot(intel.train[,2],xlab="Weeks",ylab="Product 2",type="l")
	savePlot("1_original_time_series_product2.png",type="png")
	plot(intel.train[,3],xlab="Weeks",ylab="Product 3",type="l")
	savePlot("1_original_time_series_product3.png",type="png")

#Transformed time series
	plot(diff(log(intel.train[-c(1,2,3),1])),xlab="Weeks",ylab="1 Order Difference After Log-Transformation: Product 1",type="l")
	savePlot("1_transformed_time_series_product1.png",type="png")
	plot(diff(log(intel.train[-c(1,2,3),2])),xlab="Weeks",ylab="1 Order Difference After Log-Transformation: Product 2",type="l")
	savePlot("1_transformed_time_series_product2.png",type="png")
	plot(diff(log(intel.train[-c(1,2,3),3])),xlab="Weeks",ylab="1 Order Difference After Log-Transformation: Product 3",type="l")
	savePlot("1_transformed_time_series_product3.png",type="png")





#	#	#	Question 2: Univariate Analysis	#	#	#	
	dlogintel.train1=diff(log(intel.train[-c(1,2,3),1]))
	dlogintel.train2=diff(log(intel.train[-c(1,2,3),2]))
	dlogintel.train3=diff(log(intel.train[-c(1,2,3),3]))

	#Model selection
		#ACF/PACF

			##Product 1
			acf(dlogintel.train1,main="ACF Product 1")
			savePlot("2_acf_product1.png",type="png")
			pacf(dlogintel.train1,main="PACF Product 1")
			savePlot("2_pacf_product1.png",type="png")

			##Product 2
			acf(dlogintel.train2,main="ACF Product 2")
			savePlot("2_acf_product2.png",type="png")
			pacf(dlogintel.train2,main="PACF Product 2")
			savePlot("2_pacf_product2.png",type="png")

			##Product 3
			acf(dlogintel.train3,main="ACF Product 3")
			savePlot("2_acf_product3.png",type="png")
			pacf(dlogintel.train3,main="PACF Product 3")
			savePlot("2_pacf_product3.png",type="png")

		#EACF
			library(TSA)
			eacf(dlogintel.train1)
			eacf(dlogintel.train2)
			eacf(dlogintel.train3)

		#AIC
			select.ARMA=function(ts,norder)
			{
				n = length(ts)
				p = c(1:norder)-1; q = c(1:norder)-1
				aic = matrix(0,norder,norder)

				for(i in 1:norder)
				{
					for(j in 1:norder)
					{
						if((p[i]+q[j])>0)
						{
							modij = arima(ts,order = c(p[i],0,q[j]), method='ML')
							aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
						}
					}  
				}

				aicv = as.vector(aic) 
				indexp = rep(c(1:norder),norder)
				indexq = rep(c(1:norder),each=norder)
				aicv=aicv[-1]
				indexp=indexp[-1]
				indexq=indexq[-1]
				indexaic = which(aicv == min(aicv))
				porder = indexp[indexaic]-1
				qorder = indexq[indexaic]-1
			
				return(list(porder=porder,qorder=qorder))
			}
   
			select.ARMA(dlogintel.train1,3)
			select.ARMA(dlogintel.train2,3)
			select.ARMA(dlogintel.train3,3)


	#Model Fitting

		#Product 1
			mod.prod1=arima(log(intel.train[-c(1,2,3),1]),order = c(0,1,1), method='ML')
			mod.prod1
			c(mod.prod1$coef-1.96*sqrt(mod.prod1$var.coef),mod.prod1$coef+1.96*sqrt(mod.prod1$var.coef))
		#Product 2
			mod.prod2=arima(log(intel.train[-c(1,2,3),2]),order = c(0,1,1), method='ML')
			mod.prod2
			c(mod.prod2$coef-1.96*sqrt(mod.prod2$var.coef),mod.prod2$coef+1.96*sqrt(mod.prod2$var.coef))
		#Product 3
			mod.prod3=arima(log(intel.train[-c(1,2,3),3]),order = c(1,1,0), method='ML')
			mod.prod3
			c(mod.prod3$coef-1.96*sqrt(mod.prod3$var.coef),mod.prod3$coef+1.96*sqrt(mod.prod3$var.coef))


	#Model Forecasting

		#Prodcut 1
			pred_prod1 = as.vector(predict(mod.prod1,n.ahead=4))
			par(mfrow=c(1,1))
			plot(seq(70,82),log(intel[70:82,1]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 1")
			points(seq(79,82),pred_prod1$pred,col="red")
			lines(seq(79,82),pred_prod1$pred+1.96*pred_prod1$se,lty=3,lwd= 2, col="blue")
			lines(seq(79,82),pred_prod1$pred-1.96*pred_prod1$se,lty=3,lwd= 2, col="blue")
			savePlot("2_predictions_product1.png",type="png")

		#Prodcut 2
			pred_prod2 = as.vector(predict(mod.prod2,n.ahead=4))
			par(mfrow=c(1,1))
			plot(seq(70,82),log(intel[70:82,2]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 2")
			points(seq(79,82),pred_prod2$pred,col="red")
			lines(seq(79,82),pred_prod2$pred+1.96*pred_prod2$se,lty=3,lwd= 2, col="blue")
			lines(seq(79,82),pred_prod2$pred-1.96*pred_prod2$se,lty=3,lwd= 2, col="blue")
			savePlot("2_predictions_product2.png",type="png")

		#Prodcut 3
			pred_prod3 = as.vector(predict(mod.prod3,n.ahead=4))
			par(mfrow=c(1,1))
			plot(seq(70,82),log(intel[70:82,3]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 3")
			points(seq(79,82),pred_prod3$pred,col="red")
			lines(seq(79,82),pred_prod3$pred+1.96*pred_prod3$se,lty=3,lwd= 2, col="blue")
			lines(seq(79,82),pred_prod3$pred-1.96*pred_prod3$se,lty=3,lwd= 2, col="blue")
			savePlot("2_predictions_product3.png",type="png")



#	#	#	Question 3: Multivariate Analysis	#	#	#	

	#VAR Model
		library(vars)
		dlogintel.train=cbind(dlogintel.train1,dlogintel.train2,dlogintel.train3)

		#Model Selection
			VARselect(dlogintel.train, lag.max = 10, type ="none")

		#Model Fitting and Diagnosis
			model.var=VAR(dlogintel.train, p=2, type ="none")
			summary(model.var)

	#Diagnostics
		arch.test(model.var)
		normality.test(model.var)
		serial.test(model.var)


	#Model Forecasting
		pred.var=predict(model.var,n.ahead=4)$fcst

		par(mfrow=c(1,1))

		#Product 1
		dlogprod1_fsct=diffinv(pred.var$dlogintel.train1[,1],xi=log(intel.train[78,1]))
		dlogprod1_lower=diffinv(pred.var$dlogintel.train1[,2],xi=log(intel.train[78,1]))
		dlogprod1_upper=diffinv(pred.var$dlogintel.train1[,3],xi=log(intel.train[78,1]))

		plot(seq(70,82),log(intel[70:82,1]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 1")
		points(seq(79,82),dlogprod1_fsct[-1],col="red")
		lines(seq(79,82),dlogprod1_lower[-1],lty=3,lwd= 2, col="blue")
		lines(seq(79,82),dlogprod1_upper[-1],lty=3,lwd= 2, col="blue")
		savePlot("3_var_predictions_product1.png",type="png")

		#Product 2
		dlogprod2_fsct=diffinv(pred.var$dlogintel.train2[,1],xi=log(intel.train[78,2]))
		dlogprod2_lower=diffinv(pred.var$dlogintel.train2[,2],xi=log(intel.train[78,2]))
		dlogprod2_upper=diffinv(pred.var$dlogintel.train2[,3],xi=log(intel.train[78,2]))

		plot(seq(70,82),log(intel[70:82,2]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 2")
		points(seq(79,82),dlogprod2_fsct[-1],col="red")
		lines(seq(79,82),dlogprod2_lower[-1],lty=3,lwd= 2, col="blue")
		lines(seq(79,82),dlogprod2_upper[-1],lty=3,lwd= 2, col="blue")
		savePlot("3_var_predictions_product2.png",type="png")

		#Product 3
		dlogprod3_fsct=diffinv(pred.var$dlogintel.train3[,1],xi=log(intel.train[78,3]))
		dlogprod3_lower=diffinv(pred.var$dlogintel.train3[,2],xi=log(intel.train[78,3]))
		dlogprod3_upper=diffinv(pred.var$dlogintel.train3[,3],xi=log(intel.train[78,3]))

		plot(seq(70,82),log(intel[70:82,3]),type="l", xlab="Week", ylab="Log(Sales)",ylim=c(-2,6),main="Product 3")
		points(seq(79,82),dlogprod3_fsct[-1],col="red")
		lines(seq(79,82),dlogprod3_lower[-1],lty=3,lwd= 2, col="blue")
		lines(seq(79,82),dlogprod3_upper[-1],lty=3,lwd= 2, col="blue")

		savePlot("3_var_predictions_product3.png",type="png")


#	#	#	Question 4: Model Comaprison   	#	#	#	


	#Model Comparison
		logprod.test=log(intel.test)

	#ARIMA model
		mean(abs(logprod.test[,1]-as.numeric(pred_prod1$pred)))
		mean((logprod.test[,1]-as.numeric(pred_prod1$pred))^2)

		mean(abs(logprod.test[,2]-as.numeric(pred_prod2$pred)))
		mean((logprod.test[,2]-as.numeric(pred_prod2$pred))^2)

		mean(abs(logprod.test[,3]-as.numeric(pred_prod3$pred)))
		mean((logprod.test[,3]-as.numeric(pred_prod3$pred))^2)

	#VAR model
		mean(abs(logprod.test[,1]-dlogprod1_fsct[-1]))
		mean((logprod.test[,1]-dlogprod1_fsct[-1])^2)

		mean(abs(logprod.test[,2]-dlogprod2_fsct[-1]))
		mean((logprod.test[,2]-dlogprod2_fsct[-1])^2)

		mean(abs(logprod.test[,3]-dlogprod3_fsct[-1]))
		mean((logprod.test[,3]-dlogprod3_fsct[-1])^2)
