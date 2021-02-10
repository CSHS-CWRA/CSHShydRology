#'Estimate the Probable Maximum Precipitation based on Herschfield Methodology
#'
#'PMP Estimation based on Hershfield methodology from Daily values
#'@param val.max        Vector with annual maximum.
#'@param input.hr      PMP to be defined 24 hrs, 6 hrs or 1 hr.
#'@param area.km     Watershed area. This methodology is for Western US. By default, if the correction is not needed is recommended a value of 1.
#'@param number.of.obs.units How many observation units are implemented. ie Daily info for 24 hrs PMP is 1.
#'@param graph    True/False if the graphical output is needed.
#'
#'@return
#' List Elements:
#' \itemize{
#' \item values - maximum values used in the analysis
#' \item n - ammount of years
#' \item correction - data.frame with the parameters and corrections for average and St. Dev.
#' \item Km - corrections for rainfal duration and mean annual series (based on Figure 4.1)
#' \item PMP - probably maximum precipitation estimated. This value does not include correction from Figure 4.7, which is aplicable to Western US.
#' \item PMP.with.area.correction.for.West.US - PMP value corrected based on watershed area based on Figure 4.7
#' }
#' Please note that data from NA are removed in the analysis.
#'@examples
#' ## EX 1: PMP for the daily maximum values for a 24 hours PMP in an area of 200km2 (Optional for Western US).
#' ## Random 30 number from 60 to 150
#' PMP(val.max = runif(30,min = 60,150),input.hrs = 24,number.of.obs.unit = 1,area.km = 200,graphs = T)
#' @references [WMO] World Meteorological Organization (2009), Manual on Estimation of Probable Maximum Precipitation (PMP) WMO-No 1045
#'
#'@author Victor Munoz
#'@export
PMP<-function(val.max,input.hrs=24,number.of.obs.unit=1,area.km=1,graphs=T){

        if(graphs==T){

                require(ggplot2)
                require(gridExtra)

        }

        if(is.na(match(input.hrs,c(24,6,1)))){
                return(print("input.hrs should be 24, 6 or 1 hr."))
        }


        if(number.of.obs.unit<=0){
                return(print("number.of.obs.unit should be positive"))
        }


        if(!round(number.of.obs.unit)-number.of.obs.unit==0){
                return(print("number.of.obs.unit should be integer"))
        }


        Max.year<-val.max

        #remove NA
        X.n<-Max.year[!is.na(Max.year)]

        X.max.location<-match(max(X.n),X.n)

        X.n_m<-X.n[-X.max.location]

        n_PMP<-length(X.n)
        n_PMP<<-n_PMP
        X<-list(c(X.n),c(X.n_m))

        #Estimations of mean and sd
        X.mean<-lapply(X,mean)
        X.sd<-lapply(X,sd)

        Xn_m_div<-X.mean[[2]]/X.mean[[1]]
        Sn_m_div<-X.sd[[2]]/X.sd[[1]]

        #Fig 4.2
        Fig_4.2f<-function(n_PMP,Xn_m_div){
                ((0.0018*n_PMP^2 - 0.2037*n_PMP + 106.53)*
                         Xn_m_div+-0.00005945*n_PMP^3 + 0.00758*n_PMP^2 - 0.3111*n_PMP + 4.864)/100}

        Fig_4.2<-Fig_4.2f(n_PMP = n_PMP,Xn_m_div = Xn_m_div)


        #Fig 4.3
        Fig_4.3f<-function(n_PMP,Sn_m_div){
                (Sn_m_div*(134.14*n_PMP^-0.053)+(+-0.0006781*n_PMP^2 + 0.01089*n_PMP + 0.3812))/100}

        Fig_4.3<-Fig_4.3f(n_PMP,Sn_m_div)

        #Fig 4.4
        Fig_4.4m<-function(n_PMP){
                #Expression over 50 years in smaller than 1 - Corrected with max
                pmax(1,(99.7196936977294+(107.866021487998-99.7196936977294)/
                                (1+(n_PMP/13.0261452397594)^2.25690664033008))/100)
        }

        Fig_4.4s<-function(n_PMP){
                #Expression over 50 years in smaller than 1 - Corrected with max
                pmax(1,(741.591367770118*exp(-n_PMP/2.63780903040506)+24.8370732678786*
                                exp(-n_PMP/20.9101626977193)+97.9173078131786)/100)

        }

        Fig_4.4<-c(Fig_4.4m(n_PMP),Fig_4.4s(n_PMP))

        #Fig 4.1
        Km.f.24<-function(X.mean){
                0.0000000001638*X.mean^4 - 0.0000002397*X.mean^3 + 0.0001439*X.mean^2 - 0.05951*X.mean + 19.98}
        Km.f.6<-function(X.mean){
                -0.000000000038701397*X.mean^5 + 0.000000031987514*X.mean^4 - 0.000010135438*X.mean^3 +
                        0.0015812728*X.mean^2 - 0.16896843*X.mean + 19.951655}
        Km.f.1<-function(X.mean){
                +-0.0000086920029*X.mean^3 + 0.0023070606*X.mean^2 - 0.29365613*X.mean + 19.971686}

        if (input.hrs==24){Km.f<-Km.f.24}

        if (input.hrs==6){Km.f<-Km.f.6}

        if (input.hrs==1){Km.f<-Km.f.1}

        Km<-Km.f(X.mean[[1]])

        X.n_adj<-X.mean[[1]]*Fig_4.2*Fig_4.4[1]
        S.n_adj<-X.sd[[1]]*Fig_4.3*Fig_4.4[2]

        PMP1<-X.n_adj+Km*S.n_adj

        #Fig 4.5
        nou.f<-function(number.of.obs.unit){
                (100.2766207711+(684.0989213113-100.2766207711)/
                         (1+(number.of.obs.unit/0.03150054213)^1.116899589012))/100}

        PMP2<-PMP1*nou.f(number.of.obs.unit)

        #Fig 4.7
        area_corr.f.24<-function(area.km){
                (-1.13762590487202E-13*area.km^5+3.43734418674257E-10*area.km^4+
                         -4.00454902021794E-07*area.km^3+0.000226801496747288*area.km^2+
                         -0.0710318092970015*area.km+101.210146730303)/100}
        area_corr.f.6<-function(area.km){
                (+-901.2243442608+(113.7869909952--901.2243442608)/
                         (1+(area.km/1796029203.58)^0.2343007717958))/100}

        area_corr.f.1<-function(area.km){
                (47.13563121645+(126.0011250857-47.13563121645)/
                         (1+(area.km/69.77177029402)^0.5654945864788))/100}

        if (input.hrs==24){area_corr<-area_corr.f.24(area.km)}

        if (input.hrs==6){area_corr<-area_corr.f.6(area.km)}

        if (input.hrs==1){area_corr<-area_corr.f.1(area.km)}

        #For small area is corrected to a a maximum of 1
        area_corr<-pmin(area_corr,1)

        PMP3<-PMP2*area_corr

        ####FIGURES######
        if(graphs==T){


                #Figure 4.2 - GGPLOT
                n_graph<-c(10,15,20,30,50)
                Xn_m_graph<-seq(0.7,1,0.05)

                Fig_4.2.df<-data.frame(Xn_m=rep(Xn_m_graph,5),
                                       n_graph=as.character(rep(n_graph,each=7)),
                                       Xn_adj=Fig_4.2f(n_PMP=rep(n_graph,each=7),Xn_m_div = rep(Xn_m_graph,5)))

                Fig_4.2.site<-data.frame(Xn_m=Xn_m_div,n_graph=paste(n_PMP,"years"),Xn_adj=Fig_4.2)


                Fg4.2<-ggplot2::ggplot(data=Fig_4.2.df,aes(x=Xn_m,y=Xn_adj))+
                        ggplot2::geom_line(aes(color=n_graph,group=n_graph))+
                        ggplot2::scale_x_continuous(breaks=seq(0.7,1,0.05))+
                        ggplot2::scale_y_continuous(breaks=seq(0.7,1.1,0.05))+
                        ggplot2::scale_color_discrete()+
                        ggplot2::labs(x="Xn-m/Xn",y="Xn adjustment factor",color="Length of\nrecord\n[yrs]",
                                      title="Figure 4.2 - WMO No 1045 (2009)",
                                      subtitle="Adjust.of mean annual series for maximum observed rainfall")+
                        ggplot2::geom_point(data=Fig_4.2.site,aes(x=Xn_m,y=Xn_adj),size=3,color="red")+
                        ggplot2::geom_text(data=Fig_4.2.site,aes(x=Xn_m,y=Xn_adj,label=n_graph),vjust=-0.5)



                #Figure 4.3 - GGPLOT
                n_graph<-c(10,15,20,30,50)
                Sn_m_graph<-seq(0.2,1,0.05)

                Fig_4.3.df<-data.frame(Sn_m=rep(Sn_m_graph,5),
                                       n_graph=as.character(rep(n_graph,each=17)),
                                       Sn_adj=Fig_4.3f(n_PMP=rep(n_graph,each=17),Sn_m_div = rep(Sn_m_graph,5)))

                Fig_4.3.site<-data.frame(Sn_m=Sn_m_div,n_graph=paste(n_PMP,"years"),Sn_adj=Fig_4.3)


                Fg4.3<-ggplot2::ggplot(data=Fig_4.3.df,aes(x=Sn_m,y=Sn_adj))+
                        ggplot2::geom_line(aes(color=n_graph,group=n_graph))+
                        ggplot2::scale_x_continuous(breaks=seq(0.2,1,0.1))+
                        ggplot2::scale_y_continuous(breaks=seq(0.2,1.2,0.1))+
                        ggplot2::scale_color_discrete()+
                        ggplot2::labs(x="Sn-m/Sn",y="Sn adjustment factor",color="Length of\nrecord\n[yrs]",
                                      title="Figure 4.3 - WMO No 1045 (2009)",
                                      subtitle="Adjust. of SD of annual series for maximum observed rainfall")+
                        ggplot2::geom_point(data=Fig_4.3.site,aes(x=Sn_m,y=Sn_adj),size=3,color="red")+
                        ggplot2::geom_text(data=Fig_4.3.site,aes(x=Sn_m,y=Sn_adj,label=n_graph),vjust=-0.5)


                #Figure 4.4 - GGPLOT
                n_graph<-seq(10,max(n_PMP,50),length.out = 41)

                Fig_4.4.df<-data.frame(n_graph=rep(n_graph,time=2),
                                       Xn_Sn=c(Fig_4.4m(n_graph),Fig_4.4s(n_graph)),
                                       Par=rep(c("Mean","SD"),each=41))

                Fig_4.4.site<-data.frame(n_graph=c(length(X.n),length(X.n)),Xn_Sn=Fig_4.4,Par=c("Mean","Standard Deviation"))

                Fg4.4<-ggplot2::ggplot(data=Fig_4.4.df,aes(x=n_graph,y=Xn_Sn))+
                        ggplot2::geom_line(aes(color=Par,group=Par))+
                        ggplot2::scale_x_continuous(breaks=seq(10,100,10))+
                        ggplot2::scale_y_continuous(breaks=seq(1,1.3,0.05))+
                        ggplot2::scale_color_discrete()+
                        ggplot2::labs(x="Length of record [years]",y="Adjustment factor",color="Param.",
                                      title="Figure 4.4 - WMO No 1045 (2009)",
                                      subtitle="Adjust. of mean and SD of annual series for length of record")+
                        ggplot2::geom_point(data=Fig_4.4.site,aes(x=n_PMP,y=Xn_Sn),size=3,color="red")

                #Fig 4.1 - GGPLOT

                mamr<-seq(0,600,1)

                Fig_4.1.df<-data.frame(mamr=rep(mamr,times=3),
                                       rainfall.duration=as.character(rep(c("01","06","24"),each=601)),
                                       Km=c(Km.f.1(mamr),Km.f.6(mamr),Km.f.24(mamr)))

                Fig_4.1.site<-data.frame(mamr=X.mean[[1]],rainfall.duration=input.hrs,Km=Km)

                Fg4.1<-ggplot2::ggplot(data=Fig_4.1.df,aes(x=mamr,y=Km))+
                        ggplot2::geom_line(aes(color=rainfall.duration,group=rainfall.duration))+
                        ggplot2::scale_x_continuous(breaks=seq(0,600,100))+
                        ggplot2::scale_y_continuous(limits=c(5,20),breaks=seq(5,20,2.5))+
                        ggplot2::scale_color_discrete()+
                        ggplot2::labs(x="Mean Annual Maximum Rainfall [mm]",y="Km",color="Storm\nDuration\n[hrs]",
                                      title="Figure 4.1 - WMO No 1045 (2009)",
                                      subtitle="Km as a function of rainfall duration and mean of annual series")+
                        ggplot2::geom_point(data=Fig_4.1.site,aes(x=mamr,y=Km),size=3,color="red")


                #Fig 4.5 - GGPLOT
                nou<-seq(0,24,0.1)

                Fig_4.5.df<-data.frame(nou=nou,
                                       adjf=nou.f(nou))

                Fig_4.5.site<-data.frame(nou=number.of.obs.unit,
                                         adjf=nou.f(number.of.obs.unit))

                Fg4.5<-ggplot2::ggplot(data=Fig_4.5.df,aes(x=nou,y=adjf))+
                        ggplot2::geom_line()+
                        ggplot2::scale_x_continuous(breaks=seq(0,24,2))+
                        ggplot2::scale_y_continuous(limits=c(1,1.15),breaks=seq(1,1.15,0.02))+
                        ggplot2::labs(x="Number of Observational Units",y="Adjustment Factor",
                                      title="Figure 4.5 - WMO No 1045 (2009)",
                                      subtitle="Adjust. of fixed-interval precip. amounts for number of observ. units within the interval")+
                        ggplot2::geom_point(data=Fig_4.5.site,aes(x=nou,y=adjf),size=3,color="red")

                #Fig 4.7 - GGPLOT

                area<-seq(0,1000,5)

                Fig_4.7.df<-data.frame(area=area,
                                       rainfall.duration=as.character(rep(c("01","06","24"),each=201)),
                                       ppm=c(area_corr.f.1(area),area_corr.f.6(area),area_corr.f.24(area)))

                Fig_4.7.site<-data.frame(area=area.km,rainfall.duration=input.hrs,ppm=area_corr)


                Fg4.7<-ggplot2::ggplot(data=Fig_4.7.df,aes(x=area,y=ppm))+
                        ggplot2::geom_line(aes(color=rainfall.duration,group=rainfall.duration))+
                        ggplot2::scale_x_continuous(breaks=seq(0,1000,100))+
                        ggplot2::scale_y_continuous(limits=c(0.6,1),breaks=seq(0.6,1,0.05))+
                        ggplot2::labs(x="Area[km2]",y="Percentage of probable maximum\npoint, or 24 km2, rainfall",
                                      title="Figure 4.7 - WMO No 1045 (2009)",
                                      subtitle="Depth-area, or area-reduction, curves for Western U.S.",
                                      color="Storm\nDuration\n[hrs]")+
                        ggplot2::geom_point(data=Fig_4.7.site,aes(x=area,y=ppm),size=3,color="red")

        }



        ###TABLE RESULTS
        #data.frame with resultant values
        correction<-data.frame(Parameter=c("Xn","Xn_m","Xn_m/Xn","Sn_m/Sn","Fig 4.2","Fig 4.3", "Fig 4.4",
                                           "Adj.","Corrected Value"),
                               Avg=c(X.mean[[1]],X.mean[[2]],Xn_m_div,NA,Fig_4.2,NA,Fig_4.4[1],
                                     Fig_4.2*Fig_4.4[1],Fig_4.2*Fig_4.4[1]*X.mean[[1]]),
                               StDev=c(X.sd[[1]],X.sd[[2]],NA,Sn_m_div,NA,Fig_4.3,Fig_4.4[2],
                                       Fig_4.3*Fig_4.4[2],Fig_4.3*Fig_4.4[2]*X.sd[[1]]))

        Results<-list(val.max,n_PMP,correction,Km,
                      round(PMP2,0),round(PMP3,0))

        names(Results)<-c("values","n","correction","Km(Fig 4.1)",
                          "PMP","PMP.with.area.correction.for.West.US")

        ###GRAPH OUTPUS
        if(graphs==T){

                suppressWarnings(gridExtra::grid.arrange(Fg4.1,Fg4.2,Fg4.3,Fg4.4,Fg4.5,Fg4.7,ncol=2))

        }

        return(Results)

}

