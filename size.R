rm(list = ls())
size<-function(xt.r,b.x.t.s, p, n, m){
  rank(xt.r)->rt
  mean(rt) -> rt.mm 
  NULL->rou.rl
  for(ell in 1:m){
    rt[(ell+1):n] - rt.mm -> za.2
    rt[((ell+1):n) - ell] - rt.mm -> zb.2
    12*sum( za.2*zb.2 )/(n*(n^2-1)) -> vv
    c( rou.rl, vv ) ->rou.rl 
  }
  rou.rl[1:m]->hat.rho
  tam<-NULL
  for(i in 1:10){
    n*(sum ( hat.rho[1:i] ^2 ))->ta
    tam<-c(tam,ta)
  }
  
  rank(b.x.t.s)->rt.s
  mean(rt.s) -> rt.mm.s
  rou.rl.s<-NULL
  for(ell in 1:m){
    rt.s[(ell+1):n] - rt.mm.s -> zaa.s
    rt.s[((ell+1):n) - ell] - rt.mm.s -> zbb.s
    sum( zaa.s*zbb.s )*12/( n*(n^2-1) ) -> vvv.s
    c( rou.rl.s, vvv.s ) -> rou.rl.s 
  }
  tbm<-NULL
  for(i in 1:10){
    n*(sum ( rou.rl.s[1:i] ^2 ))->tb
    tbm<-c(tbm,tb)
  }
  
  return(list(tam,tbm))
}


j.tn<-function(ta.all,j.j.l){
  j.j.y.tn<-sum(ta.all>j.j.l)/N
  return(j.j.y.tn)
}
j.j.l<-qchisq(0.95,df=1)


m=10
n=10;p=5*n
N<-5000
tnm.all<-NULL
wnm.all<-NULL
max1.all<-NULL
max2.all<-NULL

for( ii in 1:N){
  cat( "ii", ii)
  if(ii== N){ cat("\n") }
  
  
  
  NULL -> gg
  for(i in 1:n){
    et<-as.matrix(rnorm(p, mean=0, sd=1)) 
    A<-diag(c(rep(20,p/2),rep(100,p/2)))
    xt<-A%*%et
    cbind(gg, xt)-> gg
  }
  

  xt.r1<-(colSums((abs(gg))^0.1))^(1/0.1)
  xt.r2 <- (colSums((abs(gg))^0.2))^(1/0.2)
  xt.r3<- (colSums((abs(gg))^0.3))^(1/0.3)
  xt.r4<- (colSums((abs(gg))^0.4))^(1/0.4)
  xt.r5 <-(colSums((abs(gg))^0.5))^(1/0.5)
  xt.r6<- (colSums((abs(gg))^0.6))^(1/0.6)
  xt.r7<- (colSums((abs(gg))^0.7))^(1/0.7)
  xt.r8<- (colSums((abs(gg))^0.8))^(1/0.8)
  xt.r9<- (colSums((abs(gg))^0.9))^(1/0.9)
  xt.r10<- colSums(abs(gg))
  xt.r11 <- sqrt(colSums(gg^2))
  xt.r12<- (colSums((abs(gg))^3))^(1/3)
  xt.r13<- (colSums((abs(gg))^4))^(1/4)
  xt.r14 <-(colSums((abs(gg))^5))^(1/5)
  xt.r15<- (colSums((abs(gg))^6))^(1/6)
  xt.r16<- (colSums((abs(gg))^7))^(1/7)
  xt.r17<- (colSums((abs(gg))^8))^(1/8)
  xt.r18<- (colSums((abs(gg))^9))^(1/9)
  xt.r19<- (colSums((abs(gg))^10))^(1/10)
  
  
  sin2<-NULL
  a<-apply(gg,1,mean)
  for (i in 1:p){
    s<-sum((gg[i,]-a[i])^2)/(n-1) 
    sin2<-c(sin2,s)
  }
  sigm.i<-NULL
  for (i in 1:p){
    sigm.i1<-median(abs(gg[i,]-median(gg)))
    sigm.i<-c(sigm.i,sigm.i1)
  }
  
  b.x.t<-gg/sqrt(sigm.i)
  
  
  b.x.t.s1<-(colSums((abs(b.x.t))^0.1))^(1/0.1)#
  b.x.t.s2 <- (colSums((abs(b.x.t))^0.2))^(1/0.2)#
  b.x.t.s3<- (colSums((abs(b.x.t))^0.3))^(1/0.3)#
  b.x.t.s4<- (colSums((abs(b.x.t))^0.4))^(1/0.4)#
  b.x.t.s5 <-(colSums((abs(b.x.t))^0.5))^(1/0.5)#
  b.x.t.s6<- (colSums((abs(b.x.t))^0.6))^(1/0.6)#
  b.x.t.s7<- (colSums((abs(b.x.t))^0.7))^(1/0.7)#
  b.x.t.s8<- (colSums((abs(b.x.t))^0.8))^(1/0.8)#
  b.x.t.s9<- (colSums((abs(b.x.t))^0.9))^(1/0.9)#
  b.x.t.s10<- colSums(abs(b.x.t))
  b.x.t.s11 <- sqrt(colSums(b.x.t^2))#
  b.x.t.s12<- (colSums((abs(b.x.t))^3))^(1/3)#
  b.x.t.s13<- (colSums((abs(b.x.t))^4))^(1/4)#
  b.x.t.s14 <-(colSums((abs(b.x.t))^5))^(1/5)#
  b.x.t.s15<- (colSums((abs(b.x.t))^6))^(1/6)#
  b.x.t.s16<- (colSums((abs(b.x.t))^7))^(1/7)#
  b.x.t.s17<- (colSums((abs(b.x.t))^8))^(1/8)#
  b.x.t.s18<- (colSums((abs(b.x.t))^9))^(1/9)#
  b.x.t.s19<- (colSums((abs(b.x.t))^10))^(1/10)#
  
  
  
  tnn1<-size(xt.r1,b.x.t.s1,p,n,m)
  tnn2<-size(xt.r2,b.x.t.s2,p,n,m)
  tnn3<-size(xt.r3,b.x.t.s3,p,n,m)
  tnn4<-size(xt.r4,b.x.t.s4,p,n,m)
  tnn5<-size(xt.r5,b.x.t.s5,p,n,m)
  tnn6<-size(xt.r6,b.x.t.s6,p,n,m)
  tnn7<-size(xt.r7,b.x.t.s7,p,n,m)
  tnn8<-size(xt.r8,b.x.t.s8,p,n,m)
  tnn9<-size(xt.r9,b.x.t.s9,p,n,m)
  tnn10<-size(xt.r10,b.x.t.s10,p,n,m)
  tnn11<-size(xt.r11,b.x.t.s11,p,n,m)
  tnn12<-size(xt.r12,b.x.t.s12,p,n,m)
  tnn13<-size(xt.r13,b.x.t.s13,p,n,m)
  tnn14<-size(xt.r14,b.x.t.s14,p,n,m)
  tnn15<-size(xt.r15,b.x.t.s15,p,n,m)
  tnn16<-size(xt.r16,b.x.t.s16,p,n,m)
  tnn17<-size(xt.r17,b.x.t.s17,p,n,m)
  tnn18<-size(xt.r18,b.x.t.s18,p,n,m)
  tnn19<-size(xt.r19,b.x.t.s19,p,n,m)
  
  tn1<-tnn1[[1]]
  tn2<-tnn2[[1]]
  tn3<-tnn3[[1]]
  tn4<-tnn4[[1]]
  tn5<-tnn5[[1]]
  tn6<-tnn6[[1]]
  tn7<-tnn7[[1]]
  tn8<-tnn8[[1]]
  tn9<-tnn9[[1]]
  tn10<-tnn10[[1]]
  tn11<-tnn11[[1]]
  tn12<-tnn12[[1]]
  tn13<-tnn13[[1]]
  tn14<-tnn14[[1]]
  tn15<-tnn15[[1]]
  tn16<-tnn16[[1]]
  tn17<-tnn17[[1]]
  tn18<-tnn18[[1]]
  tn19<-tnn19[[1]]
  
  wn1<-tnn1[[2]]
  wn2<-tnn2[[2]]
  wn3<-tnn3[[2]]
  wn4<-tnn4[[2]]
  wn5<-tnn5[[2]]
  wn6<-tnn6[[2]]
  wn7<-tnn7[[2]]
  wn8<-tnn8[[2]]
  wn9<-tnn9[[2]]
  wn10<-tnn10[[2]]
  wn11<-tnn11[[2]]
  wn12<-tnn12[[2]]
  wn13<-tnn13[[2]]
  wn14<-tnn14[[2]]
  wn15<-tnn15[[2]]
  wn16<-tnn16[[2]]
  wn17<-tnn17[[2]]
  wn18<-tnn18[[2]]
  wn19<-tnn19[[2]]
  
  
  
  
  mnjz<-t(matrix(c(tn1,tn2,tn3,tn4,tn5,tn6,tn7,tn8,tn9,tn10,tn11,tn12,tn13,tn14
                   ,tn15,tn16,tn17,tn18,tn19),19,byrow="T"))
  mnjz2<-t(matrix(c(wn1,wn2,wn3,wn4,wn5,wn6,wn7,wn8,wn9,wn10,wn11,wn12,wn13,wn14
                    ,wn15,wn16,wn17,wn18,wn19),19,byrow="T"))
  
  pi<-NULL
  for(i in 1:10){
    p1<-10*i
    pi<-c(pi,p1)
  }
  pj<-NULL
  for(j in c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8, 0.9, 1, 2,3,4,5,6,7,8,9,10)){
    p2<-10*log(n)/j
    pj<-c(pj,p2)
  }
  cfjz<-matrix(c(pi+pj[1],pi+pj[2],pi+pj[3],pi+pj[4],pi+pj[5],pi+pj[6],
                 pi+pj[7],pi+pj[8],pi+pj[9],pi+pj[10],pi+pj[11],pi+pj[12],pi+pj[13],
                 pi+pj[14],pi+pj[15],pi+pj[16],pi+pj[17],pi+pj[18],pi+pj[19]),10)
  
  tnmm<-mnjz-cfjz
  max1<-which(tnmm==tnmm[which.max(tnmm)],arr.ind=T)
  tnm<-mnjz[max1]
  
  wnmm<-mnjz2-cfjz
  max2<-which(wnmm==wnmm[which.max(wnmm)],arr.ind=T)
  wnm<-mnjz2[max2]
  
  max1.all<-list(max1.all,max1)
  max2.all<-c(max2.all,max2)
  tnm.all<-c(tnm.all,tnm)
  wnm.all<-c(wnm.all,wnm)
}


j.j.y.maxtn<-j.tn(tnm.all,j.j.l)
j.j.y.maxwn<-j.tn(wnm.all,j.j.l)




