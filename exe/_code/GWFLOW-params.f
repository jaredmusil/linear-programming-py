	program gw_params
c
c used to calculate gwflow_coef and gwstor_init parameters for PRMS
c
	real qobs(100,366),qn(100,366)
	real bQ0(3,100),bQ1(3,100)
	real ra(1000),flow,flowlow(100),lowflow
	real darea(100)
        integer tlow(100),qdate(44000),k,iro,itmx,itmn,ipan,isol
        integer gagebeg(100),gageend(100)
	integer WY(100),flag(100)
	character header*60,input*25,nme*8,abbrev(100)*8
        real KK,t,gwflow_coef(100),gwstor_init(100),Q0(100),Q1(100)
	real t0(100),t1(100),qmth(100,12)
        real base_gwflow(3,100),base_gwstor(3,100)
	real X(400),Y(400),qall(44000),baseOBS(44000,3)
	integer qnum(44000),numq(100,12),YRbeg(100),YRend(100)
	character state(100)*2,name(100)*20,prms_file*20
	real QdayMEAN(3,366),QdayNUM(366)
        real ppt(100),tmax(100),tmin(100),qdat(100,40000)
        real sol(100),pan(100)
        integer year(40000),month(40000),day(40000),ngage

c--------------------------------------------------------------------
        iro=0 

	open(59,file='gwflow_params',status='replace')
        open(58,file='gwstor_params',status='replace')
        open(89,file='gw_params.LUCA',status='replace')
        write(89,164)
164     format('abbrev   gwflow_coef     MIN        MAX',
     .  '    gwstor_init    MIN        MAX')
c       ----------------
        close(40)
        print *,'Enter name of PRMS input data file>'
        read(*,*)prms_file
        print *,'PRMS data file is: ',prms_file
        open(40,file=prms_file,status='old')
54      read(40,4)header
4       format(a60)
        if(header(1:10).eq.'##########')go to 55
        if(header(28:33).eq.'runoff')then
          print *,header(3:60)
          iro=iro+1
          abbrev(iro)=header(19:26)
        end if

        if(header(1:4).eq.'tmax')print *,header
        if(header(1:4).eq.'tmin')print *,header
        if(header(1:4).eq.'prec')print *,header
        if(header(1:4).eq.'solr')print *,header
        if(header(1:4).eq.'pan_')print *,header
        if(header(1:4).eq.'form')print *,header
        if(header(1:4).eq.'runo')print *,header
        if(header(1:4).eq.'rain')print *,header

        go to 54
55      continue

        print *,'Enter # tmax>'
        read(*,*)ntmx
        print *,'Enter # tmin>'
        read(*,*)ntmn
        print *,'Enter # precip>'
        read(*,*)nppt
        print *,'Enter # solrad>'
        read(*,*)nsol
        print *,'Enter # pan_evap>'
        read(*,*)npan
        print *,'Enter # runoff>'
        read(*,*)ngage

        do i=1,ngage
         print *,'Gage ',i,' ',abbrev(i),' Enter area (in km**2)>'
         read(*,*)darea(i)
        end do
        do i=1,ngage
         print *,'Gage ',i,' ',abbrev(i),' area (km**2)=',darea(i)
         gagebeg(i)=0
         gageend(i)=0
        end do
        j=1
56      read(40,*,end=57)year(j),month(j),day(j),i1,i2,i3,
     .  (tmax(i),i=1,ntmx),
     .  (tmin(i),i=1,ntmn),
     .  (ppt(i),i=1,nppt),
     .  (sol(i),i=1,nsol),
     .  (pan(i),i=1,npan),
     .  (qdat(i,j),i=1,ngage)
        do i=1,ngage
         if(qdat(i,j).gt.0.0)then
           if(gagebeg(i).eq.0)gagebeg(i)=j
           gageend(i)=j
         end if
        end do
        j=j+1
        go to 56
57      idays=j-1
        close(40)
        do i=1,ngage
         i1=gagebeg(i)
         i2=gageend(i)
         print *,'Gage ',i,' ',abbrev(i),year(i1),month(i1),day(i1),
     .         ' - ',year(i2),month(i2),day(i2)
        end do
c       ================

c	===============================================
        do ibas=1,ngage
         i1=gagebeg(ibas)
         i2=gageend(ibas)
	 print *,'abbrev=',abbrev(ibas)
c       ===============================================
	do i=1,100
	WY(i)=0
	YRbeg(i)=0
	YRend(i)=0
	tlow(i)=-999
	flowlow(i)=-999.0
	do j=1,366
	 qobs(i,j)=-999.
	 qn(i,j)=0.
	end do
	end do
	do i=1,100
	do m=1,12
	 qmth(i,m)=0.0
	 numq(i,m)=0
	end do
	end do
	do i=1,366
	 QdayMEAN(1,i)=99999999.
         QdayMEAN(2,i)=0.0
         QdayMEAN(3,i)=-999.
	 QdayNUM(i)=0.0
	end do
c       ================
c drainage area is in km**2 -- convert to ft**2
	area=darea(ibas)*10760000.0
c-----------------------------------------------------
c-----------------------------------------------------
c-----------------------------------------------------
	ibeg=1001
c
	ny=0
	n=0
	nyr=0
	iflag=0

        do 11 j=i1,i2

        qo=qdat(ibas,j)
        iy=year(j)
        im=month(j)
        nd=day(j)

        idate=(im*100)+nd
	if(iflag.eq.0)then
	  if(idate.eq.ibeg)iflag=1
	end if
	if(iflag.eq.0)go to 11
c
	if(qo.lt.0.0)then
	 print *,abbrev(ibas),iy,im,nd,qo
	end if
c
	n=n+1
	if(idate.eq.ibeg)then
	 nyr=nyr+1
	 YRbeg(nyr)=n
	 ny=0
	end if
        if(idate.eq.0930)then
	 YRend(nyr)=n
	 WY(nyr)=iy
	end if
c	
	if(nyr.eq.0)then
	 print *,'nyr=0'
	 stop
	end if
c
        ny=ny+1
c
	if(qo.ge.0.0)then
	 QdayMEAN(2,ny)=QdayMEAN(2,ny)+qo
	 if(QdayMEAN(1,ny).gt.qo)QdayMEAN(1,ny)=qo
         if(QdayMEAN(3,ny).lt.qo)QdayMEAN(3,ny)=qo
	 QdayNUM(ny)=QdayNUM(ny)+1.
c
         numq(nyr,im)=numq(nyr,im)+1
         qmth(nyr,im)=qmth(nyr,im)+qo
	end if
c
         qobs(nyr,ny)=qo
         qn(nyr,ny)=n
         qall(n)=qo
         qnum(n)=ny
c
	qdate(n)=idate+(iy*10000)
c
c       ----------------
11	continue
	ndays=n
	print *,qdate(n),' =final DATE'
c
	do i=1,366
	 QdayMEAN(2,i)=QdayMEAN(2,i)/QdayNUM(i)
	end do
c
	do n=1,nyr
	do m=1,12
	 qmth(n,m)=qmth(n,m)/float(numq(n,m))
	end do
	end do
c
c       ----------------------------------------------
c sort and find low flow values for year:
	do i=1,nyr
	n=0
c
	do j=20,366
	 if(qobs(i,j).gt.0.0)then
	  n=n+1
	  ra(n)=qobs(i,j)
	 end if
	end do

         ny=n
	 if(ny.gt.30)then
          call sort(ny,ra)
          flowlow(i)=ra(1)
	 else
	  flowlow(i)=-9.8
	  tlow(i)=0
	 end if
	end do
c
	do i=1,nyr
	ijk=0
        do j=20,366
         if(qobs(i,j).eq.flowlow(i))then
           tlow(i)=qn(i,j)
	   ijk=1
c	   ----------------------------------------
	   if(j.ge.365)then
	   print *,'look into next year for low flow'
	    n=i+1
	    if(n.gt.nyr)go to 333
	    q=flowlow(i)
c
	    do ij=1,100
            if(qobs(n,ij).le.0.0)go to 333
	     if(qobs(n,ij).le.q)then
	        tlow(i)=qn(n,ij)
	        q=qobs(n,ij)
	     else
	        go to 333
	     end if
	    end do
c
333	    continue
	   print *,'#=',ij-1,flowlow(i),(qobs(n,ii),ii=1,ij-1)
	   end if
c          ----------------------------------------
	 end if
        end do
	print *,flowlow(i),i
	end do
c
c	----------------------------------------------
c get base flow separation using hysep (part) program
        DA=darea(ibas)
        call PART(qall,ndays,baseOBS,DA)
c
c
c==================================================================
c now go thru each year and find lowest flow (Qlow) 
c and count backward until flow is greater than Qlow*2
	do i=1,nyr
	 Q0(i)=-999.9
	 Q1(i)=-999.9
	 t0(i)=-999.
	 t1(i)=-999.
	 do k=1,3
	  bQ0(k,i)=-999.
	  bQ1(k,i)=-999.
	 end do
	 gwflow_coef(i)=-999.9
	 gwstor_init(i)=-999.9
	 do k=1,3
	  base_gwflow(k,i)=-999.9
          base_gwstor(k,i)=-999.9
	 end do
	end do
c	
        xcoef=0.0
        xinit=0.0
        nump=0
c	------------
	do 5555 i=1,nyr
         n=tlow(i)
	 Qt=qall(n)
	 if(Qt.le.0.0)go to 5555
c       ------------
	 num=0
	 nn=n-100
	 if(nn.lt.0)nn=1
c look for point where baseflow is no longer increasing
c
	 do j=n,nn,-1
c
         if((baseOBS(j,1)*2.0).lt.baseOBS(j+1,1))go to 444
         if((baseOBS(j,2)*2.0).lt.baseOBS(j+1,2))go to 444
         if((baseOBS(j,3)*2.0).lt.baseOBS(j+1,3))go to 444
c
 	 if(qall(j).gt.(baseOBS(j,1)*1.65))go to 444
         if(qall(j).gt.(baseOBS(j,2)*1.65))go to 444
         if(qall(j).gt.(baseOBS(j,3)*1.65))go to 444
c
	  num=num+1
	  X(num)=j
	  Y(num)=qall(j)
	 end do
444	continue
	print *,'num=',num
	if(num.lt.15)go to 555
c
	MWT=0
	NDATA=num
c 
	call FIT(X,Y,NDATA,MWT,A,B,SIGA,SIGB,CHI2,Q)
c Y = A + BX
c	
c now use line to get Qo and Qt
	iflag=0
	t0(i)=X(num)
	t1(i)=X(1)
	Q0(i)=A + (B*t0(i))
	Q1(i)=A + (B*t1(i))
	if(Q0(i).le.0.0)Q0(i)=0.000001
        if(Q1(i).le.0.0)Q1(i)=0.000001
c
	if(Q1(i).gt.Q0(i))iflag=3
	if(iflag.eq.0)then
          t = t1(i)-t0(i)+1
          KK = (log(Q0(i)/Q1(i)))/(t)
          gwflow_coef(i) = KK
	  xcoef=xcoef+gwflow_coef(i)
	  Qinches=Q0(i)*(86400.0/area)*12.0
          gwstor_init(i) = Qinches/gwflow_coef(i)
	  xinit=xinit+gwstor_init(i)
	  nump=nump+1
c now figure out params based on baseflow calc's
          n1=X(1)
	  n0=X(num)
c       =================
c       =================
	  do k=1,3
	   bQ0(k,i)=baseOBS(n0,k)
	   bQ1(k,i)=baseOBS(n1,k)
	   nflag=0
c       ............................................
	if(bQ1(k,i).gt.bQ0(k,i))then
	nflag=1
	 print *,'k=',k
	 print *,(baseOBS(nn,k),nn=n0,n1)
	 xx=float(num)/3.0
	 nx=xx
	 do nn=1,nx
	   if(baseOBS(n0+nn,k).gt.bQ1(k,i))then
            bQ0(k,i)=baseOBS(n0+nn,k)
	    t=t-nn
	    print *,'Found new bQ0 ',k,i,nn
	    nflag=2
	   end if
	  if(nflag.eq.2)go to 543
	 end do
	 print *,'never figured it out',num,nx,bQ1(k,i)
543	 continue
	end if
c	............................................
	if(nflag.eq.1)then
	   base_gwflow(k,i)=-99.
           base_gwstor(k,i)=-99.
	else
	 bQ0(k,i)=bQ0(k,i)+0.000011
	 bQ1(k,i)=bQ1(k,i)+0.00001
	   base_gwflow(k,i)=(log(bQ0(k,i)/bQ1(k,i)))/(t)
           Qinches=bQ0(k,i)*(86400.0/area)*12.0
           base_gwstor(k,i)=Qinches/base_gwflow(k,i)
	end if
c
	  end do
c	=================
c       =================
	end if
c        =================
c
555	 continue
c       ------------
5555	continue
c       ------------
c
	if(nump.gt.0)then
	 xgwflow=xcoef/float(nump)
	 xgwstor=xinit/float(nump)
	else
	 print *,'No calc made ',abbrev(ibas)
	 stop
	 xgwflow=0.001
	 xgwstor=1.0
	 do i=1,nyr
	 do k=1,3
	  base_gwflow(k,i)=0.001
	  base_gwstor(k,i)=1.0
	 end do
	 end do
	end if
c
c if the baseflow parameters differ greatly from the i
c observed -- don't use that year.
        do i=1,nyr
	flag(i)=0
	do k=1,3
c
         if((gwflow_coef(i)*2.0).lt.base_gwflow(k,i))flag(i)=flag(i)+1
         if((base_gwflow(k,i)*2.0).lt.gwflow_coef(i))flag(i)=flag(i)+1
c
	end do
        end do
c
c find the year that is the closest to the mean 
c and print the output to a file
c
	xdiff=10000.
	do i=1,nyr
	 diff=abs(gwflow_coef(i)-xgwflow)
	 if(xdiff.gt.diff)then
 	  xdiff=diff
	  numyr=i
	 end if
	end do
c
c write out the data for chosen year
	qmax=0.0
        i=numyr
	idate=qdate(tlow(i))
	nx1=YRbeg(i)
	nx2=YRend(i)
	if(nx1.lt.0)nx1=1
	nn=0
        do j=nx1,nx2
	 nn=nn+1
c        write(49,149)abbrev(ibas),j,qall(j),
c    .         QdayMEAN(1,nn),QdayMEAN(2,nn),QdayMEAN(3,nn),
c    .        (baseOBS(j,k),k=1,3)
149	format(a5,i5,1x,8(f9.1,1x))
	     if(qmax.lt.qall(j))qmax=qall(j)
        end do
c	write(79,139)abbrev(ibas),qmax,gwflow_coef(i),gwstor_init(i),
c     .               nx1,nx2,idate,WY(i)
139	format('./basinGW-plots.sc ',
     .   a5,1x,f9.0,1x,f7.4,1x,f7.3,1x,2i6,1x,
     .   i8,1x,i4)
c
cwrite(69,*)abbrev(ibas),t0(numyr),Q0(numyr)
c       write(69,*)abbrev(ibas),t1(numyr),Q1(numyr)
	do k=1,3 
c        write(68,*)abbrev(ibas),k,t0(numyr),bQ0(k,numyr)
c        write(68,*)abbrev(ibas),k,t1(numyr),bQ1(k,numyr)
	end do
c
        xmnGW=0.0
        xmaxGW=0.0
        xminGW=1.0
        xmnST=0.0
        xmaxST=0.0
        xminST=20.0
        nGW=0
        nST=0
	do i=1,nyr
	if(flag(i).lt.3)then
	if(gwflow_coef(i).ge.0.0)then
c	 ----------------------------
	 write(59,122)abbrev(ibas),WY(i),gwflow_coef(i),
     .              (base_gwflow(k,i),k=1,3),flag(i)
         write(58,122)abbrev(ibas),WY(i),gwstor_init(i),
     .              (base_gwstor(k,i),k=1,3)
         if(gwflow_coef(i).lt.xminGW)xminGW=gwflow_coef(i)
         if(gwstor_init(i).lt.xminST)xminST=gwstor_init(i)
         xmnGW=xmnGW+gwflow_coef(i)
         xmnST=xmnST+gwstor_init(i)
         nGW=nGW+1
         nST=nST+1
         do k=1,3
          if(base_gwflow(k,i).ge.0.0)then
	   if(base_gwflow(k,i).lt.xminGW)
     .      xminGW=base_gwflow(k,i)
           if(base_gwflow(k,i).gt.xmaxGW)xmaxGW=base_gwflow(k,i)
           xmnGW=xmnGW+base_gwflow(k,i)
           nGW=nGW+1
	  end if
c
          if(base_gwstor(k,i).ge.0.0)then
           if(base_gwstor(k,i).lt.xminST)xminST=base_gwstor(k,i)
           if(base_gwstor(k,i).gt.xmaxST)xmaxST=base_gwstor(k,i)
           xmnST=xmnST+base_gwstor(k,i)
           nST=nST+1
	  end if
         end do
c        ----------------------------
	end if
	end if
	end do
        xmnGW=xmnGW/float(nGW)
        xmnST=xmnST/float(nGW)
        if(xmaxST.gt.20.0)xmaxST=20.0
        write(89,165)abbrev(ibas),xmnGW,xminGW,xmaxGW,
     .         xmnST,xminST,xmaxST
165     format(a8,1x,6f11.5)
122	format(a8,1x,i4,1x,4(f10.5,1x),i2)
c---------------------------------------
c---------------------------------------
c
	end do
	close(49)
	close(59)
	close(69)
c
	stop
177	format(i8,1x,12(f7.0,1x))
	end
      SUBROUTINE sort(N,RA)
      real RA(1000)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
c
      SUBROUTINE FIT(X,Y,NDATA,MWT,A,B,SIGA,SIGB,CHI2,Q)
      real X(400),Y(400),SIG(400)
      SX=0.
      SY=0.
      ST2=0.
      B=0.
      IF(MWT.NE.0) THEN
        SS=0.
        DO 11 I=1,NDATA
          WT=1./(SIG(I)**2)
          SS=SS+WT
          SX=SX+X(I)*WT
          SY=SY+Y(I)*WT
11      CONTINUE
      ELSE
        DO 12 I=1,NDATA
          SX=SX+X(I)
          SY=SY+Y(I)
12      CONTINUE
        SS=FLOAT(NDATA)
      ENDIF
      SXOSS=SX/SS
      IF(MWT.NE.0) THEN
        DO 13 I=1,NDATA
          T=(X(I)-SXOSS)/SIG(I)
          ST2=ST2+T*T
          B=B+T*Y(I)/SIG(I)
13      CONTINUE
      ELSE
        DO 14 I=1,NDATA
          T=X(I)-SXOSS
          ST2=ST2+T*T
          B=B+T*Y(I)
14      CONTINUE
      ENDIF
      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      CHI2=0.
      IF(MWT.EQ.0) THEN
        DO 15 I=1,NDATA
          CHI2=CHI2+(Y(I)-A-B*X(I))**2
15      CONTINUE
        Q=1.
        SIGDAT=SQRT(CHI2/(NDATA-2))
        SIGA=SIGA*SIGDAT
        SIGB=SIGB*SIGDAT
      ELSE
        DO 16 I=1,NDATA
          CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
16      CONTINUE
        Q=GAMMQ(0.5*(NDATA-2),0.5*CHI2)
      ENDIF
      RETURN
      END
c
      FUNCTION GAMMQ(A,X)
      IF(X.LT.0..OR.A.LE.0.)stop
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.-GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMQ=GAMMCF
      ENDIF
      RETURN
      END
c
      SUBROUTINE GCF(GAMMCF,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=FLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      print *,'A too large, ITMAX too small'
      stop
1     GAMMCF=EXP(-X+A*ALOG(X)-GLN)*G
      RETURN
      END
c
      SUBROUTINE GSER(GAMSER,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF(X.LT.0.)stop
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      print *,'A too large, ITMAX too small'
      stop
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END
c
      FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
c
      subroutine PART(Q1D,ndays,B1D,DA)
C          BY AL RUTLEDGE, USGS           2002 VERSION
c modified by L. Hay March 2003
c
C  THIS PROGRAM PERFORMS STREAMFLOW PARTITIONING (A FORM OF HYDROGRAPH
C  SEPARATION) USING A DAILY-VALUES RECORD OF STREAMFLOW.  THIS AND
C  OTHER PROGRAMS ARE DOCUMENTED IN USGS WRIR 98-4148. THIS 2002
C  VERSION OF THE PROGRAM READS DAILY-VALUES FROM AN MMS output file
C
      real Q1D(44000)
      real B1D(44000,3)
      INTEGER ITBASE
      INTEGER TBASE1
      REAL QMININT
      CHARACTER*1 ALLGW(44000)
      REAL DA
C
C-------------------------- INITIALIZE VARIABLES : -------------------
        ICOUNT=ndays
C
      DO 11 I=1,44000
      ALLGW(I)= ' '
      DO 11 IBASE=1,3
      B1D(I,IBASE)= -888.0
   11 CONTINUE

C
   15 FORMAT (1I5,4F11.2,5X,3I4)
C
C
C OBTAIN DRAINAGE AREA(DA), THEN CALCULATE THE REQUIREMENT OF ANTECEDENT
C                          RECESSION (DA**0.2)
C
c convert DA from km**2 to miles**2
	DA=DA*0.3861	

C  ----DETERMINE THREE INTEGER VALUES FOR THE REQMT. OF ANTEC. RECESSION ----
C  -----(ONE THAT IS LESS THAN, AND TWO THAT ARE MORE THAN DA**0.2 )---------
C
      TBASE1= INT(DA**0.2)
      IF(TBASE1.GT.DA**0.2) THEN
         TBASE1=TBASE1-1
       END IF
      IF(DA**0.2-TBASE1.GT.1.0) THEN
         WRITE (*,*) 'PROBLEMS WITH CALCULATION OF REQUIREMENT OF  '
         WRITE (*,*) 'ANTECEDENT RECESSION. '
         GO TO 1500
        END IF
C
      THRESH= 0.1

C
C --------  REPEAT FOR EACH OF THE 3 VALUES  ----------
C --------  FOR THE REQUIREMENT OF ANTECEDENT RECESSION        --------------
C
      ITBASE= TBASE1-1
      DO 500 IBASE=1,3
      ITBASE= ITBASE+1

      DO 185 I=1,ICOUNT
  185 ALLGW(I)= ' '
C
C---DESIGNATE BASEFLOW=STREAMFLOW ON DATES PRECEEDED BY A RECESSION PERIOD ---
C---AND ON DATES OF ZERO STREAMFLOW. SET VARIABLE ALLGW='*' ON THESE DATES.---
C
      DO 200 I= ITBASE+1, ICOUNT
         INDICAT=1
         IBACK= 0
  190    IBACK= IBACK + 1
            IF(Q1D(I-IBACK).LT.Q1D(I-IBACK+1)) INDICAT=0
            IF(IBACK.LT.ITBASE) GO TO 190
            IF(INDICAT.EQ.1) THEN
                   B1D(I,IBASE)= Q1D(I)
                   ALLGW(I)= '*'
               ELSE
                   B1D(I,IBASE)= -888.0
                   ALLGW(I)= ' '
             END IF
  200 CONTINUE
      DO 220 I=1,ITBASE
          IF(Q1D(I).EQ.0.0) THEN
             B1D(I,IBASE)= 0.0
             ALLGW(I)= '*'
           END IF
  220 CONTINUE
C
C ------ DURING THESE RECESSION PERIODS, SET ALLGW = ' ' IF THE DAILY  ------
C ----------  DECLINE OF LOG Q EXCEEDS THE THRESHOLD VALUE: -----------------
C
      I=1
  221 I=I+1
      IF (I.EQ.ICOUNT) GO TO 224
      IF (ALLGW(I).NE.'*') GO TO 221
      IF (ALLGW(I+1).NE.'*') GO TO 221
      IF (Q1D(I).EQ.0.0.OR.Q1D(I+1).EQ.0.0) GO TO 221
      IF (ALLGW(I+1).NE.'*') GO TO 221
      IF(Q1D(I).LT.0.0.OR.Q1D(I+1).LT.0.0) GO TO 221
      DIFF= ( LOG(Q1D(I)) - LOG(Q1D(I+1)) ) / 2.3025851
      IF (DIFF.GT.THRESH) ALLGW(I)= ' '
      IF (I+1.LE.ICOUNT) GO TO 221
  224 CONTINUE

  225 CONTINUE
C
C ----EXTRAPOLATE BASEFLOW IN THE FIRST FEW DAYS OF THE PERIOD OF INTEREST:----
C
      J=0
  230 J=J+1
      IF(ALLGW(J).EQ.' ') GO TO 230
      STARTBF= B1D(J,IBASE)
      DO 240 I=1,J
  240 B1D(I,IBASE)= STARTBF
C
C ---- EXTRAPOLATE BASEFLOW IN LAST FEW DAYS OF PERIOD OF INTEREST: ----------
C
      J=ICOUNT+1
  250 J=J-1
      IF(ALLGW(J).EQ.' ') GO TO 250
      ENDBF= B1D(J,IBASE)
      DO 260 I=J,ICOUNT
  260 B1D(I,IBASE)= ENDBF

C
C ---------  INTERPOLATE DAILY VALUES OF BASEFLOW FOR PERIODS WHEN   ----------
C -------------------------  BASEFLOW IS NOT KNOWN:   -------------------------
C
C                  FIND VERY FIRST INCIDENT OF BASEFLOW DATA:
  280 CONTINUE
      I= 0
  290 CONTINUE
      I=I + 1
      IF(ALLGW(I).EQ.' ') GO TO 290


C                 NOW THAT A DAILY BASEFLOW IS FOUND, MARCH
C                        AHEAD TO FIRST GAP IN DATA:
  300 CONTINUE
      IF(B1D(I,IBASE).EQ.0.0) THEN
          BASE1= -999.0
        ELSE
          IF (B1D(I,IBASE).LT.0.0) THEN
C           WRITE (*,*) '(B)ARG OF LOG FUNC<0 AT I=', I
                BASE1= -999.0
            ELSE
                BASE1= LOG(B1D(I,IBASE)) / 2.3025851
           END IF
       END IF
      I= I + 1
      IF(I.GT.ICOUNT) GO TO 350
      IF(ALLGW(I).NE.' ') GO TO 300
      ITSTART= I-1
C
C                  MARCH THROUGH GAP IN BASEFLOW DATA:
      ITIME= 1
  320 ITIME= ITIME + 1
      IF(ITIME+ITSTART.GT.ICOUNT) GO TO 350
      IF(ALLGW(ITSTART+ITIME).EQ.' ') GO TO 320
      IDAYS= ITIME-1
      T2= ITIME
       IF (B1D(I+IDAYS,IBASE).EQ.0.0) THEN
          BASE2= -999.0
         ELSE
          IF(B1D(I+IDAYS,IBASE).LT.0.0) THEN
C              WRITE (*,*) '(C)ARG OF LOG FUNC<0 AT I=', I+DAYS
               BASE2= -999.0
             ELSE
               BASE2= LOG(B1D(I+IDAYS,IBASE)) / 2.3025851
           END IF
        END IF
C
C                     FILL IN ESTIMATED BASEFLOW DATA:
C
      IF(BASE1.EQ.-999.0.OR.BASE2.EQ.-999.0) THEN
         DO 330 J=1,IDAYS
             B1D(J+ITSTART,IBASE)=0.0
  330      CONTINUE
       ELSE
         DO 340 J=1,IDAYS
            T=J
            Y= BASE1 + (BASE2-BASE1) * T / T2
            B1D(J+ITSTART,IBASE)= 10**Y
  340     CONTINUE
      END IF
C
      I= I + IDAYS
      IF(I.LE.ICOUNT) GO TO 300
C
  350 CONTINUE
C
C ------ TEST TO FIND OUT IF BASE FLOW EXCEEDS STREAMFLOW ON ANY DAY: -----
C
      QLOW=0
      DO 395 I=1,ICOUNT
         IF(B1D(I,IBASE).GT.Q1D(I)) QLOW=1
  395 CONTINUE
      IF(QLOW.EQ.0) GO TO 420
C
C ------- IF ANY DAYS EXIST WHEN BASE FLOW > STREAMFLOW AND SF=0, ASSIGN
C ------- BF=SF, THEN RUN THROUGH INTERPOLATION (ABOVE):
C
      QLOW2=0
      DO 397 I=1,ICOUNT
        IF (B1D(I,IBASE).GT.Q1D(I).AND.Q1D(I).EQ.0.0) THEN
             QLOW2=1
             B1D(I,IBASE)= 0.0
             ALLGW(I)= '*'
          END IF
  397 CONTINUE
      IF (QLOW2.EQ.1) GO TO 225
C
C --------  LOCATE INTERVALS OF INTERPOLATED BASEFLOW IN WHICH AT LEAST ------
C --------  ONE BASEFLOW EXCEEDS STREAMFLOW ON THE CORRESPONDING DAY. --------
C --------  LOCATE THE DAY ON THIS INTERVAL OF THE MAXIMUM "BF MINUS SF", ----
C --------  AND ASSIGN BASEFLOW=STREAMFLOW ON THIS DAY. THEN RUN THROUGH -----
C --------  THE INTERPOLATION SCHEME (ABOVE) AGAIN.  -------------------------
C
      I=0
  400 CONTINUE
      I=I+1
      IF(B1D(I,IBASE).GT.Q1D(I)) THEN
           QMININT= Q1D(I)
           IF (B1D(I,IBASE).LT.0.0.OR.Q1D(I).LT.0.0) THEN
C               WRITE (*,*) '(D)ARG OF LOG FUNC<0 AT I=', I
                DELMAX=-999.0
              ELSE
                DELMAX= LOG(B1D(I,IBASE)) - LOG(Q1D(I))
            END IF
           ISEARCH= I
           IMIN=I
  402      CONTINUE
           ISEARCH= ISEARCH + 1
           IF(ALLGW(ISEARCH).NE.' '.OR.B1D(ISEARCH,IBASE).LE.
     $        Q1D(ISEARCH).OR.ISEARCH.GT.ICOUNT) GO TO 405
           IF (B1D(ISEARCH,IBASE).LT.0.0.OR.Q1D(ISEARCH).LT.0.0) THEN
C              WRITE (*,*) '(E)ARG OF LOG FUNC<0 AT I=', ISEARCH
               DEL= -999.0
             ELSE
               DEL= LOG(B1D(ISEARCH,IBASE)) - LOG(Q1D(ISEARCH))
            END IF
           IF(DEL.GT.DELMAX) THEN
               DELMAX=DEL
               QMININT= Q1D(ISEARCH)
               IMIN= ISEARCH
            END IF
           IF(ALLGW(ISEARCH).EQ.' ') GO TO 402
  405      CONTINUE
           B1D(IMIN,IBASE)= QMININT
           ALLGW(IMIN)= '*'
           I= ISEARCH+1
        END IF
      IF(I.LT.ICOUNT) GO TO 400

      IF(QLOW.EQ.1) GO TO 225

  420 CONTINUE
C
C
C ---------- DETERMINE RANGE OF VALUES FOR STREAMFLOW AND BASEFLOW:  ----------
C
            QMINPI= Q1D(1)
            QMAXPI= Q1D(1)
            BMINPI= B1D(1,IBASE)
            BMAXPI= B1D(1,IBASE)
            DO 470 I=1,ICOUNT
                IF(Q1D(I).LT.QMINPI) QMINPI=Q1D(I)
                IF(Q1D(I).GT.QMAXPI) QMAXPI=Q1D(I)
                IF(B1D(I,IBASE).LT.BMINPI) BMINPI= B1D(I,IBASE)
                IF(B1D(I,IBASE).GT.BMAXPI) BMAXPI= B1D(I,IBASE)
  470        CONTINUE
C
  500 CONTINUE

C
C  ------- WRITE DAILY STREAMFLOW AND BASEFLOW FOR ONE OR MORE YEARS: ---
c          WRITE (12,*)'                         BASE FLOW FOR EACH'
c          WRITE (12,*)'                            REQUIREMENT OF  '
c          WRITE (12,*)'          STREAM         ANTECEDENT RECESSION '
c          WRITE (12,*)' DAY #     FLOW        #1         #2         #3'
           ICNT=0
c          DO 840 I=1,ndays
c                WRITE (12,15) I,Q1D(I),B1D(I,1),
c    $            B1D(I,2), B1D(I,3)
c 840       CONTINUE


  845  CONTINUE


 1500 CONTINUE


      CLOSE (10,STATUS='KEEP')
      CLOSE (12,STATUS='KEEP')
      CLOSE (13,STATUS='KEEP')
      CLOSE (15,STATUS='KEEP')
      CLOSE (16,STATUS='KEEP')

      RETURN
      END
