        program efcQ
        implicit none
c used to separate daily flows into:
c 1. large floods
c 2. small floods
c 3. high flow pulses
c 4. low flows
c 5. extreme low flows
c 6. HIGH == 1,2,3
c 7. LOW == 4,5
c-------------------------------------
c
       real qobs(30000,1000),RA(30000),RB(30000),xx
       real qsort(30000,1000,2),median(1000),p10th(1000),p75th(1000)
       real RI(30000,1000)
       integer yr(30000),mth(30000),day(30000),nq,n
       integer i1,i2,i3,numSORT(1000),i,j,nn,ndays,start(1000)
       integer efc(30000,1000),high_low(30000,1000),iflag
       integer ngage
       character header*30,sid(1000)*10,fname*20,ID*2

       common /data/qobs,p75th,median,ndays,nq,efc,
     +               qsort,numSORT,RI,start

c--------------------------------------------------------------

        print *,'Enter name of PRMS input file with gage data:'
        print *,'****Must be downsizer file with streamflow only!'
        print *,'****replace tabs with spaces in file'
        read(*,*)fname
        close(20)
        open(20,file=fname,status='old')

1       read(20,2)ID
        print *,ID
        if(ID.ne.'ID')go to 1
2       format(3x,a2)

        ngage=1
11      read(20,3,end=12)sid(ngage)
        if(sid(ngage).eq.'//////////')go to 12
3       format(3x,a10)
        ngage=ngage+1
        go to 11
12      ngage=ngage-1
        nq=ngage
        print *,ngage,nq

110       read(20,111)header
           print *,header(1:6)
           if(header(1:6).eq.'runoff')print *,header
           if(header(1:6).eq.'######')go to 112
           go to 110
111       format(a30)
112       continue
c-----------------------------------------------------
c
        n=1
10       read(20,*,end=1111)yr(n),mth(n),day(n),i1,i2,i3,
     .        (qobs(n,j),j=1,ngage)
c       ----------------
       n=n+1
        go to 10
1111      close(20)

       ndays=n-1
       print *,'ndays=',ndays
       
       do 5000 i=1,nq
        start(i)=0
        do j=1,ndays-1
         if(qobs(j,i).ge.0.0.and.qobs(j+1,i).ge.0.0)go to 1112
        end do
        go to 1113
1112     start(i)=j
1113     continue
        print *,i,j,start(i),' START ',sid(i)
        if(start(i).eq.0)go to 5000

        do j=1,ndays
         RA(j)=qobs(j,i)
         RB(j)=j
         efc(j,i)=-1
          high_low(j,i)=-1
         RI(j,i)=-1
         end do

        call SORT2(ndays,RA,RB)

c find median (find days with q > 0.0)
c      10th percentile and 75th percentile

        n=0
        do j=1,ndays
         if(RA(j).gt.0.0)then
           n=n+1
           qsort(n,i,1)=RA(j)
           qsort(n,i,2)=RB(j)
         end if
        end do

        numSORT(i)=n
        nn=n/2
        print *,nn,i,sid(i),n,ndays
        median(i)=qsort(nn,i,1)
        print *,'median flow =',median(i),' #days=',n
        nn=n*0.75
        p75th(i)=qsort(nn,i,1)
         print *,' p75th =',p75th(i)
         nn=n*0.10
         p10th(i)=qsort(nn,i,1)
         print *,' p10th =',p10th(i)

5000    continue
c calculate recurrence intervals
       call RecInt
c
c First classify every day as a low(1) or high ascending(2) or high descending(3) flow event
        do 6000 i=1,nq
        print *,i,start(i)
        if(start(i).eq.0)go to 6000

c       --------------
c Classify the first 2 days of flow in each time series of Q data
           i1=start(i)
           i2=i1+1
               if(qobs(i1,i).lt.0.0)then
                   print * ,i,i1,i2,' 1 qobs<0  ',sid(i),i
                   stop
               end if              
               if(qobs(i2,i).lt.0.0)then
                   print * ,i,i1,i2,' 2 qobs<0 ',sid(i),i
                   stop
               end if
           high_low(i1,i)=1
           high_low(i2,i)=1
          iflag=0
c flow on day 1 is greater than median?
            if(qobs(i1,i).gt.median(i))iflag=iflag+1
            xx=(qobs(i1,i)*0.25)+qobs(i1,i)
c flow on day 2  25% greater than on day 1?
            if(xx.lt.qobs(i2,i))iflag=iflag+1

           if(iflag.eq.0)go to 500

c high flow event -- is it ascending(2) or descending(3)?
             high_low(i1,i)=2
              high_low(i2,i)=2
              xx=qobs(i1,i)-(qobs(i1,i)*0.25)
c if flow drops by more than 25% than considered to be descending
             if(qobs(i2,i).lt.xx)then
              high_low(i1,i)=3
               high_low(i2,i)=3
             end if
500           continue
c       --------------

        i3=i2+1
         do j=i3,ndays
        iflag=0

         if(qobs(j,i).lt.0.0)go to 999

c always a low flow event if drops below the median
         if(qobs(j,i).lt.median(i))then
          high_low(j,i)=1
          iflag=1
         end if
        if(iflag.eq.1)go to 600

c 1 ---------------------------------
c If day before is a low flow day
       if(high_low(j-1,i).eq.1)then
       iflag=1

c  then this day is ascending if qobs > p75th
        if(qobs(j,i).gt.p75th(i))then
           high_low(j,i)=2
           iflag=2
         end if

c  or this day is ascending if qobs > median and daily increase is > 25%
        xx=qobs(j-1,i)+(qobs(j-1,i)*0.25)
         if(qobs(j,i).gt.median(i).and.qobs(j,i).gt.xx)then
           high_low(j,i)=2
           iflag=2
         end if
        if(iflag.eq.1)high_low(j,i)=1 
       end if
c 1 ---------------------------------

c 2 ---------------------------------
c If day before is an ascending(2) limb, 
c   continue ascending until daily flow decreases by more than 10%
c   at which time descending(3) limb is started
       if(high_low(j-1,i).eq.2)then
        xx=qobs(j-1,i)-(qobs(j-1,i)*0.10)
         if(qobs(j,i).gt.xx)then
          high_low(j,i)=2
        else
          high_low(j,i)=3
        end if
        end if
c 2 ---------------------------------

c 3 ---------------------------------
c If day before is descending, ascending is restarted 
c   if daily flow increases by more than 25%
        if(high_low(j-1,i).eq.3)then
         xx=qobs(j-1,i)+(qobs(j-1,i)*0.25)
         if(qobs(j,i).gt.xx)then
           high_low(j,i)=2
         else
           high_low(j,i)=3
         end if
        end if
c 3 ---------------------------------

c 4 ---------------------------------
c If day before is descending, event is ended 
c   if the rate of decrease of flow drops below 10%/day
c   (change in flow is between -10% and 25%)
c   Unless flow is still > p75th
        if(high_low(j-1,i).eq.3)then

        if(qobs(j,i).gt.p75th(i))then
         high_low(j,i)=3 

        else

          xx=qobs(j-1,i)-(qobs(j-1,i)*0.10)
         if(qobs(j,i).lt.xx)then
          high_low(j,i)=1
         else
          high_low(j,i)=3
         end if

        end if

        end if
c 4 ---------------------------------

c 5 ---------------------------------
       if(qobs(j,i).lt.median(i))high_low(j,i)=1
c 5 ---------------------------------

600        continue

999        continue
         end do
6000    continue


c Initial high and low flow events are calculated. Now assign EFC:
c EFC
c 1. large floods
c 2. small floods
c 3. high flow pulses
c 4. low flows
c 5. extreme low flows

       do 2000 i=1,nq
       print *,i,sid(i)
         if(start(i).eq.0)go to 2000
       do 1000 j=1,ndays
       if(qobs(j,i).lt.0.0)go to 1000

        if(high_low(j,i).gt.1)then

c high flow
c   determine efc high flow status
c   1. large floods
c   2. small floods
c   3. high flow pulses

c   high flow pulse
         efc(j,i)=3
c     small flood
          if(RI(j,i).gt.2.0)efc(j,i)=2
c     high flood
          if(RI(j,i).gt.10.0)efc(j,i)=1

        end if
        if(high_low(j,i).eq.1)then

c low flow
c   determine efc high flow status
c   4. low flows
c   5. extreme low flows
          efc(j,i)=4
c       extreme low flow
           if(qobs(j,i).lt.p10th(i))efc(j,i)=5

        end if

1000       continue
2000       continue



        open(29,file='runEFCplots.sc',status='replace')


        do 8600 i=1,nq
        print *,i,start(i),sid(i),nq
        if(start(i).eq.0)go to 8600
        close(20)
        open(20,file='./efcs/HIGH6.LOW7_subdivide.'
     .   //sid(i),status='replace')
        close(27)
        open(27,file='./efcs/EFC_subdivide.'
     .   //sid(i),status='replace')
        close(28)
        open(28,file='./efcs/nEFC_subdivide.'
     .   //sid(i),status='replace')
	write(29,197)sid(i)
197	format('./EFC_ts.sc ',a10)

        do j=1,ndays
        n=0
         if(efc(j,i).eq.1)n=6
         if(efc(j,i).eq.2)n=6
         if(efc(j,i).eq.3)n=6
         if(efc(j,i).eq.4)n=7
         if(efc(j,i).eq.5)n=7
         write(20,6)yr(j),mth(j),day(j),n,qobs(j,i),efc(j,i)
         write(27,6)yr(j),mth(j),day(j),efc(j,i),qobs(j,i)
         if(qobs(j,i).ge.0.0)
     .     write(28,*)j,yr(j),mth(j),day(j),efc(j,i),qobs(j,i)
        end do
8600    continue


        stop
6       format(i4,1x,i4,1x,i2,1x,i2,1x,f12.3,1x,i3)
        end


       subroutine RecInt
c calculates recurrence intervals (RI)
        implicit none

        real qobs(30000,1000),p75th(1000),median(1000)
        integer ndays,nq,k,m,start(1000)
       real nyr
        integer efc(30000,1000),i,j,numSORT(1000)
        real qsort(30000,1000,2),RI(30000,1000)
        common /data/qobs,p75th,median,ndays,nq,efc,
     +               qsort,numSORT,RI,start


c       RI = (nyr+1)/m
c where 
c       nyr  = number of years of observations
c       m    = rank of the observation

       do 2000 i=1,nq
c # years of data (nyr)
        nyr=float(numSORT(i)/365)
        print *,i,' #yrs =',nyr,numSORT(i),start(i)
         if(start(i).eq.0)go to 2000
        
        do 1000 j=1,numSORT(i)
         k=qsort(j,i,2)

c values are sorted lowest to highest -- calulate rank:
         m=numSORT(i)-j+1

         RI(k,i)=(nyr+1.0)/float(m)
1000        continue        

2000       continue

       return
       end

      SUBROUTINE SORT2(N,RA,RB)
      real RA(30000),RB(30000)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
          RRB=RB(L)
        ELSE
          RRA=RA(IR)
          RRB=RB(IR)
          RA(IR)=RA(1)
          RB(IR)=RB(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RB(1)=RRB
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
            RB(I)=RB(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
        RB(I)=RRB
      GO TO 10
      END

