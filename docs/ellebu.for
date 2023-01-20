C        PROGRAM ELLIPSE
C      
C     *************************************************************      
C                            VERSION 2.2
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1999
C               Modification and testing S. Brynda April 1999
C     *************************************************************

C                PROGRAM TO DETERMINE OPTIMAL ELLIPSE    
C                      EBU COPYRIGHT 1999
        SUBROUTINE ELLEBU(STATUS,PERR,RERR,SKERR,AXMIN,ORBPOS,N,
     2             TLAT,TLONG,MAJ,MIN,ORI,BLAT,BLONG,EAREA,ANTGAIN)
C
!MS$ ATTRIBUTES DLLEXPORT :: ELLEBU
C
C     This is the main subroutine for calculation of an ellipse for
C     a set of test points and an orbital position of satellite. This 
C     calculation includes also pointing error, rotation error and 
C     station keeping error.      
C     *************************************************************      
C                            VERSION 1.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1996
C      MODIFICATIONS, CREATION OF GRAPHICAL INTERFACE AND DLL:
C		STANISLAW BRYNDA, ITU
C     *************************************************************
C
C                PROGRAM TO DETERMINE OPTIMAL ELLIPSE    
C                      EBU/ITU COPYRIGHT 1999
C     --------------------- changes S. BRYNDA, ITU, Avril 1999 ----
C     ARGUMENTS:
C			STATUS - defines the behaviour of the program
C			0 - program worked OK; valid results
C			1 - invalid value of NPTS (number of test points <=0 or >50)
C			2 - invalid orbital position (ORBPOS < -180.0 or ORBPOS > 180.0)
C                       3 - invalid value of pointing error (PERR <0.0 or > 1.0)
C                       4 - invalid value of rotation error (RERR < 0.0 or > 3.0)
C                       5 - invalid value of station keeping error
C                           (SKERR < 0.0 or > 1.0).
C                       6 - invalid value of minimal value of axis (AXMIN < 0.0)
C                       7 - some points are not visible from the satellite .	.
C		INPUT:
C			PERR   - pointing error (normally 0.1 degrees, if PERR=0.0 then
C                                pointing error is not taken into account)
C			RERR   - rotation error (normally 1.0 degrees, if RERR =0.0 then
C                      rotation error is not taken into account)
C             SKERR  - station keeping error (normally 0.1 degrees, if SKERR=0.0, then
C                      station keeping error is not taken into account)
C             AXMIN  - minimum ellipse semi- axis 
C			ORBPOS - orbital position (in degrees)
C			N      - number of test points
C			TLAT   - the latitudes of test points (floating array, degrees)
C			TLONG  - the longitudes of test points (floating array, degrees)
C		OUTPUT:
C 			MAJ    - major axis (in degrees)
C			MIN    - minor axis (in degrees)
C			ORI    - orientation (in degrees)
C			BLAT   - the latitude of the ellipse boresight
C			BLONG  - the longitude of the ellipse boresight 
C			EAREA  - the area of the optimal ellipse
C			ANTGAIN- antenna gain (in dB)                
C
C
C     SUBROUTINES CALLED:
C			CONVEX
C             GENDAT
C			POLTOREC	
C			LOOP
C			ORIENT      
C
C     -------------------------------------------------------------------    

      COMMON/SATLONGI/SATLON,THC,PHC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON/PARAM/IPTERR,IROTER,ISKERR,ISKPRE,IMINEL
      COMMON/CENGRAV/THCG,PHCG
      COMMON /ERRORS/XSKERR,XPTERR,XRTERR,XMIN
      COMMON /FUNCTS/COCO,COSI,SI
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
      COMMON /NEWONE/THMAXX,THMINN,PHMAXX,PHMINN
C     --------------------- changes S. BRYNDA, ITU, Avril 1999 ----

C      COMMON/VECTORS/ZVEC,YVEC 
C	INTEGER STATUS,PEOPT,N
	INTEGER STATUS,N ! Initializing status and N (N is aninput)
      INTEGER J,NT,NTIN,NTOUT,KI !Intermediate parameters (J index of GS)
	INTEGER IPTERR,IROTER,ISKERR,ISKPRE,IAREA,IMINEL !I stands for 1:Yes 0:No; !IPTERR=Pointing error; IROTER=Rotational error; ISKERR= Station keeping error; IMINEL: MINIMUM ELLIPSE SEMI-AXIS; IAREA: Area of the ellipse
      REAL PERR,RERR,SKERR,AXMIN,ORBPOS ! Values of the errors
	REAL XSKERR,XPTERR,XRTERR,XMIN ! REDUNDANCY!! E.G.: XSKERR=ISKERR*SKERR
      REAL TLAT(100),TLONG(100) ! Arrays long and lat for GSs
      REAL DELPHI,DK2 ! DELPHI: Difference longitude GS and SAT; DK2 = (Radius of GEO orbit)^2
       REAL MAJ,MIN,ORI,BLAT,BLONG,AREA,EAREA,ANTGAIN ! OUTPUTS 
       REAL PTSGEO(400,2),PTRECT(400,3),SATLON,PTSPH(400,2) !PTSGEO (LAT,LON); SATLON: Longitude of GEO Sat
  	REAL THCG,PHCG,THMAX,THMIN,THIN,PHMAX,PHMIN,PT1,PT2,PHIN !THCG,PHCG IS CENTER OF GRAVITY OF POLYGON
 	REAL XOUT,YOUT,ZOUT,DIST2,THETA,PHC,THC,BETA,OMEGA,SMX
 	REAL SMN,ALPHA,YORI,PHI
       REAL COCO(4,2),COSI(4,2),SI(4,2)  ! COCO(I,1) = COS(TH(I,1))*COS(PH(I,1)); COSI(I,2) = COS(TH(I,2))*SIN(PH(I,2));SI(I,1)   = SIN(TH(I,1))
C      REAL COCO(4,2),COSI(4,2),SI(4,2)
  	REAL RK,RAD,TANPE,TANSK,CROT,SROT ! RK: GEO radius; RAD: 180/pi; TAN(XPTERR DEGREES); TAN(XSKERR DEGREES); CROT = COS (XRTERR DEGREES); SROT = SIN (XRTERR DEGREES) 
	CHARACTER*24  STR
	INTEGER K
        REAL*8 A,B,CK
	A=0
	B=0
	CK=0
	ALPHA=0
	BETA=0
	OMEGA=0
	K=0
      STATUS = 0
      SATLON=ORBPOS
	IPTERR=0
	ISKERR=0
	IROTER=0
	IAREA=0
	DO 5 I1=1,400
	DO 5 I2=1,3
	PTRECT(I1,I2)=0.0
    5	CONTINUE


C      REAL COCO(4,2),COSI(4,2),SI(4,2)
C      REAL PTSGEO(400,2),PTRECT(400,3),SATLON,PTSPH(400,2)
C      REAL ANG(36,2)
C      REAL*8 A,B,CK
C----------------------------------
C     --------------------- changes S. BRYNDA, ITU, Avril 1999 ----
C
C      
C      WRITE(*,*) 'POINTING ERROR ? (1= YES, 0= NO)'
C      READ(5,*) IPTERR
C
C        IF(IPTERR.EQ.0) THEN
C        XPTERR=0.
C        ELSE
C        WRITE(6,*) 'POINTING ERROR IN DEGREES'
C        READ(5,*) XPTERR
C        ENDIFC
C
C      WRITE(*,*) 'STATION KEEPING ERROR ?'
C      WRITE(*,*) '(0= NO,1=YES )'
C      READ(5,*) ISKERR
C
C        IF(ISKERR.EQ.0) THEN
C        XSKERR=0.
C        ELSE
C        WRITE(6,*) 'STATION KEEPING ERROR IN DEGREES'
C        READ(5,*) XSKERR
C        ENDIF
C
C      WRITE(*,*) 'ROTATIONAL ERROR ? (1= YES, 0= NO)'
C      READ(5,*) IROTER
C      
C        IF(IROTER.EQ.0) THEN
C        XRTERR=0.
C        ELSE
C        WRITE(6,*) 'ROTATIONAL ERROR IN DEGREES'
C        READ(5,*) XRTERR
C        ENDIF
C
C      WRITE(*,*) 'MINIMUM ELLIPSE SEMI-AXIS > 0.005 ? (1= YES, 0= NO)'
C      READ(5,*) IMINEL
C
C        IF(IMINEL.EQ.0) THEN
C        XMIN =  0.005
C        IMINEL = 1
C
C        ELSE
C        WRITE(6,*) 'MINIMUM ELLIPSE SEMI-AXIS IN DEGREES (> 0.005)'
C        READ(5,*) XMIN
C        IF(XMIN.LT.0.005) XMIN = 0.005
C        ENDIF
C ------------------------------------------------------------
C     --------------------- changes S. BRYNDA, ITU, Avril 1999 ----
	IF ((N .GT. 50) .OR. (N .LE. 0))  THEN
		STATUS=1
		GOTO 1000
	ENDIF
	IF ((ORBPOS .LT. -180.0) .OR. (ORBPOS .GT. 180.0)) THEN
		STATUS = 2
		GOTO 1000
	ENDIF 
	IF ((PERR .LT. 0.0) .OR. (PERR .GT. 1.0)) THEN
		STATUS =3 
		GOTO 1000
	ENDIF
	IF (PERR .EQ. 0.0) THEN
	        IPTERR=0
			XPTERR=0.0
	ELSE
	        IPTERR=1
			XPTERR=PERR
	ENDIF
	IF ((RERR .LT. 0.0) .OR. (RERR .GT. 3.0)) THEN
		STATUS =4 
		GOTO 1000
	ENDIF
	IF (RERR .EQ. 0.0) THEN
	        IROTER=0
			XRTERR=0.0
	ELSE
	        IROTER=1
			XRTERR=RERR
	ENDIF
 
	IF ((SKERR .LT. 0.0) .OR. (SKERR .GT. 1.0)) THEN
		STATUS =5 
		GOTO 1000
	ENDIF
	IF (SKERR .EQ. 0.0) THEN
	        ISKERR=0
			XSKERR=0.0
	ELSE
	        ISKERR=1
			XSKERR=SKERR
	ENDIF
	IF (AXMIN .LT. 0.0) THEN
		STATUS =6  
		GOTO 1000
	ENDIF
	IF (AXMIN .EQ. 0.0) THEN
	        IMINEL=0
	        XMIN=0.3
	ELSE
	        IMINEL=1
	        XMIN=AXMIN
	ENDIF
 
C	IF ((PEOPT .NE. 0) .AND. (PEOPT .NE. 1)) THEN
C		STATUS =7 
C		GOTO 1000
C	ENDIF
C      IAREA=PEOPT
      ANTGAIN=0.0
 	DO 11 I=1,20
	PTSGEO(I,1)=0.0
	PTSGEO(I,2)=0.0
   11 CONTINUE
    
C     -----------------------------------------------------------------
        
      CALL GENDAT
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----      
C      OPEN(20,STATUS='OLD',FILE='ELLDATA.DAT')
C      OPEN(21,STATUS='UNKNOWN',FILE='INTER1.DAT')
C      OPEN(22,STATUS='UNKNOWN',FILE='INTER2.DAT')
C      OPEN(23,STATUS='UNKNOWN',FILE='ELLOUT.DAT')
C      
C      READ (20,9001) N
C 9001 FORMAT(I3)
C      READ (20,9002) SATLON
C 9002 FORMAT(F7.2)
C
C     --------------------------------------------------
C   THCG,PHCG IS CENTER OF GRAVITY OF POLYGON
      THCG  =  0.
      PHCG  =  0.
C     --------------------- changes S. BRYNDA, ITU, April 1999 ---- 
C      WRITE(23,*)'                 ELLIPSE GENERATION RESULTS'
C      WRITE(23,*)
C      WRITE(23,*)
C      WRITE(23,*)'               TEST POINTS                    ORBITAL'
C      WRITE(23,*)'        LATITUDE         LONGITUDE           POSITION' 
C      WRITE(23,*)'         + for N          + for E             + for E'
C      WRITE(23,*)'         - for S          - for W             - for W'
C      WRITE(23,*)'        --------------------------           --------'
C
C     ---------------------------------------------------
      THMAXX = -1000000
      THMINN =  1000000
      PHMAXX = -1000000
      PHMINN =  1000000
       
      DO 10 J=1,N
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      PTSGEO(J,1)=TLAT(J)
      PTSGEO(J,2)=TLONG(J)
C     ----------------------------------
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
C      READ (20,9003) (PTSGEO(J,I),I=1,2)
C 9003 FORMAT(F7.2,F7.2)
C        IF(J.EQ.1) THEN
C        WRITE(23,9102) (PTSGEO(J,I),I=1,2),SATLON
C        ELSE
C        WRITE(23,9103) (PTSGEO(J,I),I=1,2)
C        ENDIF        
C 9102 FORMAT(9X,F6.2,10X,F7.2,15X,F6.1)
C 9103 FORMAT(9X,F6.2,10X,F7.2)
C
C     ----------------------------------------------------------------
      DELPHI=PTSGEO(J,2)-SATLON
        IF(DELPHI.LE.-180.) THEN
        PTSGEO(J,2)=PTSGEO(J,2)+360.
        ELSEIF(DELPHI.GE.180.) THEN
        PTSGEO(J,2)=PTSGEO(J,2)-360.
        ENDIF
        
      THCG = THCG + PTSGEO(J,1)
      PHCG = PHCG + PTSGEO(J,2)
      THMAXX = AMAX1(THMAXX,PTSGEO(j,1))
      THMINN = AMIN1(THMINN,PTSGEO(j,1))
      PHMAXX = AMAX1(PHMAXX,PTSGEO(j,2))
      PHMINN = AMIN1(PHMINN,PTSGEO(j,2))
      
   10 CONTINUE

      THCG = THCG/N
      PHCG = PHCG/N

      NFLAG=0
        IF(N.EQ.1) THEN
        THETA = PTSGEO(1,1)
        PHI   = PTSGEO(1,2)
        OMEGA = 90
          IF(XMIN.EQ.0.) THEN
          ALPHA = 10000000000.
          ELSE
          ALPHA = 1./(TAN(XMIN/RAD))**2
          ENDIF
        BETA  = ALPHA
        GOTO 200
c        ELSEIF (N.EQ.2) THEN
c        PTSGEO(3,1)=(PTSGEO(1,1)+PTSGEO(2,1))/2
c        PTSGEO(3,2)=(PTSGEO(1,2)+PTSGEO(2,2))/2
c        N=3
c        NFLAG=1
        ENDIF
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
C      DO 30 J=1,N
C      WRITE (21,9003) PTSGEO(J,1),PTSGEO(J,2)
C   30 CONTINUE
C     ----------------------------------------------
      NT = N
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
C    
C      REWIND 21
C     --------------------------------------------------------------
      DK2  = RK*RK - 1.

      NTIN = NT
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      CALL CONVEX(NTIN,NTOUT,PTSGEO)
C	-------------------------------------------------------------
      NT   = NTOUT

      DO 50 J=1,NT
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      PT1=PTSGEO(J,1)
      PT2=PTSGEO(J,2)
C      READ (22,9003) PT1,PT2
C     -------------------------------------------------------------

      THIN = PT1
      PHIN = PT2 - SATLON
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      XOUT=0.0
      YOUT=0.0
      ZOUT=0.0      
      CALL POLTOREC(THIN,PHIN,XOUT,YOUT,ZOUT)
C   DO NOT CONTINUE IF SOME POINTS ARE NOT VISIBLE FROM THE SATELLITE
      DIST2 = (RK-XOUT)**2 + YOUT**2 + ZOUT**2
        IF(DIST2.GT.DK2) THEN
          STATUS=7
C        WRITE(6,*) 'SOME POINTS NOT VISIBLE FROM SATELLITE'
          GOTO 1000
        ENDIF
C     ----------------------------------------------------------------
        IF(ISKERR.EQ.1) THEN
 
        DO 42 KI=1,4
        DIST2 = MAX(DIST2,(XOUT-RK*COCO(KI,1))**2
     1   +(YOUT-RK*COSI(KI,1))**2+(ZOUT-RK*SI(KI,1))**2)
   42   CONTINUE

        DO 44 KI=1,4
        DIST2 = MAX(DIST2,(XOUT-RK*COCO(KI,2))**2
     1     +(YOUT-RK*COSI(KI,2))**2+(ZOUT-RK*SI(KI,2))**2)
   44   CONTINUE

        IF(DIST2.GT.DK2) THEN
	     STATUS =7 
	     GOTO 1000
	  endif
        ENDIF
  
      PTRECT(J,1) = XOUT
      PTRECT(J,2) = YOUT
      PTRECT(J,3) = ZOUT
      PTSPH(J,1)  = THIN
      PTSPH(J,2)  = PHIN
   50 CONTINUE
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      A=0.0
	B=0.0
	CK=0.0
	THETA=0.0
	PHI=0.0

 
      CALL LOOP(NT,PTRECT,A,B,CK,AREA,THETA,PHI)
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
	ALPHA=0.0
	BETA=0.0
	OMEGA=0.0
C     -----------------------------------------------------------------      
      CALL ORIENT(A,B,CK,ALPHA,BETA,OMEGA)

  200 CONTINUE

      SMX =2.*57.2957795*ATAN(1./SQRT(ALPHA))
      SMN =2.*57.2957795*ATAN(1./SQRT(BETA))
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
C      WRITE(23,*)
C      WRITE(23,*)
C      WRITE(23,*)
C      WRITE(23,*) '                OPTIMAL ELLIPSE PARAMETERS'
C      WRITE(23,*)
C      WRITE(23,*)
C      WRITE(23,*) 'CONDITIONS FOR GENERATION:'
C        IF(IPTERR.EQ.1) THEN
C        WRITE(23,8881) XPTERR
C 8881 FORMAT('  1.  POINTING ERROR           =     ',F7.2,' DEGREES:')
C        ELSE
C        WRITE(23,*) ' 1.  NO POINTING ERROR'
C        END IF
C
C        IF(IROTER.EQ.1) THEN
C        WRITE(23,8882) XRTERR
C 8882 FORMAT('  2.  ROTATIONAL ERROR         = +/- ',F7.2,' DEGREES:')
C        ELSE
C        WRITE(23,*) ' 2.  NO ROTATIONAL ERROR'
C        END IF
C
C        IF(ISKERR.EQ.0) THEN
C        WRITE(23,*) ' 3.  NO STATION KEEPING ERROR'
C        ELSE
C        WRITE(23,8883) XSKERR
C 8883 FORMAT('  3.  STATION KEEPING ERROR    =     ',F7.2,' DEGREES:')
C        END IF
C
C        IF(IMINEL.EQ.1) THEN
C        WRITE(23,8884) XMIN
C 8884 FORMAT('  4.  MIN. ELLIPSE SEMI-AXIS   =     ',F7.3,' DEGREES ')
C        ELSE
C        WRITE(23,*) ' 4.  NO LIMIT TO MINIMUM ELLIPSE SEMI-AXIS'
C        END IF
C        
C      WRITE(23,*)
C      WRITE(23,*)
C      WRITE(23,*)
C     1'    MAJOR       MINOR         ORIENTATION          BORESIGHT'
C      WRITE(23,*)
C     1'     AXIS        AXIS       (WRT EQUATORIAL      LAT.      LONG.'
C      WRITE(23,*)
C     1'                               PLANE)           +N/-S      +E/-W'
C      WRITE(23,*)
C     1'    ------------------------------------------------------------'
C    ----------------------------------------------------------
      YORI = 90-OMEGA
      IF(YORI.LT.0.) YORI = 180. + YORI

        IF(PHI.LE.-180.) THEN
        PHI=PHI+360.
        ELSEIF(PHI.GT.180.) THEN
        PHI=PHI-360.
        ENDIF
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----        
C      WRITE(23,9104) SMX,SMN,YORI,THETA,PHI
C 9104 FORMAT(1X,F8.5,4X,F8.5,12X,F6.2,7X,F8.4,2X,F9.4//)
C
C      DO 8898 I=1,N
C      PTXX=PTSGEO(I,2)
C        IF(PTXX.LE.-180.) THEN
C        PTXX=PTXX+360.
C        ELSEIF(PTXX.GT.180.) THEN
C        PTXX=PTXX-360.
C        ENDIF
C 8898 CONTINUE
C
C      CALL PROJECT(THETA,PHI,ALPHA,BETA,OMEGA,ANG)
C      WRITE(23,*)
C      WRITE(23,*)'                    SCHEMATIC ELLIPSE'
C      WRITE(23,*)'         ELLIPSE (Xs), CENTER (O) AND TPs (Ys,Zs)'
C      WRITE(23,*)
C      WRITE(23,*)
C
C      IF(NFLAG.EQ.1) N=2
C      CALL DRAW(N,THETA,PHI,PTSGEO,ANG)
C      ---------------------------------------------------------------
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----  
	MAJ=SMX
	MIN=SMN
	ORI=YORI
	BLAT=THETA
	BLONG=PHI
 	EAREA=AREA
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      ANTGAIN=10.0*(LOG10(27843/(SMX*SMN)))
 1000 CONTINUE      
      
C      STOP
	RETURN
C     ----------------------------------------------------------------

      END

      SUBROUTINE LOOP(NTOT,PTRECT,A,B,CK,AREA,THETA,PHI)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1999
C     *************************************************************

C           THIS SUBROUTINE LOOPS THROUGH THE CALCULATION
C                    EBU COPYRIGHT 1999

      COMMON/SATLONGI/SATLON,THC,PHC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON/PARAM/IPTERR,IROTER,ISKERR,ISKPRE,IMINEL
      COMMON/CENGRAV/THCG,PHCG
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
      COMMON /NEWONE/THMAXX,THMINN,PHMAXX,PHMINN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----  
	SAVE /SATLONGI/
	SAVE /CONST/
	SAVE /PARAM/
	SAVE /CENGRAV/
	SAVE /MINELL/
	SAVE /NEWONE/
      INTEGER  NTOT
 	REAL     AREA,THETA,PHI
C 	REAL     THMAXX,THMINN,PHMAXX,PHMINN,AREA,THETA,PHI
C     --------------------------------------------------------------	            
      REAL PTRECT(400,3)
      REAL SATLON

      REAL*8 A,B,CK,AL,BL,CKL
      real*8 A1,B1,CK1,A2,B2,CK2,A3,B3,CK3
C     ---------------- chnges S. BRYNDA, ITU, April 1999 ------------
  	A=0.0
	B=0.0
	CK=0.0
	AREA=0.0
	THETA=0.0
	PHI=0.0
C     -------------------------------------------------------------------
      AREAl = 1000000
      DANGL = 1000000
      DISTPH = (PHMAXX - PHMINN)/2
      DISTTH = (THMAXX - THMINN)/2
      DISTP = DISTPH
      DISTT = DISTTH
      THMI = THMINN + DISTT
      DISTT= .4*DISTT
      dang = dangl
      
    5 CONTINUE
      DISTP = DISTPH
      PH1 = PHMINN + DISTP
      PH2 = PH1 - .4*DISTP
      PH3 = PH1 + .4*DISTP

      ARMIN = 1000000
      DO 100 IT = -1,1

      DISTP = DISTPH
      TH1 = THMI + IT*DISTT
      if(th1.gt.thmaxx) th1 = thmaxx
      if(th1.lt.thminn) th1 = thminn

   10 CONTINUE   
      if(ph1.gt.phmaxx) ph1 = phmaxx
      if(ph2.lt.phminn) ph2 = phminn
      if(ph3.gt.phmaxx) ph3 = phmaxx
      CALL LOOP1(NTOT,TH1,PH1,PTRECT,A1,B1,CK1,AREA1,DANG,THETA,PHI)
        IF(AREA1.LT.AREAL.OR.(AREA1.EQ.AREAL.AND.DANG.LT.DANGL)) THEN
        AREAL = AREA1
        AL     = A1
        BL     = B1
        CKL    = CK1
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG
        ENDIF
 
      CALL LOOP1(NTOT,TH1,PH2,PTRECT,A2,B2,CK2,AREA2,DANG,THETA,PHI)
        IF(AREA2.LT.AREAL.OR.(AREA2.EQ.AREAL.AND.DANG.LT.DANGL)) THEN
        AREAL = AREA2
        AL     = A2
        BL     = B2
        CKL    = CK2
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG
        ENDIF

      CALL LOOP1(NTOT,TH1,PH3,PTRECT,A3,B3,CK3,AREA3,DANG,THETA,PHI)
        IF(AREA3.LT.AREAL.OR.(AREA3.EQ.AREAL.AND.DANG.LT.DANGL)) THEN
        AREAL = AREA3
        AL     = A3
        BL     = B3
        CKL    = CK3
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG
        ENDIF

      DISTP = .6*DISTP
        IF(AREA1.LE.AREA2.AND.AREA1.LE.AREA3) THEN
        PH1 = PH1
        PH2 = PH1 - DISTP
        PH3 = PH1 + DISTP
        AREL = AREA1
        
        ELSEIF(AREA2.LE.AREA1.AND.AREA2.LE.AREA3) THEN
        PH1 = PH2
        PH2 = PH1 - DISTP
        PH3 = PH1 + DISTP
        AREL = AREA2

        ELSEIF(AREA3.LE.AREA1.AND.AREA3.LE.AREA2) THEN
        PH1 = PH3
        PH2 = PH1 - DISTP
        PH3 = PH1 + DISTP
        AREL = AREA3
        ENDIF

      IF(DISTP.LT..000006) GOTO 90
      GOTO 10
   90 CONTINUE
        IF(AREL.LT.ARMIN.or.(arel.eq.armin.and.abs(thmix-thcg).gt.
     1   abs(th1-thcg))) THEN
        ARMIN = AREL
        THMIX = TH1
        ENDIF
  100 CONTINUE
  
      DISTT = .6*DISTT
      IF(DISTT.LT..000006) GOTO 30
      THMI = THMIX

      GOTO 5

   30 CONTINUE

        AREA = AREAL
        A     = AL
        B     = BL
        CK    = CKL
        THETA = THETAL
        PHI   = PHIL

      RETURN
      END

      SUBROUTINE LOOP1(NTOT,THCT,PHCT,PTRECT,A,B,CK,AREA,DANGO
     1 ,THETA,PHI)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1999
C     *************************************************************

C           THIS SUBROUTINE LOOPS THROUGH THE CALCULATION
C                    EBU COPYRIGHT 1999

      COMMON/SATLONGI/SATLON,THC,PHC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON/PARAM/IPTERR,IROTER,ISKERR,ISKPRE,IMINEL
      COMMON/CENGRAV/THCG,PHCG
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----  
	SAVE /SATLONGI/
	SAVE /CONST/
	SAVE /PARAM/
	SAVE /CENGRAV/
	SAVE /MINELL/
	REAL AREA,DANGO,THCT,PHCT
	INTEGER NTOT
C	--------------------------------------------------------------	            
      REAL PSRECT(400,3),PTRECT(400,3),PSOUT(400,3),ANGDIF(400)
      REAL SATLON,PBS(2),PTERTH(3),PTSAT(3),PTNORM(3),PXRECT(400,3)
      REAL MGTOS(3,3),MSTOG(3,3)
      REAL*8 A,B,CK,A0,B0,CK0,AL,BL,CKL

C    'AREA' IS USED TO OPTIMISE ON ELLIPSE AREA
      AREAL = 1000000.

C    'DANG' IS USED TO OPTIMISE BORESIGHT OF ELLIPSE, IE. LEAST
C         DISTANCE FROM CENTER OF GRAVITY OF POLYGON
      DANGL = dango
      IFLAGC = 0
     
      THETA = THCT
      PHI   = PHCT

      PBS(1) = THETA
      PBS(2) = PHI
      THC    = THETA
      PHC    = PHI
      
      IF(ISKERR.EQ.1) CALL STAKEP(THC,PHC,NTOT,PTRECT,ANGDIF)

      CALL TRAMAT(PBS,MGTOS,MSTOG)

C   CONVERT FROM E-C COORDINATES FOR TEST POINTS TO 'NORMALISED'
C      S-C COORDINATES ( I.E., Z-COMPONENT ALWAYS EQUAL TO 1 )
C   PTERTH ARE THE E-C COORDINATES OF THE POINT AS SEEN FROM THE
C      SATELLITE; USING VECTOR NOTATION: OS + SP = OP, WHERE O IS
C      THE ORIGIN (CENTER OF EARTH) , S IS THE POSITION OF SATELLITE
C      ( RK , 0 , 0 ), P IS THE POINT; THUS SP = OP - OS

      DO 200 K=1,NTOT
      PTERTH(1) = PTRECT(K,1) - RK
      PTERTH(2) = PTRECT(K,2)
      PTERTH(3) = PTRECT(K,3)

C  CHANGE TO S-C SYSTEM ( I.E., ROTATE WITH MGTOS )      
      CALL ERTOSAT(MGTOS,PTERTH,PTSAT)

C   NORMALISE TO HAVE Z-COMPONENT = 1.      
      CALL NORM(PTSAT,PTNORM)
      PSRECT(K,1) = PTNORM(1)
      PSRECT(K,2) = PTNORM(2)
      PSRECT(K,3) = PTNORM(3)

  200 CONTINUE

        IF(ISKERR.EQ.1.OR.IPTERR.EQ.1.OR.IROTER.EQ.1) THEN
        CALL XTNDEL(NTOT,PSRECT,ANGDIF,MX,PXRECT)
        CALL ELIM(MX,PXRECT,M,PSOUT)
        ELSE
        CALL ELIM(NTOT,PSRECT,M,PSOUT)
        ENDIF

      DANG0  = (THETA-THCG)**2 + (PHI-PHCG)**2
        IF(IMINEL.EQ.1) THEN
        CALL MINCHK(M,PSOUT,A0,B0,CK0,AREA0)

          IF(AREA0.LT.AREAL.OR.(AREA0.EQ.AREAL.AND.DANG0.LT.DANGL)) THEN
          AREAL = AREA0
          AL     = A0
          BL     = B0
          CKL    = CK0
          THETAL = THETA
          PHIL   = PHI
          DANGL  = DANG0
          IFLAGC = 1
          ENDIF
        ENDIF

      IF(IFLAGC.EQ.1) GOTO 600
      IF(M.LT.3) GOTO 250

      CALL THREEPTS(M,PSOUT,A0,B0,CK0,AREA0)

      IF(AREA0.GE.1000000.) GOTO 250
      CALL ORIENT(A0,B0,CK0,ALPHA,BETA,OMEGA)

      IF(ALPHA.GT.COEFAB.OR.BETA.GT.COEFAB) GOTO 250

        IF(AREA0.LT.AREAL.OR.(AREA0.EQ.AREAL.AND.DANG0.LT.DANGL )) THEN
        AREAL  = AREA0
        AL     = A0
        BL     = B0
        CKL    = CK0
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG0
        ENDIF

  250 CONTINUE

      IF(M.LT.2) GOTO 400
      CALL TWOPTS(M,PSOUT,A0,B0,CK0,AREA0)

      IF(AREA0.GT.1000000.) GOTO 400
      CALL ORIENT(A0,B0,CK0,ALPHA,BETA,OMEGA)
      IF(ALPHA.GT.COEFAB.OR.BETA.GT.COEFAB) GOTO 400
     
        IF(AREA0.LT.AREAL.OR.(AREA0.EQ.AREAL.AND.DANG0.LT.DANGL)) THEN
        AREAL  = AREA0
        AL     = A0
        BL     = B0
        CKL    = CK0
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG0
        ENDIF

  400 CONTINUE

      CALL MINAX1(M,PSOUT,A0,B0,CK0,AREA0)

        IF(AREA0.LT.AREAL.OR.(AREA0.EQ.AREAL.AND.DANG0.LT.DANGL)) THEN
        AREAL  = AREA0
        AL     = A0
        BL     = B0
        CKL    = CK0
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG0
        ENDIF

  500 CONTINUE

      CALL MINAX2(M,PSOUT,A0,B0,CK0,AREA0)

        IF(AREA0.LT.AREAL.OR.(AREA0.EQ.AREAL.AND.DANG0.LT.DANGL)) THEN
        AREAL  = AREA0
        AL     = A0
        BL     = B0
        CKL    = CK0
        THETAL = THETA
        PHIL   = PHI
        DANGL  = DANG0
        ENDIF

  600 CONTINUE
      AREA  = AREAL
      A     = AL
      B     = BL
      CK    = CKL
      THETA = THETAL
      PHI   = PHIL
      DANGO = DANGL

      RETURN
      END

      SUBROUTINE POLTOREC(THIN,PHIN,XOUT,YOUT,ZOUT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************
     
C         THIS SUBROUTINE CHANGES GEO POLAR COORDINATES
C            INTO E-C RECTANGULAR COORDINATES ON UNIT EARTH
C                         EBU COPYRIGHT 1997

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----  
	SAVE /CONST/
	REAL THIN,PHIN,XOUT,YOUT,ZOUT
	XOUT=0.0
	YOUT=0.0
	ZOUT=0.0
C	-------------------------------------------------------------    
C   TRANSFORM FROM GEO TO EARTH CENTERED ( E-C ) POLAR COORDINATES;
C       AT THE SAME TIME, CONVERT FROM DEGREES TO RADIANS

      THINR = THIN/RAD
      PHINR = PHIN/RAD
C   TRANSFORM FROM E-C POLAR COORDINATES TO E-C RECTANGULAR COORDINATES 

      XOUT = COS(THINR)*COS(PHINR)
      YOUT = COS(THINR)*SIN(PHINR)
      ZOUT = SIN(THINR)

      RETURN
      END       

      SUBROUTINE TRAMAT(PBS,MGTOS,MSTOG)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE DETERMINES THE TRANSFORMATION MATRIX:

C                          'MGTOS'
C       FROM EARTH CENTERED ( E-C ) COORDINATES 
C           TO SATELLITE CENTERED ( S-C ) COORDINATES

C   AND THE TRANSFORMATION MATRIX:

C                          'MSTOG'
C       FROM S-C COORDINATES TO E-C COORDINATES.
C                   EBU COPYRIGHT 1997

C   THE E-C SYSTEM HAS ITS ORIGIN AT THE EARTH'S CENTER:
C      Z-AXIS ( Zg ) POINTED NORTH,
C      X-AXIS ( Xg ) POINTED TOWARD THE SATELLITE,
C      Y-AXIS ( Yg ) POINTED 90 DEGREES EAST OF Xg.
C   THE EARTH RADIUS ( 6378.153 km ) IS TAKEN AS THE UNIT OF DISTANCE.

C   THE SAT. IS LOCATED IN THE EQUATORIAL PLANE, AT LONGITUDE 'SATLON'.
C   THE SAT. GSO RADIUS = RK = 6.61072 ( EARTH RADII ).
C   THE S-C SYSTEM HAS ITS ORIGIN AT THE SATELLITE:
C     THE Z-AXIS ( Za ) OF THE SAT. SYSTEM POINTS FROM THE SATELLITE TO
C        A POINT ON THE EARTH (BORESIGHT) WITH ( 'GEO' ) LATITUDE 'PBS(1)',
C        AND LONGITUDE 'PBS(2)'.
C     THE Y-AXIS ( Ya ) OF THE SAT. SYSTEM IS DEFINED BY THE DIRECTION
C        Za x Zg (I.E., DIRECTED TOWARD THE EAST IN THE EQUATORIAL PLANE).
C     THE X-AXIS ( Xa ) OF THE SAT. SYSTEM IS DEFINED BY THE DIRECTION
C        OF Ya x Za.

      COMMON/SATLONGI/SATLON,THC,PHC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----  
      SAVE /SATLONGI/
	SAVE /CONST/
      REAL THETA,RAD,PHI,BSL,ZAXZGL
	INTEGER I
C     -----------------------------------------------------------------

      REAL PBS(2),SATLON,MGTOS(3,3),MSTOG(3,3),BS(3)
      REAL XA(3),YA(3),ZA(3)

C  RK =   6.61072, DISTANCE OF SATELLITE FROM CENTER OF EARTH, IN EARTH
C         RADII
C  CHANGE BORESIGHT 'GEO' COORDINATES TO E-C COORDINATES.
      THETA = PBS(1)/RAD
      PHI   = (PBS(2)-SATLON)/RAD
C     --------------------- changes S. BRYNDA, ITU, Avril 1999 ----
      DO 10 I=1,3
      MGTOS(I,1)=0.0
	MGTOS(I,2)=0.0
	MGTOS(I,3)=0.0
	MSTOG(I,1)=0.0
	MSTOG(I,2)=0.0
	MSTOG(I,3)=0.0
   10 CONTINUE
C     -----------------------------------------------------------------      
C   CALCULATE THE E-C COORDINATES OF THE SAT. AS SEEN FROM THE BORESIGHT
      BS(1) = RK-COS(THETA)*COS(PHI)
      BS(2) = -COS(THETA)*SIN(PHI)
      BS(3) = -SIN(THETA)

C   CALCULATE THE DISTANCE FROM BORESIGHT TO SAT.
      BSL   = SQRT(BS(1)**2 + BS(2)**2 + BS(3)**2)
      
C   CALCULATE THE UNIT VECTOR IN THE Za DIRECTION, IN E-C COORDINATES
      ZA(1) = -BS(1)/BSL
      ZA(2) = -BS(2)/BSL
      ZA(3) = -BS(3)/BSL

C   CALCULATE THE UNIT VECTOR IN THE Ya DIRECTION, IN E-C COORDINATES
C       Ya = ( Za x Zg ) / | Za x Zg |
      ZAXZGL = SQRT(ZA(2)**2+ZA(1)**2)

      YA(1) = ZA(2)/ZAXZGL
      YA(2) = -ZA(1)/ZAXZGL
      YA(3) = 0.

C   CALCULATE THE UNIT VECTOR IN THE Xa DIRECTION, IN E-C COORDINATES
C       Xa = ( Ya x Za )
      XA(1) = YA(2)*ZA(3)
      XA(2) =-YA(1)*ZA(3)
      XA(3) = YA(1)*ZA(2) - YA(2)*ZA(1)

C   DETERMINE TRANSFORMATION MATRIX FROM S-C TO E-C SYSTEM
      MSTOG(1,1) = XA(1)
      MSTOG(2,1) = XA(2)
      MSTOG(3,1) = XA(3)
      MSTOG(1,2) = YA(1)
      MSTOG(2,2) = YA(2)
      MSTOG(3,2) = YA(3)
      MSTOG(1,3) = ZA(1)
      MSTOG(2,3) = ZA(2)
      MSTOG(3,3) = ZA(3)

C   DETERMINE TRANSFORMATION MATRIX FROM E-C TO S-C SYSTEM
C       ORTHOGONAL MATRIX: INVERSE IS TRANSPOSE
      MGTOS(1,1) = XA(1)
      MGTOS(1,2) = XA(2)
      MGTOS(1,3) = XA(3)
      MGTOS(2,1) = YA(1)
      MGTOS(2,2) = YA(2)
      MGTOS(2,3) = YA(3)
      MGTOS(3,1) = ZA(1)
      MGTOS(3,2) = ZA(2)
      MGTOS(3,3) = ZA(3)

      RETURN
      END
      
      SUBROUTINE ERTOSAT(MGTOS,PTERTH,PTSAT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************
  
C   THIS SUBROUTINE TRANSFORMS EARTH CENTERED COORDINATES ( PTERTH() )
C      TO SATELLITE CENTERED COORDINATES ( PTSAT() )
C                   EBU COPYRIGHT 1997 
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      IMPLICIT NONE
	INTEGER I,J
C     -----------------------------------------------------------------


      REAL PTERTH(3),PTSAT(3),MGTOS(3,3)

C   PERFORM MATRIX MULTIPLICATION
C      PTSAT = MGTOS x PTERTH
      DO 100 I = 1,3
      PTSAT(I) = 0.
      DO 100 J = 1,3
      PTSAT(I) = PTSAT(I) + MGTOS(I,J)*PTERTH(J)
  100 CONTINUE

      RETURN
      END

      SUBROUTINE NORM(PTSAT,PTNORM)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE NORMALISES VECTORS IN SATELLITE SYSTEM
C      TO HAVE A UNIT Z-COMPONENT, I.E., TO LIE IN A 
C         PLANE PERPENDICULAR TO THE Z-AXIS
C                      EBU COPYRIGHT 1997
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      IMPLICIT NONE
      INTEGER I
C	--------------------------------------------------------------
      REAL PTSAT(3),PTNORM(3)

      DO 100 I=1,3
      PTNORM(I) = PTSAT(I)/PTSAT(3)
  100 CONTINUE

      RETURN
      END

      SUBROUTINE ELIM(N,PSRECT,M,PSOUT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE ELIMINATES POINTS WHICH ARE AUTOMATICALLY WITHIN
C      THE ELLIPSE IF OTHER POINTS ARE WITHIN OR ON THE ELLIPSE.
C                      EBU COPYRIGHT 1997 

C   USING TWO POINTS AND SYMMETRY WITH RESPECT TO THE ORIGIN, ANY
C      POINT CONTAINED WITHIN THE PARALLELOGRAM FORMED BY THE TWO
C         POINTS AND THEIR IMAGES REFLECTED THROUGH THE ORIGIN NEED
C            NOT BE CONSIDERED:
C      IT WILL AUTOMATICALLY ALSO BE CONTAINED WITHIN ANY
C         ELLIPSE CONTAINING THE TWO GIVEN POINTS.
C     --------------------- changes S. BRYNDA, ITU, April 1999 ----
      IMPLICIT NONE
      INTEGER N,M,N6,I,J,K
	REAL PT1,PT2,PX1,PX2,X1,X2,X3,Y1,Y2,Y3,P1,P2
C	--------------------------------------------------------------

      REAL PSRECT(400,3),PSOUT(400,3),M1,M2
      INTEGER INVEC(400)
      REAL RLEN(400)

      N6=N
      DO 50 I=1,N6
      INVEC(I) = 1
   50 CONTINUE

C GET RID OF SYMMETRIC POINTS
      DO 65 I=1,N6
      RLEN(I) = SQRT(PSRECT(I,1)**2 + PSRECT(I,2)**2)
   65 CONTINUE

      DO 80 I=1,N6
      IF(INVEC(I).EQ.0) GOTO 80
      PT1 = PSRECT(I,1)
      PT2 = PSRECT(I,2)
      DO 70 J=1,N6
      IF(J.EQ.I.OR.INVEC(J).EQ.0) GOTO 70
      PX1 = PSRECT(J,1)
      PX2 = PSRECT(J,2)
      IF(ABS(RLEN(I)-RLEN(J)).GT..0000001) GOTO 70
        IF((ABS(PT1-PX1).LT..0000001.AND.ABS(PT2-PX2).LT..0000001).OR.
     1  (ABS(PT1+PX1).LT..0000001.AND.ABS(PT2+PX2).LT..0000001)) THEN
          IF(RLEN(I).GE.RLEN(J)) THEN
          INVEC(J) = 0
          ELSE
          INVEC(I) = 0
          GOTO 80
          ENDIF
        ENDIF
     
   70 CONTINUE
   80 CONTINUE
   
C ELIMINATE SUPERFLUOUS POINTS
      DO 300 I=1,N6-1
      IF(INVEC(I).EQ.0) GOTO 300
      X1 = PSRECT(I,1)
      Y1 = PSRECT(I,2)
      DO 200 J=I+1,N6
      IF(INVEC(J).EQ.0) GOTO 200
      X2 = PSRECT(J,1)
      Y2 = PSRECT(J,2)
      DO 100 K=1,N6
      IF(K.EQ.I.OR.K.EQ.J.OR.INVEC(K).EQ.0) GOTO 100
      X3 = PSRECT(K,1)
      Y3 = PSRECT(K,2)
        IF(ABS(X1).LT..00000001.AND.ABS(X2).LT..00000001) THEN
        IF(ABS(X3).GT..00000001) GOTO 100
        IF(ABS(Y3).GT.ABS(Y1).AND.ABS(Y3).GT.ABS(Y2)) GOTO 100
        INVEC(K) = 0
        
        ELSEIF(ABS(X1-X2).LE..00000001) THEN
        P2 = (X1*Y2-X2*Y1) / (X1+X2)
        M2 = (Y1+Y2) / (X1+X2)
        IF((ABS(Y3-M2*X3).LE.ABS(P2)).AND.
     1       (ABS(X3).LE.ABS(X1))) INVEC(K)=0

        ELSEIF(ABS(X1+X2).LE..00000001) THEN
        P1 = (X2*Y1-X1*Y2) / (X2-X1)
        M1 = (Y2-Y1) / (X2-X1)
        IF((ABS(Y3-M1*X3).LE.ABS(P1)).AND.
     1      (ABS(X3).LE.ABS(X1))) INVEC(K)=0

        ELSE
        P1 = (X2*Y1-X1*Y2) / (X2-X1)
        M1 = (Y2-Y1) / (X2-X1)
        P2 = (X1*Y2-X2*Y1) / (X1+X2)
        M2 = (Y1+Y2) / (X1+X2)
        IF((ABS(Y3-M1*X3).LE.ABS(P1)).AND.
     1     (ABS(Y3-M2*X3).LE.ABS(P2))) INVEC(K)=0
        ENDIF
        
  100 CONTINUE
  200 CONTINUE
  300 CONTINUE

      M = 0        
      DO 400 I=1,N6
      IF(INVEC(I).EQ.0) GOTO 400
      M = M+1
      PSOUT(M,1) = PSRECT(I,1)
      PSOUT(M,2) = PSRECT(I,2)
      PSOUT(M,3) = PSRECT(I,3)
  400 CONTINUE

      RETURN
      END

      SUBROUTINE MINCHK(M,PSOUT,A0,B0,CK0,AREA0)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   THIS SUBROUTINE DETERMINES WHETHER ALL POINTS ARE CONTAINED
C     IN THE MINIMUM ELLIPSE HAVING THE SMALLEST AXES
C                   EBU COPYRIGHT 1998

      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /MINELL/
	INTEGER    M
	REAL       AREA0,XL,YL
C     ------------------------------------------------------------------      
      REAL PSOUT(400,3)
      REAL*8 A0,B0,CK0,RAMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      A0=0.0
      B0=0.0
	CK0=0.0
C     -------------------------------------------------------------------
      RAMIN = AMIN
      AREA0 = 1000001.
      
      DO 100 I=1,M

      XL = PSOUT(I,1)
      YL = PSOUT(I,2)

      IF(COEFAB*(XL*XL + YL*YL).GT.1.) RETURN

  100 CONTINUE

      AREA0 = ARMIN
      A0    = 1/RAMIN**2
      B0    = A0
      CK0   = 0

      RETURN
      END

      SUBROUTINE MINAX1(M,PSOUT,A0,B0,CK0,AREA0)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   THIS SUBROUTINE DETERMINES AN ELLIPSE, WITH A GIVEN MINIMUM
C     SEMI-MINOR AXIS, HAVING THE SMALLEST AREA, CONTAINING ONE POINT
C       OF PSOUT, AND ENCOMPASSING THE REST (M-1). THE SEMI-MAJOR AXIS
C         IS DEFINED AS THE DISTANCE TO THE POINT.
C                   EBU COPYRIGHT 1998

      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /MINELL/      
	INTEGER M
	REAL AREA0
C     ----------------------------------------------------------------      
      REAL PSOUT(400,3)
      REAL*8 XL,YL,DL2,DL,ALP,BET,CO,CO2,SI,SI2,CK1,RAMIN,A,B,CK,XJ,YJ
      REAL*8 A0,B0,CK0
      
      RAMIN = AMIN
      BET = 1/RAMIN**2
      AREA0 = 1000001.
      
      DO 200 I=1,M
      XL  = PSOUT(I,1)
      YL  = PSOUT(I,2)
      DL2 = XL**2 + YL**2
      DL  = DSQRT(DL2)
      
      IF(DL.LT.AMIN) GOTO 200
      ALP = 1/DL2
      CO  = XL/DL
      CO2 = CO**2
      SI  = YL/DL
      SI2 = SI**2
      CK1 = 2*CO*SI
      A   = CO2*ALP + SI2*BET
      B   = CO2*BET + SI2*ALP
      CK  = CK1*(ALP - BET)

      DO 100 J=1,M
c      IF(J.EQ.I) GOTO 100
      XJ  = PSOUT(J,1)
      YJ  = PSOUT(J,2)

      IF(A*XJ**2+B*YJ**2+CK*XJ*YJ.GT.1.00001) GOTO 200
  100 CONTINUE
      
      AREA = 3.1415926*DL*RAMIN

        IF(AREA.LT.AREA0) THEN
        AREA0 = AREA
        A0    = A
        B0    = B
        CK0   = CK
        ENDIF

  200 CONTINUE

      RETURN
      END

      SUBROUTINE MINAX2(M,PSOUT,A0,B0,CK0,AREA0)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1999
C     *************************************************************

C   THIS SUBROUTINE DETERMINES AN ELLIPSE HAVING A GIVEN SEMI-MINOR
C      AXIS, CONTAINING TWO POINTS OF PSOUT, AND ENCOMPASSING
C         THE REST (M-2).
C                   EBU COPYRIGHT 1999 

      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /MINELL/
	INTEGER M
	REAL    AREA0      
C	---------------------------------------------------------------      
      REAL PSOUT(400,3)
      REAL*8 A0,B0,CK0,SI1,SI2,CO1,CO2,A,B,CK,RBET2,RAMIN,THFIN,ALP2

      PIHALF = 1.57079632679
      PI     = 3.14159265359
      AREA0  = 1000001.
      BET2   = 1/AMIN2
      RAMIN = AMIN
      RBET2 = 1/RAMIN**2
      
      DO 400 I=1,M-1
      X1 = PSOUT(I,1)
      Y1 = PSOUT(I,2)
      R12 = X1**2+Y1**2
      IF(R12.LE.AMIN2) GOTO 400
      C1 = 1/R12
      D1 = C1 - BET2
      R1 = SQRT(R12)
      CV = X1/R1
      IF(CV.GE.1.) CV = 1.
      IF(CV.LE.-1.) CV = -1.
        IF(Y1.GE.0.) THEN
        TH1 = ACOS(CV)
        ELSE
        TH1 = ACOS(-CV)
        ENDIF
      IF(TH1.GT.PIHALF) TH1 = TH1 - PI
      
      DO 300 J=I+1,M
      X2 = PSOUT(J,1)
      Y2 = PSOUT(J,2)
      R22 = X2**2+Y2**2
      IF(R22.LE.AMIN2) GOTO 300

      R2 = SQRT(R22)

      CV = X2/R2
      IF(CV.GE.1.) CV = 1.
      IF(CV.LE.-1.) CV = -1.
        IF(Y2.GE.0.) THEN
        TH2 = ACOS(CV)
        ELSE
        TH2 = ACOS(-CV)
        ENDIF
      IF(TH2.GT.PIHALF) TH2 = TH2 - PI

        IF(TH1-TH2.GT.PIHALF) THEN
        TH2 = TH2+PI
        ELSEIF(TH2-TH1.GT.PIHALF) THEN
        TH1 = TH1+PI
        ENDIF

      CALL SOLVE(X1,Y1,X2,Y2,TH1,TH2,ALP2,THFIN)
      IF (ALP2.LT.0.) GOTO 300

      SI1  = DSIN(THFIN)
      SI2  = SI1**2
      CO1  = DCOS(THFIN)
      CO2  = CO1**2
      A    = CO2*ALP2 + SI2*RBET2
      B    = CO2*BET2 + SI2*ALP2
      CK   = 2*CO1*SI1*(ALP2-RBET2)

      DO 100 L=1,M
c      IF(L.EQ.I.OR.L.EQ.J) GOTO 100
      XL = PSOUT(L,1)
      YL = PSOUT(L,2)

      Q  = A*XL*XL + B*YL*YL + CK*XL*YL
      
C   DUE TO INPRECISION IN CERTAIN CASES OF SYMMETRY,
C   TEST INSTEAD FOR Q.GT.1.00001
C      IF(Q.GT.1.) GOTO 300
      IF(Q.GT.1.000001) GOTO 300
  100 CONTINUE

      IF(4*A*B-CK*CK.LE.0) GOTO 300
      AREA = 2.*3.14159/DSQRT(4*A*B-CK*CK)

        IF(AREA.LT.AREA0) THEN
        AREA0 = AREA
        A0    = A
        B0    = B
        CK0   = CK
        ENDIF

  300 CONTINUE
  400 CONTINUE        

      RETURN
      END

      SUBROUTINE SOLVE(X1,Y1,X2,Y2,TH1,TH2,ALP,THFIN)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1999
C     *************************************************************

C   THIS SUBROUTINE SOLVES TRIG. EQUATIONS APPROXIMATELY, USING 
C         DOUBLE PRECISION
C                   EBU COPYRIGHT 1999


      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
	SAVE /MINELL/
	REAL X1,Y1,X2,Y2,TH1,TH2
C	---------------------------------------------------------------
      REAL*8 RAMIN,RBET,RX1,RY1,RX2,RY2,RC1,RC2,RD1,RD2
      REAL*8 RTH1,RTH2,ALP,THFIN,alp1,alp2
      REAL*8 THFIN1,THFIN2,RNU1,RNU2,RDE1,RDE2,R1,R2
      RAMIN = AMIN
      RBET = 1/(RAMIN*RAMIN)

      RTH1 = TH1
      RTH2 = TH2
      
      RX1 = X1
      RY1 = Y1
      RX2 = X2
      RY2 = Y2

      RC1 = 1/(RX1**2 + RY1**2)
      RD1 = RC1 - RBET
      RC2 = 1/(RX2**2 + RY2**2)
      RD2 = RC2 - RBET

      R1 = DSQRT(-RD1)
      R2 = DSQRT(-RD2)
      RNU1 = R1*DCOS(RTH2) + R2*DCOS(RTH1)
      RDE1 = R1*DSIN(RTH2) + R2*DSIN(RTH1)
      RNU2 = R1*DCOS(RTH2) - R2*DCOS(RTH1)
      RDE2 = R1*DSIN(RTH2) - R2*DSIN(RTH1)

        IF(RDE1.EQ.0.D0) THEN
          IF(RNU1.GE.0.D0) THEN
          THFIN1 = -1.5707963267949D0
          ELSEIF(RNU1.LT.0.D0) THEN
          THFIN1 = 1.5707963267949D0
          ENDIF
        ELSE          
        THFIN1 = DATAN(-RNU1/RDE1)
        ENDIF
C     S.Brynda 12/04/99
        IF(DABS(THFIN1-RTH1).LT.1.570796327.AND.
     1     DABS(THFIN1-RTH1).GT.1.570796326)THEN 	 
        ALP1 = RC1 + RD1*DTAN(-1.5707963267949D0)**2
        ELSE 
        ALP1 = RC1 + RD1*DTAN(THFIN1-RTH1)**2
        ENDIF
      
        IF(RDE2.EQ.0.D0) THEN
          IF(RNU2.GE.0.D0) THEN
          THFIN2 = -1.5707963267949D0
          ELSEIF(RNU2.LT.0.D0) THEN
          THFIN2 = 1.5707963267949D0
          ENDIF
        ELSE          
        THFIN2 = DATAN(-RNU2/RDE2)
        ENDIF
C	 S.Brynda 12/04/99        
        IF(DABS(THFIN2-RTH1).LT.1.570796327.AND.
     1     DABS(THFIN2-RTH1).GT.1.570796326)THEN
        ALP2 = RC1 + RD1*DTAN(-1.5707963267949D0)**2
        ELSE 
        ALP2 = RC1 + RD1*DTAN(THFIN2-RTH1)**2
        ENDIF
        
        IF(ALP1.GT.0.) THEN
        ALP = ALP1
        THFIN = THFIN1
        ELSEIF(ALP2.GT.0) THEN
        ALP = ALP2
        THFIN = THFIN2
        ELSE
        ALP = -1.
        ENDIF

      RETURN
      END

      SUBROUTINE THREEPTS(M,PSOUT,A0,B0,CK0,AREA0)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1997
C     *************************************************************

C   THIS SUBROUTINE DETERMINES AN ELLIPSE CONTAINING THREE POINTS
C      OF PSOUT, AND ENCOMPASSING THE REST (M-3).
C                     EBU COPYRIGHT 1998

      REAL PSOUT(400,3)
      REAL*8 A0,B0,CK0,X1,Y1,X2,Y2,X3,Y3,XL,YL,A,B,CK
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
	SAVE /MINELL/
	REAL AREA0
	INTEGER M
	INTEGER I,J,K,L
C	REAL*8 AREA,Q    Brynda ??? AREA and Q should be rather REAL*8
C	---------------------------------------------------------------

      AREA0 = 1000001.
      
      DO 400 I=1,M-2
      X1 = PSOUT(I,1)
      Y1 = PSOUT(I,2)
      
      DO 300 J=I+1,M-1
      X2 = PSOUT(J,1)
      Y2 = PSOUT(J,2)
      
      DO 200 K=J+1,M
      X3 = PSOUT(K,1)
      Y3 = PSOUT(K,2)
      
      CALL DETRM3(X1,Y1,X2,Y2,X3,Y3,A,B,CK)

      IF(A.LE.0.OR.B.LE.0.OR.4*A*B-CK*CK.LE.0.) GOTO 200
c
      IF(A + B - DSQRT((A-B)**2 + CK*CK).GT. 2*COEFAB) GOTO 200
      
C   CHECK TO SEE IF ALL OTHER POINTS LIE WITHIN OR ON THE ELLIPSE
      DO 100 L=1,M
c      IF(L.EQ.I.OR.L.EQ.J.OR.L.EQ.K) GOTO 100
      XL = PSOUT(L,1)
      YL = PSOUT(L,2)
      Q  = A*XL*XL + B*YL*YL + CK*XL*YL
C   DUE TO INPRECISION IN CERTAIN CASES OF SYMETRY,
C   TEST INSTEAD FOR Q.GT.1.00001
C      IF(Q.GT.1.) GOTO 200
      IF(Q.GT.1.00001) GOTO 200
  100 CONTINUE

      
        AREA = 2.*3.14159/DSQRT(4*A*B-CK*CK)

        IF(AREA.LT.AREA0) THEN
        AREA0 = AREA
        A0    = A
        B0    = B
        CK0   = CK

        ENDIF

  200 CONTINUE
  300 CONTINUE
  400 CONTINUE        

      RETURN
      END

      SUBROUTINE DETRM3(X1,Y1,X2,Y2,X3,Y3,A,B,CK)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   SOLUTION TO LINEAR EQUATIONS IN 3 UNKNOWNS : A,B,CK
C                   EBU COPYRIGHT 1998
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      IMPLICIT NONE
C	--------------------------------------------------------------- 
      REAL*8 X1,Y1,X2,Y2,X3,Y3,A,B,CK,D

      A  = 0
      B  = 0
      CK = 0
      
C   DETERMINANT
      D = X1*X1*Y2*Y3*(X3*Y2-X2*Y3)+
     1         X2*X2*Y1*Y3*(X1*Y3-X3*Y1)+
     1              X3*X3*Y1*Y2*(X2*Y1-X1*Y2)

      A  = (Y1*Y1*(X2*Y2-X3*Y3)+Y2*Y2*(X3*Y3-X1*Y1)+Y3*Y3*(X1*Y1-X2*Y2))
      A  = A/D
      B  = (X1*X1*(X3*Y3-X2*Y2)+X2*X2*(X1*Y1-X3*Y3)+X3*X3*(X2*Y2-X1*Y1))
      B  = B/D
      CK = (X1*X1*(Y2*Y2-Y3*Y3)+X2*X2*(Y3*Y3-Y1*Y1)+X3*X3*(Y1*Y1-Y2*Y2))
      CK = CK/D

      RETURN
      END

      SUBROUTINE TWOPTS(M,PSOUT,A0,B0,CK0,AREA0)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   THIS SUBROUTINE DETERMINES AN ELLIPSE HAVING THE SMALLEST 
C      AREA, CONTAINING TWO POINTS OF PSOUT, AND ENCOMPASSING
C         THE REST (M-2).
C                   EBU COPYRIGHT 1998

      REAL PSOUT(400,3)
      REAL*8 A0,B0,CK0,X1,Y1,X2,Y2,XL,YL,A,B,CK
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /MINELL/
C	REAL*8 AREA,Q         Brynda AREA and Q should be rather REAL*8
	REAL AREA0
	INTEGER I,J,K,L,M
C	---------------------------------------------------------------
      AREA0 = 1000001.
      
      DO 400 I=1,M-1
      X1 = PSOUT(I,1)
      Y1 = PSOUT(I,2)
      
      DO 300 J=I+1,M
      X2 = PSOUT(J,1)
      Y2 = PSOUT(J,2)

      CALL DETRM2(X1,Y1,X2,Y2,A,B,CK)

      IF(A.LE.0.OR.B.LE.0.OR.4*A*B-CK*CK.LE.0.) GOTO 300
c
      IF(A + B - DSQRT((A-B)**2 + CK*CK).GT. 2*COEFAB) GOTO 300

      DO 100 L=1,M
c      IF(L.EQ.I.OR.L.EQ.J) GOTO 100
      XL = PSOUT(L,1)
      YL = PSOUT(L,2)

      Q  = A*XL*XL + B*YL*YL + CK*XL*YL
      
C   DUE TO INPRECISION IN CERTAIN CASES OF SYMETRY,
C   TEST INSTEAD FOR Q.GT.1.00001
C      IF(Q.GT.1.) GOTO 300
      IF(Q.GT.1.00001) GOTO 300
  100 CONTINUE

      AREA = 2.*3.14159/SQRT(4*A*B-CK*CK)

        IF(AREA.LT.AREA0) THEN
        AREA0 = AREA
        A0    = A
        B0    = B
        CK0   = CK
        ENDIF

  300 CONTINUE
  400 CONTINUE        

      RETURN
      END

      SUBROUTINE DETRM2(X1,Y1,X2,Y2,A,B,CK)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEVBER 1998
C     *************************************************************

C   THIS SUBROUTINE EVALUATES A DETERMINANT.      
C                   EBU COPYRIGHT 1998       

      REAL*8 X1,Y1,X2,Y2,A,B,CK,D

      D  =    (X1*Y2-X2*Y1)**2
        IF(D.EQ.0.) THEN
C DEGENERATE ELLIPSE; STRAIGHT LINE
        A = 0
        B = 0
        CK = 0
        ELSE
        A  =    (Y1*Y1+Y2*Y2)/D
        B  =    (X1*X1+X2*X2)/D
        CK = -2*(X1*Y1+X2*Y2)/D
        ENDIF
      
      RETURN
      END   
      
      SUBROUTINE ORIENT(A,B,CK,ALPHA,BETA,OMEGA)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   GIVEN ELLIPSE, A*X**2+B*Y*2+CK*X*Y=1, THE NORMAL ORIENTATION 
C      IS FOUND ALPHA*X'**2+BETA*Y'**2=1; OMEGA IS THE ROTATION
C         OF THE X'-Y' AXES RELATIVE TO THE X-Y AXES.
C                        EBU COPYRIGHT 1998

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
	SAVE /CONST/
	REAL ALPHA,BETA,OMEGA
C	---------------------------------------------------------------
      REAL*8 A,B,CK,SURD
      SURD  = DSQRT(CK**2 + (A-B)**2)
      
C   ALPHA IS THE (INVERSE SQUARED SEMI-)MAJOR AXIS; I.E., ALPHA =< BETA
C   OMEGA GIVES THE ORIENTATION OF THE MAJOR AXIS RELATIVE TO THE X-AXIS
      ALPHA = (A+B-SURD)/2.
      BETA  = (A+B+SURD)/2.
        IF(A.EQ.B) THEN
          IF(CK.EQ.0.) THEN
          OMEGA = 0.
          ELSEIF(CK.LT.0.) THEN
          OMEGA = 45.
          ELSE
          OMEGA = 135.
          ENDIF
        ELSEIF(A.GT.B) THEN
        OMEGA = .5*RAD*DATAN(CK/(A-B)) + 90.
        ELSEIF(A.LT.B) THEN
        OMEGA = .5*RAD*DATAN(CK/(A-B))
        IF(OMEGA.LT.0.) OMEGA = OMEGA + 180.
        ENDIF

      RETURN
      END

      SUBROUTINE XTNDEL(N,PSRECT,ANGDIF,MX,PXRECT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE EXTENDS THE POINTS TO BE CONSIDERED SO 
C      THAT THE VARIOUS POINTING/ROTATIONAL ERRORS CAN BE
C         TAKEN INTO ACCOUNT.
C                     EBU COPYRIGHT 1997 

      COMMON /ERRORS/XSKERR,XPTERR,XRTERR,XMIN
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON/PARAM/IPTERR,IROTER,ISKERR,ISKPRE,IMINEL
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /ERRORS/
      SAVE /CONST/
      SAVE /PARAM/
	REAL DIFANG,XDIF
	INTEGER N,MX,I,J,K,NTOT
C	---------------------------------------------------------------
      REAL PSRECT(400,3),PXRECT(400,3),ANGDIF(400)
      REAL PTIN(3),PTOUT(3),PT3OUT(3,3),PT7OUT(7,3)
      REAL PTMED1(400,3),PTMED2(400,3)

      MX=N

        IF(ISKERR.EQ.1) THEN

        DO 100 I=1,MX

        DO 10 J=1,3
        PTIN(J) = PSRECT(I,J)
   10   CONTINUE

        DIFANG=ANGDIF(I)
        CALL SKERR(PTIN,DIFANG,PTOUT)
        
        DO 20 J=1,3
        PTMED1(I,J) = PTOUT(J)
   20   CONTINUE

  100   CONTINUE

        ELSE

        DO 200 I=1,MX

        DO 180 J=1,3
        PTMED1(I,J) = PSRECT(I,J)
  180   CONTINUE

  200   CONTINUE

        ENDIF

      NTOT=0

        IF(IPTERR.EQ.1) THEN

        DO 300 I=1,MX

        DO 280 J=1,3
        PTIN(J)=PTMED1(I,J)
  280   CONTINUE

C  IF WITHIN MINIMUM SEMI-AXIS DISTANCE,REDUCED BY XPTERR
        XDIF=XMIN-XPTERR
        IF(XDIF.LE.0.) XDIF=0.
          IF(PTIN(1)**2+PTIN(2)**2.LT.(XDIF/RAD)**2) THEN
          NTOT=NTOT+1
          DO 285 J=1,3
          PTMED2(NTOT,J)=PTIN(J)
  285     CONTINUE

          ELSE

          CALL PTERR(PTIN,PT7OUT)
          DO 290 K=1,7
          NTOT=NTOT+1
          DO 290 J=1,3
          PTMED2(NTOT,J) = PT7OUT(K,J)
  290     CONTINUE

          ENDIF  

  300   CONTINUE

        MX=NTOT

        ELSE

        DO 400 I=1,MX
        NTOT=NTOT+1

        DO 380 J=1,3
        PTMED2(I,J) = PTMED1(I,J)
  380   CONTINUE

  400   CONTINUE

        MX=NTOT

        ENDIF
        
        IF(IROTER.EQ.1) THEN

        NTOT=0

        DO 500 I=1,MX

        DO 460 J=1,3
        PTIN(J) = PTMED2(I,J)
  460   CONTINUE
      
        CALL ROTERR(PTIN,PT3OUT)

        DO 480 K=1,3
        NTOT = NTOT + 1
        DO 480 J=1,3
        PXRECT(NTOT,J) = PT3OUT(K,J)
  480   CONTINUE

  500   CONTINUE

        MX=NTOT

        ELSE

        NTOT=0

        DO 600 I=1,MX

        NTOT=NTOT+1

        DO 580 J=1,3
        PXRECT(I,J) = PTMED2(I,J)
  580   CONTINUE

  600 CONTINUE

        MX=NTOT

        ENDIF

      RETURN
      END

      SUBROUTINE SKERR(PTIN,DIFANG,PTOUT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C  THIS SUBROUTINE TAKES POINT PTIN AND TRANSFORMS IT WITH 
C     RESPECT TO STATION KEEPING ERROR(.1 DEGREE)
C                  EBU COPYRIGHT 1997 

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/      
	REAL DIFANG
      REAL X,Y,D,FAC
C	----------------------------------------------------------------	      
      REAL PTIN(3),PTOUT(3)

      TANSK=TAN(DIFANG)
             
      X   =  PTIN(1)
      Y   =  PTIN(2)
      D   =  SQRT(X**2+Y**2)

        IF(D.EQ.0.) THEN
        FAC = 1.
        ELSE
        FAC = (1 + TANSK/D)/(1. - TANSK*D)

        ENDIF
C   STRETCH VECTOR BY DIFANG      
      PTOUT(1)  = X*FAC
      PTOUT(2)  = Y*FAC
      PTOUT(3)  = 1.

      RETURN
      END

      SUBROUTINE PTERR(PTIN,PT7OUT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C  THIS SUBROUTINE TAKES POINT PTIN AND TRANSFORMS IT WITH 
C     RESPECT TO POINTING ERROR(.1 DEGREE)
C                  EBU COPYRIGHT 1997 

      COMMON /ERRORS/XSKERR,XPTERR,XRTERR,XMIN
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /ERRORS/
	SAVE /CONST/
      
      REAL X,Y,D,FAC,XX,YY,DELX,DELY,ANG
	INTEGER I
C	---------------------------------------------------------------	      
      REAL PTIN(3),PT7OUT(7,3)

C TANPE = TAN(XPTERR DEGREES)
      X    = PTIN(1)
      Y    = PTIN(2)
      D    = SQRT(X**2+Y**2)

      FAC  = (1 + TANPE/D)/(1. - TANPE*D)

C   STRETCH VECTOR BY XPTERR DEGREE
      XX   = X*FAC
      YY   = Y*FAC
      DELX = XX-X
      DELY = YY-Y

      DO 100 I=1,7
      ANG = (-120+I*30)/RAD
      PT7OUT(I,1)  = X+DELX*COS(ANG)-DELY*SIN(ANG)
      PT7OUT(I,2)  = Y+DELY*COS(ANG)+DELX*SIN(ANG)
      PT7OUT(I,3)  = 1.
  100 CONTINUE

      RETURN
      END

      SUBROUTINE ROTERR(PTIN,PTSOUT)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C  THIS SUBROUTINE TAKES POINT PTIN AND TRANSFORMS IT WITH
C     RESPECT TO ROTATION ERROR(+-1 DEGREE).
C                   EBU COPYRIGHT 1997 

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON /ERRORS/XSKERR,XPTERR,XRTERR,XMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/
	SAVE /ERRORS/      
      REAL X0,Y0
C	---------------------------------------------------------------      
      REAL PTIN(3),PTSOUT(3,3)

C CROT = COS (XRTERR DEGREES)  
C SROT = SIN (XRTERR DEGREES)   

      X0 = PTIN(1)
      Y0 = PTIN(2)

C   ROTATE VECTOR BY 0, +XRTERR, -XRTERR DEGREES 
      PTSOUT(1,1)  = X0
      PTSOUT(1,2)  = Y0
      PTSOUT(1,3)  = 1.

      PTSOUT(2,1) = X0*CROT - Y0*SROT
      PTSOUT(2,2) = X0*SROT + Y0*CROT
      PTSOUT(2,3) = 1.
      
      PTSOUT(3,1) = X0*CROT + Y0*SROT
      PTSOUT(3,2) =-X0*SROT + Y0*CROT
      PTSOUT(3,3) = 1.

      RETURN
      END

      SUBROUTINE TRANSYZ(MGTOS)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C  THIS SUBROUTINE CALCULATES THE COMPONENTS OF A .1 DEGREE 
C     DISPLACEMENT IN THE EQUATORIAL PLANE (N-S, AND E-W)
C        IN THE S-C COORDINATE SYSTEM.
C                      EBU COPYRIGHT 1997 

      COMMON /VECTORS/ZVEC,YVEC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /VECTORS/
	SAVE /CONST/
      REAL DIST
C	---------------------------------------------------------------
      REAL MGTOS(3,3)
      REAL ZVEC(3),YVEC(3)

C  TANSK= TAN(.1 DEGREE) = .001745331024
C  RK = 6.61072 : THE DISTANCE IN EARTH RADII TO GEO ORBIT

      DIST = RK*TANSK

C CALCULATE ZVEC: (0,0,DIST) IN S-C COORDINATES
      ZVEC(1) = DIST*MGTOS(1,3)
      ZVEC(2) = DIST*MGTOS(2,3)
      ZVEC(3) = DIST*MGTOS(3,3)

C CALCULATE YVEC: (0,DIST,0) IN S-C COORDINATES
      YVEC(1) = DIST*MGTOS(1,2)
      YVEC(2) = DIST*MGTOS(2,2)
      YVEC(3) = DIST*MGTOS(3,2)

      RETURN
      END    

      SUBROUTINE PROJECT(THETA,PHI,ALPHA,BETA,OMEGA,ANG)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE PROJECTS SATELLITE ELLIPSE ONTO THE EARTH
C                    EBU COPYRIGHT 1997 

      COMMON/SATLONGI/SATLON,THC,PHC
      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /SATLONGI/
	SAVE /CONST/ 
	REAL THETA,PHI,ALPHA,BETA,OMEGA
C	----------------------------------------------------------------
      REAL ANG(36,2),MGTOS(3,3),MSTOG(3,3)
      REAL PBS(2),ELPTS(36,2)
      REAL PTSAT(3),PTERTH(3)

      RK2    = RK**2
      PBS(1) = THETA
      PBS(2) = PHI

C   DETERMINE TRANSFORMATION MATRIX
      CALL TRAMAT(PBS,MGTOS,MSTOG)

C   DETERMINE POLYGON APPROX. TO ELLIPSE
      CALL ELPOLY(ALPHA,BETA,ELPTS)

      PTSAT(3) = 1.
      DO 100 I=1,36
      XPRI = ELPTS(I,1)
      YPRI = ELPTS(I,2)

C   ROTATE COMPONENTS BACK TO S-C COORDINATE SYSTEM
      CALL ROTATE(XPRI,YPRI,OMEGA,X,Y)
      PTSAT(1) = X
      PTSAT(2) = Y

C   EVALUATE COMPONENTS IN E-C SYSTEM      
      CALL SATOERT(MSTOG,PTSAT,PTERTH)
      XE = PTERTH(1)
      YE = PTERTH(2)
      ZE = PTERTH(3)

      R2 = XE*XE + YE*YE + ZE*ZE
      QT = XE**2*RK2- (RK2-1)*R2
        IF(QT.GE.0) THEN
        FAC   = (-XE*RK - SQRT(QT))/R2
        XE    = RK + FAC*XE
        YE    = FAC*YE
        ZE    = FAC*ZE
        ELSE
        PLROT = MSTOG(1,1)*X + MSTOG(1,2)*Y
        ACOEF = (X**2 + Y**2)*(RK2-1) - RK2*PLROT**2
        BCOEF = -2*RK2*MSTOG(1,3)*PLROT
        CCOEF = (RK2-1) - RK2*MSTOG(1,3)**2

        SQT      = SQRT(BCOEF**2 - 4*ACOEF*CCOEF)
        RLAMB    = (-BCOEF+SQT)/(2*ACOEF)
        IF(RLAMB.LT.0.) RLAMB = (-BCOEF - SQT)/(2*ACOEF)
        PTSAT(1) = X*RLAMB        
        PTSAT(2) = Y*RLAMB
C   EVALUATE COMPONENTS IN E-C SYSTEM      
        CALL SATOERT(MSTOG,PTSAT,PTERTH)
        XE  = PTERTH(1)
        YE  = PTERTH(2)
        ZE  = PTERTH(3)
        R2  = XE*XE + YE*YE + ZE*ZE
        QT  = XE**2*RK2 - (RK2-1)*R2

        FAC = -XE*RK/R2
        XE  = RK + FAC*XE
        YE  = FAC*YE
        ZE  = FAC*ZE
        ENDIF
        
C   TRANSFORM ORTHOGONAL COORDINATES ON UNIT SPHERE
C      TO ANGULAR COORDINATES
      TH       = ASIN(ZE)
      ANG(I,1) = TH*RAD
      ANG(I,2) = ASIN(YE/COS(TH))*RAD + SATLON

  100 CONTINUE

      RETURN
      END

      SUBROUTINE SATOERT(MSTOG,PTSAT,PTERTH)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE TRANSFORMS SATELLITE CENTERED
C      COORDINATES ( PTSAT() ) TO EARTH CENTERED
C         COORDINATES ( PTERTH() )
C                       EBU COPYRIGHT 1997 

      REAL PTERTH(3),PTSAT(3),MSTOG(3,3)

C   PERFORM MATRIX MULTIPLICATION
C      PTERTH = MSTOG x PTSAT
      DO 100 I=1,3
      PTERTH(I) = 0.
      DO 100 J=1,3
      PTERTH(I) = PTERTH(I) + MSTOG(I,J)*PTSAT(J)
  100 CONTINUE

      RETURN
      END

      SUBROUTINE ROTATE(XPRI,YPRI,OMEGA,X,Y)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE CONVERTS X',Y' COORDINATES 
C      TO X,Y COORDINATES
C   THE X'-Y' SYSTEM IS ROTATED RELATIVE TO THE
C      X-Y SYSTEM BY OMEGA, COUNTERCLOCKWISE
C                    EBU COPYRIGHT 1997

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/
	REAL XPRI,YPRI,OMEG,X,Y
      X=0.0
	Y=0.0
C	---------------------------------------------------------------	      
      OMEG=OMEGA/RAD
      X = XPRI*COS(OMEG) - YPRI*SIN(OMEG)
      Y = XPRI*SIN(OMEG) + YPRI*COS(OMEG)

      RETURN
      END

      SUBROUTINE ELPOLY(ALPHA,BETA,ELPTS)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE DETERMINES EDGE OF AN ELLIPSE WITH 
C      POLYGON APPROX. EVERY 30 DEGREES, COUNTERCLOCKWISE
C         FROM X AXIS
C                    EBU COPYRIGHT 1997 

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/
	REAL ALPHA,BETA
	INTEGER	I
	REAL    TA,TA2
C	----------------------------------------------------------------
      REAL ELPTS(36,2)
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------

 	DO 10 I=1,36
	   ELPTS(I,1)=0.0
	   ELPTS(I,2)=0.0
   10 CONTINUE     
C	---------------------------------------------------------------

      DO 100 I = 1,17
      TA  = TAN(10*(I-9)/RAD)
      TA2 = TA**2
      ELPTS(I,1)    =  1/SQRT(ALPHA + BETA*TA2)
      ELPTS(I,2)    =  ELPTS(I,1)*TA
      ELPTS(I+18,1) = -ELPTS(I,1)
      ELPTS(I+18,2) = -ELPTS(I,2)
  100 CONTINUE
      ELPTS(18,1)   =  0
      ELPTS(18,2)   =  1/SQRT(BETA)
      ELPTS(36,1)   =  -ELPTS(18,1)
      ELPTS(36,2)   =  -ELPTS(18,2)

      RETURN
      END

      SUBROUTINE DRAW(N,THETA,PHI,PTSGEO,ANG)
      
C     *************************************************************      
C                            VERSION 2.0
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY 1997
C     *************************************************************

C   THIS SUBROUTINE GIVES A PRIMITIVE VIEW OF ELLIPSE
C                EBU COPYRIGHT 1997 

      COMMON/SATLONGI/SATLON,THC,PHC

      REAL PTSGEO(400,2),ANG(36,2)
      CHARACTER*1 CH(51,76),CHBL,CHX,CHY,CHZ,CHO

      DELPHI=PHI-SATLON
        IF(DELPHI.LE.-180.) THEN
        PHI=PHI+360.
        ELSEIF(DELPHI.GE.180.) THEN
        PHI=PHI-360.
        ENDIF

      CHBL  = ' '
      CHX   = 'X'
      CHY   = 'Y'
      CHZ   = 'Z'
      CHO   = 'O'
      
      THMAX = -1000.
      THMIN =  1000.
      PHMAX = -1000.
      PHMIN =  1000.
      
      DO 10 I = 1,36
      IF(THMAX.LT.ANG(I,1)) THMAX = ANG(I,1)
      IF(THMIN.GT.ANG(I,1)) THMIN = ANG(I,1)
      IF(PHMAX.LT.ANG(I,2)) PHMAX = ANG(I,2)
      IF(PHMIN.GT.ANG(I,2)) PHMIN = ANG(I,2)
   10 CONTINUE

      DO 20 I=1,N
      IF(THMAX.LT.PTSGEO(I,1)) THMAX = PTSGEO(I,1)
      IF(THMIN.GT.PTSGEO(I,1)) THMIN = PTSGEO(I,1)
      IF(PHMAX.LT.PTSGEO(I,2)) PHMAX = PTSGEO(I,2)
      IF(PHMIN.GT.PTSGEO(I,2)) PHMIN = PTSGEO(I,2)
   20 CONTINUE

      DTH   = THMAX - THMIN
      DPH   = PHMAX - PHMIN
      
      DO 100 I=1,76
      DO 100 J=1,51
      CH(J,I) = CHBL
  100 CONTINUE

      DO 200 I=1,36
      ITH         = 50.*(THMAX - ANG(I,1))/DTH + 1.0001
      IPH         = 75.*(ANG(I,2) - PHMIN)/DPH + 1.0001
      CH(ITH,IPH) = CHX
  200 CONTINUE
  
C   CENTER OF ELLIPSE

      ITH         = 50.*(THMAX-THETA)/DTH + 1.0001
      IPH         = 75.*(PHI-PHMIN)/DPH + 1.0001
      CH(ITH,IPH) = CHO

      DO 400 I=1,N

      ITH = 50.*(THMAX - PTSGEO(I,1))/DTH + 1.0001
      IPH = 75.*(PTSGEO(I,2) - PHMIN)/DPH + 1.0001

        IF(CH(ITH,IPH).EQ.CHX) THEN
        CH(ITH,IPH) = CHZ
        ELSE
        CH(ITH,IPH) = CHY
        ENDIF

  400 CONTINUE

      DO 500 I=1,51
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------

C      WRITE(23,*) (CH(I,J),J=1,76)
C	----------------------------------------------------------------
  500 CONTINUE
      
      RETURN
      END

      SUBROUTINE STAKEP(THC,PHC,N,PTRECT,ANGDIF)
      
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, NOVEMBER 1998
C     *************************************************************

C   THIS SUBROUTINE CALCULATES ANGLE DIFFERENCES TO TAKE CARE OF THE
C     STATION KEEPING ERROR
C                   EBU COPYRIGHT 1998

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON/PARAM/IPTERR,IROTER,ISKERR,ISKPRE,IMINEL
      COMMON /FUNCTS/COCO,COSI,SI
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/
      SAVE /PARAM/
	SAVE /FUNCTS/      
C	---------------------------------------------------------------      
      REAL PTRECT(400,3),ANGDIF(400)
      REAL COCO(4,2),COSI(4,2),SI(4,2),PC(4,2)
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
	REAL THC,PHC
	INTEGER N
	INTEGER I
      DO 10 I=1,400
	ANGDIF(I)=0.0
   10 CONTINUE
C	--------------------------------------------------------------
C SATELLITE HEIGHT SQUARED
      RK2 = RK*RK

C  COORDINATES OF BORESIGHT, AND LENGTH OF VECTOR TO SATELLITE
      CALL POLTOREC(THC,PHC,XC,YC,ZC)
      PC0 = SQRT(1+RK2-2*XC*RK)

C CALCULATE LENGTHS OF DISPLACED VECTORS
      DO 110 IND2=1,2
      DO 100 I=1,4
      PC(I,IND2) = SQRT(1.+RK2-2.*RK*(XC*COCO(I,IND2)
     1     +YC*COSI(I,IND2)+ZC*SI(I,IND2)))
  100 CONTINUE
  110 CONTINUE

      DO 200 I=1,N
      XA=PTRECT(I,1)
      YA=PTRECT(I,2)
      ZA=PTRECT(I,3)
      PA=SQRT(1.+RK2-2.*XA*RK)
      PROD=XA*XC+YA*YC+ZA*ZC+RK2
      DOT=PROD-RK*(XA+XC)
C  OMEGA = ANGLE BETWEEN ORIGINAL VECTORS

      ANGL = DOT/(2*PA*PC0)

        IF(ANGL.GE.1.0) THEN
        OMEGA=0.
        ELSEIF(ANGL.LE.-1.0) THEN
        OMEGA = 3.141592654
        ELSE
        OMEGA=ACOS(ANGL)
        ENDIF

      DIFMAX=0.
      DO 160 IND2=1,2
      DO 150 J=1,4
      PAP=SQRT(1.+RK2-2.*RK*(XA*COCO(J,IND2)
     1      +YA*COSI(J,IND2)+ZA*SI(J,IND2)))

      DOTP=PROD-RK*((XA+XC)*COCO(J,IND2)
     1    +(YA+YC)*COSI(J,IND2)+(ZA+ZC)*SI(J,IND2))
C  OMEGAP = ANGLE BETWEEN DISPLACED VECTORS

      ANGL = DOTP/(2*PAP*PC(J,IND2))

        IF(ANGL.GE.1.0) THEN
        OMEGAP = 0.
        ELSEIF(ANGL.LE.-1.0) THEN
        OMEGAP = 3.141592654
        ELSE
        OMEGAP = ACOS(ANGL)
        ENDIF

        IF(OMEGAP.GT.OMEGA) THEN
        DIF=OMEGAP-OMEGA
        IF(DIF.GT.DIFMAX) DIFMAX=DIF
        ENDIF
  150 CONTINUE
  160 CONTINUE

      ANGDIF(I)=DIFMAX
  200 CONTINUE

      RETURN
      END
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
C      SUBROUTINE CONVEX(NIN,NOUT)
      SUBROUTINE CONVEX(NIN,NOUT,PSGEO)
C     ---------------------------------------------------------------     
C     *************************************************************      
C                            VERSION 2.1
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, OCTOBER 1998
C     *************************************************************

C    THIS SUBROUTINE DELETES POINTS FROM AN ARBITRARY SET OF 
C       POINTS TO PRODUCE A 'CONVEX SET', I.E., ONE SUCH
C          THAT NO POINT IS CONTAINED WITHIN A TRIANGLE 
C             FORMED BY ANY THREE OF THE OTHERS.
C                    EBU COPYRIGHT 1998
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
            IMPLICIT NONE

	INTEGER    NIN,NOUT
	INTEGER    NUM
      REAL       PSGEO(400,2)
	INTEGER    I,J,K,L
	REAL	   X1,Y1,X2,Y2,X3,Y3,A12,B12,C12,A23,B23,C23,A31,B31,C31
	REAL       V1,V2,V3,XP,YP,VP
C	----------------------------------------------------------------
      REAL COORDS(200,2)
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      REAL EPS
C      DIMENSION IAMOUT(200)
	INTEGER IAMOUT(200)
      EPS = 0.0001
C      DO 10 I=1,NIN
C      IAMOUT(I) = 0
C      READ(21,9002) COORDS(I,1),COORDS(I,2)
C 9002 FORMAT(F7.2,F7.2)
C   10 CONTINUE
      DO 10 I=1,NIN
      IAMOUT(I) = 0
      COORDS(I,1)=PSGEO(I,1)
      COORDS(I,2)=PSGEO(I,2)
   10 CONTINUE
C	-------------------------------------------------------------      

      DO 500 I=1,NIN-2
      IF(IAMOUT(I).EQ.1) GOTO 500
      X1  =  COORDS(I,1)
      Y1  =  COORDS(I,2)
      DO 400 J=I+1,NIN-1
      IF(IAMOUT(J).EQ.1) GOTO 400
      X2  =  COORDS(J,1)
      Y2  =  COORDS(J,2)
      A12 =  Y2 - Y1
      B12 =  X1 - X2
      C12 = -X1*A12 - Y1*B12
      DO 300 K=J+1,NIN
      IF(IAMOUT(K).EQ.1) GOTO 300
      X3  =  COORDS(K,1)
      Y3  =  COORDS(K,2)
      
      A23 =  Y3 - Y2
      B23 =  X2 - X3
      C23 = -X2*A23 - Y2*B23

      A31 =  Y1 - Y3
      B31 =  X3 - X1
      C31 = -X3*A31 - Y3*B31

C THE FOLLOWING THREE EQUATIONS ARE EQUIVALENT TO
C     V = X1*(Y3-Y2) + X2*(Y1-Y3) + X3*(Y2-Y1) =
C     V = Y1*(X2-X3) + Y2*(X3-X1) + Y3*(X1-X2)
C BUT ARE INCLUDED IN THE FORM BELOW FOR CLARITY IN
C THE SUCCEEDING DO-LOOPS
      V1 = A23*X1 + B23*Y1 + C23
      V2 = A31*X2 + B31*Y2 + C31
      V3 = A12*X3 + B12*Y3 + C12

C V1( = V2 = V3) = 0 MEANS CO-LINEAR POINTS; GET RID OF THE ONE
C BETWEEN THE OTHER TWO
        IF(ABS(V1).LE.EPS.OR.ABS(V2).LE.EPS.OR.ABS(V3).LE.EPS) THEN
        CALL BETWEEN(X1,Y1,X2,Y2,X3,Y3,NUM)
          IF(NUM.EQ.1) THEN
          IAMOUT(I) = 1
          GOTO 500
          ELSEIF(NUM.EQ.2) THEN
          IAMOUT(J) = 1
          GOTO 400
          ELSEIF(NUM.EQ.3) THEN
          IAMOUT(K) = 1
          GOTO 300
          ENDIF
        ENDIF
      
      DO 200 L=1,NIN
      IF(L.EQ.I.OR.L.EQ.J.OR.L.EQ.K.OR.IAMOUT(L).EQ.1) GOTO 200
      XP = COORDS(L,1)
      YP = COORDS(L,2)
      
      VP = A23*XP + B23*YP + C23

C CHECK FOR CO-LINEARITY
        IF(ABS(VP).LE.EPS) THEN
        CALL BETWEEN(XP,YP,X2,Y2,X3,Y3,NUM)
          IF(NUM.EQ.1)THEN
          IAMOUT(L) = 1
          GOTO 200
          ELSEIF(NUM.EQ.2) THEN
          IAMOUT(J) = 1
          GOTO 400
          ELSEIF(NUM.EQ.3) THEN
          IAMOUT(K) = 1
          GOTO 300
          ENDIF
        ENDIF

      IF(VP*V1.LT.0.) GOTO 200
      
      VP = A31*XP + B31*YP+ C31

C CHECK FOR CO-LINEARITY
        IF(ABS(VP).LE.EPS) THEN
        CALL BETWEEN(X1,Y1,XP,YP,X3,Y3,NUM)
          IF(NUM.EQ.1)THEN
          IAMOUT(I) = 1
          GOTO 500
          ELSEIF(NUM.EQ.2) THEN
          IAMOUT(L) = 1
          GOTO 200
          ELSEIF(NUM.EQ.3) THEN
          IAMOUT(K) = 1
          GOTO 300
          ENDIF
        ENDIF

      IF(VP*V2.LT.0.) GOTO 200
      
      VP = A12*XP + B12*YP + C12

C CHECK FOR CO-LINEARITY
        IF(ABS(VP).LE.EPS) THEN
        CALL BETWEEN(X1,Y1,X2,Y2,XP,YP,NUM)
          IF(NUM.EQ.1)THEN
          IAMOUT(I) = 1
          GOTO 500
          ELSEIF(NUM.EQ.2) THEN
          IAMOUT(J) = 1
          GOTO 400
          ELSEIF(NUM.EQ.3) THEN
          IAMOUT(L) = 1
          GOTO 200
          ENDIF
        ENDIF

      IF(VP*V3.LT.0.) GOTO 200

      IAMOUT(L) = 1

  200 CONTINUE
  300 CONTINUE
  400 CONTINUE
  500 CONTINUE

      NOUT = 0
      DO 1000 I=1,NIN
      IF(IAMOUT(I).EQ.1) GOTO 1000
      NOUT = NOUT + 1
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      PSGEO(NOUT,1)=COORDS(I,1)
      PSGEO(NOUT,2)=COORDS(I,2)
C      WRITE(22,9002) COORDS(I,1),COORDS(I,2)
 1000 CONTINUE
C      REWIND 22 
C	----------------------------------------------------------------  
      RETURN
      END

      SUBROUTINE BETWEEN(X1,Y1,X2,Y2,X3,Y3,NUM)

C     *************************************************************      
C                            VERSION 2.1
C
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, OCTOBER  1998
C     *************************************************************

C     THIS SUBROUTINE DETERMINES WHICH ONE, OF THREE COLINEAR POINTS
C     LIES BETWEEN THE OTHER TWO.

C                       EBU COPYRIGHT 1998
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      IMPLICIT NONE
	REAL    X1,Y1,X2,Y2,X3,Y3
      INTEGER NUM
	REAL    D12,D13,D23,DM
      NUM=0
C	-----------------------------------------------------------
      D12 = (X1-X2)**2 + (Y1-Y2)**2
      D13 = (X1-X3)**2 + (Y1-Y3)**2
      D23 = (X2-X3)**2 + (Y2-Y3)**2

      DM = AMAX1(D12,D13,D23)
        IF(DM.EQ.D12) THEN
        NUM = 3
        ELSEIF(DM.EQ.D13) THEN
        NUM = 2
        ELSE
        NUM = 1
        ENDIF

      RETURN
      END

      SUBROUTINE GENDAT

C     *************************************************************      
C                            VERSION 2.0
C
C      AUTHOR : TERRY O'LEARY           EBU GENEVA, JANUARY  1997
C     *************************************************************
      
C                       COPYRIGHT EBU 1997

      COMMON /CONST/RK,TANPE,TANSK,CROT,SROT,RAD
      COMMON /ERRORS/XSKERR,XPTERR,XRTERR,XMIN
      COMMON /FUNCTS/COCO,COSI,SI
      COMMON /MINELL/AMIN,AMIN2,COEFAB,ARMIN
C     --------------------- changes S. BRYNDA, ITU, April 1999 ------
      SAVE /CONST/
	SAVE /ERRORS/
	SAVE /FUNCTS/
	SAVE /MINELL/
C	---------------------------------------------------------------
      REAL COCO(4,2),COSI(4,2),SI(4,2)
      REAL TH(4,2),PH(4,2)

C RK   =  6.61072 : THE DISTANCE IN EARTH RADII TO GEO ORBIT
C RAD  = 57.295779513082; CONVERSION FACTOR RADIANS TO DEGREES
      RK    =  6.61072
      RAD   = 57.295779513082

C TAKE INTO ACCOUNT MINIMUM SIZE (SEMI-AXIS) OF ELLIPSE
      PI    = 3.14159265359
         
      COEFAB = 10000000000.
        IF(XMIN.GT.0.) THEN
        AMIN   = TAN(XMIN/RAD)
        AMIN2  = AMIN**2
        COEFAB = 1/AMIN2
        ARMIN  = PI*AMIN2
        ENDIF

C TANSK=  TAN(XSKERR DEGREE)    STATION KEEPING ERROR
C TANPE=  TAN(XPTERR DEGREE)    POINTING        ERROR
C CROT =  COS (XRTERR DEGREE)   ROTATIONAL      ERROR
C SROT =  SIN (XRTERR DEGREE)   ROTATIONAL      ERROR 

      TANSK = TAN(XSKERR/RAD)
      TANPE = TAN(XPTERR/RAD)
      CROT  = COS(XRTERR/RAD)
      SROT  = SIN(XRTERR/RAD)

C DISPLACEMENTS FOR STATION KEEPING ERROR 
      RSKERR  =  XSKERR/RAD
      TH(1,1) =  0.
      PH(1,1) =  RSKERR
      TH(2,1) =  RSKERR
      PH(2,1) =  0.
      TH(3,1) =  0.
      PH(3,1) = -RSKERR
      TH(4,1) = -RSKERR
      PH(4,1) =  0.

      TH(1,2) =  RSKERR
      PH(1,2) =  RSKERR
      TH(2,2) =  RSKERR
      PH(2,2) = -RSKERR
      TH(3,2) = -RSKERR
      PH(3,2) = -RSKERR
      TH(4,2) = -RSKERR
      PH(4,2) =  RSKERR

      DO 100 I=1,4
      COCO(I,1) = COS(TH(I,1))*COS(PH(I,1))
      COCO(I,2) = COS(TH(I,2))*COS(PH(I,2))
      COSI(I,1) = COS(TH(I,1))*SIN(PH(I,1))
      COSI(I,2) = COS(TH(I,2))*SIN(PH(I,2))
      SI(I,1)   = SIN(TH(I,1))
      SI(I,2)   = SIN(TH(I,2))
  100 CONTINUE

       RETURN
       END
