       SUBROUTINE STAR(X,Y,HGT,RATIO)

        HGT2=RATIO*HGT/2.0
        YPH=Y+HGT2
        YMH=Y-HGT2
        XMH=X-HGT2
        XPH=X+HGT2
        CALL GVECT(X,YPH,0)
        CALL GVECT(X,YMH,1)
        CALL GVECT(XMH,Y,0)
        CALL GVECT(XPH,Y,1)
  
        HGT2=HGT2*0.7071
        XMH=X-HGT2
        XPH=X+HGT2
        YMH=Y-HGT2
        YPH=Y+HGT2
        CALL GVECT(XMH,YPH,0)
        CALL GVECT(XPH,YMH,1)
        CALL GVECT(XMH,YMH,0)
        CALL GVECT(XPH,YPH,1)
      RETURN
      END
