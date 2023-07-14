*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZHCM_EXPRT_LGMAP
*   generation date: 23.09.2022 at 15:10:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZHCM_EXPRT_LGMAP   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
