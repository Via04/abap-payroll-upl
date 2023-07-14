*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.09.2022 at 15:11:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHCM_EXPRT_LGMAP................................*
DATA:  BEGIN OF STATUS_ZHCM_EXPRT_LGMAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCM_EXPRT_LGMAP              .
CONTROLS: TCTRL_ZHCM_EXPRT_LGMAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCM_EXPRT_LGMAP              .
TABLES: ZHCM_EXPRT_LGMAP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
