*&---------------------------------------------------------------------*
*&  Include  zhcm_exprt_payroll_result_ssc
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS p_sch TYPE schem.
PARAMETERS p_rul TYPE avrule.
PARAMETERS p_awrt TYPE awart.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
PARAMETERS p_lga TYPE lgart.
PARAMETERS p_depth(2) TYPE n DEFAULT zclhr_exprt_payroll_result=>ac_upl_depth.
PARAMETERS p_file TYPE localfile.
PARAMETERS p_len(8) TYPE n.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS p_exid TYPE zhcm_exprt_res-operation_id.
SELECTION-SCREEN END OF BLOCK b03.

INITIALIZATION.
  CREATE OBJECT go_main.
  TRY.
      p_file = go_main->zif_exprt_payroll_constants~get_upl_def_path( ).
      go_main->zif_exprt_payroll_constants~get_lgart_mapping( ).
    CATCH zcx_exprt_payroll_err INTO go_err. " Класс-обработчик ошибок
      WHILE go_err->previous IS BOUND.
        go_err ?= go_err->previous.
      ENDWHILE..
      MESSAGE go_err->get_text( ) TYPE 'E'.
  ENDTRY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  TRY.
      p_file = go_main->get_path( iv_init_path = p_file ).
    CATCH zcx_exprt_payroll_err INTO go_err. " Класс-обработчик ошибок
      WHILE go_err->previous IS BOUND.
        go_err ?= go_err->previous.
      ENDWHILE..
      MESSAGE go_err->get_text( ) TYPE 'E'.
  ENDTRY.

START-OF-SELECTION.
  go_main->set_depth( p_depth ).
  go_main->set_base_date( pn-begda ).
  go_main->set_file_length( p_len ).
  IF p_exid IS NOT INITIAL.
    go_main->get_results_from_db( p_exid ).
    STOP.
  ENDIF.

GET peras.
  TRY.
      go_main->process_pernr(
        EXPORTING
          iv_pernr = peras-pernr
      ).
    CATCH zcx_exprt_payroll_err INTO go_err. " Класс-обработчик ошибок
      WHILE go_err->previous IS BOUND.
        go_err ?= go_err->previous.
      ENDWHILE.
      WRITE go_err->get_text( ). " для дебага
  ENDTRY.

END-OF-SELECTION.
  TRY.
      IF sy-batch EQ abap_false.
        go_main->prepare_and_download( ).
      ENDIF.
      IF p_exid IS INITIAL.
        go_main->save_results_to_db( ).
      ENDIF.
    CATCH zcx_exprt_payroll_err INTO go_err. " Класс-обработчик ошибок
      WHILE go_err->previous IS BOUND.
        go_err ?= go_err->previous.
      ENDWHILE.
      MESSAGE go_err->get_text( ) TYPE 'E'.
  ENDTRY.
