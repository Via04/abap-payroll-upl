CLASS zclhr_exprt_payroll_result DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_exprt_payroll_constants .

    ALIASES ty_t_lgart_mapping
      FOR zif_exprt_payroll_constants~ty_t_lgart_mapping .
    ALIASES ac_upl_depth
      FOR zif_exprt_payroll_constants~ac_upl_depth.
    ALIASES ty_analysis_depth
      FOR zif_exprt_payroll_constants~ty_analysis_depth.
    ALIASES ac_molga_33_rus
      FOR zif_exprt_payroll_constants~ac_molga_33_rus.
    ALIASES ty_t_rgdir
      FOR zif_exprt_payroll_constants~ty_t_rgdir.
    ALIASES ty_out_data
      FOR zif_exprt_payroll_constants~ty_out_data.
    ALIASES ty_t_out_data
      FOR zif_exprt_payroll_constants~ty_t_out_data.
    ALIASES ty_t_out_data_sorted
      FOR zif_exprt_payroll_constants~ty_t_out_data_sorted.
    ALIASES ty_pernr_map
      FOR zif_exprt_payroll_constants~ty_pernr_map.
    ALIASES ty_t_pernr_map
      FOR zif_exprt_payroll_constants~ty_t_pernr_map.
    ALIASES ac_out_system_code
      FOR zif_exprt_payroll_constants~ac_out_system_code.
    ALIASES ty_file_length
      FOR zif_exprt_payroll_constants~ty_file_length.
    ALIASES ac_dynamic_classname
      FOR zif_exprt_payroll_constants~ac_dynamic_classname.

    TYPES: BEGIN OF ty_pernr_data,
             pernr     TYPE persno,
             hire_date TYPE datum,
             p0002     TYPE p0002,
           END OF ty_pernr_data.

    "! Вызывает диалог открытия файла и выдает полный путь в выходной переменной
    "! @parameter iv_init_path | Начальный путь для диалога выбора
    "! @parameter rv_path | Выбранный пользователем путь
    "! @raising zcx_exprt_payroll_err | Класс-обработчик ошибок
    METHODS get_path
      IMPORTING iv_init_path   TYPE localfile
      RETURNING VALUE(rv_path) TYPE localfile
      RAISING   zcx_exprt_payroll_err.
    "! Метод анализа очередного ТН. Основной логический метод для программы выгрузки исторических зп.
    METHODS process_pernr
      IMPORTING iv_pernr TYPE persno
      RAISING   zcx_exprt_payroll_err.
    METHODS: set_depth IMPORTING iv_depth TYPE ty_analysis_depth,
      get_depth RETURNING VALUE(rv_depth) TYPE ty_analysis_depth.
    METHODS: get_base_date RETURNING VALUE(rv_datum) TYPE datum,
      set_base_date IMPORTING iv_datum TYPE datum,
      get_file_length RETURNING VALUE(r_result) TYPE ty_file_length,
      set_file_length IMPORTING iv_file_length TYPE ty_file_length.
    "! Подготовить файл на основе таблицы at_out_data
    METHODS prepare_and_download
      RAISING zcx_exprt_payroll_err.
    "! Сохраняет результаты работы программы в БД на основе таблицы AT_OUT_DATA
    METHODS save_results_to_db.
    "! Загружает результаты работы программы из БД в атрибут AT_OUT_DATA
    "! @parameter iv_op_id | ИД операции
    METHODS get_results_from_db
      IMPORTING iv_op_id TYPE zhcm_exprt_res-operation_id.
    CLASS-METHODS table_to_range
      IMPORTING i_tab     TYPE table
                i_colname TYPE string OPTIONAL
      EXPORTING e_range   TYPE table
      RAISING   zcx_exprt_payroll_err.
  PROTECTED SECTION.
    "! Основная выходная таблица с данными
    DATA at_out_data TYPE ty_t_out_data.
    "! Метод смотрит на дату приема сотрудника и если он был принят менее av_depth месяцев назад, то возвращает true
    "! @parameter rv_is | abap_true/abap_false
    METHODS is_depth_accept
      RETURNING VALUE(rv_is) TYPE abap_bool.
    "! Получить дату приема для текущего сотрудника (av_cur_pernr-pernr)
    METHODS get_hire_date
      RETURNING VALUE(rv_date) TYPE datum.
    "! Получить список актуальных расчетов сотрудника, у которых for период полностью попадает в промежуток между датой приема и базовой датой.
    "! @parameter et_rgdir | Выходная таблица со списком расчетов
    "! @raising zcx_exprt_payroll_err | Обработчик ошибок
    METHODS get_evaluated_rgdir
      EXPORTING et_rgdir TYPE ty_t_rgdir
      RAISING   zcx_exprt_payroll_err.
    "! Получить таблицу RT из PAYRU_RESULT, содержащую записи только по видам оплаты из at_lga_map
    "! @parameter is_payru_result | Заполненная структура <em>payru_result</em>
    "! @parameter et_rt | Таблица RT с релевантными записями по видам оплаты.
    METHODS get_relevant_rt
      IMPORTING is_payru_result TYPE payru_result
      EXPORTING et_rt           TYPE hrpay99_rt.

  PRIVATE SECTION.
    DATA at_lga_map TYPE ty_t_lgart_mapping.
    "! Папка выгрузки
    DATA av_path TYPE string.
    "! Название файла выгрузки
    DATA av_file TYPE string.
    "! Полный путь выгрузки
    DATA av_fullpath TYPE string.
    " Кличество строк в выходном файле
    DATA av_file_length TYPE ty_file_length.
    "! Глубина анализа ТН (в месяцах)
    DATA av_depth TYPE ty_analysis_depth.
    "! Текущий ТН
    DATA as_cur_pernr TYPE ty_pernr_data.
    "! Базовая дата анализа
    DATA av_datum TYPE datum.

    "! Заполнить в выходную таблицу (at_out_data) оставшиеся данные, такие как ФИО, вид оплаты из новой системы, новый ТН
    METHODS fill_residual_data.

ENDCLASS.



CLASS zclhr_exprt_payroll_result IMPLEMENTATION.


  METHOD zif_exprt_payroll_constants~get_lgart_mapping.
    IF at_lga_map IS INITIAL.
      SELECT * FROM zhcm_exprt_lgmap INTO TABLE at_lga_map.
    ENDIF.
    et_lga_map = at_lga_map.
  ENDMETHOD.


  METHOD zif_exprt_payroll_constants~get_upl_def_path.
    DATA lv_workdir TYPE string.


    cl_gui_frontend_services=>get_sapgui_workdir(
      CHANGING
        sapworkdir            = lv_workdir
      EXCEPTIONS
        get_sapworkdir_failed = 1
        cntl_error            = 2
        error_no_gui          = 3
        not_supported_by_gui  = 4
        OTHERS                = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_exprt_payroll_err
        EXPORTING
          textid = zcx_exprt_payroll_err=>read_filesystem_err.
    ENDIF.
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = ev_sep
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_exprt_payroll_err
        EXPORTING
          textid = zcx_exprt_payroll_err=>read_filesystem_err.
    ENDIF.
    rv_path = lv_workdir.
  ENDMETHOD.


  METHOD get_path.
    DATA lv_title TYPE string.
    DATA lv_default_name TYPE string.
    DATA lv_init_dir TYPE string.
    DATA lv_filename TYPE string.
    DATA lv_path TYPE string.
    DATA lv_fullpath TYPE string.


    lv_default_name = |{ sy-uname }_{ sy-datum }_{ sy-uzeit }_zhcm_exprt_payroll|.
    lv_title = 'Создание файла выгрузки'.
    lv_init_dir = iv_init_path.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = lv_title
        default_extension         = '*.csv'
        default_file_name         = lv_default_name
        file_filter               = 'File with delimiter ";" (*.csv)|*.csv'
        initial_directory         = lv_init_dir
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_exprt_payroll_err
        EXPORTING
          textid = zcx_exprt_payroll_err=>read_filesystem_err.
    ENDIF.
    av_file = lv_filename.
    av_path = lv_path.
    av_fullpath = lv_fullpath.
    rv_path = lv_fullpath.
  ENDMETHOD.

  METHOD get_hire_date.
    DATA lv_pernr TYPE p_pernr.
    DATA lv_hire_date TYPE begda.

    lv_pernr = as_cur_pernr-pernr.
    CALL FUNCTION 'RP_GET_HIRE_DATE'
      EXPORTING
        persnr          = lv_pernr
        check_infotypes = '0000'
      IMPORTING
        hiredate        = lv_hire_date.
    as_cur_pernr-hire_date = lv_hire_date.
    rv_date = lv_hire_date.
  ENDMETHOD.

  METHOD is_depth_accept.
    DATA lv_sub_months TYPE dlymo.
    DATA lv_date_ext TYPE begda.
    DATA lv_hire_date TYPE begda.

    rv_is = abap_false.
    lv_sub_months = av_depth.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = av_datum
        days      = '0'
        months    = lv_sub_months
        signum    = '-'
        years     = '0'
      IMPORTING
        calc_date = lv_date_ext.
    lv_hire_date = get_hire_date( ).
    IF lv_hire_date GE lv_date_ext.
      rv_is = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_evaluated_rgdir.
    DATA lt_rgdir TYPE STANDARD TABLE OF pc261 WITH DEFAULT KEY.
    DATA ls_rgdir LIKE LINE OF lt_rgdir.


    CALL FUNCTION 'CU_READ_RGDIR_NEW'
      EXPORTING
        persnr                = as_cur_pernr-pernr
*       check_read_authority  = 'X'
*       imp_client            =
*      IMPORTING
*       molga                 =
      TABLES
        in_rgdir              = lt_rgdir
      EXCEPTIONS
        no_record_found       = 0
        import_mismatch_error = 1
        no_read_authority     = 2
        inconsistent_data     = 1
        OTHERS                = 5.
    CASE sy-subrc.
      WHEN 1 OR 5.
        RAISE EXCEPTION TYPE zcx_exprt_payroll_err
          EXPORTING
            textid = zcx_exprt_payroll_err=>read_rgdir_err
            argv1  = as_cur_pernr-pernr.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_exprt_payroll_err
          EXPORTING
            textid = zcx_exprt_payroll_err=>read_rgdir_auth_err
            argv1  = as_cur_pernr-pernr.
    ENDCASE.
    LOOP AT lt_rgdir INTO ls_rgdir.
      IF ls_rgdir-srtza NE 'A'.
        DELETE lt_rgdir.
      ENDIF.
      IF NOT ( ls_rgdir-fpend LE av_datum AND ls_rgdir-fpbeg GE as_cur_pernr-hire_date ).
        DELETE lt_rgdir.
      ENDIF.
    ENDLOOP.
    et_rgdir = lt_rgdir.
  ENDMETHOD.

  METHOD process_pernr.
    DATA lt_rgdir TYPE ty_t_rgdir.
    DATA ls_rgdir LIKE LINE OF lt_rgdir.
    DATA lo_err TYPE REF TO zcx_exprt_payroll_err.
    DATA ls_pay_result TYPE payru_result.
    DATA lt_rt TYPE hrpay99_rt.
    DATA ls_rt LIKE LINE OF lt_rt.
    DATA ls_out_data TYPE ty_out_data.
    DATA lt_p0002 TYPE STANDARD TABLE OF p0002.
    DATA ls_p0002 LIKE LINE OF lt_p0002.
    DATA lt_out_tab_sorted TYPE ty_t_out_data_sorted.


    as_cur_pernr-pernr = iv_pernr.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0002'
      TABLES
        infty_tab       = lt_p0002
      EXCEPTIONS
        infty_not_found = 0
        invalid_input   = 0
        OTHERS          = 0.
    SORT lt_p0002 DESCENDING BY begda endda.
    READ TABLE lt_p0002 INTO ls_p0002 INDEX 1.
    as_cur_pernr-p0002 = ls_p0002.
    " Если ТН соответствует заданному периоду
    CHECK is_depth_accept( ).
    " то нужно прочить RGDIR за период от даты приема до базовой
    TRY.
        get_evaluated_rgdir(
          IMPORTING
            et_rgdir = lt_rgdir
        ).
      CATCH zcx_exprt_payroll_err INTO lo_err. " Класс-обработчик ошибок
        RAISE EXCEPTION TYPE zcx_exprt_payroll_err
          EXPORTING
            previous = lo_err.
    ENDTRY.
    LOOP AT lt_rgdir INTO ls_rgdir.
      CLEAR ls_out_data.
      ls_out_data-tbn = as_cur_pernr-pernr.
      ls_out_data-fpbeg = ls_rgdir-fpbeg.
      ls_out_data-fpend = ls_rgdir-fpend.
      CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
        EXPORTING
          employeenumber               = as_cur_pernr-pernr
          sequencenumber               = ls_rgdir-seqnr
        CHANGING
          payroll_result               = ls_pay_result
        EXCEPTIONS
          illegal_isocode_or_clusterid = 1
          error_generating_import      = 2
          import_mismatch_error        = 3
          subpool_dir_full             = 4
          no_read_authority            = 11
          no_record_found              = 0
          versions_do_not_match        = 7
          error_reading_archive        = 8
          error_reading_relid          = 9
          OTHERS                       = 10.
      IF sy-subrc <> 0.
        IF sy-subrc LE 10.
          RAISE EXCEPTION TYPE zcx_exprt_payroll_err
            EXPORTING
              textid = zcx_exprt_payroll_err=>read_pay_result_err
              argv1  = as_cur_pernr-pernr.
        ENDIF.
        IF sy-subrc EQ 11.
          RAISE EXCEPTION TYPE zcx_exprt_payroll_err
            EXPORTING
              textid = zcx_exprt_payroll_err=>read_pay_result_auth_err
              argv1  = as_cur_pernr-pernr.
        ENDIF.
      ENDIF.
      get_relevant_rt(
        EXPORTING
          is_payru_result = ls_pay_result
        IMPORTING
          et_rt           = lt_rt
      ).
      LOOP AT lt_rt INTO ls_rt.
        ls_out_data-code  = ls_rt-lgart.
        ls_out_data-anzhl = ls_rt-anzhl.
        ls_out_data-betpe = ls_rt-betpe.
        ls_out_data-betrg = ls_rt-betrg.
        COLLECT ls_out_data INTO lt_out_tab_sorted.
      ENDLOOP.
    ENDLOOP.
    APPEND LINES OF lt_out_tab_sorted TO at_out_data.
    fill_residual_data( ).
  ENDMETHOD.

  METHOD set_depth.
    av_depth = iv_depth.
  ENDMETHOD.

  METHOD get_depth.
    rv_depth = av_depth.
  ENDMETHOD.

  METHOD get_base_date.
    rv_datum = me->av_datum.
  ENDMETHOD.

  METHOD set_base_date.
    me->av_datum = iv_datum.
  ENDMETHOD.

  METHOD get_relevant_rt.
    DATA lt_rt LIKE is_payru_result-inter-rt.
    DATA ls_rt LIKE LINE OF lt_rt.
    DATA lt_dyn_rt LIKE is_payru_result-inter-rt.
    DATA lt_rt_pool LIKE is_payru_result-inter-rt.
    DATA lt_lgart_range TYPE RANGE OF lgart.
    DATA lo_err TYPE REF TO zcx_exprt_payroll_err.
    DATA ls_lga_map LIKE LINE OF at_lga_map.
    DATA lv_dyn_method_name TYPE string.

    TRY.
        zclhr_exprt_payroll_result=>table_to_range(
          EXPORTING
            i_tab     = at_lga_map
            i_colname = 'LGART1'
          IMPORTING
            e_range   = lt_lgart_range
        ).
      CATCH zcx_exprt_payroll_err INTO lo_err. " Класс-обработчик ошибок
        " не вижу смысла обрабатывать, но на всякий случай обработку вставил
    ENDTRY.
    CLEAR lt_rt_pool.
    LOOP AT is_payru_result-inter-rt INTO ls_rt WHERE lgart IN lt_lgart_range.
      READ TABLE at_lga_map INTO ls_lga_map WITH KEY lgart1 = ls_rt-lgart.
      " Доработка для Валентина с вызовом кастомного метода из таблицы
      check ls_lga_map-call_method IS NOT INITIAL.
        lv_dyn_method_name = |{ ac_dynamic_classname }=>{ ls_lga_map-call_method }|.
        " метод динамический и внешний, а выходной параметр передается по ссылке, поэтому нужно его очистить
        CLEAR lt_dyn_rt.
        CALL METHOD (lv_dyn_method_name)
          EXPORTING
            is_rt   = ls_rt
            it_wpbp = is_payru_result-inter-wpbp
          IMPORTING
            et_rt   = lt_dyn_rt.
        APPEND LINES OF lt_dyn_rt TO lt_rt_pool.
    ENDLOOP.
    LOOP AT is_payru_result-inter-rt INTO ls_rt WHERE lgart IN lt_lgart_range.
      READ TABLE at_lga_map INTO ls_lga_map WITH KEY lgart1 = ls_rt-lgart.
      CHECK ls_lga_map-call_method IS INITIAL.
      IF ls_lga_map-anzhl EQ abap_false.
        ls_rt-anzhl = 0.
      ENDIF.
      IF ls_lga_map-betpe EQ abap_false.
        ls_rt-betpe = 0.
      ENDIF.
      IF ls_lga_map-betrg EQ abap_false.
        ls_rt-betpe = 0.
      ENDIF.
      APPEND ls_rt TO lt_rt.
    ENDLOOP.
    APPEND LINES OF lt_rt_pool TO lt_rt.
    SORT lt_rt BY lgart.
    et_rt = lt_rt.
  ENDMETHOD.

  METHOD table_to_range.
    DATA lo_data TYPE REF TO data. " Ccaika BxogHow Ta6nnubI
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE. " CvMBon BxoqHow Ta6nMubl
    FIELD-SYMBOLS <ls_table_wa> TYPE any. " CTpoka exogHow Ta6nnubt

    DATA lo_comp TYPE REF TO data. " Ccbinka Ha KOMNOHeHT CTpoKA
    FIELD-SYMBOLS <ls_comp> TYPE any. " CumBon Ha KOMNOHeHT cTpoKM (ecaM cTpyKTypa)
    FIELD-SYMBOLS <lv_comp> TYPE any. " CwMBon, KOTOpbIa HaM HyXeH AA 3aHeCeHMA B peHAX - NMGO CTpoKa Bx. Ta6mnubl, \
    " pw60 ABNAeTCA KOMNOHeHTOM <ls_comp>
    DATA lo_out TYPE REF TO data. " Ccbinka Ha BbIXOAHO peHax
    FIELD-SYMBOLS <lt_out> TYPE STANDARD TABLE. " Ta6nmua sbixoQHoro peHaxKa
    FIELD-SYMBOLS <ls_out> TYPE any. " CTpoka sbixogHoro peHaxKa
    FIELD-SYMBOLS: <lv_low>    TYPE any,
                   <lv_high>   TYPE any,
                   <lv_option> TYPE any,
                   <lv_sign>   TYPE any.


    CLEAR e_range.
    GET REFERENCE OF i_tab INTO lo_data.
    ASSIGN lo_data->* TO <lt_table>.
    READ TABLE <lt_table> INDEX 1 ASSIGNING <ls_table_wa>.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_exprt_payroll_err
        EXPORTING
          textid = zcx_exprt_payroll_err=>empty_tab_err.
    ENDIF. " READ TABLE <1t_table> INDEX 1 ASSIGNING <ls_table wa>. not initial
    GET REFERENCE OF e_range INTO lo_out.
    ASSIGN lo_out->* TO <lt_out>.
    lo_typedescr ?= cl_abap_typedescr=>describe_by_data( p_data = <ls_table_wa> ).
    LOOP AT <lt_table> ASSIGNING <ls_table_wa>.
      APPEND INITIAL LINE TO <lt_out> ASSIGNING <ls_out>.
      GET REFERENCE OF <ls_table_wa> INTO lo_comp.
      IF lo_typedescr->kind EQ cl_abap_typedescr=>kind_elem.
        ASSIGN lo_comp->* TO <lv_comp>.
      ELSEIF lo_typedescr->kind EQ cl_abap_typedescr=>kind_struct.
        ASSIGN lo_comp->* TO <ls_comp>.
        ASSIGN COMPONENT i_colname OF STRUCTURE <ls_comp> TO <lv_comp>.
      ELSE.
        RAISE EXCEPTION TYPE zcx_exprt_payroll_err
          EXPORTING
            textid = zcx_exprt_payroll_err=>invalid_line_err.
      ENDIF.

      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_out> TO <lv_low>.
      <lv_low> = <lv_comp>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_out> TO <lv_option>.
      <lv_option> = 'EQ'.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_out> TO <lv_sign>.
      <lv_sign> = 'I'.
    ENDLOOP. " at <lt_table> ASSIGNING <ls_table_wa>.


  ENDMETHOD.

  METHOD fill_residual_data.
    FIELD-SYMBOLS <ls_out_data> LIKE LINE OF at_out_data.
    DATA ls_pernr_map TYPE ty_pernr_map.
    DATA ls_lga_map LIKE LINE OF at_lga_map.


    SELECT SINGLE * FROM zhr_migr_pernr INTO @ls_pernr_map
      WHERE zpernr_hist = @as_cur_pernr-pernr.
    LOOP AT at_out_data ASSIGNING <ls_out_data> WHERE tbn = as_cur_pernr-pernr. " Таблица общая, а результаты у нас по одному человеку
      CLEAR ls_lga_map.
      <ls_out_data>-fio = |{ as_cur_pernr-p0002-nachn } { as_cur_pernr-p0002-vorna } { as_cur_pernr-p0002-nach2 }|.
      <ls_out_data>-pernr = ls_pernr_map-zpernr_new.
      READ TABLE at_lga_map INTO ls_lga_map WITH KEY lgart1 = <ls_out_data>-code.
      <ls_out_data>-lgart = ls_lga_map-lgart2.
      <ls_out_data>-lgtext = ls_lga_map-lgtxt2.
    ENDLOOP.
  ENDMETHOD.


  METHOD prepare_and_download.
    DATA lv_err_message TYPE string.
    DATA lt_out_text TYPE STANDARD TABLE OF string.
    DATA lv_out_text LIKE LINE OF lt_out_text.
    DATA ls_out_data LIKE LINE OF at_out_data.
    DATA lo_err TYPE REF TO cx_dynamic_check.


    APPEND 'PRED;FIO;PNALT;PERNR;FPBEG;FPEND;CODE;LGART;LGTXT;BETPE;ANZHL;BETRG' TO lt_out_text.
    IF av_file_length EQ 0.
      av_file_length = lines( at_out_data ).
    ENDIF.
    LOOP AT at_out_data INTO ls_out_data TO av_file_length.
      CLEAR lv_out_text.
      lv_out_text = |{ lv_out_text }{ ac_out_system_code };{ ls_out_data-fio };{ ls_out_data-tbn };|.
      lv_out_text = |{ lv_out_text }{ ls_out_data-pernr };{ ls_out_data-fpbeg };{ ls_out_data-fpend };|.
      lv_out_text = |{ lv_out_text }{ ls_out_data-code };{ ls_out_data-lgart };{ ls_out_data-lgtext };|.
      lv_out_text = |{ lv_out_text }{ ls_out_data-betpe };{ ls_out_data-anzhl };{ ls_out_data-betrg }|.
      APPEND lv_out_text TO lt_out_text.
      " lv_out_text = |{ lv_out_text }{ cl_abap_char_utilities=>newline }|.
    ENDLOOP.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = av_fullpath
        codepage                  = '4110'
      CHANGING
        data_tab                  = lt_out_text
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_exprt_payroll_err
        EXPORTING
          textid = zcx_exprt_payroll_err=>open_file_err.
    ENDIF.
  ENDMETHOD.

  METHOD get_file_length.
    r_result = me->av_file_length.
  ENDMETHOD.

  METHOD set_file_length.
    me->av_file_length = iv_file_length.
  ENDMETHOD.

  METHOD save_results_to_db.
    DATA lo_err TYPE REF TO cx_root.
    TRY.
        zclhr_exprt_table_service=>export_tab(
          EXPORTING
            it_data = at_out_data
        ).
      CATCH zcx_exprt_table_service INTO lo_err. " Класс-обработчик ошибок для экспорта/импорта данных Сибур
        RAISE EXCEPTION TYPE zcx_exprt_payroll_err
          EXPORTING
            previous = lo_err.
    ENDTRY.
  ENDMETHOD.

  METHOD get_results_from_db.
    CLEAR at_out_data.
    zclhr_exprt_table_service=>get_table(
      EXPORTING
        iv_op_id = iv_op_id
      IMPORTING
        et_tab   = at_out_data
    ).
  ENDMETHOD.

ENDCLASS.
