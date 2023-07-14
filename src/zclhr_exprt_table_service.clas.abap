CLASS zclhr_exprt_table_service DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      "! ТТ вида таблицы экспорта. <br/>
      "! <ol><li><em>MANDT</em> - мандант</li>
      "!     <li><em>OPERATION_ID</em> - ИД операции</li>
      "!     <li><em>SEQNR</em> - номер строки</li>
      "!     <li><em>FIELD_NAME</em> - имя поля</li>
      "!     <li><em>FIELD_TYPE</em> - тип поля ABAP (c, n, d и тд)</li>
      "!     <li><em>FIELD_LENGTH</em> - длина поля в байтах</li>
      "!     <li><em>FIELD_DECIMALS</em> - количество десятичных разрядов для численных типов</li>
      "!     <li><em>VALUE</em> - значение поля</li>
      "! </ol>
      ty_t_exprt_res TYPE STANDARD TABLE OF zhcm_exprt_res WITH DEFAULT KEY,
      ty_t_exprt_inf TYPE STANDARD TABLE OF zhcm_exprt_opdat WITH DEFAULT KEY,
      ty_t_dat_range TYPE RANGE OF datum,
      ty_t_tim_range TYPE RANGE OF syuzeit.
    CLASS-METHODS:
      "! Метод читает поля внутренней таблицы и записывает ее в БД в таблицу ZHCM_EXPRT_RES
      "! @parameter it_data | Входная внутренняя таблица
      "! @parameter et_tab | Выходная таблица как строки в бд
      "! @parameter rv_uuid | Сгенерированный ИД операции
      export_tab
        IMPORTING it_data        TYPE table
        EXPORTING et_tab         TYPE ty_t_exprt_res
        RETURNING VALUE(rv_uuid) TYPE sysuuid_c32
        RAISING   zcx_exprt_table_service,
      "! Найти результаты экспорта по критерию
      "! @parameter iv_op_id | ИД операции
      "! @parameter iv_uname | Имя пользователя создавшего экспорт
      "! @parameter it_datum | Даты экспорта
      "! @parameter it_time  | Время экспорта
      "! @parameter iv_cprog | Вызывающая программа экспорта
      search
        IMPORTING iv_op_id     TYPE sysuuid_c32 OPTIONAL
                  iv_uname     TYPE syuname OPTIONAL
                  it_dat_range TYPE ty_t_dat_range OPTIONAL
                  it_tim_range TYPE ty_t_tim_range OPTIONAL
                  iv_cprog     TYPE sycprog OPTIONAL
        EXPORTING et_results   TYPE ty_t_exprt_inf,
      "! Метод заполняет данными из БД экспорта по переданному ИД выохдную внутреннюю таблицу
      "! @parameter iv_op_id | ИД экспорта
      "! @parameter et_tab | выходная таблица (тип внутренней таблицы должен быть тот же, что был при сохранении в БД)
      get_table
        IMPORTING iv_op_id TYPE sysuuid_c32
        EXPORTING et_tab   TYPE table.
  PROTECTED SECTION.
    "! Преобразует переданный элемент данных в рендж
    "! @parameter iv_elem | Элемент данных (не структура)
    "! @parameter et_range | Рендж
    CLASS-METHODS convert_elem_to_range
      IMPORTING iv_elem  TYPE any
      EXPORTING et_range TYPE table.
  PRIVATE SECTION.
    CLASS-DATA at_work_tab TYPE ty_t_exprt_res.
    CLASS-DATA as_work_inf TYPE zhcm_exprt_opdat.
    "! Сохраняет результаты в БД
    "! @parameter it_work_tab | Таблица для сохранения
    CLASS-METHODS save_to_db
      IMPORTING
        it_work_tab TYPE zclhr_exprt_table_service=>ty_t_exprt_res.
    "! Сохраняет информацию об операции в БД (Дата, имя пользователя, время и тд)
    "! @parameter is_work_inf | Структура с информацией
    CLASS-METHODS save_information_to_db
      IMPORTING
        is_work_inf TYPE zhcm_exprt_opdat.
ENDCLASS.



CLASS zclhr_exprt_table_service IMPLEMENTATION.
  METHOD export_tab.
    DATA: lo_tab_descr    TYPE REF TO cl_abap_tabledescr,
          lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_err          TYPE REF TO cx_root.
    FIELD-SYMBOLS: <ls_line> TYPE any,
                   <lv_comp> TYPE any.
    DATA ls_struct_comp TYPE abap_compdescr.
    DATA: lt_exprt_res TYPE ty_t_exprt_res,
          ls_exprt_res LIKE LINE OF lt_exprt_res.
    DATA ls_exprt_inf TYPE zhcm_exprt_opdat.



    lo_tab_descr ?= cl_abap_tabledescr=>describe_by_data( it_data ).
    IF lo_tab_descr->kind NE cl_abap_typedescr=>kind_table.
      RAISE EXCEPTION TYPE zcx_exprt_table_service
        EXPORTING
          textid = zcx_exprt_table_service=>not_tab_err.
    ENDIF.
    ls_exprt_res-mandt = sy-mandt.
    TRY.
        ls_exprt_res-operation_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO lo_err. " Error Class for UUID Processing Errors
        RAISE EXCEPTION TYPE zcx_exprt_table_service
          EXPORTING
            previous = lo_err.
    ENDTRY.
    LOOP AT it_data ASSIGNING <ls_line>.
      ls_exprt_res-seqnr = sy-tabix.
      lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( <ls_line> ).
      LOOP AT lo_struct_descr->components INTO ls_struct_comp.
        ASSIGN COMPONENT ls_struct_comp-name OF STRUCTURE <ls_line> TO <lv_comp>.
        ls_exprt_res-field_name     = ls_struct_comp-name.
        ls_exprt_res-field_type     = ls_struct_comp-type_kind.
        ls_exprt_res-field_length   = ls_struct_comp-length.
        ls_exprt_res-field_decimals = ls_struct_comp-decimals.
        ls_exprt_res-value = <lv_comp>.
        APPEND ls_exprt_res TO lt_exprt_res.
      ENDLOOP.
    ENDLOOP.
    ls_exprt_inf-mandt = sy-mandt.
    ls_exprt_inf-call_prog = sy-cprog.
    ls_exprt_inf-date_created = sy-datum.
    ls_exprt_inf-operation_id = ls_exprt_res-operation_id.
    ls_exprt_inf-time_created = sy-uzeit.
    ls_exprt_inf-uname = sy-uname.
    rv_uuid = ls_exprt_res-operation_id.
    et_tab = lt_exprt_res.
    as_work_inf = ls_exprt_inf.
    at_work_tab = lt_exprt_res.
    save_to_db( at_work_tab ).
    save_information_to_db( as_work_inf ).
  ENDMETHOD.


  METHOD save_to_db.
    CALL FUNCTION 'Z_HCM_SAVE_EXPRT_TAB' IN UPDATE TASK
      EXPORTING
        it_tab = it_work_tab.                 " Входная таблица вида таблицы экспорта
    COMMIT WORK.
  ENDMETHOD.

  METHOD save_information_to_db.
    CALL FUNCTION 'Z_HCM_SAVE_EXPRT_INF'
      EXPORTING
        is_inf = is_work_inf.                 " данные экспорта для Сибур
  ENDMETHOD.

  METHOD search.
    DATA lt_op_id_range TYPE RANGE OF sysuuid_c32.
    DATA lt_uname_range TYPE RANGE OF syuname.
    DATA lt_cprog_range TYPE RANGE OF cprog.


    CLEAR et_results.
    IF iv_op_id IS SUPPLIED.
      zclhr_exprt_table_service=>convert_elem_to_range(
        EXPORTING
          iv_elem  = iv_op_id
        IMPORTING
          et_range = lt_op_id_range
      ).
    ENDIF.
    IF iv_uname IS SUPPLIED.
      zclhr_exprt_table_service=>convert_elem_to_range(
        EXPORTING
          iv_elem  = iv_uname
        IMPORTING
          et_range = lt_uname_range
      ).
    ENDIF.
    IF iv_cprog IS SUPPLIED.
      zclhr_exprt_table_service=>convert_elem_to_range(
              EXPORTING
                iv_elem  = iv_cprog
              IMPORTING
                et_range = lt_cprog_range
            ).
    ENDIF.
    SELECT * FROM zhcm_exprt_opdat INTO TABLE @et_results
      WHERE operation_id IN @lt_op_id_range
        AND uname        IN @lt_uname_range
        AND date_created IN @it_dat_range
        AND time_created IN @it_tim_range
        AND call_prog    IN @lt_cprog_range.
  ENDMETHOD.

  METHOD convert_elem_to_range.
    FIELD-SYMBOLS: <ls_line_range> TYPE any,
                   <lv_range_comp> TYPE any.


    APPEND INITIAL LINE TO et_range ASSIGNING <ls_line_range>.
    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_line_range> TO <lv_range_comp>.
    <lv_range_comp> = 'EQ'.
    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_line_range> TO <lv_range_comp>.
    <lv_range_comp> = 'I'.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_line_range> TO <lv_range_comp>.
    <lv_range_comp> = iv_elem.
  ENDMETHOD.

  METHOD get_table.
    DATA: lt_export TYPE ty_t_exprt_res,
          ls_export LIKE LINE OF lt_export.
    DATA lv_old_seqnr LIKE ls_export-seqnr.
    FIELD-SYMBOLS: <ls_out_line> TYPE any,
                   <lv_out_comp> TYPE any.


    SELECT * FROM zhcm_exprt_res INTO TABLE lt_export
      WHERE operation_id EQ iv_op_id.

    CLEAR et_tab.
    lv_old_seqnr = 0.
    SORT lt_export BY seqnr.
    LOOP AT lt_export INTO ls_export.
      IF lv_old_seqnr NE ls_export-seqnr.
        APPEND INITIAL LINE TO et_tab ASSIGNING <ls_out_line>.
      ENDIF.
      ASSIGN COMPONENT ls_export-field_name OF STRUCTURE <ls_out_line> TO <lv_out_comp>.
      <lv_out_comp> = ls_export-value.
      lv_old_seqnr = ls_export-seqnr.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
