CLASS zcx_exprt_payroll_err DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_exprt_payroll_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_exprt_payroll_err .
    CONSTANTS:
      BEGIN OF read_filesystem_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF read_filesystem_err .
    CONSTANTS:
      BEGIN OF read_rgdir_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF read_rgdir_err .
    CONSTANTS:
      BEGIN OF read_rgdir_auth_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF read_rgdir_auth_err .
    CONSTANTS:
      BEGIN OF read_pay_result_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF read_pay_result_err .
    CONSTANTS:
      BEGIN OF read_pay_result_auth_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF read_pay_result_auth_err .
    CONSTANTS:
      BEGIN OF empty_tab_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF empty_tab_err .
    CONSTANTS:
      BEGIN OF invalid_line_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_line_err.
    CONSTANTS:
      BEGIN OF open_file_err,
        msgid TYPE symsgid VALUE 'ZHR_EXPRT_PY_RES',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'ARGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF open_file_err.
    DATA argv1 TYPE string .
    DATA argv2 TYPE string .
    DATA argv3 TYPE string .
    DATA argv4 TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !argv1    TYPE any OPTIONAL
        !argv2    TYPE any OPTIONAL
        !argv3    TYPE any OPTIONAL
        !argv4    TYPE any OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_exprt_payroll_err IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_exprt_payroll_err .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->argv1 = argv1.
    me->argv2 = argv2.
    me->argv3 = argv3.
    me->argv4 = argv4.
  ENDMETHOD.
ENDCLASS.
