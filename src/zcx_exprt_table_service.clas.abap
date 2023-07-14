CLASS zcx_exprt_table_service DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF not_tab_err,
        msgid TYPE symsgid      VALUE 'ZHR_EXPRT_RES_SRVC',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_tab_err.

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
    DATA:
      argv1 TYPE string,
      argv2 TYPE string,
      argv3 TYPE string,
      argv4 TYPE string.
ENDCLASS.



CLASS zcx_exprt_table_service IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->argv1 = argv1.
    me->argv2 = argv2.
    me->argv3 = argv3.
    me->argv4 = argv4.
  ENDMETHOD.
ENDCLASS.
