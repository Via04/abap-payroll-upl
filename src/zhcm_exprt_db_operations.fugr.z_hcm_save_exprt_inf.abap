FUNCTION z_hcm_save_exprt_inf.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_INF) TYPE  ZHCM_EXPRT_OPDAT
*"----------------------------------------------------------------------

  INSERT zhcm_exprt_opdat FROM is_inf.
ENDFUNCTION.
