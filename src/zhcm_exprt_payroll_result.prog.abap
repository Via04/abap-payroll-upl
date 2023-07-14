*&---------------------------------------------------------------------*
*& Report zhcm_exprt_payroll_result
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_exprt_payroll_result.

NODES: peras.
TABLES: pernr.
INFOTYPES: 0002.

INCLUDE zhcm_exprt_payroll_result_top.
INCLUDE zhcm_exprt_payroll_result_ssc.
