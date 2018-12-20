FUNCTION ZCILIB_EXIT_RAISE_EVENT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EVENT) TYPE  CHAR1
*"     VALUE(IV_TRANSPORT) TYPE  TRKORR
*"     VALUE(IV_SYSTEM) TYPE  SYST_SYSID
*"----------------------------------------------------------------------
  zcl_cilib_exit_event_handler=>handle_tr_feedback(
    iv_event     = iv_event
    iv_transport = iv_transport
    iv_system    = iv_system
  ).
ENDFUNCTION.
