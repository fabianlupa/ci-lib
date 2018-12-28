"! Bot
CLASS zcl_cilib_bot DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_bot.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_name TYPE zcilib_bot_name.
ENDCLASS.



CLASS zcl_cilib_bot IMPLEMENTATION.
  METHOD zif_cilib_bot~update_cts_comments.

  ENDMETHOD.
ENDCLASS.
