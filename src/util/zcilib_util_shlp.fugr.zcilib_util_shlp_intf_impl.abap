"! Interface implementation search help exit
FUNCTION ZCILIB_UTIL_SHLP_INTF_IMPL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_result,
           clsname     TYPE seoclsname,
           description TYPE seodescr,
         END OF lty_result,
         lty_result_tab TYPE STANDARD TABLE OF lty_result WITH DEFAULT KEY.
  DATA: lt_result          TYPE lty_result_tab,
        lt_selopt          TYPE ddshselops,
        lt_implementations TYPE seor_implementing_keys.

  CHECK callcontrol-step = 'SELECT'.

  CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
    EXPORTING
      parameter   = 'INTFNAME'
    TABLES
      shlp_tab    = shlp_tab
      record_tab  = record_tab
      selopt_tab  = lt_selopt
    CHANGING
      shlp        = shlp
      callcontrol = callcontrol
    EXCEPTIONS
      OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  TRY.
      CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
        EXPORTING
          intkey       = VALUE seoclskey( clsname = CONV #( lt_selopt[ 1 ]-low ) )
        IMPORTING
          impkeys      = lt_implementations
        EXCEPTIONS
          not_existing = 1
          OTHERS       = 2.
      ASSERT sy-subrc = 0.
      lt_result = CORRESPONDING #( lt_implementations ).
    CATCH cx_sy_itab_line_not_found.
      RETURN.
  ENDTRY.

  LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
    DATA(lo_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( <ls_result>-clsname ) ).
    IF lo_descr->is_instantiatable( ) = abap_false.
      DELETE lt_implementations FROM <ls_result>.
      CONTINUE.
    ELSE.
      SELECT SINGLE descript INTO @<ls_result>-description
        FROM seoclasstx
        WHERE clsname = @<ls_result>-clsname
          AND langu   = @sy-langu.
    ENDIF.
  ENDLOOP.

  CHECK lt_result IS NOT INITIAL.

  CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
    EXPORTING
      parameter   = 'CLSNAME'
      fieldname   = 'CLSNAME'
    TABLES
      shlp_tab    = shlp_tab[]
      record_tab  = record_tab[]
      source_tab  = lt_result[]
    CHANGING
      shlp        = shlp
      callcontrol = callcontrol
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
    EXPORTING
      parameter   = 'DESCRIPTION'
      fieldname   = 'DESCRIPTION'
    TABLES
      shlp_tab    = shlp_tab[]
      record_tab  = record_tab[]
      source_tab  = lt_result[]
    CHANGING
      shlp        = shlp
      callcontrol = callcontrol
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  callcontrol-step = 'DISP'.
ENDFUNCTION.
