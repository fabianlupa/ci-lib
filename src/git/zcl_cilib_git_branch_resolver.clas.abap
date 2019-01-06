"! Branching strategy resolver
CLASS zcl_cilib_git_branch_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_git_branch_resolver.
    METHODS:
      constructor IMPORTING ii_cts_api TYPE REF TO zif_cilib_cts_api.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mi_cts_api TYPE REF TO zif_cilib_cts_api.
ENDCLASS.



CLASS zcl_cilib_git_branch_resolver IMPLEMENTATION.
  METHOD constructor.
    mi_cts_api = ii_cts_api.
  ENDMETHOD.

  METHOD zif_cilib_git_branch_resolver~get_branch_for_transport.
    IF iv_strategy NS zif_cilib_git_branch_resolver=>gc_strategies OR iv_transport IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument.
    ENDIF.

    CASE iv_strategy.
      WHEN zif_cilib_git_branch_resolver=>gc_strategies-transport.
        rv_branch = iv_transport.

*      WHEN zif_cilib_git_branch_resolver=>gc_strategies-attribute_value.
*      WHEN zif_cilib_git_branch_resolver=>gc_strategies-text_id.
        ##TODO. " These need more settings

      WHEN zif_cilib_git_branch_resolver=>gc_strategies-cts_project.
         rv_branch = mi_cts_api->get_cts_project_for_transport( iv_transport ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_cilib_not_implemented.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
