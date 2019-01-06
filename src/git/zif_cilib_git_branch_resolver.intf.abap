"! Branching strategy resolver
INTERFACE zif_cilib_git_branch_resolver PUBLIC.
  CONSTANTS:
    BEGIN OF gc_strategies,
      attribute_value TYPE zcilib_git_branchstrategy VALUE 'A',
      text_id         TYPE zcilib_git_branchstrategy VALUE 'R',
      cts_project     TYPE zcilib_git_branchstrategy VALUE 'P',
      transport       TYPE zcilib_git_branchstrategy VALUE 'T',
    END OF gc_strategies.
  METHODS:
    get_branch_for_transport IMPORTING iv_transport     TYPE trkorr
                                       iv_strategy      TYPE zcilib_git_branchstrategy
                             RETURNING VALUE(rv_branch) TYPE string
                             RAISING zcx_cilib_illegal_argument.
ENDINTERFACE.
