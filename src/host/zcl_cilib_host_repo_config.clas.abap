"! Repository configuration
CLASS zcl_cilib_host_repo_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_repo TYPE zcilib_host_repo
                            is_data TYPE zcilib_host_repo_data,
      get_bot_name RETURNING VALUE(rv_bot_name) TYPE zcilib_bot_name,
      get_branching_strategy RETURNING VALUE(rv_strategy) TYPE zcilib_git_branchstrategy.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_name TYPE zcilib_host_repo,
      ms_data TYPE zcilib_host_repo_data.
ENDCLASS.



CLASS zcl_cilib_host_repo_config IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_repo.
    ms_data = is_data.
  ENDMETHOD.

  METHOD get_bot_name.
    rv_bot_name = ms_data-bot.
  ENDMETHOD.

  METHOD get_branching_strategy.
    rv_strategy = ms_data-branching_strategy.
  ENDMETHOD.
ENDCLASS.
