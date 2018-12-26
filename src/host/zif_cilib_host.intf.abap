"! Git host
INTERFACE zif_cilib_host PUBLIC.
  TYPES:
    BEGIN OF gty_comment,
      id     TYPE i,
      author TYPE string,
    END OF gty_comment,
    gty_comment_tab TYPE STANDARD TABLE OF gty_comment WITH EMPTY KEY.
  METHODS:
    authenticate RAISING zcx_cilib_http_comm_error
                         zcx_cilib_unsupp_operation,
    does_repo_exist IMPORTING iv_repository    TYPE string
                    RETURNING VALUE(rv_exists) TYPE abap_bool
                    RAISING   zcx_cilib_http_comm_error
                              zcx_cilib_unsupp_operation,
    get_repo_branches IMPORTING iv_repository      TYPE string
                      RETURNING VALUE(rt_branches) TYPE stringtab
                      RAISING   zcx_cilib_not_found
                                zcx_cilib_http_comm_error
                                zcx_cilib_unsupp_operation,
    get_pull_request_for_branch IMPORTING iv_repository          TYPE string
                                          iv_branch              TYPE string
                                RETURNING VALUE(rv_pull_request) TYPE i
                                RAISING   zcx_cilib_not_found
                                          zcx_cilib_http_comm_error
                                          zcx_cilib_unsupp_operation,
    get_comments_for_pull_request IMPORTING iv_repository      TYPE string
                                            iv_pull_request    TYPE i
                                            iv_by_author       TYPE string OPTIONAL
                                  RETURNING VALUE(rt_comments) TYPE gty_comment_tab
                                  RAISING   zcx_cilib_not_found
                                            zcx_cilib_http_comm_error
                                            zcx_cilib_unsupp_operation,
    get_pr_comment_content IMPORTING iv_repository     TYPE string
                                     iv_pull_request   TYPE i
                                     iv_comment        TYPE i
                           RETURNING VALUE(rv_content) TYPE string
                           RAISING   zcx_cilib_not_found
                                     zcx_cilib_http_comm_error
                                     zcx_cilib_unsupp_operation,
    set_pr_comment_content IMPORTING iv_repository   TYPE string
                                     iv_pull_request TYPE i
                                     iv_comment      TYPE i
                                     iv_content      TYPE string
                           RAISING   zcx_cilib_not_found
                                     zcx_cilib_http_comm_error
                                     zcx_cilib_unsupp_operation,
    create_pr_comment IMPORTING iv_repository     TYPE string
                                iv_pull_request   TYPE i
                                iv_content        TYPE string
                      RETURNING VALUE(rv_comment) TYPE i
                      RAISING   zcx_cilib_not_found
                                zcx_cilib_http_comm_error
                                zcx_cilib_unsupp_operation,
    get_repo_name_from_url IMPORTING iv_url               TYPE string
                           RETURNING VALUE(rv_repository) TYPE string
                           RAISING   zcx_cilib_illegal_argument
                                     zcx_cilib_unsupp_operation.
  DATA:
    mv_is_authenticated TYPE abap_bool READ-ONLY.
ENDINTERFACE.
