"! Git host
INTERFACE zif_cilib_host PUBLIC.
  TYPES:
    BEGIN OF gty_comment,
      id     TYPE i,
      author TYPE string,
    END OF gty_comment,
    BEGIN OF gty_wiki_page,
      name   TYPE string,
      format TYPE string,
      title  TYPE string,
    END OF gty_wiki_page,
    BEGIN OF gty_wiki_page_with_content.
      INCLUDE TYPE gty_wiki_page.
  TYPES:
    content TYPE string,
    END OF gty_wiki_page_with_content,
    gty_comment_tab   TYPE STANDARD TABLE OF gty_comment WITH EMPTY KEY,
    gty_wiki_page_tab TYPE SORTED TABLE OF gty_wiki_page WITH UNIQUE KEY name.
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
                                     zcx_cilib_unsupp_operation,
    get_wiki_pages IMPORTING iv_repository   TYPE string
                   RETURNING VALUE(rt_pages) TYPE gty_wiki_page_tab
                   RAISING   zcx_cilib_not_found
                             zcx_cilib_http_comm_error
                             zcx_cilib_unsupp_operation,
    get_wiki_page IMPORTING iv_repository  TYPE string
                            iv_page_name   TYPE string
                  RETURNING VALUE(rs_page) TYPE gty_wiki_page_with_content
                  RAISING   zcx_cilib_not_found
                            zcx_cilib_http_comm_error
                            zcx_cilib_unsupp_operation,
    create_wiki_page IMPORTING iv_repository TYPE string
                               iv_page_name  TYPE string
                               iv_content    TYPE string
                               iv_title      TYPE string
                               iv_format     TYPE string OPTIONAL
                     RAISING   zcx_cilib_not_found
                               zcx_cilib_http_comm_error
                               zcx_cilib_unsupp_operation,
    update_wiki_page IMPORTING iv_repository TYPE string
                               iv_page_name  TYPE string
                               iv_content    TYPE string OPTIONAL
                               iv_title      TYPE string OPTIONAL
                               iv_format     TYPE string OPTIONAL
                     RAISING   zcx_cilib_not_found
                               zcx_cilib_http_comm_error
                               zcx_cilib_unsupp_operation,
    delete_wiki_page IMPORTING iv_repository TYPE string
                               iv_page_name  TYPE string
                     RAISING   zcx_cilib_not_found
                               zcx_cilib_http_comm_error
                               zcx_cilib_unsupp_operation,
    get_config RETURNING VALUE(ro_config) TYPE REF TO zcl_cilib_host_config ##TODO, " Should this be included?
    get_host_path RETURNING VALUE(rv_host_path) TYPE zcilib_host_hostpath.
  DATA:
    mv_is_authenticated TYPE abap_bool READ-ONLY.
ENDINTERFACE.
