REPORT zcilib_test.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      run.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD run.
    DATA(li_host) = zcl_cilib_factory=>get_host_for_repo_url( 'https://gitlab.dummy.nodomain/department-a/test-project' ).
*    DATA(li_host) = CAST zif_cilib_host(
*      NEW zcl_cilib_host_gitlab( NEW #( VALUE #( destination = 'GITLAB' api_token = 'n6ER2bx8odvTAMhox33y' ) ) )
*    ).

    TRY.
        li_host->authenticate( ).
        WRITE: / li_host->does_repo_exist( 'department-a/test-project' ),
                 li_host->does_repo_exist( 'test-project' ),
                 li_host->does_repo_exist( 'department-a/test-project1' ),
                 li_host->does_repo_exist( 'test-project1' ).
        LOOP AT li_host->get_repo_branches( 'department-a/test-project' ) ASSIGNING FIELD-SYMBOL(<lv_branch>).
          WRITE: / <lv_branch>.
        ENDLOOP.
        WRITE: / li_host->get_pull_request_for_branch( iv_repository = 'department-a/test-project' iv_branch = 'test' ).
        LOOP AT li_host->get_comments_for_pull_request(
                  iv_repository   = 'department-a/test-project'
                  iv_pull_request = 1
                  iv_by_author    = 'cts-bot'
                ) ASSIGNING FIELD-SYMBOL(<ls_comment>).
          WRITE: / li_host->get_pr_comment_content(
                     iv_repository   = 'department-a/test-project'
                     iv_pull_request = 1
                     iv_comment      = <ls_comment>-id ).
          li_host->set_pr_comment_content(
            iv_repository   = 'department-a/test-project'
            iv_pull_request = 1
            iv_comment      = <ls_comment>-id
            iv_content      = 'This is a test -> EDITED'
          ).
        ENDLOOP.
      CATCH zcx_cilib_http_comm_error INTO DATA(lx_ex).
        MESSAGE lx_ex TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

*    li_host->create_pr_comment(
*      iv_repository   = 'department-a/test-project'
*      iv_pull_request = 1
*      iv_content      = 'This is a test'
*    ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->run( ).
