"! Bot repo transport listener
CLASS zcl_cilib_bot_repo_tr_listener DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_badi_interface,
      zif_cilib_exit_repo_tr.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_bot_repo_tr_listener IMPLEMENTATION.
  METHOD zif_cilib_exit_repo_tr~on_transport_imported.
    ##TODO.
  ENDMETHOD.

  METHOD zif_cilib_exit_repo_tr~on_transport_released.
    DATA: lv_update_comment TYPE i.

    DATA(li_logger) = zcl_cilib_factory=>get_logger( ).

    DATA(li_abapgit) = zcl_cilib_factory=>get_abapgit_api( ).
    DATA(lv_repo_url) = li_abapgit->get_repo_url( iv_repo_id ).
    DATA(li_host) = zcl_cilib_factory=>get_host_for_repo_url( lv_repo_url ).
    DATA(lv_repo_name) = li_host->get_repo_name_from_url( lv_repo_url ).

    TRY.
        IF li_host->does_repo_exist( lv_repo_name ) = abap_true.
          LOOP AT li_host->get_repo_branches( lv_repo_name )
               ASSIGNING FIELD-SYMBOL(<lv_branch>)
               WHERE table_line = iv_transport
                  OR table_line = to_lower( iv_transport ).

            DATA(lv_pull_request) = li_host->get_pull_request_for_branch(
              iv_repository = lv_repo_name
              iv_branch     = <lv_branch>
            ).
            li_logger->debug( |Found pull request { lv_pull_request }| ).
            DATA(lt_comments) = li_host->get_comments_for_pull_request(
              iv_repository   = lv_repo_name
              iv_pull_request = lv_pull_request
              iv_by_author    = 'cts-bot' ##TODO
            ).
            li_logger->debug( |Found { lines( lt_comments ) NUMBER = USER } comments by bot user on PR| ).
            LOOP AT lt_comments ASSIGNING FIELD-SYMBOL(<ls_comment>).
              DATA(lv_content) = li_host->get_pr_comment_content(
                iv_repository   = lv_repo_name
                iv_pull_request = lv_pull_request
                iv_comment      = <ls_comment>-id
              ).
              IF lv_content CP 'CTS Status*' ##TODO.
                lv_update_comment = <ls_comment>-id.
                EXIT.
              ENDIF.
            ENDLOOP.

            DATA(lv_comment_line) = |{ sy-datum }{ sy-uzeit }{ sy-uname }: Released { iv_transport } on { iv_system }|.

            IF lv_update_comment IS NOT INITIAL.
              li_logger->debug( |Found existing PR comment { lv_update_comment }, updating| ).
              li_host->set_pr_comment_content(
                iv_repository   = lv_repo_name
                iv_pull_request = lv_pull_request
                iv_comment      = lv_update_comment
                iv_content      = lv_content && cl_abap_char_utilities=>cr_lf && lv_comment_line
              ).
            ELSE.
              li_logger->debug( |Creating new PR comment| ).
              li_host->create_pr_comment(
                iv_repository   = lv_repo_name
                iv_pull_request = lv_pull_request
                iv_content      = 'CTS Status:' && cl_abap_char_utilities=>cr_lf && lv_comment_line
              ).
            ENDIF.

            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            li_logger->error( |Could not find branch '{ iv_transport }' with target 'master'| ).
          ENDIF.
        ELSE.
          li_logger->error( |Repository '{ lv_repo_name }' not found.| ).
        ENDIF.
      CATCH zcx_cilib_http_comm_error INTO DATA(lx_comm_error).
        li_logger->exception( lx_comm_error ).
      CATCH zcx_cilib_not_found INTO DATA(lx_not_found).
        " Repo / branch / pull request / comment was not found
        li_logger->exception( lx_not_found ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
