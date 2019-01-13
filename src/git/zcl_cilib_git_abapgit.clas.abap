"! abapGit API facade
"! <p>
"! https://docs.abapgit.org/development/api.html
"! </p>
"! <p>
"! There is no hardcoded dependency to abapGit because CILIB_EXIT might be used on production and test systems
"! while abapGit might only be installed in a local package on the development system. Events are redirected to the
"! development system using RFC.
"! </p>
CLASS zcl_cilib_git_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_git_abapgit.
    METHODS:
      constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_git_abapgit IMPLEMENTATION.
  METHOD zif_cilib_git_abapgit~get_repo_url.
    DATA: lo_repo_srv TYPE REF TO object,
          lo_repo     TYPE REF TO object.

    TRY.
        CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>('GET_INSTANCE')
          RECEIVING
            ri_srv = lo_repo_srv.

        CALL METHOD lo_repo_srv->('ZIF_ABAPGIT_REPO_SRV~GET')
          EXPORTING
            iv_key  = iv_repo_key
          RECEIVING
            ro_repo = lo_repo.

        IF cl_abap_typedescr=>describe_by_object_ref( lo_repo )->absolute_name NP '\CLASS=ZCL_ABAPGIT_REPO_ONLINE*'.
          RAISE EXCEPTION TYPE zcx_cilib_not_found.
        ENDIF.

        CALL METHOD lo_repo->('GET_URL')
          RECEIVING
            rv_url = rv_url.

      CATCH cx_root INTO DATA(lx_ex).
        IF cl_abap_typedescr=>describe_by_object_ref( lx_ex )->absolute_name CP '\CLASS=ZCX_ABAPGIT_EXCEPTION*'.
          RAISE EXCEPTION TYPE zcx_cilib_not_found.
        ELSE.
          RAISE EXCEPTION lx_ex.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cilib_git_abapgit~is_object_part_of_online_repo.
    DATA: lt_package_range TYPE RANGE OF devclass,
          lo_found_repo    TYPE REF TO object,
          lo_repo_srv      TYPE REF TO object,
          lt_repos         TYPE STANDARD TABLE OF REF TO object,
          lv_repo_package  TYPE devclass.

    SELECT SINGLE devclass INTO @DATA(lv_package)
      FROM tadir
      WHERE pgmid    = 'R3TR'
        AND object   = @is_object-type
        AND obj_name = @is_object-name.
    IF sy-subrc <> 0 OR lv_package IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    INSERT VALUE #( sign = 'I' option = 'EQ' low = lv_package ) INTO TABLE lt_package_range.

    DO.
      SELECT SINGLE parentcl INTO @DATA(lv_parent_package)
        FROM tdevc
        WHERE devclass = @lv_package.
      ASSERT sy-subrc = 0.

      IF lv_parent_package IS INITIAL OR lv_parent_package = lv_package.
        EXIT.
      ENDIF.

      INSERT VALUE #( sign = 'I' option = 'EQ' low = lv_parent_package ) INTO TABLE lt_package_range.
      lv_package = lv_parent_package.
    ENDDO.

    CALL METHOD ('ZCL_ABAPGIT_REPO_SRV')=>('GET_INSTANCE')
      RECEIVING
        ri_srv = lo_repo_srv.

    CALL METHOD lo_repo_srv->('ZIF_ABAPGIT_REPO_SRV~LIST')
      RECEIVING
        rt_list = lt_repos.

    LOOP AT lt_repos INTO DATA(lo_repo).
      CALL METHOD lo_repo->('GET_PACKAGE')
        RECEIVING
          rv_package = lv_repo_package.
      IF lv_repo_package IN lt_package_range.
        IF cl_abap_typedescr=>describe_by_object_ref( lo_repo )->absolute_name CP '\CLASS=ZCL_ABAPGIT_REPO_ONLINE*'.
          lo_found_repo = lo_repo.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lo_found_repo IS BOUND.
      rv_is_part = abap_true.
      CALL METHOD lo_found_repo->('GET_KEY')
        RECEIVING
          rv_key = ev_repo_key.
    ELSE.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    CONSTANTS: lc_version_attribute TYPE string VALUE `ZIF_ABAPGIT_VERSION=>GC_ABAP_VERSION`.
    FIELD-SYMBOLS: <lv_version> TYPE string.

    ASSIGN (lc_version_attribute) TO <lv_version>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_git_abapgit_missing.
    ENDIF.

    SPLIT <lv_version> AT '.' INTO DATA(lv_major) DATA(lv_minor) DATA(lv_patch).
    IF lv_major < 1 OR ( lv_major = 1 AND lv_minor < 81 ).
      RAISE EXCEPTION TYPE zcx_cilib_git_abapgit_missing.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
