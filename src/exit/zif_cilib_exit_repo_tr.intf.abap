INTERFACE zif_cilib_exit_repo_tr PUBLIC.
  INTERFACES:
    if_badi_interface.
  METHODS:
    on_transport_imported IMPORTING iv_system    TYPE syst_sysid
                                    iv_transport TYPE trkorr
                                    iv_repo_id   TYPE zif_cilib_abapgit_api=>gty_repo_key,
    on_transport_released IMPORTING iv_system    TYPE syst_sysid
                                    iv_transport TYPE trkorr
                                    iv_repo_id   TYPE zif_cilib_abapgit_api=>gty_repo_key.
ENDINTERFACE.
