"! abapGit API
"! <p>
"! https://docs.abapgit.org/development/api.html
"! </p>
"! <p>
"! There is no hardcoded dependency to abapGit because CILIB_EXIT might be used on production and test systems
"! while abapGit may only be installed in a local package on the development system. Events are redirected to the
"! development system using RFC.
"! </p>
CLASS zcl_cilib_abapgit_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_abapgit_api.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_abapgit_api IMPLEMENTATION.
ENDCLASS.
