{******************************************************************************}
{                                                                              }
{       Ghostscript API Wrapper: An extended Ghostscript API for Delphi        }
{       to simplify use of Ghostscript.                                        }
{                                                                              }
{       Copyright (c) 2021-2022 (SKI-Systems)                                  }
{       Author: Jan Blumstengel                                                }
{                                                                              }
{       https://github.com/SKI-Systems/Ghostscript-API-Wrapper                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{    This program is free software: you can redistribute it and/or modify      }
{    it under the terms of the GNU Affero General Public License as            }
{    published by the Free Software Foundation, either version 3 of the        }
{    License, or (at your option) any later version.                           }
{                                                                              }
{    This program is distributed in the hope that it will be useful,           }
{    but WITHOUT ANY WARRANTY; without even the implied warranty of            }
{    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             }
{    GNU Affero General Public License for more details.                       }
{                                                                              }
{    You should have received a copy of the GNU Affero General Public License  }
{    along with this program.  If not, see <https://www.gnu.org/licenses/>.    }
{                                                                              }
{******************************************************************************}

//  Wrapper unit for the iapi.h file
unit SkiSys.GS_Dll;

{$IFDEF FPC} // Free Pascal
  {$MODE DELPHI}
  {$H+} //use AnsiStrings in Free Pascal
{$ELSE} //Delphi
  {$DEFINE DELPHI}
{$ENDIF}

interface

// don't show platform warnings, because no other support is added atm
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses
  SkiSys.GS_Types, SkiSys.GS_gdevdsp
{$IFDEF FPC}
  , Classes, SysUtils, Windows
{$ENDIF}
{$IFDEF DELPHI}
  , System.Classes, System.SysUtils, WinApi.Windows
{$ENDIF}
  ;

  // C declaration is 4 byte long we need it for the DLL implementation
  {$MINENUMSIZE 4}


  // DLL Methods - declared as delayed for later use in Delphi
  // FPC doesn't support this feature atm.

{$IFNDEF display_callback_DEFINED}
// declared to avoid errors from an deprecated methode
type
  display_callback = Pointer;
{$ENDIF}

  /// <summary>
  ///  Get version numbers and strings.
  ///  This is safe to call at any time.
  ///  You should call this first to make sure that the correct version
  ///  of the Ghostscript is being used.
  ///  pr is a pointer to a revision structure.
  ///  len is the size of this structure in bytes.
  ///  Returns 0 if OK, or if len too small (additional parameters
  ///  have been added to the structure) it will return the required
  ///  size of the structure.
  /// </summary>
  function gsapi_revision(pr: p_gsapi_revision_t; len: Integer): Integer;
                          stdcall; external GS_DLL
                          {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Create a new instance of Ghostscript.
  ///  This instance is passed to most other API functions.
  ///  The caller_handle will be provided to callback functions.
  /// </summary>
  function gsapi_new_instance(out pinstance: Pointer;
                              caller_handle: Pointer): Integer;
                              stdcall; external GS_DLL
                              {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Destroy an instance of Ghostscript
  ///  Before you call this, Ghostscript must have finished.
  ///  If Ghostscript has been initialised, you must call gsapi_exit()
  ///  before gsapi_delete_instance.
  /// </summary>
  procedure gsapi_delete_instance(instance: Pointer);
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Set the callback functions for stdio
  ///  The stdin callback function should return the number of
  ///  characters read, 0 for EOF, or -1 for error.
  ///  The stdout and stderr callback functions should return
  ///  the number of characters written.
  ///  If a callback address is NULL, the real stdio will be used.
  /// </summary>
  function gsapi_set_stdio(instance: Pointer; stdin_fn: stdin_fn_t;
                           stdout: stdout_fn_t; stderr: stderr_fn_t): Integer;
                           stdcall; external GS_DLL
                           {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Does the same as the above, but using the caller_handle given here,
  ///  rather than the default one specified at gsapi_new_instance time.
  /// </summary>
  function gsapi_set_stdio_with_handle(instance: Pointer; stdin_fn: stdin_fn_t;
                                       stdout: stdout_fn_t; stderr: stderr_fn_t;
                                       caller_handle: Pointer): Integer;
                                       stdcall; external GS_DLL
                                       {$IFDEF DELPHI}delayed{$ENDIF};


  /// <summary>
  ///  Set the callback function for polling.
  ///  This is used for handling window events or cooperative
  ///  multitasking.  This function will only be called if
  ///  Ghostscript was compiled with CHECK_INTERRUPTS
  ///  as described in gpcheck.h.
  ///  The polling function should return 0 if all is well,
  ///  and negative if it wants ghostscript to abort.
  ///  The polling function must be fast.
  /// </summary>
  function gsapi_set_poll(instance: Pointer; poll_fn: poll_fn_t): Integer;
                          stdcall; external GS_DLL
                          {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Does the same as the above, but using the caller_handle given here,
  ///  rather than the default one specified at gsapi_new_instance time.
  /// </summary>
  function gsapi_set_poll_with_handle(instance: Pointer; poll_fn: poll_fn_t;
                                      caller_handle: Pointer): Integer;
                                      stdcall; external GS_DLL
                                      {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Set the display device callback structure.
  ///  If the display device is used, this must be called
  ///  after gsapi_new_instance() and before gsapi_init_with_args().
  ///  See gdevdisp.h for more details.
  ///  DEPRECATED: Use the gsapi_register_callback mechanism instead.
  /// </summary>
  function gsapi_set_display_callback(instance: Pointer;
                                      callback: pdisplay_callback): Integer;
                                      stdcall; external GS_DLL
                                      {$IFDEF DELPHI}delayed{$ENDIF};
           deprecated 'Use the gsapi_register_callout mechanism instead.';

  /// <summary>
  ///  Register a handler for gs callouts.
  ///  This must be called after gsapi_new_instance() and (typically)
  ///  before gsapi_init_with_args().
  /// </summary>
  function gsapi_register_callout(instance: Pointer; callout: gs_callout;
                                  callout_handle: Pointer): Integer;
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Deregister a handler for gs callouts.
  /// </summary>
  procedure gsapi_deregister_callout(instance: Pointer; callout: gs_callout;
                                     callout_handle: Pointer);
                                     stdcall; external GS_DLL
                                     {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Set the string containing the list of default device names
  ///  for example "display x11alpha x11 bbox". Allows the calling
  ///  application to influence which device(s) gs will try in order
  ///  to select the default device
  ///  *Must* be called after gsapi_new_instance() and before
  ///  gsapi_init_with_args().
  /// </summary>
  function gsapi_set_default_device_list(instance: Pointer;
                                         list: PAnsiChar; listlen: Integer): Integer;
                                         stdcall; external GS_DLL
                                         {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Returns a pointer to the current default device string
  ///  The list of devices is seperated with spaces.
  ///  *Must* be called after gsapi_new_instance().
  /// </summary>
  function gsapi_get_default_device_list(instance: Pointer;
                                         list: PList;
                                         out listlen: Integer): Integer;
                                         stdcall; external GS_DLL
                                         {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Set the encoding used for the args. By default we assume
  ///  'local' encoding. For windows this equates to whatever the current
  ///  codepage is. For linux this is utf8.
  ///  Use of this API (gsapi) with 'local' encodings (and hence without calling
  ///  this function) is now deprecated!
  /// </summary>
  function gsapi_set_arg_encoding(instance: Pointer; encoding: Integer): Integer;
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};


  /// <summary>
  ///  Initialise the interpreter.
  ///  This calls gs_main_init_with_args() in imainarg.c <para/>
  ///  1. If quit or EOF occur during gsapi_init_with_args(),
  ///     the return value will be gs_error_Quit.  This is not an error.
  ///     You must call gsapi_exit() and must not call any other
  ///     gsapi_XXX functions. <para/>
  ///  2. If usage info should be displayed, the return value will be gs_error_Info
  ///     which is not an error.  Do not call gsapi_exit(). <para/>
  ///  3. Under normal conditions this returns 0.  You would then
  ///     call one or more gsapi_run_*() functions and then finish
  ///     with gsapi_exit(). <para/>
  /// </summary>
  function gsapi_init_with_args(instance: Pointer; argc: Integer; argv: PArgv): Integer;
                                stdcall; external GS_DLL
                                {$IFDEF DELPHI}delayed{$ENDIF};

  {$IF Defined(MSWindows)}
  function gsapi_init_with_argsA(instance: Pointer; argc: Integer; argv: PArgv): Integer;
                                 stdcall; external GS_DLL
                                 {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_init_with_argsW(instance: Pointer; argc: Integer; argv: PArgv): Integer;
                                 stdcall; external GS_DLL
                                 {$IFDEF DELPHI}delayed{$ENDIF};
  {$ENDIF}

  /// <summary>
  ///  The gsapi_run_* functions are like gs_main_run_* except
  ///  that the error_object is omitted.
  ///  If these functions return <= -100, either quit or a fatal
  ///  error has occured.  You then call gsapi_exit() next.
  ///  The only exception is gsapi_run_string_continue()
  ///  which will return gs_error_NeedInput if all is well.
  /// </summary>

  function gsapi_run_string_begin(instance: Pointer;
                                  user_errors: Integer;
                                  out pexit_code: Integer): Integer;
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_string_continue(instance: Pointer;
                                     const str: PAnsiChar; length: UInt;
                                     user_errors: Integer;
                                     out pexit_code: Integer): Integer;
                                     stdcall; external GS_DLL
                                     {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_string_end(instance: Pointer;
                                user_errors: Integer; out pexit_code: Integer): Integer;
                                stdcall; external GS_DLL
                                {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_string_with_length(instance: Pointer;
                                        const str: PAnsiChar; length: UInt;
                                        user_errors: Integer;
                                        out pexit_code: Integer): Integer;
                                        stdcall; external GS_DLL
                                        {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_string(instance: Pointer;
                            const str: PAnsiChar; user_errors: Integer;
                            out pexit_code: Integer): Integer;
                            stdcall; external GS_DLL
                            {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_file(instance: Pointer;
                          const file_name: PAnsiChar;
                          user_errors: Integer; out pexit_code: Integer): Integer;
                          stdcall; external GS_DLL
                          {$IFDEF DELPHI}delayed{$ENDIF};

  {$ifdef __WIN32__}
  function gsapi_run_fileA(instance: Pointer;
                          const file_name: PAnsiChar;
                          user_errors: Integer; out pexit_code: Integer): Integer;
                          stdcall; external GS_DLL
                          {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_run_fileW(instance: Pointer;
                          const file_name: PAnsiChar;
                          user_errors: Integer; out pexit_code: Integer): Integer;
                          stdcall; external GS_DLL
                          {$IFDEF DELPHI}delayed{$ENDIF};
  {$endif}

  /// <summary>
  ///  Exit the interpreter.
  ///  This must be called on shutdown if gsapi_init_with_args()
  ///  has been called, and just before gsapi_delete_instance().
  /// </summary>
  function gsapi_exit(instance: Pointer): Integer;
                      stdcall; external GS_DLL {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  gs_spt_parsed allows for a string such as "<< /Foo 0 /Bar true >>" or
  ///  "[ 1 2 3 ]" etc to be used so more complex parameters can be set. ///
  /// </summary>
  function gsapi_set_param(instance: Pointer;
                           const param: PAnsiChar; const value: Pointer;
                           atype: gs_set_param_type): Integer;
                           stdcall; external GS_DLL
                           {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Called to get a value. value points to storage of the appropriate
  ///  type. If value is passed as NULL on entry, then the return code is
  ///  the number of bytes storage required for the type. Thus to read a
  ///  name/string/parsed value, call once with value=NULL, then obtain
  ///  the storage, and call again with value=the storage to get a nul
  ///  terminated string. (nul terminator is included in the count - hence
  ///  an empty string requires 1 byte storage). Returns gs_error_undefined
  ///  (-21) if not found. ///
  /// </summary>
  function gsapi_get_param(instance: Pointer;
                           const param: PAnsiChar; value: Pointer;
                           atype: gs_set_param_type): Integer;
                           stdcall; external GS_DLL
                           {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Enumerator to list all the parameters.
  ///  Caller defines void *iter = NULL, and calls with &iter.
  ///  Each call, iter is updated to reflect the position within the
  ///  enumeration, so passing iterator back in gets the next key. The call
  ///  returns negative values for errors, 0 for success, and 1 for "no more
  ///  keys".
  /// </summary>
  ///   void *iter = NULL;
  ///   gs_set_param_type type;
  ///   const char *key;
  ///   int code;
  ///   while ((code = gsapi_enumerate_params(inst, &iter, &key, &type)) == 0) {
  ///       // Process key
  ///   }

  /// <summary>
  ///  Note that the ordering of enumerations is NOT defined. key is valid
  ///  until the next call to gsapi_enumerate_params. Only one enumeration
  ///  at a time (starting a new enumeration will invalidate any previous
  ///  enumeration).
  /// </summary>
  function gsapi_enumerate_params(instance: Pointer;
                                  iterator: array of Pointer;
                                  const key: array of PAnsiChar;
                                  atype: gs_set_param_type): Integer;
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Add a path to one of the sets of permitted paths.
  /// </summary>
  function gsapi_add_control_path(instance: Pointer;
                                  atype: Integer; const path: PAnsiChar): Integer;
                                  stdcall; external GS_DLL
                                  {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Remove a path from one of the sets of permitted paths.
  /// </summary>
  function gsapi_remove_control_path(instance: Pointer;
                                     atype: Integer; const path: PAnsiChar): Integer;
                                     stdcall; external GS_DLL
                                     {$IFDEF DELPHI}delayed{$ENDIF};

  /// <summary>
  ///  Purge all the paths from the one of the sets of permitted paths.
  /// </summary>
  procedure gsapi_purge_control_paths(instance: Pointer; atype: Integer);
                                      stdcall; external GS_DLL
                                      {$IFDEF DELPHI}delayed{$ENDIF};

  procedure gsapi_activate_path_control(instance: Pointer; enable: Integer);
                                        stdcall; external GS_DLL
                                        {$IFDEF DELPHI}delayed{$ENDIF};

  function gsapi_is_path_control_active(instance: Pointer): Integer;
                                        stdcall; external GS_DLL
                                        {$IFDEF DELPHI}delayed{$ENDIF};

  //GSDLLEXPORT int GSDLLAPI
  //gsapi_add_fs(void *instance, gsapi_fs_t *fs, void *secret);

  //GSDLLEXPORT void GSDLLAPI
  //gsapi_remove_fs(void *instance, gsapi_fs_t *fs, void *secret);

  (* function prototypes  *)
  //typedef int (GSDLLAPIPTR PFN_gsapi_revision)(
  //    gsapi_revision_t *pr, int len);
  //typedef int (GSDLLAPIPTR PFN_gsapi_new_instance)(
  //    void **pinstance, void *caller_handle);
  //typedef void (GSDLLAPIPTR PFN_gsapi_delete_instance)(
  //    void *instance);
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_stdio)(void *instance,
  //    int (GSDLLCALLPTR stdin_fn)(void *caller_handle, char *buf, int len),
  //    int (GSDLLCALLPTR stdout_fn)(void *caller_handle, const char *str, int len),
  //    int (GSDLLCALLPTR stderr_fn)(void *caller_handle, const char *str, int len));
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_poll)(void *instance,
  //    int(GSDLLCALLPTR poll_fn)(void *caller_handle));
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_display_callback)(
  //    void *instance, display_callback *callback);
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_default_device_list)(
  //    void *instance, char *list, int listlen);
  //typedef int (GSDLLAPIPTR PFN_gsapi_get_default_device_list)(
  //    void *instance, char **list, int *listlen);
  //typedef int (GSDLLAPIPTR PFN_gsapi_init_with_args)(
  //    void *instance, int argc, char **argv);
  {$ifdef __WIN32__}
  //typedef int (GSDLLAPIPTR PFN_gsapi_init_with_argsA)(
  //    void *instance, int argc, char **argv);
  //typedef int (GSDLLAPIPTR PFN_gsapi_init_with_argsW)(
  //    void *instance, int argc, wchar_t **argv);
  {$endif}
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_arg_encoding)(
  //    void *instance, int encoding);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_string_begin)(
  //    void *instance, int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_string_continue)(
  //    void *instance, const char *str, unsigned int length,
  //    int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_string_end)(
  //    void *instance, int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_string_with_length)(
  //    void *instance, const char *str, unsigned int length,
  //    int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_string)(
  //    void *instance, const char *str,
  //    int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_file)(void *instance,
  //    const char *file_name, int user_errors, int *pexit_code);
  {$ifdef __WIN32__}
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_fileA)(void *instance,
  //    const char *file_name, int user_errors, int *pexit_code);
  //typedef int (GSDLLAPIPTR PFN_gsapi_run_fileW)(void *instance,
  //    const wchar_t *file_name, int user_errors, int *pexit_code);
  {$endif}
  //typedef int (GSDLLAPIPTR PFN_gsapi_exit)(void *instance);
  //typedef int (GSDLLAPIPTR PFN_gsapi_set_param)(void *instance, const char *param, const void *value, gs_set_param_type type);

  //typedef int (GSDLLAPIPTR PFN_gsapi_add_control_path)(void *instance, int type, const char *path);
  //typedef int (GSDLLAPIPTR PFN_gsapi_remove_control_path)(void *instance, int type, const char *path);
  //typedef void (GSDLLAPIPTR PFN_gsapi_purge_control_paths)(void *instance, int type);
  //typedef void (GSDLLAPIPTR PFN_gsapi_activate_path_control)(void *instance, int enable);
  //typedef int (GSDLLAPIPTR PFN_gsapi_is_path_control_active)(void *instance);
  //typedef int (GSDLLAPIPTR PFN_gsapi_add_fs)(void *instance, gsapi_fs_t *fs, void *secret);
  //typedef void (GSDLLAPIPTR PFN_gsapi_remove_fs)(void *instance, gsapi_fs_t *fs, void *secret);


implementation

end.
