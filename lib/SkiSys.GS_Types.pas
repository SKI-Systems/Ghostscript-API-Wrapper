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

unit SkiSys.GS_Types;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}
{$ELSE} //Delphi
  {$DEFINE DELPHI}
{$ENDIF}

interface


// C declaration is 4 byte long we need it for the DLL implementation
{$MINENUMSIZE 4}

const
{$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
    GS_EXE = 'gswin32.exe';
    GS_CMD_EXE = 'gswin32c.exe';
    GS_DLL = 'gsdll32.dll';
  {$ELSE}
    GS_EXE = 'gswin64.exe';
    GS_CMD_EXE = 'gswin64c.exe';
    GS_DLL = 'gsdll64.dll';
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  GS_EXE = 'gs';
  GS_CMD_EXE = 'gsc';
  GS_DLL = 'libgs.so';
{$ENDIF}


type
  // We need to define a clear pointer and string type in the right format
  // for all platforms and enviroments and other data types as well
  {$IFDEF FPC}
    //CUTF8String = UTF8String;
    UShort = Word;
  {$ENDIF}
  {$IFDEF DELPHI}
    //CUTF8String = AnsiString; // check if we could use UTF8String as well
    SizeInt = NativeInt; //platform compatible integer (64bit Int64 ...)
  {$ENDIF}

  PArgv = array of PAnsiChar;
  PList = ^PArgv;

  p_gsapi_revision_t = ^gsapi_revision_t;
  gsapi_revision_t = record
    product: PAnsiChar;
    copyright: PAnsiChar;
    revision: {$IFDEF UNIX}SizeInt{$ELSE}LongInt{$ENDIF};
    revisiondate: {$IFDEF UNIX}SizeInt{$ELSE}LongInt{$ENDIF};
  end;


  stdin_fn_t = function(caller_handle: Pointer; buf: PAnsiChar; len: Integer)
                        : Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  stdout_fn_t = function(caller_handle: Pointer; const str: PAnsiChar; len: Integer)
                         : Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  stderr_fn_t = function(caller_handle: Pointer; const str: PAnsiChar; len: Integer)
                         : Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  poll_fn_t = function(caller_handle: Pointer)
                       : Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  /// <summary>
  ///  The callout mechanism allows devices to query "callers" (users of the
  ///  DLL) in device specific ways. The callout function pointer type will
  ///  be called with: <para/>
  ///    callout_handle = the value given at registration <para/>
  ///    device_name    = the name of the current device <para/>
  ///    id             = An integer, guaranteed to be unique within the
  ///                     callouts from a given device, identifying the
  ///                     purpose of this call. <para/>
  ///    size           = device/id specific, but typically the size of 'data'. <para/>
  ///    data           = device/id specific, but typically the pointer to
  ///                     an in/out data block. <para/>
  ///   Returns an error code (gs_error_unknownerror (-1) if unclaimed,
  ///   non-negative on success, standard gs error numbers recommended).
  /// </summary>
  gs_callout = function(instance: Pointer; callout_handle: Pointer;
                        const device_name: PAnsiChar;
                        id, size: Integer; data: Pointer): Integer;
                        {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  /// <summary>
  ///  GS Argument encoding
  /// </summary>
  GS_ARG_ENCODING = (
      GS_ARG_ENCODING_LOCAL = 0,
      GS_ARG_ENCODING_UTF8 = 1,
      GS_ARG_ENCODING_UTF16LE = 2
  );

  /// <summary>
  ///  Setting a typed param causes it to be instantly fed to to the
  ///  device. This can cause the device to reinitialise itself. Hence,
  ///  setting a sequence of typed params can cause the device to reset
  ///  itself several times. Accordingly, if you OR the type with
  ///  gs_spt_more_to_come_bit, the param will held ready to be passed into
  ///  the device, and will only actually be sent when the next typed
  ///  param is set without this flag (or on device init). Not valid
  ///  for get_typed_param. <para/>
  ///  Set the bit 31 for more informations coming to avoid this issue
  /// </summary>
  gs_set_param_type = (
      gs_spt_invalid = -1,
      gs_spt_null    = 0,   //  void * is NULL
      gs_spt_bool    = 1,   //  void * is a pointer to an int (0 false,
                            //  non-zero true).
      gs_spt_int     = 2,   //  void * is a pointer to an int
      gs_spt_float   = 3,   //  void * is a float*
      gs_spt_name    = 4,   //  void * is a char*
      gs_spt_string  = 5,   //  void * is a char*
      gs_spt_long    = 6,   //  void * is a long*
      gs_spt_i64     = 7,   //  void * is an int64_t*
      gs_spt_size_t  = 8,   //  void * is a size_t*
      gs_spt_parsed  = 9,   //  void * is a pointer to a char * to be parsed
      gs_spt_more_to_come_bit = 1 shl 31
  );

  GS_PERMIT = (
    GS_PERMIT_FILE_READING = 0,
    GS_PERMIT_FILE_WRITING = 1,
    GS_PERMIT_FILE_CONTROL = 2
  );

  /// <summary>
  ///  Details of gp_file can be found in gp.h.
  ///  Users wanting to use this function should include
  ///  that file. Not included here to avoid bloating the
  ///  API inclusions for the majority of people who won't
  ///  want it.
  /// </summary>
  {$ifndef gp_file_name_sizeof}
    {$define gp_file_name_sizeof 4096}
  {$endif}

  // SkiSys.GS_Api
  // at the moment is gp_file.h not included in the API

  //typedef struct
  //{
  //    int (*open_file)(const gs_memory_t *mem,
  //                           void        *secret,
  //                     const char        *fname,
  //                     const char        *mode,
  //                           gp_file    **file);
  //    int (*open_pipe)(const gs_memory_t *mem,
  //                           void        *secret,
  //                     const char        *fname,
  //                           char        *rfname, ///  4096 bytes ///
  //                     const char        *mode,
  //                           gp_file    **file);
  //    int (*open_scratch)(const gs_memory_t *mem,
  //                              void        *secret,
  //                        const char        *prefix,
  //                              char        *rfname, ///  4096 bytes ///
  //                        const char        *mode,
  //                              int          rm,
  //                              gp_file    **file);
  //    int (*open_printer)(const gs_memory_t *mem,
  //                              void        *secret,
  //                              char        *fname, ///  4096 bytes ///
  //                              int          binary,
  //                              gp_file    **file);
  //    int (*open_handle)(const gs_memory_t *mem,
  //                             void        *secret,
  //                             char        *fname, ///  4096 bytes ///
  //                       const char        *mode,
  //                             gp_file    **file);
  //} gsapi_fs_t;

  //TODO: implement the other header files for this
  gs_memory_t = Pointer;
  gp_file = Pointer;

  open_file = function(const mem: gs_memory_t;
                       secret: Pointer;
                       const fname: PAnsiChar;
                       const mode: PansiChar;
                       afile: gp_file): Integer; stdcall;


implementation

end.
