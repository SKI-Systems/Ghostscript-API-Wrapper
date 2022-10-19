{******************************************************************************}
{                                                                              }
{       Ghostscript API Wrapper: An extended Ghostscript API for Delphi        }
{       to simplify use of Ghostscript.                                        }
{                                                                              }
{       Copyright (c) 2021-2022 (Ski-Systems)                                  }
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

// Wrapper unit for gserrors.h
unit SkiSys.GS_Errors;

interface

(*** Ghostscript description of gserrors.h

  A procedure that may return an error always returns
  a non-negative value (zero, unless otherwise noted) for success,
  or negative for failure.
  We don't use a typedef internally to avoid a lot of casting.
*)

type
gs_error_type = (
  gs_error_ok = 0,
  gs_error_unknownerror = -1,	// unknown error
  gs_error_dictfull = -2,
  gs_error_dictstackoverflow = -3,
  gs_error_dictstackunderflow = -4,
  gs_error_execstackoverflow = -5,
  gs_error_interrupt = -6,
  gs_error_invalidaccess = -7,
  gs_error_invalidexit = -8,
  gs_error_invalidfileaccess = -9,
  gs_error_invalidfont = -10,
  gs_error_invalidrestore = -11,
  gs_error_ioerror = -12,
  gs_error_limitcheck = -13,
  gs_error_nocurrentpoint = -14,
  gs_error_rangecheck = -15,
  gs_error_stackoverflow = -16,
  gs_error_stackunderflow = -17,
  gs_error_syntaxerror = -18,
  gs_error_timeout = -19,
  gs_error_typecheck = -20,
  gs_error_undefined = -21,
  gs_error_undefinedfilename = -22,
  gs_error_undefinedresult = -23,
  gs_error_unmatchedmark = -24,
  gs_error_VMerror = -25,		// must be the last Level 1 error

(*** ------ Additional Level 2 errors (also in DPS, ------ ***)

  gs_error_configurationerror = -26,
  gs_error_undefinedresource = -27,

  gs_error_unregistered = -28,
  gs_error_invalidcontext = -29,
  /// <summary>
  ///  invalidid is for the NeXT DPS extension.
  /// </summary>
  gs_error_invalidid = -30,

  /// <summary>
  ///  We need a specific stackoverflow error for the PDF interpreter to avoid dropping into
  ///  the Postscript interpreter's stack extending code, when the PDF interpreter is called from
  ///  Postscript
  /// </summary>
  gs_error_pdf_stackoverflow = -31,

  /// <summary>
  /// Internal error for the C-based PDF interpreter, to indicate a circular PDF reference */
  /// </summary>
  gs_error_circular_reference = -32,

(*** ------ Pseudo-errors used internally ------ ***)

  gs_error_hit_detected = -99,

  gs_error_Fatal = -100,

  /// <summary>
  ///  Internal code for the .quit operator.
  ///  The real quit code is an integer on the operand stack.
  ///  gs_interpret returns this only for a .quit with a zero exit code.
  /// </summary>
  gs_error_Quit = -101,

  /// <sumary>
  ///  Internal code for a normal exit from the interpreter.
  ///  Do not use outside of interp.c.
  /// </summary>
  gs_error_InterpreterExit = -102,

  /// <summary>
  ///  Need the remap color error for high level pattern support *)
  /// </summary>
  gs_error_Remap_Color = -103,

  /// <summary>
  ///  Internal code to indicate we have underflowed the top block
  ///  of the e-stack.
  /// </summary>
  gs_error_ExecStackUnderflow = -104,

  /// <summary>
  ///  Internal code for the vmreclaim operator with a positive operand.
  ///  We need to handle this as an error because otherwise the interpreter
  ///  won't reload enough of its state when the operator returns.
  /// </summary>
  gs_error_VMreclaim = -105,

  /// <summary>
  ///  Internal code for requesting more input from run_string.
  /// </summary>
  gs_error_NeedInput = -106,

  /// <summary>
  ///  Internal code to all run_string to request that the data is rerun
  ///  using run_file.
  /// </summary>
  gs_error_NeedFile = -107,

  /// <summary>
  ///  Internal code for a normal exit when usage info is displayed.
  ///  This allows Window versions of Ghostscript to pause until
  ///  the message can be read.
  /// </summary>
  gs_error_Info = -110,

  /// <summary>
  ///  A special 'error', like reamp color above. This is used by a subclassing
  ///  device to indicate that it has fully processed a device method, and parent
  ///  subclasses should not perform any further action. Currently this is limited
  ///  to compositor creation.
  /// </summary>
  gs_error_handled = -111
);


implementation

end.
