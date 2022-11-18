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

unit SkiSys.GS_ParameterConst;
// gs params shown on this page: https://www.ghostscript.com/doc/current/Use.htm

{$IFNDEF FPC}
  {$DEFINE DELPHI}
{$ENDIF}

interface

uses
  SkiSys.GS_gdevdsp, SysUtils;

const
  // description of the parameters for ghostscript
  // https://ghostscript.readthedocs.io/en/latest/Use.html


  PDFACompatibilityPolicyFmt = 'dPDFACompatibilityPolicy=%d';

  FileDelim = ';';

type
  TDEVICES_HIGH_LEVEL = (
    pdfwrite, ps2write, eps2write, txtwrite, xpswrite, pxlmono, pxlcolor, docxwrite
  );

  TDEVICES_PRINTERS = (
    MSWindowsPrinter
  );

const
  DEVICES_HIGH_LEVEL: array [TDEVICES_HIGH_LEVEL] of string = (
    'pdfwrite', 'ps2write', 'eps2write', 'txtwrite', 'xpswrite', 'pxlmono',
    'pxlcolor', 'docxwrite');

  DEVICES_PRINTERS: array [TDEVICES_PRINTERS] of string = (
    'mswinpr2'
  );

  DISPLAY_DEVICE_NAME = 'display';

  DISPLAY_BYTE_ORIENTATION =
    {$IFDEF MSWINDOWS}DISPLAY_BOTTOMFIRST{$ELSE}DISPLAY_TOPFIRST{$ENDIF};
  DISPLAY_ENDIAN_OS =
    {$IFDEF MACOS}DISPLAY_BIGENDIAN{$ELSE}DISPLAY_LITTLEENDIAN{$ENDIF};


type
  /// <summary>  <param/>
  ///  PDFA_NOT_COMPLIANT (0) - (default)
  ///      Include the feature or operation in the output file, the file will not be
  ///      PDF/A compliant. Because the document Catalog is emitted before this is encountered,
  ///      the file will still contain PDF/A metadata but will not be compliant.
  ///      A warning will be emitted in this case. <param/>
  ///  PDFA_IGNORE_ERRORS (1) -
  ///      The feature or operation is ignored, the resulting PDF file will be PDF/A compliant.
  ///      A warning will be emitted for every elided feature. <param/>
  ///  PDFA_PROCESS_STOP_ONERROR (2) -
  ///      Processing of the file is aborted with an error, the exact error may vary depending
  ///      on the nature of the PDF/A incompatibility. <param/>
  /// </summary>
  TGSPDFACompatibilityPolicy = (
    PDFA_NOT_COMPLIANT = 0,
    PDFA_IGNORE_ERRORS = 1,
    PDFA_PROCESS_STOP_ONERROR = 2
  );

  TGSColorConversionStrategy = (ccsNone, ccsGray, ccsUnchanged, ccsRGB, ccsCMYK,
    ccsUseDeviceIndependentColor);

{$REGION 'DEBUG Parameters'}
  TDebugParameter = (
    dparCompiledFonts, dparCffFonts, dparCMap, dparCIEColor, dparEpsHandling,
    dparFontApi, dparInitialization, dparPdfInterpreter, dparPdfWriter,
    dparSetPageDevice, dparStaticResource, dparTTFFonts, dparViewGIF, dparViewJPEG);

  TDebugParamSet = set of TDebugParameter;

const
  //-dCCFONTDEBUG	Compiled Fonts
  DEBUG_COMPILED_FONTS = '-dCCFONTDEBUG';
  //-dCFFDEBUG	CFF Fonts
  DEBUG_CFF_FONTS = '-dCFFDEBUG';
  //-dCMAPDEBUG	CMAP
  DEBUG_CMAP = '-dCMAPDEBUG';
  //-dDOCIEDEBUG	CIE color
  DEBUG_CIE_COLOR = '-dDOCIEDEBUG';
  //-dEPSDEBUG	EPS handling
  DEBUG_EPS_HANDLING = '-dEPSDEBUG';
  //-dFAPIDEBUG	Font API
  DEBUG_FONT_API = '-dFAPIDEBUG';
  //-dINITDEBUG	Initialization
  DEBUG_INITIALIZATION = '-dINITDEBUG';
  //-dPDFDEBUG	PDF Interpreter
  DEBUG_PDF_INTERPRETER = '-dPDFDEBUG';
  //-dPDFWRDEBUG	PDF Writer
  DEBUG_PDF_WRITER = '-dPDFWRDEBUG';
  //-dSETPDDEBUG	setpagedevice
  DEBUG_SETPAGEDEVICE = '-dSETPDDEBUG';
  //-dSTRESDEBUG	Static Resources
  DEBUG_STATIC_RESOURCE = '-dSTRESDEBUG';
  //-dTTFDEBUG	TTF Fonts
  DEBUG_TTF_FONTS = '-dTTFDEBUG';
  //-dVGIFDEBUG	ViewGIF
  DEBUG_VIEW_GIF = '-dVGIFDEBUG';
  //-dVJPGDEBUG	ViewJPEG
  DEBUG_VIEW_JPEG = '-dVJPGDEBUG';

{$ENDREGION}

implementation

end.
