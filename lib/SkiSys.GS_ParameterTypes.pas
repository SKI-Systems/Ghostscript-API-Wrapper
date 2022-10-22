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

/// <summary>
///  Declaration of some Ghostscript parameters and the documentation from
///  the Ghsotscript Page
/// </summary>
unit SkiSys.GS_ParameterTypes;

interface

uses
  SkiSys.GS_ParameterConst, SkiSys.GS_gdevdsp,
  System.Classes, System.SysUtils, WinApi.Windows, Vcl.Graphics;

type
  /// <summary>
  ///  Default abstract class for Ghostscript parameters
  /// </summary>
  TGSParams = class
  protected
    FBatch: Boolean;
    FDevice: string;
    FDDpi: Integer;
    FDisplayFormat: Integer;
    FGenericResourceDir: string;
    FNoSaver: Boolean;
    FNoPause: Boolean;
    FQuiet: Boolean;
    FSources: string;
    /// <summary>
    ///  Read the display format and the resolution from the device context of
    ///  the desktop, when they are set to default 0
    /// </summary>
    procedure GetDisplayFormat; virtual;
    /// <summary>
    ///  Set the default values
    /// </summary>
    procedure SetDefaultValues; virtual;
    /// <summary>
    ///  Used to set the Params in the Ghostscript format
    /// </summary>
    procedure SetParameters(AParams: TStringList); virtual;
    /// <summary>
    ///  Search or Include path of other files like the ICCProfile path and Fonts
    /// </summary>
    property Sources: string read FSources;
  public (*** PROPERTIES ***)
    property Batch: Boolean read FBatch write FBatch;
    /// <summary>
    ///  Set the
    /// </summary>
    property DDpi: Integer read FDDpi write FDDpi;
    /// <summary>
    ///  The target device for the output
    /// </summary>
    property Device: string read FDevice write FDevice;
    /// <summary>
    ///
    /// </summary>
    property DisplayFormat: Integer read FDisplayFormat write FDisplayFormat;
    /// <summary>
    ///
    /// </summary>
    property NoSaver: Boolean read FNoSaver write FNoSaver;
    /// <summary>
    ///
    /// </summary>
    property NoPause: Boolean read FNoPause write FNoPause;
    /// <summary>
    ///
    /// </summary>
    property Quiet: Boolean read FQuiet write FQuiet;
  public (*** METHODS ***)
    /// <summary>
    ///
    /// </summary>
    procedure AddSourcePath(APath: string);
    // constructor
    constructor Create; reintroduce;
    /// <summary>
    ///  Get the full Linux file path to avoid Ghostscript Errors
    /// </summary>
    /// <returns>
    ///  a case sensitive Linux file name, when the file doesn't exist an
    ///  EFileNotFoundException will be raised
    /// </returns>
    function GetFullLinuxFilePath(AFile: string; IgnoreMatch: Boolean = False): string;
    /// <summary>
    ///  Replace all backslashes with slashes and remove all double backslashes
    ///  from the path
    /// </summary>
    function GetLinuxFilePath(AFile: string): string; virtual;
    procedure SetParam(Value, Default: Boolean; Name: string; AParams: TStringList); overload;
    procedure SetParam(Value, Default: string; Name: string; AParams: TStringList); overload;
    procedure SetParam(Value, Default: Integer; Name: string; AParams: TStringList); overload;
    procedure SetParams(AParams: TStringList); virtual; abstract;
  end;

  /// <summary>
  ///  Debug parameters for the Ghostscript library
  /// </summary>
  TGSDebugParams = class(TGSParams)
  private
    FCompiledFonts: Boolean;
    FCFFFonts: Boolean;
    FCMap: Boolean;
    FCIEColor: Boolean;
    FEPSHandling: Boolean;
    FFontApi: Boolean;
    FInit: Boolean;
    FPdfInterpreter: Boolean;
    FPdfWriter: Boolean;
    FSetPageDevice: Boolean;
    FStaticResource: Boolean;
    FTtfFonts: Boolean;
    FViewGIF: Boolean;
    FViewJPEG: Boolean;
    procedure SetDebugParams(const Value: TDebugParamSet);
  protected
    FDebugParms: TDebugParamSet;
    property Sources;
  public
    /// <summary>
    ///  A set of all Debug Parameters, to make it easier to set them
    /// </summary>
    property DebugParams: TDebugParamSet read FDebugParms write SetDebugParams;
    property CompiledFonts: Boolean read FCompiledFonts write FCompiledFonts;
    property CFFFonts: Boolean read FCFFFonts write FCFFFonts;
    property CMap: Boolean read FCMap write FCMap;
    property CIEColor: Boolean read FCIEColor write FCIEColor;
    property EPSHandling: Boolean read FEPSHandling write FEPSHandling;
    property FontApi: Boolean read FFontApi write FFontApi;
    property Init: Boolean read FInit write FInit;
    property PdfInterpreter: Boolean read FPdfInterpreter write FPdfInterpreter;
    property PdfWriter: Boolean read FPdfWriter write FPdfWriter;
    property SetPageDevice: Boolean read FSetPageDevice write FSetPageDevice;
    property StaticResource: Boolean read FStaticResource write FStaticResource;
    property TtfFonts: Boolean read FTtfFonts write FTtfFonts;
    property ViewGIF: Boolean read FViewGIF write FViewGIF;
    property ViewJPEG: Boolean read FViewJPEG write FViewJPEG;
    procedure SetParams(AParams: TStringList); override;
  end;

  /// <summary>
  ///  PDF parameters with the documentation from the Ghostscript page
  /// </summary>
  TPDFParams = class(TGSParams)
  private
    FColorConversionStrategy: TGSColorConversionStrategy;
    FEmbededFonts: Boolean;
    FNewPdf: Boolean;
    FPdfA: Boolean;
    FPdfInfo: Boolean;
    FPdfFitPage: Boolean;
    FPdfTitle: string;
    FPdfX: Boolean;
    FPrinted: Boolean;
    FUseBleedBox: Boolean;
    FUseTrimBox: Boolean;
    FUseArtBox: Boolean;
    FUseCropBox: Boolean;
    FPDFPassword: string;
    FShowAnnots: Boolean;
    FShowAcroForm: Boolean;
    FNoUserUnit: Boolean;
    FRenderTTNotDef: Boolean;
    FFirstPage: Integer;
    FLastPage: Integer;
    FPageList: string;
    FSubsetFonts: Boolean;
    procedure SetPdfA(const Value: Boolean);
    procedure SetPdfX(const Value: Boolean);
  protected
    function GetColorConversionStrategy: string; virtual;
    procedure SetDefaultValues; override;
  public
    /// <summary>
    ///
    /// </summary>
    property ColorConversionStrategy: TGSColorConversionStrategy read FColorConversionStrategy
                                                                 write FColorConversionStrategy;
    /// <summary>
    ///  Embed the fonts in the PDF
    /// </summary>
    property EmbededFonts: Boolean read FEmbededFonts write FEmbededFonts;
    /// <summary>
    ///  From release 9.55.0 Ghostscript incorporates two complete PDF interpreters;
    ///  the original long-standing interpreter is written in PostScript but there is
    ///  now a new interpreter written in C.
    ///  At present the old PostScript-based interpreter remains the default,
    ///  in future releases the new C-based interpreter will become the default,
    ///  though we would encourage people to experiment with the new interpreter and
    ///  send us feedback. While there are two interpreters the command-line switch NEWPDF
    ///  will allow selection of the existing interpreter when false and the new interpreter when true.
    /// </summary>
    property NewPdf: Boolean read FNewPdf write FNewPdf;
    /// <summary>
    ///  Set the output Format as PDF-X for the pdfwrite device
    /// </summary>
    property PdfA: Boolean read FPdfA write SetPdfA;
    /// <summary>
    ///  Starting with release 9.56.0 this new switch will work with the PDF
    ///  interpreter (GhostPDF) and with the PDF interpreter integrated into Ghostscript.
    ///  When this switch is set the interpreter will emit information regarding the file,
    ///  similar to that produced by the old pdf_info.ps program in the 'lib' folder.
    ///  The format is not entirely the same, and the search for fonts and spot
    ///  colours is 'deeper' than the old program; pdf_info.ps stops at the page
    ///  level whereas the PDFINFO switch will descend into objects such as Forms,
    ///  Images, type 3 fonts and Patterns. In addition different instances of fonts
    ///  with the same name are now enumerated.
    ///  Unlike the pdf_info.ps program there is no need to add the input file to the
    ///  list of permitted files for reading (using --permit-file-read).
    /// </summary>
    property PdfInfo: Boolean read FPdfInfo write FPdfInfo;
    /// <summary>
    ///  Rather than selecting a PageSize given by the PDF MediaBox, BleedBox (see -dUseBleedBox),
    ///  TrimBox (see -dUseTrimBox), ArtBox (see -dUseArtBox), or CropBox (see -dUseCropBox),
    ///  the PDF file will be scaled to fit the current device page size (usually the default page size).
    ///  This is useful for creating fixed size images of PDF files that may have a
    ///  variety of page sizes, for example thumbnail images.
    /// </summary>
    property PdfFitPage: Boolean read FPdfFitPage write FPdfFitPage;
    /// <summary>
    ///  Sets the PDF Title of the document, when the title isn't set
    /// </summary>
    property PdfTitle: string read FPdfTitle write FPdfTitle;
    /// <summary>
    ///  Set the output Format as PDF-X for the pdfwrite device
    /// </summary>
    property PdfX: Boolean read FPdfX write SetPdfX;
    // -dPrinted=false
    /// <summary>
    ///  Determines whether the file should be displayed or printed using the
    ///  "screen" or "printer" options for annotations and images.
    ///  With -dPrinted, the output will use the file's "print" options;
    ///  with -dPrinted=false, the output will use the file's "screen" options.
    ///  If neither of these is specified, the output will use the screen options
    ///  for any output device that doesn't have an OutputFile parameter, and the
    ///  printer options for devices that do have this parameter.
    /// </summary>
    property Printed: Boolean read FPrinted write FPrinted;
    /// <summary>
    ///  Sets the page size to the BleedBox rather than the MediaBox.
    ///  defines the region to which the contents of the page should be clipped
    ///  when output in a production environment. This may include any extra bleed
    ///  area needed to accommodate the physical limitations of cutting, folding,
    ///  and trimming equipment. The actual printed page may include printing marks
    ///  that fall outside the bleed box.
    /// </summary>
    property UseBleedBox: Boolean read FUseBleedBox write FUseBleedBox;
    /// <summary>
    ///  Sets the page size to the TrimBox rather than the MediaBox.
    ///  The trim box defines the intended dimensions of the finished page after trimming.
    ///  Some files have a TrimBox that is smaller than the MediaBox and may include white space,
    ///  registration or cutting marks outside the CropBox. Using this option
    ///  simulates appearance of the finished printed page.
    /// </summary>
    property UseTrimBox: Boolean read FUseTrimBox write FUseTrimBox;
    /// <summary>
    ///  Sets the page size to the ArtBox rather than the MediaBox.
    ///  The art box defines the extent of the page's meaningful content
    ///  (including potential white space) as intended by the page's creator.
    ///  The art box is likely to be the smallest box. It can be useful when one
    ///  wants to crop the page as much as possible without losing the content.
    /// </summary>
    property UseArtBox: Boolean read FUseArtBox write FUseArtBox;
    /// <summary>
    ///  Sets the page size to the CropBox rather than the MediaBox.
    ///  Unlike the other "page boundary" boxes, CropBox does not have a defined meaning,
    ///  it simply provides a rectangle to which the page contents will be clipped (cropped).
    ///  By convention, it is often, but not exclusively, used to aid the positioning
    ///  of content on the (usually larger, in these cases) media.
    /// </summary>
    property UseCropBox: Boolean read FUseCropBox write FUseCropBox;
    /// <summary>
    ///  Sets the user or owner password to be used in decoding encrypted PDF files.
    ///  For files created with encryption method 4 or earlier, the password is an
    ///  arbitrary string of bytes; with encryption method 5 or later, it should
    ///  be text in either UTF-8 or your locale's character set (Ghostscript tries both).
    /// </summary>
    property PDFPassword: string read FPDFPassword write FPDFPassword;
    /// <summary>
    ///  Don't enumerate annotations associated with the page Annots key. Annotations are shown by default.
    ///  In addition, finer control is available by defining an array /ShowAnnotTypes.
    ///  Annotation types listed in this array will be drawn, whilst those not listed will not be drawn.
    ///  To use this feature: -c "/ShowAnnotTypes [....] def" -f [input file]
    ///  Where the array can contain one or more of the following names:
    ///  /Stamp, /Squiggly, /Underline, /Link, /Text, /Highlight, /Ink, /FreeText, /StrikeOut and /stamp_dict.
    ///  For example, adding the follow to the command line: -c "/ShowAnnotTypes [/Text /UnderLine] def" -f [input file]
    ///  would draw only annotations with the subtypes "Text" and "UnderLine"
    /// </summary>
    property ShowAnnots: Boolean read FShowAnnots write FShowAnnots;
    /// <summary>
    ///  Don't show annotations from the Interactive Form Dictionary (AcroForm dictionary).
    ///  By default, AcroForm processing is now enabled because Adobe Acrobat does this.
    ///  This option is provided to restore the previous behavior which corresponded to older Acrobat.
    /// </summary>
    property ShowAcroForm: Boolean read FShowAcroForm write FShowAcroForm;
    /// <summary>
    ///  Ignore UserUnit parameter. This may be useful for backward compatibility
    ///  with old versions of Ghostscript and Adobe Acrobat, or for processing files
    ///  with large values of UserUnit that otherwise exceed implementation limits.
    /// </summary>
    property NoUserUnit: Boolean read FNoUserUnit write FNoUserUnit;
    /// <summary>
    ///  If a glyph is not present in a font the normal behaviour is to use
    ///  the /.notdef glyph instead. On TrueType fonts, this is often a hollow sqaure.
    ///  Under some conditions Acrobat does not do this, instead leaving a gap
    ///  equivalent to the width of the missing glyph, or the width of the /.notdef glyph
    ///  if no /Widths array is present. Ghostscript now attempts to mimic this
    ///  undocumented feature using a user parameter RenderTTNotdef. The PDF interpreter
    ///  sets this user parameter to the value of RENDERTTNOTDEF in systemdict,
    ///  when rendering PDF files. To restore rendering of /.notdef glyphs from
    ///  TrueType fonts in PDF files, set this parameter to true.
    /// </summary>
    property RenderTTNotDef: Boolean read FRenderTTNotDef write FRenderTTNotDef;
    /// <summary>
    ///  Begin on the designated page of the document. Pages of all documents in
    ///  PDF collections are numbered sequentionally.
    /// </summary>
    property FirstPage: Integer read FFirstPage write FFirstPage;
    /// <summary>
    ///  Stop after the designated page of the document. Pages of all documents in
    ///  PDF collections are numbered sequentionally.
    /// </summary>
    property LastPage: Integer read FLastPage write FLastPage;
    /// <summary>
    ///  There are three possible values for this; even, odd or a list of pages to
    ///  be processed. A list can include single pages or ranges of pages.
    ///  Ranges of pages use the minus sign '-', individual pages and ranges of
    ///  pages are separated by commas ','. A trailing minus '-' means process all remaining pages.
    ///  For example: <para/>
    ///  1,3,5 indicates that pages 1, 3 and 5 should be processed. <para/>
    ///  5-10 indicates that pages 5, 6, 7, 8, 9 and 10 should be processed. <para/>
    ///  1,5-10,12- indicates that pages 1, 5, 6, 7, 8, 9, 10 and 12 onwards should be processed. <para/>
    /// </summary>
    property PageList: string read FPageList write FPageList;
    property SubsetFonts: Boolean read FSubsetFonts write FSubsetFonts;
  public (*** PUBLIC METHODS ***)
    constructor Create;
    procedure SetParams(AParams: TStringList); override;
  end;

  /// <summary>
  ///  PDF Params and the PDF-A Params and PDF-X Params
  /// </summary>
  TPDFAXParams = class(TPDFParams)
  private
    FICCProfile: string;
    FPDFAProfile: Integer;
    FPDFACombatibilityPolicy: TGSPDFACompatibilityPolicy;
    FPDFAOutputConditionIdentifier: string;
    procedure SetICCProfile(const Value: string);
    procedure SetPDFAProfile(const Value: Integer);
  protected
    procedure SetDefaultValues; override;
  public
    /// <summary>
    ///  Specify the PDF/A Profile option to specify <para/>
    ///    1 - PDF/A-1 <para/>
    ///    2 - PDF/A-2 <para/>
    ///    3 - PDF/A-3 <para/>
    /// </summary>
    property PDFAProfile: Integer read FPDFAProfile write SetPDFAProfile;
    /// <summary>
    ///  When an operation (eg pdfmark) is encountered which cannot be emitted in a
    ///  PDF/A compliant file, this policy is consulted, there are currently three possible values.
    ///  -> see TGSPDFACompatibilityPolicy
    /// </summary>
    property PDFACombatibilityPolicy: TGSPDFACompatibilityPolicy
      read FPDFACombatibilityPolicy write FPDFACombatibilityPolicy;
    /// <summary>
    ///  Specifies with which color model a printer will print the file
    /// </summary>
    property PDFAOutputConditionIdentifier: string
      read FPDFAOutputConditionIdentifier write FPDFAOutputConditionIdentifier;
    /// <summary>
    ///  May be omitted if OutputConditionIdentifier specifies a registered identifier
    ///  of characterized printing condition (see http://www.color.org/IPA_2003-11_PDFX.pdf).
    ///  Defines a file name of an ICC profile file to be included into the output document.
    ///  You may specify either an absolute file name, or a relative path from the working directory.
    /// </summary>
    property ICCProfile: string read FICCProfile write SetICCProfile;
    procedure SetParams(AParams: TStringList); override;
  end;

implementation


{$REGION 'TGSParams' }

procedure TGSParams.AddSourcePath(APath: string);
var
  AIncludePath: string;
begin
  if (DirectoryExists(APath)) then
  begin
    AIncludePath := GetLinuxFilePath(APath);
    // all GS pathes has to end with a '/' or a '\'
    if (not AIncludePath.EndsWith('/')) then
      AIncludePath := AIncludePath + '/';
    if (not FSources.Contains(AIncludePath)) then
    begin
      if (FSources <> '') then
        FSources := FSources + ';';
      FSources := FSources + AIncludePath;
    end;
  end else
    raise EDirectoryNotFoundException.Create('AddSourcePath: directory not found - ' + APath);
end;

constructor TGSParams.Create;
begin
  inherited Create;
  SetDefaultValues;
end;

procedure TGSParams.GetDisplayFormat;
var
  DC: HDC;
  Depth: Integer;
begin
  FDisplayFormat := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
		        DISPLAY_DEPTH_1 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
  DC := GetDC(0);	//* get hdc for desktop */
  try
    depth := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
    if (FDDpi = 0) then
      FDDpi := GetDeviceCaps(DC, LOGPIXELSY);
    if (depth = 32) then
    begin
      FDisplayFormat := DISPLAY_COLORS_RGB or DISPLAY_UNUSED_LAST or
                DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
    end else
    if (depth = 16) then
    begin
      FDisplayFormat := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
                DISPLAY_DEPTH_16 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST or
                DISPLAY_NATIVE_555;
    end else
    if (depth > 8) then
    begin
      FDisplayFormat := DISPLAY_COLORS_RGB or DISPLAY_ALPHA_NONE or
                DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
    end else
    if (depth >= 8) then
    begin
      FDisplayFormat := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
                DISPLAY_DEPTH_8 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
    end else
    if (depth >= 4) then
      FDisplayFormat := DISPLAY_COLORS_NATIVE or DISPLAY_ALPHA_NONE or
                DISPLAY_DEPTH_4 or DISPLAY_LITTLEENDIAN or DISPLAY_BOTTOMFIRST;
  finally
    DeleteDC(DC);
  end;
end;

function TGSParams.GetFullLinuxFilePath(AFile: string; IgnoreMatch: Boolean): string;
var
  ACaseMatch: TFilenameCaseMatch;
begin
  Result := ExpandFileNameCase(AFile, ACaseMatch);
  if (ACaseMatch <> mkNone) or (IgnoreMatch) then
    Result := GetLinuxFilePath(ExpandFileName(AFile))
  else
    raise EFileNotFoundException.CreateFmt('file %s not found', [AFile]);
end;

function TGSParams.GetLinuxFilePath(AFile: string): string;
begin
  while (Result.Contains('\\')) do
    Result := Result.Replace('\\', '/');
  Result := Result.Replace('\', '/');
end;

procedure TGSParams.SetParam(Value, Default, Name: string;
  AParams: TStringList);
begin
  if (Value <> Default) then
    AParams.Add(Name + Value);
end;

procedure TGSParams.SetDefaultValues;
begin
  FBatch := True;
  FDDpi := 0;
  FDisplayFormat := 0;
  FNoPause := True;
  FQuiet := True;
end;

procedure TGSParams.SetParam(Value, Default: Integer; Name: string;
  AParams: TStringList);
begin
  if (Value <> Default) then
    AParams.Add(Name + IntToStr(Value));
end;

procedure TGSParams.SetParameters(AParams: TStringList);
begin
  SetParam(FDevice, '', '-sDEVICE=', AParams);
  // if a display device is choosen and the Display Format isn't set -> get it from the system
  if (LowerCase(FDevice) = DISPLAY_DEVICE_NAME) and (FDisplayFormat = 0) then
    GetDisplayFormat;
  SetParam(FDisplayFormat, 0, '-dDisplayFormat=', AParams);
  SetParam(FDDpi, 0, '-dDisplayResolution=', AParams);
  SetParam(FQuiet, False, '-q', AParams);
  SetParam(FBatch, False, '-dBATCH', AParams);
  SetParam(FNoSaver, False, '-dNOSAVER', AParams);
  SetParam(FNoPause, False, '-dNOPAUSE', AParams);
  if (FSources <> '') then
    AParams.Add('-I' + FSources);
end;

procedure TGSParams.SetParam(Value, Default: Boolean; Name: string;
  AParams: TStringList);
var
  Param: string;
begin
  if (Value <> Default) then
  begin
    Param := Name;
    if (Name.EndsWith('=')) then
      Param := Param + LowerCase(BoolToStr(Value, True));
    AParams.Add(Param);
  end;
end;

{$ENDREGION}

{$REGION 'TPDFParams' }

constructor TPDFParams.Create;
begin
  SetDefaultValues;
end;

function TPDFParams.GetColorConversionStrategy: string;
begin
  case ColorConversionStrategy of
    ccsUnchanged: Result := 'LeaveColorUnchanged';
    ccsRGB: Result := 'RGB';
    ccsCMYK: Result := 'CMYK';
    ccsGray: Result := 'Gray';
    ccsUseDeviceIndependentColor: Result := 'UseDeviceIndependentColor';
  end;
  if (PdfX) then
    if not (ColorConversionStrategy in [ccsGray, ccsCMYK]) then
      raise Exception.Create('ColorConversionStrategy for PDF-X Output has to be Gray or CMYK!');
end;

procedure TPDFParams.SetDefaultValues;
begin
  inherited;
  FColorConversionStrategy := ccsNone;
  FDevice := 'pdfwrite';
  FEmbededFonts := False;
  FPrinted := True;
  FShowAnnots := True;
  FShowAcroForm := True;
  FFirstPage := -1;
  FLastPage := -1;
  FSubsetFonts := True;
end;

procedure TPDFParams.SetParams(AParams: TStringList);
begin
  if (FColorConversionStrategy <> ccsNone) then
    AParams.Add('-sColorConversionStrategy=' + GetColorConversionStrategy);
  SetParam(FEmbededFonts, False, '-dEmbedAllFonts=', AParams);
  SetParam(FNewPdf, False, '-dNEWPDF', AParams);
  SetParam(FPdfInfo, False, '-dPDFINFO', AParams);
  SetParam(FPdfFitPage, False, '-dPDFFitPage', AParams);
  SetParam(FPdfTitle, '', '-sPDFTitle=', AParams);
  SetParam(FPrinted, True, '-dPrinted=', AParams);
  SetParam(FUseBleedBox, False, '-dUseBleedBox', AParams);
  SetParam(FUseTrimBox, False, '-dUseTrimBox', AParams);
  SetParam(FUseArtBox, False, '-dUseArtBox', AParams);
  SetParam(FUseCropBox, False, '-dUseCropBox', AParams);
  SetParam(FPDFPassword, '', '-sPDFPassword=', AParams);
  SetParam(FShowAnnots, True, '-dShowAnnots=', AParams);
  SetParam(FShowAcroForm, True, '-dShowAcroForm=', AParams);
  SetParam(FNoUserUnit, False, '-dNoUserUnit', AParams);
  SetParam(FRenderTTNotDef, False, '-dRENDERTTNOTDEF', AParams);
  SetParam(FFirstPage, -1, '-dFirstPage=', AParams);
  SetParam(FLastPage, -1, '-dLastPage=', AParams);
  SetParam(FPageList, '', '-sPageList=', AParams);
  SetParam(FSubsetFonts, True, '-dSubsetFonts=', AParams);
  SetParameters(AParams);
end;

procedure TPDFParams.SetPdfA(const Value: Boolean);
begin
  FPdfA := Value;
  if (FPdfA) then
    FPdfX := False;
end;

procedure TPDFParams.SetPdfX(const Value: Boolean);
begin
  FPdfX := Value;
  if (FPdfX) then
    FPdfA := False;
end;

{$ENDREGION}

{$REGION 'TPDFAParams' }

procedure TPDFAXParams.SetDefaultValues;
begin
  inherited;
  FPDFAProfile := 3;
  FPDFACombatibilityPolicy := PDFA_PROCESS_STOP_ONERROR;
  FPDFAOutputConditionIdentifier := '';
  ICCProfile := '';
end;

procedure TPDFAXParams.SetICCProfile(const Value: string);
begin
  if (Value <> '') then
  begin
    if (not FileExists(Value)) then
      raise EFileNotFoundException.CreateFmt('SetICCProfile: file %s does not exist', [Value]);
    FICCProfile := GetFullLinuxFilePath(Value);
  end else
    FICCProfile := Value;
end;

procedure TPDFAXParams.SetParams(AParams: TStringList);
var
  APath: string;
begin
  if (PdfA) or (PdfX) then
  begin
    APath := ExtractFilePath(ICCProfile.Replace('/', '\'));
    AddSourcePath(APath);
  end;
  inherited;
  if (PdfA) then
  begin
    AParams.Add(Format('-dPDFA=%d', [PDFAProfile]));
    AParams.Add(Format('-dPDFACompatibilityPolicy=%d', [Integer(PDFACombatibilityPolicy)]));
  end else
  if (PdfX) then
    AParams.Add('-dPDFX');

  if (PdfA) or (PdfX) then
    AParams.Add('-sICCProfile=' + ICCProfile);

  SetParam(FPDFAOutputConditionIdentifier, '', '-sPDFOutputConditionIdentifier=', AParams);
end;

procedure TPDFAXParams.SetPDFAProfile(const Value: Integer);
begin
  if (Value <> FPDFAProfile) then
  begin
    if (Value >= 1) and (Value <= 3) then
      FPDFAProfile := Value
    else
      raise Exception.Create('You can only select the supported PDFProfiles from 1 to 3!');
  end;
end;

{$ENDREGION}

{$REGION 'TGSDebugParams' }

procedure TGSDebugParams.SetDebugParams(const Value: TDebugParamSet);
begin
  if (FDebugParms <> Value) then
  begin
    FDebugParms := Value;
    FCompiledFonts := dparCompiledFonts in Value;
    FCFFFonts := dparCffFonts in Value;
    FCMap := dparCMap in Value;
    FCIEColor := dparCIEColor in Value;
    FEPSHandling := dparEpsHandling in Value;
    FFontApi := dparFontApi in Value;
    FInit := dparInitialization in Value;
    FPdfInterpreter := dparPdfInterpreter in Value;
    FPdfWriter := dparPdfWriter in Value;
    FSetPageDevice := dparSetPageDevice in Value;
    FStaticResource := dparStaticResource in Value;
    FTtfFonts := dparTTFFonts in Value;
    FViewGIF := dparViewGIF in Value;
    FViewJPEG := dparViewJPEG in Value;
  end;
end;

procedure TGSDebugParams.SetParams(AParams: TStringList);
begin
  SetParam(FCompiledFonts, False, DEBUG_COMPILED_FONTS, AParams);
  SetParam(FCFFFonts, False, DEBUG_CFF_FONTS, AParams);
  SetParam(FCMap, False, DEBUG_CMAP, AParams);
  SetParam(FCIEColor, False, DEBUG_CIE_COLOR, AParams);
  SetParam(FEPSHandling, False, DEBUG_EPS_HANDLING, AParams);
  SetParam(FFontApi, False, DEBUG_FONT_API, AParams);
  SetParam(FInit, False, DEBUG_INITIALIZATION, AParams);
  SetParam(FPdfInterpreter, False, DEBUG_PDF_INTERPRETER, AParams);
  SetParam(FPdfWriter, False, DEBUG_PDF_WRITER, AParams);
  SetParam(FSetPageDevice, False, DEBUG_SETPAGEDEVICE, AParams);
  SetParam(FStaticResource, False, DEBUG_STATIC_RESOURCE, AParams);
  SetParam(FTtfFonts, False, DEBUG_TTF_FONTS, AParams);
  SetParam(FViewGIF, False, DEBUG_VIEW_GIF, AParams);
  SetParam(FViewJPEG, False, DEBUG_VIEW_JPEG, AParams);
end;

{$ENDREGION}

end.
