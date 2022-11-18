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

// Main unit to implement the Wrapper units in delphi classes
unit SkiSys.GS_Api;

{$IFDEF FPC} //Free Pascal
  {$MODE DELPHI}
  {$H+}
{$ELSE} //Delphi
  {$DEFINE DELPHI}
{$ENDIF}

interface

uses
  SkiSys.GS_Dll, SkiSys.GS_Types, SkiSys.GS_Errors, SkiSys.GS_ParameterTypes,
  SkiSys.GS_ParameterConst, SkiSys.GS_gdevdsp,

{$IFDEF FPC}
  Classes, SysUtils, Graphics, Generics.Collections, LclIntf, LclType, GraphType
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  ;
{$ENDIF}
{$IFDEF DELPHI}
  System.Classes, System.SysUtils, WinApi.Windows, System.AnsiStrings,
  System.Generics.Collections, Vcl.Graphics;
{$ENDIF}

{$MINENUMSIZE 4}
{$I+}

const
  /// <summary>
  ///  The TGS_Api version
  /// </summary>
  API_VERSION = 1011;
  /// <summary>
  ///  Minimum required Ghostscript version, which will be checked in
  ///  <see cref="SkiSys.GS_API|TGS_Revision.CheckRevision"/>
  /// </summary>
  MIN_GHOSTSCRIPT_REVISION = 9500;

type
  TAnsiStringArray = array of AnsiString;
  TGSEvent_Std = procedure(const AText: String) of object;
  TNotifyFuncEvent = function(Sender: TObject): Integer of object;

  /// <summary>
  ///  Will be raised when a not supported Ghostscript version is used.
  /// </summary>
  EInvalidGhostscriptVersionException = Exception;


  { EGS_ApiException }

  /// <summary>
  ///  Exceptions raised by the TGS_Api
  /// </summary>
  EGS_ApiException = class(Exception)
  private
    const
      // <1>ErrorCode , <2>FunctionName , <3>ErrorMessage
      MessageFunctionFmt = 'Error(%d) on %s: %s';
      MessageErrorCodeFmt = 'Error(%d) %s';
  protected
    FErrorCode: Integer;
    FFunctionName: string;
    FInternalMessage: string;
    function GetMessage(AValue: string): string;
  public
    constructor Create(const Msg, AFunctionName: string; error: Integer); reintroduce;
    constructor CreateFmt(const Msg, AFunctionName: string; error: Integer;
                          const Args: array of const); reintroduce;
  end;

  TGS_Api = class;
  TGS_Display = class;

  { TStrSyncObject }

  /// <summary>
  ///  Object to synchronize a TGSEvent_Std
  /// </summary>
  TStrSyncObject = class
  private
    FEvent: TGSEvent_Std;
    FStr: string;
  public
    // constructor
    constructor Create(AEvent: TGSEvent_Std; AStr: string);
    /// <summary>
    ///  Method to execute the event synchronized
    /// </summary>
    procedure Execute;
  end;

  /// <summary>
  ///  Thread to run InitWithArgs as a thread with the arguments
  /// </summary>
  TGS_ApiThread = class(TThread)
  private
    FArgs: TStrings;
    {$IFDEF DELPHI}[WEAK]{$ENDIF}FApi: TGS_Api;
  protected
    procedure Execute; override;
  public
    /// <summary>
    ///  Create the thread and start it
    /// </summary>
    constructor Create(AApi: TGS_Api; Args: TStrings);
    // destructor
    destructor Destroy; override;
  end;

  /// <summary>
  ///  Image record to store the image info
  /// </summary>
  TGS_ImageData = record
    /// <summary>
    ///  Calculated bytes per image line
    /// </summary>
    ByteWidth: Integer;
    /// <summary>
    ///  Ghostscript device pointer
    /// </summary>
    Device: Pointer;
    /// <summary>
    ///  The Ghostscript display format, for further informations have a look at
    ///  the unit SkiSys.GS_gdevdsp
    /// </summary>
    Format: Cardinal;
    /// <summary>
    ///  Image height
    /// </summary>
    Height: Integer;
    /// <summary>
    ///  Pointer to the image data buffer
    /// </summary>
    ImageData: PByte;
    /// <summary>
    ///  Ghostscript bytes per image line
    /// </summary>
    Raster: Integer;
    /// <summary>
    ///  Image width
    /// </summary>
    Width: Integer;
    /// <summary>
    ///  Set the values of TGS_ImageData
    /// </summary>
    procedure SetDataAndSize(Width, Height, Raster: Integer; Format: Cardinal;
                             PImage: PByte);
  end;

  { TGS_Image }

  /// <summary>
  ///  TGS_Image inherits from TBitmap to show a preview image
  /// </summary>
  TGS_Image = class({$IFDEF FPC}Graphics.{$ENDIF}TBitmap)
  private
    {$IFDEF DELPHI}[WEAK]{$ENDIF}FDisplay: TGS_Display;
    FBmpInfoHeader: BITMAPINFOHEADER;
    FBitsPerPixel: Integer;
    FGS_Device: Pointer;
    FGS_Format: TGSDisplayFormat;
    FGS_ImageDataLoaded: Boolean;
    FGS_Raster: Integer; // GS BytesPerScanLine
    function GetGS_Format: Cardinal;
  protected
    /// <summary>
    ///  Converts the image data if needed
    /// </summary>
    function ConvertImageDataLine(ASrcLine, ADestLine: PByte): Boolean; virtual;
    /// <summary>
    ///  convert data bytes from the native 555 format
    /// </summary>
    procedure ConvertNative555(ASrcBytes, ADestBytes: PByte); virtual;
    /// <summary>
    ///  convert data bytes from the native 565 format
    /// </summary>
    procedure ConvertNative565(ASrcBytes, ADestBytes: PByte); virtual;
    /// <summary>
    ///  calculate the bytes
    /// </summary>
    function GetBytesPerLine: Integer; virtual;
    /// <summary>
    ///  calculate the bits per pixel for our formats
    /// </summary>
    function GetBitsPerPixel: Integer; virtual;
    /// <summary>
    ///  get the gray colors from normal colors
    /// </summary>
    function GetGrayLuminance(AColor: TColor; const ALum: array of Byte): TColor;
    /// <summary>
    ///  get the pointer to the image data line
    /// </summary>
    function GetScanLine(AIndex: Integer): Pointer;
    /// <summary>
    ///  Get the PixelFormat
    /// </summary>
    function GetPixelFormatFromBits: TPixelFormat; virtual;
    /// <summary>
    ///  Calls the Move method
    /// </summary>
    procedure MemCopy(Source, Dest: Pointer; ALength: Integer);
    /// <summary>
    ///  Set the BmpInfoHeader from the format
    /// </summary>
    procedure SetBmpInfoHeader(AWidth, AHeight: Integer); virtual;
    /// <summary>
    ///  Set the image in the bitmap
    /// </summary>
    procedure SetImageData(PImage: PByte); virtual;
    /// <summary>
    ///  The BmpInfoHeader
    /// </summary>
    property BmpInfoHeader: BITMAPINFOHEADER read FBmpInfoHeader write FBmpInfoHeader;
  public
    constructor Create(ADisplays: TGS_Display; AWidth, AHeight, ARaster: Integer;
                       AFormat: Cardinal; PImage: PByte); reintroduce; overload;
    constructor Create(ADisplays: TGS_Display; AData: TGS_ImageData); reintroduce; overload;
  public (*** PROPERTIES ***)
    /// <summary>
    ///  Pointer to the Ghostscript display device
    /// </summary>
    property GS_Device: Pointer read FGS_Device;
    /// <summary>
    ///  The Ghostscript display format implemented in the unit SkiSys.GS_gdevdsp
    /// </summary>
    property GS_Format: Cardinal read GetGS_Format;
    /// <summary>
    ///  Is the image buffer loaded into the bitmap
    /// </summary>
    property GS_ImageDataLoaded: Boolean read FGS_ImageDataLoaded;
    /// <summary>
    ///  The raster of the bitmap (bytes per image line)
    /// </summary>
    property GS_Raster: Integer read FGS_Raster;
  end;

  /// <summary>
  ///  Imagelist for TGS_Image
  /// </summary>
  TGS_ImageList = class(TObjectList<TGS_Image>)
  private
    FDisplays: TGS_Display;
    FImageData: TGS_ImageData;
  public
    /// <summary>
    ///  Create the image from the ImageData and add it to the list
    /// </summary>
    function AddFromImageData: Integer; virtual;
    // constructor
    constructor Create(ADisplays: TGS_Display);
    /// <summary>
    ///  Initialize the ImgaeData when the device is open
    /// </summary>
    procedure InitImageData(ADevice: Pointer); virtual;
    /// <summary>
    ///  Set the size and the buffer pointer for the image buffer. The image
    ///  buffer isn't filled at this moment.
    /// </summary>
    procedure SetDataAndSize(AWidth, AHeight, ARaster: Integer; AFormat: Cardinal;
                             PImage: PByte); virtual;
  end;

{$REGION 'TGS_Display Events Types'}
  TGS_DisplayEvent = function(ADevice: Pointer): Integer of object;
  TGS_DisplayAdjustBandHeightEvent = function(ADevice: Pointer;
                                              ABandHeight: Integer): Integer of object;
  TGS_DisplayMemAllocEvent = function(ADevice: Pointer; ASize: SIZE_T): Integer of object;
  TGS_DisplayMemFreeEvent = function(ADevice, AMem: Pointer): Integer of object;
  TGS_DisplayPageEvent = function(ADevice: Pointer; ACopies, AFlush: Integer): Integer of object;
  TGS_DisplayPresizeEvent = function(ADevice: Pointer;
                                     AWidth, AHeight, ARaster: Integer;
                                     AFormat: Cardinal): Integer of object;
  TGS_DisplayRectangleRequestEvent = function(ADevice, AMemory: Pointer;
                                              out ARaster, APlaneRaster: Integer;
                                              out X, Y, W, H: Integer): Integer of object;
  TGS_DisplaySeparationEvent = function(ADevice: Pointer; AComponent: Integer;
                                        AComponentName: string;
                                        C, M, Y, K: Word): Integer of object;
  TGS_DisplaySizeEvent = function(ADevice: Pointer;
                                  AWidth, AHeight, ARaster: Integer;
                                  AFormat: Cardinal; PImage: PByte): Integer of object;
  TGS_DisplayUpdateEvent = function(ADevice: Pointer; X, Y, W, H: Integer): Integer of object;
{$ENDREGION}

  /// <summary>
  ///  The display class to create a preview
  /// </summary>
  TGS_Display = class
  private
    {$IFDEF DELPHI}[WEAK]{$ENDIF}FApi: TGS_Api;
    FCallback: display_callback;
    FDebug: Boolean;
    FEventAdjustBandHeight: TGS_DisplayAdjustBandHeightEvent;
    FEventClose: TGS_DisplayEvent;
    FEventMemAlloc: TGS_DisplayMemAllocEvent;
    FEventMemFree: TGS_DisplayMemFreeEvent;
    FEventOpen: TGS_DisplayEvent;
    FEventPage: TGS_DisplayPageEvent;
    FEventPreclose: TGS_DisplayEvent;
    FEventPresize: TGS_DisplayPresizeEvent;
    FEventRectangleRequest: TGS_DisplayRectangleRequestEvent;
    FEventSeparation: TGS_DisplaySeparationEvent;
    FEventSize: TGS_DisplaySizeEvent;
    FEventSync: TGS_DisplayEvent;
    FEventUpdate: TGS_DisplayUpdateEvent;
  protected
    /// <summary>
    ///  Preview ImageList will be filled when the parameter -sDEVICE=display is set
    /// </summary>
    FImageList: TGS_ImageList;
    /// <summary>
    ///  write a message to std_out, when Debug is true
    /// </summary>
    procedure DebugLog(AMessage: string); overload;
    /// <summary>
    ///  write a message in the format to std_out, when Debug is true
    /// </summary>
    procedure DebugLogFmt(AFormat: string; const Args: array of const); overload;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnAdjustBandHeight" />
    /// </summary>
    function EventAdjustBandHeight(ADevice: Pointer; ABandHeight: Integer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnClose"/>
    /// </summary>
    function EventClose(ADevice: Pointer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnMemAlloc"/>
    /// </summary>
    procedure EventMemAlloc(ADevice: Pointer; ASize: SIZE_T); virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnMemFree"/>
    /// </summary>
    function EventMemFree(ADevice, AMem: Pointer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnOpen"/>
    /// </summary>
    function EventOpen(ADevice: Pointer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnPage"/>
    /// </summary>
    function EventPage(ADevice: Pointer; ACopies, AFlush: Integer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnPreclose"/>
    /// </summary>
    function EventPreclose(ADevice: Pointer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnPresize"/>
    /// </summary>
    function EventPresize(ADevice: Pointer;
                          AWidth, AHeight, ARaster: Integer;
                          AFormat: Cardinal): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnRectangleRequest"/>
    /// </summary>
    function EventRectangleRequest(ADevice, AMemory: Pointer;
                                   out ARaster, APlaneRaster: Integer;
                                   out X, Y, W, H: Integer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnSeparation"/>
    /// </summary>
    function EventSeparation(ADevice: Pointer; AComponent: Integer;
                             AComponentName: PAnsiChar; C, M, Y, K: Word): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnSize"/>
    /// </summary>
    function EventSize(ADevice: Pointer;
                       AWidth, AHeight, ARaster: Integer;
                       AFormat: Cardinal; PImage: PByte): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnSync"/>
    /// </summary>
    function EventSync(ADevice: Pointer): Integer; virtual;
    /// <summary>
    ///  see description <see cref="SkiSys.GS_Api|TGS_Display.OnUpdate"/>
    /// </summary>
    function EventUpdate(ADevice: Pointer; X, Y, W, H: Integer): Integer; virtual;
    /// <summary>
    ///  get the count of pages
    /// </summary>
    function GetPageCount: Integer; virtual;
    /// <summary>
    ///  initialize the object
    /// </summary>
    procedure Init(AApi: TGS_Api); virtual;
  public (*** PROPERTIES ***)
    /// <summary>
    ///  Set Debug=True to receive debug messages of called events with values
    /// </summary>
    property Debug: Boolean read FDebug write FDebug;
    /// <summary>
    ///  The page count
    /// </summary>
    property PageCount: Integer read GetPageCount;
  public (*** EVENTS ***)
    /// <summary>
    ///  When running in “rectangle request mode” the device first renders the
    ///  page to a display list internally. It can then be played back repeatedly
    ///  so that different regions (rectangles) of the page can be extracted in
    ///  sequence. A common use of this is to support “banded” operation, where
    ///  the page is divided into multiple non-overlapping bands of a fixed height.
    ///  The display device itself will pick an appropriate band height for it
    ///  to use. If this function pointer is left as NULL then this value will
    ///  be used unchanged. Otherwise, the proposed value will be offered to this
    ///  function. This function can override the choice of bandheight, by
    ///  returning the value that it would like to be used in preference.
    ///  In general, this figure should (as much as possible) only be adjusted
    ///  downwards. For example, a device targeting an inkjet printer with
    ///  200 nozzles in the print head might like to extract bands that are a
    ///  multiple of 200 lines high. So the function might
    ///  return max(200, 200*(bandheight/200)). If the function returns 0,
    ///  then the existing value will be used unchanged.
    ///  Any size rectangle can be chosen with any size bandheight, so ultimately
    ///  the value chosen here will not matter much. It may make some small
    ///  difference in speed in some cases.
    /// </summary>
    property OnAdjustBandHeight: TGS_DisplayAdjustBandHeightEvent
               read FEventAdjustBandHeight write FEventAdjustBandHeight;
    /// <summary>
    ///  Device has been closed.
    /// </summary>
    /// <remarks>
    ///  This is the last event from this device.
    /// </remarks>
    property OnClose: TGS_DisplayEvent read FEventClose write FEventClose;
    /// <summary>
    ///  New device has been opened
    /// </summary>
    /// <remarks>
    ///  This is the first event from this device.
    /// </remarks>
    property OnOpen: TGS_DisplayEvent read FEventOpen write FEventOpen;
    /// <summary>
    ///  Allocate memory for bitmap
    /// </summary>
    /// <remarks>
    ///  This is provided in case you need to create memory in a special
    ///  way, e.g. shared.  If the compiler option "USE_GSDisplayMemAlloc" is
    ///  not defined, the Ghostscript memory device allocates the bitmap.
    ///  This will only called to allocate the image buffer. The first row will
    ///  be placed at the address returned by display_memalloc.
    ///
    ///  In the event of this callback returning nil, Ghostscript will
    ///  look for a display_rectangle_request callback. If one is not
    ///  supplied, then this will be reported as memory exhaustion. If
    ///  one is supplied, then Ghostscript will switch to working in
    ///  rectangle request mode.
    ///  This event will only be called with the compiler option "USE_GSDisplayMemAlloc".
    /// </remarks>
    property OnMemAlloc: TGS_DisplayMemAllocEvent read FEventMemAlloc write FEventMemAlloc;
    /// <summary>
    ///  Free memory for bitmap
    ///  If this is NULL, the Ghostscript memory device will free the bitmap buffer
    /// </summary>
    property OnMemFree: TGS_DisplayMemFreeEvent read FEventMemFree write FEventMemFree;
    /// <summary>
    ///  showpage
    ///  If you want to pause on showpage, then don't return immediately
    /// </summary>
    property OnPage: TGS_DisplayPageEvent read FEventPage write FEventPage;
    /// <summary>
    ///  Device is about to be closed.
    /// </summary>
    /// <remarks>
    ///  Device will not be closed until this function returns.
    /// </remarks>
    property OnPreclose: TGS_DisplayEvent read FEventPreclose write FEventPreclose;
    /// <summary>
    ///  Device is about to be resized.
    ///  Resize will only occur if this function returns 0.
    /// </summary>
    /// <param name="ARaster">ARaster is byte count of a row.</param>
    property OnPresize: TGS_DisplayPresizeEvent read FEventPresize write FEventPresize;
    /// <summary>
    ///  If the display device chooses to use rectangle request mode, this function
    ///  will be called repeatedly to request a rectangle to render. Ghostscript
    ///  will render the rectangle, and call this function again. The implementer
    ///  is expected to handle the rectangle that has just been rendered, and to
    ///  return the details of another rectangle to render. This will continue
    ///  until a rectangle with zero height or width is returned, whereupon
    ///  Ghostscript will continue operation. <param/>
    ///  On entry, *raster and *plane_raster are set to the values expected by
    ///  the format in use. All the other pointers point to uninitialised values. <param/>
    ///  On exit, the values should be updated appropriately. The implementor is
    ///  expected to store the values returned so that the rendered output given
    ///  can be correctly interpreted when control returns to this function.
    /// </summary>
    /// <param name="AMemory">
    ///  should be updated to point to a block of memory to use for the rendered
    ///  output. Pixel ( *ox, *oy) is the first pixel represented in that block.
    /// </param>
    /// <param name="ARaster">
    ///  is the number of bytes difference between the address of component 0 of
    ///  Pixel( *ox, *oy) and the address of component 0 of Pixel( *ox, 1+``*oy``).
    /// </param>
    /// <param name="APlaneRaster">
    ///  is the number of bytes difference between the address of component 0 of
    ///  Pixel( *ox, *oy) and the address of component 1 of Pixel( *ox, *oy), if
    ///  in planar mode, 0 otherwise. *x, *y, *w and *h give the rectangle
    ///  requested within that memory block.
    /// </param>
    /// <remarks>
    ///  Any set of rectangles can be rendered with this method, so this can be
    ///  used to drive Ghostscript in various ways. Firstly, it is simple to
    ///  request a set of non-overlapping “bands” that cover the page, to drive
    ///  a printer. Alternatively, rectangles can be chosen to fill a given block
    ///  of memory to implement a window panning around a larger page. Either the
    ///  whole image could be redrawn each time, or smaller rectangles around the
    ///  edge of the panned area could be requested. The choice is down to the caller.
    /// </remarks>
    property OnRectangleRequest: TGS_DisplayRectangleRequestEvent
               read FEventRectangleRequest write FEventRectangleRequest;
    /// <summary>
    ///  When using DISPLAY_COLORS_SEPARATION, this function will be called once
    ///  for every separation component - first “Cyan”, “Magenta”, “Yellow” and
    ///  “Black”, then any spot colors used. The supplied c, m, y and k values
    ///  give the equivalent color for each spot. Each colorant value ranges
    ///  from 0 (for none) to 65535 (full).
    ///  In separation color mode you are expected to count the number of calls
    ///  you get to this function after each display_size to know how many colors
    ///  you are dealing with.
    /// </summary>
    property OnSeparation: TGS_DisplaySeparationEvent read FEventSeparation
                                                      write FEventSeparation;
    /// <summary>
    ///  Device has been resized.
    ///  New pointer to raster returned in pimage
    /// </summary>
    property OnSize: TGS_DisplaySizeEvent read FEventSize write FEventSize;
    /// <summary>
    ///  This function may be called periodically during display to flush the
    ///  page to the display.
    /// </summary>
    property OnSync: TGS_DisplayEvent read FEventSync write FEventSync;
    /// <summary>
    ///  This function may get called repeatedly during rendering to indicate
    ///  that an area of the output has been updated. Certain types of rendering
    ///  will not see this function called back at all
    ///  (in particular files using transparency).
    /// </summary>
    property OnUpdate: TGS_DisplayUpdateEvent read FEventUpdate write FEventUpdate;
  public (*** METHODS ***)
    // constructur
    constructor Create(AApi: TGS_Api);
    // destructor
    destructor Destroy; override;
    /// <summary>
    ///  Clear the images
    /// </summary>
    procedure Clear;
    /// <summary>
    ///  Get a preview page as a bitmap
    /// </summary>
    /// <param name="AIdx">The index of the page</param>
    /// <returns>
    ///  Will return the image of the page.
    ///  Otherwise will return nil, when no image exists at the index.
    /// </returns>
    function GetPage(AIdx: Integer): TGS_Image;
  end;

  /// <summary>
  ///  Class to read the revision information from the dll
  /// </summary>
  TGS_Revision = class
  private
    {$IFDEF DELPHI}[WEAK]{$ENDIF}FApi: TGS_API;
    FCopyright: string;
    FProduct: string;
    FRevision: Integer;
    FRevisionStr: string;
    FRevisionDate: Integer;
  protected (*** PROTECTED METHODS ***)
    /// <summary>
    ///  Fills the RevisionStr after GetRevision is called
    /// </summary>
    function GetRevisonStr: string; virtual;
  public (*** PUBLIC PROPERTIES ***)
    /// <summary>
    ///  The product string of Ghostscript
    /// </summary>
    property Product: string read FProduct;
    /// <summary>
    ///  The copyright of Ghostscript
    /// </summary>
    property Copyright: string read FCopyright;
    /// <summary>
    ///  Revison as an integer
    /// </summary>
    property Revision: Integer read FRevision;
    /// <summary>
    ///  Revision as a string
    /// </summary>
    property RevisionStr: string read FRevisionStr;
    /// <summary>
    ///  Revison date as an integer in the format yyyymmdd
    /// </summary>
    property RevisionDate: Integer read FRevisionDate;
  public (*** PUBLIC METHODS ***)
    /// <summary>
    ///  create the object and read the revision data from the dll
    /// </summary>
    constructor Create(AApi: TGS_API); overload;
    /// <summary>
    ///  Check the revision and return a warning on StdOut, when a higher version
    ///  is used. When the minimum supported version of Ghostscript is used, then
    ///  raise an EInvalidGhostscriptVersion.
    ///  You can deactivate the check with the compiler switch DONT_CHECK_GS_REVISION
    /// </summary>
    procedure CheckRevision; virtual;
    /// <summary>
    ///  Read the revision data from the dll and fill the fields
    /// </summary>
    procedure GetRevision(AApi: TGS_API); virtual;
  end;

  { TGS_Api }

  /// <summary>
  ///  Base API to use the Ghostscript dll functions
  /// </summary>
  TGS_Api = class(TObject)
  private
    FArgumentEncoding: GS_ARG_ENCODING;
    FDebug: Boolean;
    FDebugLastCmdArgs: string;
    FDebugParams: TGSDebugParams;
    FDebugShowCmdArgs: Boolean;
    FDefaultDeviceList: TStringList;
    FDllPath: string;
    FEventAfterExecute: TNotifyEvent;
    FEventAfterInitWithArgs: TNotifyEvent;
    FEventPoll: TNotifyFuncEvent;
    FEventStdError: TGSEvent_Std;
    FEventStdIn: TGSEvent_Std;
    FEventStdOut: TGSEvent_Std;
    FExit: Boolean; // was gsapi_exit allready called
    FInitWithArgs: Boolean;
    FInstance: Pointer; // ghostscript instance pointer
    FLastError: string;
    FLastErrors: TStringList; // list of errors occured during the process
    FLastErrorCode: Integer;
    FLogStdIn: TStringList;  // log StdIn
    FLogStdOut: TStringList; // log StdOut
    FNoExit: Boolean;
    FRevision: TGS_Revision;
    FThread: TGS_ApiThread;
    FThreadUsed: Boolean;
    function GetDefaultDeviceList: TStrings;
    function GetLastErrors: string;
    function GetLogStdIn: TStrings;
    function GetLogStdOut: TStrings;
    function GetThreadRunning: Boolean;
    /// <summary>
    ///  Register all callouts to communicate with the Ghostscript library
    /// </summary>
    procedure SetCallouts;
    procedure SetDefaultDeviceList(ADeviceList: TStrings);
    /// <summary>
    ///  Set FLastErrorCode and the internal Log. (Will not call OnStdError)
    /// </summary>
    procedure SetLastErrorInternal(AText: string; AErrorCode: Integer = -1);
  protected
    /// <summary>
    ///  Call event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterExecute"/>
    /// </summary>
    procedure AfterExecute; virtual;
    /// <summary>
    ///  Call event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </summary>
    procedure AfterInitWithArgs; virtual;
    /// <summary>
    ///  Check if run_string* operations can be executed
    /// </summary>
    function CheckRunString: Boolean; virtual;
    /// <summary>
    ///  Check if a gsapi_* method returns gs_error_Quit, if yes gsapi_exit
    ///  will be called
    /// </summary>
    function CheckResult(AResult: Integer): Integer; virtual;
    /// <summary>
    ///  This function make sure that gsapi_exit is called at the right time
    ///  to avoid errors.
    /// </summary>
    /// <remarks>
    ///  The gsapi_run_* functions are like gs_main_run_* except that the
    ///  error_object is omitted. If these functions return (smaller)= -100, either quit
    ///  or a fatal error has occured. You must call gsapi_exit() next.
    ///  The only exception is gsapi_run_string_continue() which will
    ///  return gs_error_NeedInput if all is well. See below for return codes.
    /// </remarks>
    function CheckRunResult(AResult: Integer): Integer; virtual;
    /// <summary>
    ///  Clear the internal log vars
    /// </summary>
    procedure ClearInternalLog; virtual;
    /// <summary>
    ///  Write a log when the API is in Debug Mode
    /// </summary>
    procedure DebugLog(AText: string); virtual;
    /// <summary>
    ///  Will fire the given event and the string synchronized, when the api is
    ///  execute in a thread
    /// </summary>
    procedure FireEvent(AEvent: TGSEvent_Std; AStr: string); virtual;
    /// <summary>
    ///  Free the gs_instance
    /// </summary>
    procedure FreeGSInstance; virtual;
    /// <summary>
    ///  Convert a TStrings object to a TAnsiStringArray
    /// </summary>
    function GetAnsiStrArray(AStrings: TStrings): TAnsiStringArray; overload; virtual;
    /// <summary>
    ///  Convert a TAnsiStringArray to a PArgv Pointer
    /// </summary>
    function GetPAnsiCharArray(AAnsiStrings: TAnsiStringArray): PArgv; virtual;
    /// <summary>
    ///  Is called when the object will be created and initialize the API and the DLL
    /// </summary>
    procedure Init(ADllPath: string); virtual;
    /// <summary>
    ///  Get a new Instance Pointer for API calls. This method has to be called
    ///  everytime before InitWithArgs.(To avoid an fatal error from gsapi at 2nd call)
    /// </summary>
    procedure InitGSInstance; virtual;
    /// <summary>
    ///  Initialize the log vars to store the Ghostscript output
    /// </summary>
    procedure InitInternalLog; virtual;
    /// <summary>
    ///  Main method of Ghostscript lirary to execute commands using pointers
    /// </summary>
    function InitWithArgs(AArgs: PArgv): Boolean; overload; virtual;
    /// <summary>
    ///  The callback function for polling. See full description at the event
    ///  <see cref="SkiSys.GS_Api|TGS_Api.OnPoll"/>
    /// </summary>
    function Poll: Integer; virtual;
    /// <summary>
    ///  check the param for whitespaces and set quotes if needed
    /// </summary>
    function QuoteCmdParameter(const AParam: string): string;
    /// <summary>
    ///  Set the default values of the class
    /// </summary>
    procedure SetDefaultValues; virtual;
    /// <summary>
    ///  Set the last error from the API and the Ghostscript library
    /// </summary>
    procedure SetLastError(AText: string; AErrorCode: Integer = -1); overload; virtual;
    /// <summary>
    ///  Set the last error from the API and the Ghostscript library
    /// </summary>
    procedure SetLastError(AText: string; AErrorCode: gs_error_type); overload; virtual;
    /// <summary>
    ///  Set the last error code from the API and the Ghostscript library
    /// </summary>
    procedure SetLastErrorCode(ACode: gs_error_type); virtual;
    /// <summary>
    ///  Set parameters for the InitWithArgs procedure
    /// </summary>
    procedure SetParams(AList: TStringList); virtual;
    /// <summary>
    ///  Set the StdError and call the OnStdError Event
    /// </summary>
    procedure StdError(AText: string); virtual;
    /// <summary>
    ///  Set the StdIn and call the OnStdIn Event
    /// </summary>
    procedure StdIn(AText: string); virtual;
    /// <summary>
    ///  Set the StdOut, filter some informations and call the OnStdOut Event
    /// </summary>
    procedure StdOut(AText: string); virtual;
    /// <summary>
    ///  Calls StdOut method with a linebreak at the end
    /// </summary>
    procedure StdOutLine(AText: string); virtual;
    /// <summary>
    ///  Will be called after InitWithArgs and InitWithArgsStart was executed
    /// </summary>
    procedure ThreadFinished(Sender: TObject); virtual;
  public  (*** PUBLIC PROPERTIES and VARS ***)
    /// <summary>
    ///  Will be used when the device is set to "display" to create a preview,
    ///  but some options are not availible with this device.
    /// </summary>
    GSDisplay: TGS_Display;
    /// <summary>
    ///  Set the argument encoding for Ghostscript
    /// </summary>
    property ArgumentEncoding: GS_ARG_ENCODING read FArgumentEncoding
                                               write FArgumentEncoding;
    /// <summary>
    ///  Set Debug to True to get extended informations about which Params will be
    ///  set for convert operations and other details
    /// </summary>
    property Debug: Boolean read FDebug write FDebug;
    /// <summary>
    ///  Get the last command line arguments
    /// </summary>
    property DebugLastCmdArgs: string read FDebugLastCmdArgs;
    /// <summary>
    ///  Ghostscript Debug Parameters
    /// </summary>
    property DebugParams: TGSDebugParams read FDebugParams write FDebugParams;
    /// <summary>
    ///  Show the command line args in StdOut
    /// </summary>
    property DebugShowCmdArgs: Boolean read FDebugShowCmdArgs
                                       write FDebugShowCmdArgs;
    /// <summary>
    ///  Get's and Sets a DefaultDevice list for Ghostscript
    /// </summary>
    property DefaultDeviceList: TStrings read GetDefaultDeviceList
                                         write SetDefaultDeviceList;
    /// <summary>
    ///  Returns True if Exit was called.
    /// </summary>
    property GSExit: Boolean read FExit;
    /// <summary>
    ///  returns LastError from the API or Ghostscript
    /// </summary>
    property LastError: string read FLastError;
    /// <summary>
    ///  The LastErrors, we have to read the Error from StdError and we can get
    ///  more as one. So we put them all in a list.
    /// </summary>
    property LastErrors: string read GetLastErrors;
    /// <summary>
    ///  LastErrorCode from Ghostscript
    /// </summary>
    property LastErrorCode: Integer read FLastErrorCode;
    /// <summary>
    ///  Is currently a thread running
    /// </summary>
    property ThreadRunning: Boolean read GetThreadRunning;
    /// <summary>
    ///  Can be used to turn off the automatic Exit(gsapi_exit) call after
    ///  InitWithArgs, but you have to call Exit by your self. A saver method
    ///  is to use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>.
    /// </summary>
    property NoExit: Boolean read FNoExit write FNoExit;
    /// <summary>
    ///  The event will be called after an operation is finished. (after Exit)
    /// </summary>
    property OnAfterExecute: TNotifyEvent read FEventAfterExecute
                                          write FEventAfterExecute;
    /// <summary>
    ///  The event will be called after InitWithArgs and before Exit.
    ///  You can use it to perform Run* operations.
    /// </summary>
    property OnAfterInitWithArgs: TNotifyEvent read FEventAfterInitWithArgs
                                               write FEventAfterInitWithArgs;
    /// <summary>
    ///  The callback function for polling.
    ///  The polling function should return zero if all is well, and return
    ///  negative if it wants Ghostscript to abort. This is often used for
    ///  checking for a user cancel. This can also be used for handling window
    ///  events or cooperative multitasking.
    /// </summary>
    /// <remarks>
    ///  The polling function is called very frequently during interpretation
    ///  and rendering so it must be fast. If the function is slow, then using a
    ///  counter to return 0 immediately some number of times can be used to
    ///  reduce the performance impact.
    /// </remarks>
    property OnPoll: TNotifyFuncEvent read FEventPoll write FEventPoll;
    /// <summary>
    ///  StdError from the Ghostscript library
    /// </summary>
    property OnStdError: TGSEvent_Std read FEventStdError write FEventStdError;
    /// <summary>
    ///  StdIn from the Ghostscript library
    /// </summary>
    property OnStdIn: TGSEvent_Std read FEventStdIn write FEventStdIn;
    /// <summary>
    ///  StdOut from the Ghostscript library. This event will be also used for
    ///  debug informations.
    /// </summary>
    property OnStdOut: TGSEvent_Std read FEventStdOut write FEventStdOut;
    /// <summary>
    ///  The revision information of the Ghostscript DLL
    /// </summary>
    property Revision: TGS_Revision read FRevision;
    /// <summary>
    ///  The API stores the full informations of the StdOut here
    /// </summary>
    property StdInLog: TStrings read GetLogStdIn;
    /// <summary>
    ///  The API stores the full informations of the StdOut here
    /// </summary>
    property StdOutLog: TStrings read GetLogStdOut;
  public (*** PUBLIC METHODS ***)
    /// <summary>
    ///  default constructor
    /// </summary>
    constructor Create; overload;
    /// <summary>
    ///  constructor with the path to the dll
    /// </summary>
    constructor Create(ADllPath: string); overload;
    /// <summary>
    ///  destructor
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///  call <see cref="SkiSys.GS_Dll|gsapi_exit" />
    /// </summary>
    procedure Exit; virtual;
    /// <summary>
    ///  Main method to execute Ghostscript commands
    /// </summary>
    function InitWithArgs(AStrings: TStrings): Boolean; overload; virtual;
    /// <summary>
    ///  Main method to execute Ghostscript commands in a thread. This API will
    ///  only execute one thread at a time. After the thread is finished the
    ///  OnAfterExecute event is called.
    /// </summary>
    procedure InitWithArgsStart(AStrings: TStrings);
    /// <summary>
    ///  Checks if AResult = AError
    /// </summary>
    function IsError(AResult: Integer; AError: gs_error_type): Boolean;
    /// <summary>
    ///  Executes gsapi_run_file without any file checks
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunFile(AFile: string; AUserErrors: Integer; out AExitCode: Integer): Integer;
    /// <summary>
    ///  Executes gsapi_run_string
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunString(AStr: string; AUserErrors: Integer; out AExitCode: Integer): Integer;
    /// <summary>
    ///  Executes gsapi_run_begin
    ///  This method have to be called before RunStringContinue. Make sure
    ///  to call RunStringEnd at the end, otherwise Ghostscript is waiting for
    ///  more input.
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunStringBegin(AUserErrors: Integer; out AExitCode: Integer): Integer;
    /// <summary>
    ///  Executes gsapi_run_string_continue
    ///  This method have to be called after RunStringBegin
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunStringContinue(AStr: string; ALength: Cardinal;
                               AUserErrors: Integer; out AExitCode: Integer): Integer;
    /// <summary>
    ///  Executes gsapi_run_string_end
    ///  This method have to be called at the end of RunStringBegin/RunStringContinue,
    ///  otherwise Ghostscript will wait for more input.
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunStringEnd(AUserErrors: Integer; out AExitCode: Integer): Integer;
    /// <summary>
    ///  Executes gsapi_run_string_with_length
    ///  This method have to be called after InitWithArgs and before Exit.
    /// </summary>
    /// <remarks>
    ///  All Run* operations should be called after InitWidthArgs and before Exit.
    ///  For this operations you have to set NoExit to true and to call Exit by
    ///  your self or use the event <see cref="SkiSys.GS_Api|TGS_Api.OnAfterInitWithArgs"/>
    /// </remarks>
    function RunStringWithLength(AStr: string; ALength: Cardinal;
                                 AUserErrors: Integer; out AExitCode: Integer): Integer;
  end;

{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  /// <summary>
  ///  Adds a directory to the search path used to locate DLLs for the application.
  /// </summary>
  function SetDllDirectory(lpPathName: LPCTSTR): LongBool; stdcall;
             external 'kernel32.dll' name 'SetDllDirectoryW';
  {$ENDIF}
{$ENDIF}

implementation

{$REGION 'Callback Functions'}

function StrPCopy(Dest: PAnsiChar; Source: AnsiString): PAnsiChar;
begin
  Result := {$IFDEF DELPHI}System.AnsiStrings.{$ENDIF}
            {$IFDEF FPC}SysUtils.{$ENDIF}
            StrPCopy(Dest, Source);
end;

function GSCallout(instance: Pointer; callout_handle: Pointer;
                   const device_name: PAnsiChar;
                   id, size: Integer; data: Pointer): Integer;
                   {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  ADisplayCallback: p_gs_display_get_callback_t;
  AApi: TGS_Api;
begin
  Result := -1;
  AApi := TGS_Api(callout_handle);
  if (AApi = nil) then
    raise EGS_ApiException.Create('TGS_Api object not found', 'GSCallout', -1);
  // only check the display callback
  if ((device_name <> nil) and ({$IFDEF DELPHI}System.AnsiStrings.{$ENDIF}
                                StrComp(device_name, 'display') = 0)) then
  begin
    case id of
      DISPLAY_CALLOUT_GET_CALLBACK:
      begin
        // we have to use the given pointer, that the record will get back
        ADisplayCallback := p_gs_display_get_callback_t(data);
        ADisplayCallback^.callback := @AApi.GSDisplay.FCallback;
        ADisplayCallback^.caller_handle := AApi.GSDisplay;
        AApi.DebugLog('TGSDisplay: Display structure initialized');
        Result := 0;
      end;
    end;
  end;
end;

function GSStdIn(ACaller: Pointer; ABuffer: PAnsiChar;
  ALen: Integer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  Text: AnsiString;
  AApi: TGS_Api;
begin
  //TODO: Check the StdIn, because its usally an input
  Text := '';
  AApi := TGS_Api(ACaller);
  AApi.StdIn(string(Text));
  StrPCopy(ABuffer, Text);
  Result := ALen;
end;

function GSStdOut(ACaller: Pointer; const ABuffer: PAnsiChar;
  ALen: Integer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  AStr: AnsiString;
  AApi: TGS_Api;
begin
  Result := 0;
  if (ALen > 0) then
  begin
    AApi := TGS_Api(ACaller);
    SetString(AStr, ABuffer, ALen);
    AApi.StdOut(string(AStr));
    Result := ALen;
  end;
end;

function GSStdErr(ACaller: Pointer; const ABuffer: PAnsiChar;
  ALen: Integer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  AStr: AnsiString;
  AApi: TGS_Api;
begin
  Result := 0;
  if (ALen > 0) then
  begin
    AApi := TGS_Api(ACaller);
    SetString(AStr, ABuffer, ALen);
    AApi.StdError(string(AStr));
    Result := ALen;
  end;
end;

function GSPoll(ACaller: Pointer): Integer; {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Result := TGS_Api(ACaller).Poll;
end;

{$ENDREGION}

{$REGION 'Display Callback Functions'}

function GSDisplayAdjustBandHeight(handle, device: Pointer;
                                   bandheight: Integer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventAdjustBandHeight(device, bandheight);
end;

function GSDisplayClose(handle, device: Pointer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventClose(device);
end;

procedure GSDisplayMemAlloc(handle, device: Pointer; size: SIZE_T); cdecl;
begin
  TGS_Display(handle).EventMemAlloc(device, size);
end;

function GSDisplayMemFree(handle, device, mem: Pointer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventMemFree(device, mem);
end;

function GSDisplayOpen(handle: Pointer; device: Pointer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventOpen(device);
end;

function GSDisplayPage(handle, device: Pointer;
                       copies, flush: Integer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventPage(device, copies, flush);
end;

function GSDisplayPreclose(handle, device: Pointer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventPreclose(device);
end;

function GSDisplayPresize(handle, device: Pointer;
                          width, height, raster: Integer;
                          format: Cardinal): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventPresize(device, width, height, raster, format);
end;

function GSDisplayRectangleRequest(handle, device, memory: Pointer;
                                   out raster, plane_raster: Integer;
                                   out x, y, w, h: Integer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventRectangleRequest(device, memory,
                                                      raster, plane_raster,
                                                      x, y, w, h);
end;

function GSDisplaySeparation(handle, device: Pointer;
                             component: Integer; const component_name: PAnsiChar;
                             c, m, y, k: Word): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventSeparation(device, component, component_name,
                                                c, m, y, k);
end;

function GSDisplaySize(handle, device: Pointer;
                       width, height, raster: Integer;
                       format: Cardinal; pimage: PByte): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventSize(device, width, height, raster,
                                          format, pimage);
end;

function GSDisplaySync(handle, device: Pointer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventSync(device);
end;

function GSDisplayUpdate(handle, device: Pointer;
                         x, y, w, h: Integer): Integer; cdecl;
begin
  Result := TGS_Display(handle).EventUpdate(device, x, y, w, h);
end;

{$ENDREGION}

{$REGION 'TStrSyncObject' }

constructor TStrSyncObject.Create(AEvent: TGSEvent_Std; AStr: string);
begin
  FEvent := AEvent;
  FStr := AStr;
end;

procedure TStrSyncObject.Execute;
begin
  FEvent(FStr);
end;

{$ENDREGION}

{$REGION 'EGS_ApiException' }

function EGS_ApiException.GetMessage(AValue: string): string;
begin
  FInternalMessage := AValue;
  if (FFunctionName <> '') then
    Result := Format(MessageFunctionFmt, [FErrorCode, FFunctionName,
                                          FInternalMessage])
  else
    Result := Format(MessageErrorCodeFmt, [FErrorCode, FFunctionName,
                                           FInternalMessage]);
end;

constructor EGS_ApiException.Create(const Msg, AFunctionName: string;
  error: Integer);
begin
  FFunctionName := AFunctionName;
  FErrorCode := error;
  inherited Create(GetMessage(Msg));
end;

constructor EGS_ApiException.CreateFmt(const Msg, AFunctionName: string;
  error: Integer; const Args: array of const);
begin
  FFunctionName := AFunctionName;
  FErrorCode := error;
  inherited CreateFmt(GetMessage(Msg), Args);
end;

{$ENDREGION}

{$REGION 'TGS_Api' }

constructor TGS_Api.Create;
begin
  Init('');
end;

function TGS_Api.CheckRunResult(AResult: Integer): Integer;
begin
  Result := AResult;
  if (AResult <= ord(gs_error_Fatal)) and
     (AResult <> ord(gs_error_NeedInput)) and
     (AResult <> ord(gs_error_NeedFile)) then
  begin
    // call Exit
    Result := CheckResult(-101);
  end;
end;

function TGS_Api.CheckRunString: Boolean;
begin
  Result := FInitWithArgs and not FExit;
  if (not Result) then
    SetLastError('RunString* operations have to be called after InitWithArgs ' +
                 'and before Exit');
end;

procedure TGS_Api.AfterExecute;
begin
  if (Assigned(FEventAfterExecute)) then
    FEventAfterExecute(Self);
end;

procedure TGS_Api.AfterInitWithArgs;
begin
  if (Assigned(FEventAfterInitWithArgs)) then
    FEventAfterInitWithArgs(Self);
end;

function TGS_Api.CheckResult(AResult: Integer): Integer;
begin
  Result := AResult;
  if (IsError(Result, gs_error_Quit)) then
  begin
    // we need to call gsapi_exit, this is not an error -> see SkiSys.GS_Errors
    Exit;
    Result := 0;
  end;
end;

procedure TGS_Api.ClearInternalLog;
begin
  FLastErrors.Clear;
  FLastError := '';
  FLastErrorCode := 0;
  FLogStdIn.Clear;
  FLogStdOut.Clear;
end;

constructor TGS_Api.Create(ADllPath: string);
begin
  Init(ADllPath);
end;

procedure TGS_Api.DebugLog(AText: string);
begin
  if (Debug) then
    StdOutLine(AText);
end;

procedure TGS_Api.FireEvent(AEvent: TGSEvent_Std; AStr: string);
var
  ASync: TStrSyncObject;
begin
  if (Assigned(AEvent)) and (AStr <> '') then
  begin
    if (FThread <> nil) then
    begin
      ASync := TStrSyncObject.Create(AEvent, AStr);
      try
        TThread.Synchronize(FThread, ASync.Execute);
      finally
        FreeAndNil(ASync);
      end;
    end else
      AEvent(AStr);
  end;
end;

destructor TGS_Api.Destroy;
begin
  //FreeAndNil(FCriticalSection);
  FreeAndNil(FDefaultDeviceList);
  FreeAndNil(GSDisplay);
  FreeAndNil(FDebugParams);
  FreeAndNil(FLastErrors);
  FreeAndNil(FLogStdIn);
  FreeAndNil(FLogStdOut);
  FreeAndNil(FRevision);
  FreeGSInstance;
  inherited;
end;

procedure TGS_Api.Exit;
var
  AError: Integer;
begin
  (* When gsapi_exit fails it can have different issues
   *   1. read to much bytes from PImage pointer
   *   2. the instance of the api can't be used anymore
   *)
  if (not FExit) then
  begin
    try
      AError := gsapi_exit(FInstance);
      FExit := AError > -1;
      if (FExit) then
        DebugLog('gsapi_exit called succesfully')
      else
        SetLastError(Format('Error(%d) on gsapi_exit', [AError]));
    except
      on E: Exception do
      begin
        SetLastError('Warning(-1) gsapi_exit failed: ' + E.Message, 0);
        FExit := True; // set it to true to avoid more errors
      end;
    end;
  end;
end;

procedure TGS_Api.FreeGSInstance;
begin
  if (Assigned(FInstance)) then
  begin
    gsapi_deregister_callout(FInstance, @GSCallout, Self);
    gsapi_delete_instance(FInstance);
    FInstance := nil;
    FExit := False;
    FInitWithArgs := False;
  end;
end;

function TGS_Api.GetAnsiStrArray(AStrings: TStrings): TAnsiStringArray;
var
  i: Integer;
begin
  SetLength(Result, AStrings.Count);
  for i := 0 to High(Result) do
    Result[i] := AnsiString(AStrings[i]);
end;

function TGS_Api.GetDefaultDeviceList: TStrings;
var
  PStr: PList;
  ALen, i: Integer;
  AError: string;
begin
  Result := nil;
  ALen := 0;
  if (FDefaultDeviceList = nil) then
  begin
    // we need to check if InitWidthArgs was be called before, because this
    // function can only be excuted before gsapi_init_with_args
    if (FInitWithArgs) then
      InitGSInstance;

    FLastErrorCode := CheckResult(gsapi_get_default_device_list(FInstance,
                                                                @PStr, ALen));
    if (FLastErrorCode = 0) then
    begin
      FDefaultDeviceList := TStringList.Create;
      Result := FDefaultDeviceList;
      for i := 0 to ALen - 1 do
      begin
        if (PStr^[i] <> '') then
          FDefaultDeviceList.Add(String(AnsiString(PStr^[i])));
      end;
    end else
    begin
      AError := Format('Error on GetDefaultDeviceList: error_code=%d', [FLastErrorCode]);
      SetLastError(AError, 0); // set the error and not the error_code
    end;
  end else
    Result := FDefaultDeviceList;
end;

function TGS_Api.GetLastErrors: string;
begin
  Result := FLastErrors.Text;
end;

function TGS_Api.GetLogStdIn: TStrings;
begin
  Result := FLogStdIn;
end;

function TGS_Api.GetLogStdOut: TStrings;
begin
  Result := FLogStdOut;
end;

function TGS_Api.GetThreadRunning: Boolean;
begin
  Result := (FThread <> nil) and (FThread.CheckTerminated);
end;

function TGS_Api.GetPAnsiCharArray(AAnsiStrings: TAnsiStringArray): PArgv;
var
  i: Integer;
begin
  SetLength(Result, High(AAnsiStrings) + 1);
  for i := 0 to High(Result) do
    Result[i] := PAnsiChar(AAnsiStrings[i]);
end;

procedure TGS_Api.Init(ADllPath: string);
var
  ADllFile: string;
begin
  InitInternalLog;
  SetDefaultValues;
  ADllFile := GS_DLL;
  FDllPath := Trim(ADllPath);
  if (FDllPath <> '') then
  begin
    if (not FDllPath.EndsWith(PathDelim)) then
      FDllPath := FDllPath + PathDelim;
    ADllFile := FDllPath + GS_DLL;
    {$IFDEF MSWINDOWS}
    SetDllDirectory(PChar(FDllPath));
    {$ENDIF}
  end else
    ADllFile := ExpandFileName(ADllFile);

  {$IFDEF MSWINDOWS}
  if (not FileExists(ADllFile)) then
    raise EFileNotFoundException.CreateFmt('couldn''t find the Ghostscript Dll at %s', [ADllFile]);
  {$ENDIF}

  DebugParams := TGSDebugParams.Create;
  GSDisplay := TGS_Display.Create(Self);

  InitGSInstance;
end;

procedure TGS_Api.InitGSInstance;
begin
  if (not Assigned(FInstance)) or (FInitWithArgs) then
  begin
    FreeGSInstance;

    FLastErrorCode := gsapi_new_instance(FInstance, @Self);
    if (FLastErrorCode < 0) then
      raise EGS_ApiException.Create('GS Instance couldn''t be created!', '',
                                    FLastErrorCode);
    if (FRevision = nil) then
      FRevision := TGS_Revision.Create(Self)
    else
      FRevision.GetRevision(Self);
    SetCallouts;
  end;
end;

procedure TGS_Api.InitInternalLog;
begin
  if (FLastErrors = nil) then
    FLastErrors := TStringList.Create;
  if (FLogStdIn = nil) then
    FLogStdIn := TStringList.Create;
  if (FLogStdOut = nil) then
    FLogStdOut := TStringList.Create;
end;

function TGS_Api.InitWithArgs(AArgs: PArgv): Boolean;
var
  AError: Boolean;
begin
  AError := False;
  try
    try
      // create a new instance, if gsapi_init_with_args was used before
      // to prevent a fatal error from ghostscript at the 2nd try
      InitGSInstance;
      // set the argument encoding
      FLastErrorCode := gsapi_set_arg_encoding(FInstance, Integer(FArgumentEncoding));
      if (FLastErrorCode < 0) then
        raise EGS_ApiException.Create('', 'gsapi_set_arg_encoding', FLastErrorCode);
      // call init_with_args
      FLastErrorCode := CheckResult(gsapi_init_with_args(FInstance,
                                                         High(AArgs) + 1, AArgs));
      AfterInitWithArgs;
    except
      on E: Exception do
      begin
        SetLastError('Error InitWithArgs: ' + E.Message, -1);
        AError := True;
      end;
    end;
  finally
    FInitWithArgs := True;
    if (not FNoExit) and (not AError) then
      Self.Exit;
  end;
  Result := FLastErrorCode = 0;
end;

function TGS_Api.Poll: Integer;
begin
  Result := 0;
  if (Assigned(FEventPoll)) then
    Result := FEventPoll(Self);
end;

function TGS_Api.QuoteCmdParameter(const AParam: string): string;
var
  AValues: TArray<string>;
begin
  Result := AParam;
  if (Result.Contains(' ')) then
  begin
    if (Result.Contains('=')) then
    begin
      AValues := Result.Split(['=']);
      Result := AValues[0] + '=' + AValues[1].QuotedString('"');
    end else
    if (not Result.StartsWith('-')) then // quote a filename
    begin
      Result := Result.QuotedString('"');
    end;
  end;
end;

function TGS_Api.InitWithArgs(AStrings: TStrings): Boolean;
var
  AAnsiStrs: TAnsiStringArray;
  AArgs: PArgv;
  i: Integer;
begin
  if (AStrings <> nil) and (AStrings.Count > 0) then
  begin
    ClearInternalLog;
    if (Debug) then
    begin
      FDebugLastCmdArgs := GS_CMD_EXE;
      StdOutLine('---  Debug Init Parameters  ---');
      for i := 0 to AStrings.Count - 1 do
      begin
        StdOutLine('SetParam: ' + AStrings[i]);
        // check the arguments for whitspaces
        FDebugLastCmdArgs := FDebugLastCmdArgs + ' ' + QuoteCmdParameter(AStrings[i]);
      end;
      StdOutLine('--- END ---');
    end;
    if (DebugShowCmdArgs) then
    begin
      StdOutLine('--- CMD Args ---');
      StdOutLine(FDebugLastCmdArgs);
      StdOutLine('--- CMD Args END ---');
    end;

    AAnsiStrs := GetAnsiStrArray(AStrings);
    AArgs := GetPAnsiCharArray(AAnsiStrs);
    Result := InitWithArgs(AArgs);
    if (not Self.FThreadUsed) then
      ThreadFinished(Self);
  end else
    raise EGS_ApiException.Create('No parameters set, operation canceled',
                                  'InitWithArgs', -1010);
end;

procedure TGS_Api.InitWithArgsStart(AStrings: TStrings);
begin
  // only 1 thread can run at a time
  if (not FThreadUsed) then
  begin
    Self.FThreadUsed := True;
    FThread := TGS_ApiThread.Create(Self, AStrings);
  end;
end;

function TGS_Api.IsError(AResult: Integer; AError: gs_error_type): Boolean;
begin
  Result := AResult = Integer(AError);
end;

function TGS_Api.RunFile(AFile: string; AUserErrors: Integer;
  out AExitCode: Integer): Integer;
begin
  Result := CheckRunResult(gsapi_run_file(FInstance, PAnsiChar(AnsiString(AFile)),
                                          AUserErrors, AExitCode));
  if (Result < 0) then
    FLastErrorCode := Result;
end;

function TGS_Api.RunString(AStr: string; AUserErrors: Integer;
  out AExitCode: Integer): Integer;
begin
  Result := -1;
  if (CheckRunString) then
  begin
    Result := CheckRunResult(gsapi_run_string(FInstance, PAnsiChar(AnsiString(AStr)),
                                              AUserErrors, AExitCode));
    if (Result < 0) then
      FLastErrorCode := Result;
  end;
end;

function TGS_Api.RunStringBegin(AUserErrors: Integer;
  out AExitCode: Integer): Integer;
begin
  Result := -1;
  if (CheckRunString) then
  begin
    Result := CheckRunResult(gsapi_run_string_begin(FInstance, AUserErrors, AExitCode));
    if (Result < 0) then
      FLastErrorCode := Result;
  end;
end;

function TGS_Api.RunStringContinue(AStr: string; ALength: Cardinal;
  AUserErrors: Integer; out AExitCode: Integer): Integer;
begin
  Result := -1;
  if (CheckRunString) then
  begin
    Result := CheckRunResult(gsapi_run_string_continue(FInstance,
                               PAnsiChar(AnsiString(AStr)), ALength,
                               AUserErrors, AExitCode));
    if (Result < 0) then
      FLastErrorCode := Result;
  end;
end;

function TGS_Api.RunStringEnd(AUserErrors: Integer;
  out AExitCode: Integer): Integer;
begin
  Result := -1;
  if (CheckRunString) then
  begin
    Result := CheckRunResult(gsapi_run_string_end(FInstance, AUserErrors, AExitCode));
    if (Result < 0) then
      FLastErrorCode := Result;
  end;
end;

function TGS_Api.RunStringWithLength(AStr: string; ALength: Cardinal;
  AUserErrors: Integer; out AExitCode: Integer): Integer;
begin
  Result := -1;
  if (CheckRunString) then
  begin
    Result := CheckRunResult(gsapi_run_string_with_length(FInstance,
                               PAnsiChar(AnsiString(AStr)), ALength,
                               AUserErrors, AExitCode));
    if (Result < 0) then
      FLastErrorCode := Result;
  end;
end;

procedure TGS_Api.SetDefaultDeviceList(ADeviceList: TStrings);
begin
  if (FDefaultDeviceList <> ADeviceList) then
  begin
    if (FDefaultDeviceList = nil) then
      FDefaultDeviceList := TStringList.Create;

    FDefaultDeviceList.Assign(ADeviceList);
    FDefaultDeviceList.Delimiter := ' ';
    FLastErrorCode := CheckResult(gsapi_set_default_device_list(FInstance,
                        PAnsiChar(AnsiString(FDefaultDeviceList.Text)),
                        FDefaultDeviceList.Text.Length));
    if (FLastErrorCode < 0) then
      SetLastErrorInternal('Error on SetDefaultDeviceList', FLastErrorCode);
  end;
end;

procedure TGS_Api.SetDefaultValues;
begin
  FDllPath := GetCurrentDir;
  FDebug := False;
  FDefaultDeviceList := nil; // will be initialized as nil and when needed filled
  FInstance := nil;
  FLastError := '';
  FLastErrorCode := 0;
  FArgumentEncoding := GS_ARG_ENCODING_UTF8;
  FInitWithArgs := False;
  FExit := False;
end;

procedure TGS_Api.SetLastError(AText: string; AErrorCode: Integer = -1);
begin
  if (AText <> '') then
  begin
    SetLastErrorInternal(AText, AErrorCode);
    // push the external event
    if (Assigned(FEventStdError)) then
      FEventStdError(AText);
  end;
end;

procedure TGS_Api.SetLastError(AText: string; AErrorCode: gs_error_type);
begin
  SetLastError(AText, Integer(AErrorCode));
end;

procedure TGS_Api.SetLastErrorCode(ACode: gs_error_type);
begin
  FLastErrorCode := Integer(ACode);
end;

procedure TGS_Api.SetLastErrorInternal(AText: string; AErrorCode: Integer = -1);
begin
  FLastError := AText;
  FLastErrors.Add(AText);
  if (AErrorCode <> 0) then
    FLastErrorCode := AErrorCode;
end;

procedure TGS_Api.SetParams(AList: TStringList);
begin
  DebugParams.SetParams(AList);
end;

procedure TGS_Api.SetCallouts;
begin
  if (gsapi_set_stdio_with_handle(FInstance, @GSStdIn, @GSStdOut, @GSStdErr, Self) <> 0) then
    raise Exception.Create('Could not set stdio functions');
  // for display operations
  if (gsapi_set_poll_with_handle(FInstance, @GSPoll, Self) <> 0) then
    raise Exception.Create('Could not set poll function');

  if (gsapi_register_callout(FInstance, @GSCallout, Self) <> 0) then
    raise Exception.Create('Could not register callout handler');
  GSDisplay.Clear;
end;

procedure TGS_Api.StdError(AText: string);
var
  AStr: string;
begin
  // Filter Product and Version
  if (not AText.StartsWith(FRevision.Product)) and
     (not AText.StartsWith(FRevision.RevisionStr)) and
     (AText <> ': ') then
  begin
    AStr := AText.Replace(#10#10, #13#10);
    AStr := AStr.Replace(#10, #13#10);
    // internal stuff
    SetLastErrorInternal(AText, 0);
    // push the external event
    FireEvent(FEventStdError, AStr);
    //if (Assigned(FEventStdError)) then
    //  FEventStdError(AStr);
  end;
end;

procedure TGS_Api.StdIn(AText: string);
begin
  FLogStdIn.Add(AText);
  if (Assigned(FEventStdIn)) then
    FEventStdIn(AText);
end;

procedure TGS_Api.StdOut(AText: string);
var
  AStr: string;
begin
  AStr := AText.Replace(#10, #13#10);
  if (AStr.Contains('error')) then //sometimes the error is written to stdout
    StdError(AText);
  FLogStdOut.Text := FLogStdOut.Text + AStr;
  FireEvent(FEventStdOut, AStr);
  //if (Assigned(FEventStdOut)) then
  //  FEventStdOut(AStr);
end;

procedure TGS_Api.StdOutLine(AText: string);
begin
  StdOut(AText + #10);
end;

procedure TGS_Api.ThreadFinished(Sender: TObject);
begin
  FThread := nil;
  AfterExecute;
end;

{$ENDREGION}

{$REGION 'TGS_Revision' }

procedure TGS_Revision.CheckRevision;
begin
{$IFNDEF DONT_CHECK_GS_REVISION}
  if (Revision < MIN_GHOSTSCRIPT_REVISION) then
    raise EInvalidGhostscriptVersionException.Create(
      'This Ghostscript version is not supported by the API');
{$ENDIF}
end;

constructor TGS_Revision.Create(AApi: TGS_API);
begin
  FApi := AApi;
  GetRevision(AApi);
end;

procedure TGS_Revision.GetRevision(AApi: TGS_API);
var
  AError: string;
  ARevision: gsapi_revision_t;
  AOuterException: Exception;
begin
  try
    AApi.FLastErrorCode := gsapi_revision(@ARevision, SizeOf(ARevision));
    if (AApi.FLastErrorCode = 0) then
    begin
      FProduct := string(AnsiString(ARevision.product));
      FCopyright := string(AnsiString(ARevision.copyright));
      FRevision := ARevision.revision;
      FRevisionStr := GetRevisonStr;
      FRevisionDate := ARevision.revisiondate;
      CheckRevision;
    end else
    // I don't raise an error here, because the API should still work
    if (AApi.FLastErrorCode > 0) then
      FApi.SetLastError('Error on GetRevsion: wrong structure size', 0);
  except
    on E: Exception do
    begin
      AError := 'Error on TGS_Revision.GetRevision: ' + E.Message;
      AApi.SetLastError(AError);
      AOuterException := Exception.Create(AError);
      {$IFDEF DELPHI}
      E.RaiseOuterException(AOuterException);
      {$ENDIF}
      {$IFDEF FPC}
      raise AOuterException at get_caller_addr(get_frame), get_caller_frame(get_frame);
      {$ENDIF}
    end;
  end;
end;

function TGS_Revision.GetRevisonStr: string;
var
  AStr: string;
  ARevisionMajorLen: Integer;
begin
  ARevisionMajorLen := 1;
  if (Revision >= 10000) then
    inc(ARevisionMajorLen);
  AStr := IntToStr(Revision);
  Result := Format('%s.%s.%s', [copy(AStr, 1, ARevisionMajorLen),     //revsion major
                                copy(AStr, ARevisionMajorLen + 1, 2), //revision minor
                                copy(AStr, ARevisionMajorLen + 3, Length(AStr))]);
end;

{$ENDREGION}

{$REGION 'TGS_ApiThread' }

constructor TGS_ApiThread.Create(AApi: TGS_Api; Args: TStrings);
begin
  FreeOnTerminate := True;
  FApi := AApi;
  FArgs := Args;
  OnTerminate := AApi.ThreadFinished;
  inherited Create(False);
end;

destructor TGS_ApiThread.Destroy;
begin
  if (FArgs <> nil) then
    FreeAndNil(FArgs);
  FApi.FThreadUsed := False;
  inherited Destroy;
end;

procedure TGS_ApiThread.Execute;
begin
  if (not Terminated) then
  begin
    try
      FApi.InitWithArgs(FArgs);
    except
      on E: Exception do
        FApi.SetLastError('Error on TGS_ApiThread.Execute: ' + E.Message);
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGS_Displays' }

procedure TGS_Display.Clear;
begin
  FImageList.Clear;
end;

constructor TGS_Display.Create(AApi: TGS_Api);
begin
  Init(AApi);
end;

procedure TGS_Display.DebugLog(AMessage: string);
begin
  if (FDebug) then
    FApi.DebugLog(AMessage);
end;

procedure TGS_Display.DebugLogFmt(AFormat: string; const Args: array of const);
begin
  DebugLog(Format(AFormat, Args));
end;

destructor TGS_Display.Destroy;
begin
  if (FImageList <> nil) then
    FreeAndNil(FImageList);
  inherited;
end;

function TGS_Display.EventAdjustBandHeight(ADevice: Pointer;
  ABandHeight: Integer): Integer;
begin
  DebugLogFmt('TGS_Displays.EventMemAlloc: device=%p band_height=%d',
              [ADevice, ABandHeight]);
  if (Assigned(FEventAdjustBandHeight)) then
    Result := FEventAdjustBandHeight(ADevice, ABandHeight)
  else
    Result := 0;
end;

function TGS_Display.EventClose(ADevice: Pointer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventClose: close device %p', [ADevice]);
  if (Assigned(FEventClose)) then
    Result := FEventClose(ADevice)
  else
    Result := 0;
end;

procedure TGS_Display.EventMemAlloc(ADevice: Pointer; ASize: SIZE_T);
begin
  DebugLogFmt('TGS_Displays.EventMemAlloc: device=%p size=%d',
              [ADevice, Cardinal(ASize)]);
  //can be used to implement to allocate your own memory for the bitmap
  if (Assigned(FEventMemAlloc)) then
    FEventMemAlloc(ADevice, ASize);
end;

function TGS_Display.EventMemFree(ADevice, AMem: Pointer): Integer;
begin
  DebugLogFmt('TGS_Displays.EventMemFree: device=%p mem=%p',
              [ADevice, AMem]);
  if (Assigned(FEventMemFree)) then
    Result := FEventMemFree(ADevice, AMem)
  else
    Result := 0;
end;

function TGS_Display.EventOpen(ADevice: Pointer): Integer;
const
  DebugMsg = 'TGSDisplays.EventOpen: open device %p';
begin
  DebugLogFmt(DebugMsg, [ADevice]);
  FImageList.InitImageData(ADevice);
  if (Assigned(FEventOpen)) then
    Result := FEventOpen(ADevice)
  else
    Result := 0;
end;

function TGS_Display.EventPage(ADevice: Pointer; ACopies, AFlush: Integer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventPage: copies=%d flush=%d', [ACopies, AFlush]);
  FImageList.AddFromImageData;
  if (Assigned(FEventPage)) then
    Result := FEventPage(ADevice, ACopies, AFlush)
  else
    Result := 0;
end;

function TGS_Display.EventPreclose(ADevice: Pointer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventPreclose: device=%p', [ADevice]);
  if (Assigned(FEventPreClose)) then
    Result := FEventPreClose(ADevice)
  else
    Result := 0;
end;

function TGS_Display.EventPresize(ADevice: Pointer;
  AWidth, AHeight, ARaster: Integer; AFormat: Cardinal): Integer;
const
  DebugMsg = 'TGSDisplays.EventPresize: width=%d height=%d raster=%d format=%d';
begin
  DebugLogFmt(DebugMsg, [AWidth, AHeight, ARaster, AFormat]);
  if (Assigned(FEventPresize)) then
    Result := FEventPresize(ADevice, AWidth, AHeight, ARaster, AFormat)
  else
    Result := 0;
end;

function TGS_Display.EventRectangleRequest(ADevice, AMemory: Pointer;
  out ARaster, APlaneRaster: Integer; out X, Y, W, H: Integer): Integer;
const
  DebugMsg = 'TGSDisplays.EventRectangleRequest: ' +
             'memory=%p raster=%d plane_raster=%d x=%d y=%d w=%d h=%d';
begin
  if (Assigned(FEventRectangleRequest)) then
  begin
    Result := FEventRectangleRequest(ADevice, AMemory, ARaster, APlaneRaster,
                                     X, Y, W, H);
    DebugLogFmt(DebugMsg, [AMemory, ARaster, APlaneRaster, X, Y, W, H]);
  end
  else
    Result := 0;
end;

function TGS_Display.EventSeparation(ADevice: Pointer;
  AComponent: Integer; AComponentName: PAnsiChar; C, M, Y, K: Word): Integer;
const
  DebugMsg = 'TGSDisplays.EventSeparation: device=%p' +
             'component=%d component_name=%d c=%d m=%d y=%d k=%d';
begin
  DebugLogFmt(DebugMsg, [ADevice, AComponent, AComponentName, C, M, Y, K]);
  if (Assigned(FEventSeparation)) then
    Result := FEventSeparation(ADevice, AComponent, String(AnsiString(AComponentName)),
                               C, M, Y, K)
  else
    Result := 0;
end;

function TGS_Display.EventSize(ADevice: Pointer;
  AWidth, AHeight, ARaster: Integer; AFormat: Cardinal; PImage: PByte): Integer;
const
  DebugMsg = 'TGSDisplays.EventSize: device=%p width=%d height=%d ' +
                                    'raster=%d format=%d pimage=%p';
begin
  DebugLogFmt(DebugMsg, [ADevice, AWidth, AHeight, ARaster, AFormat,
                         PImage]);
  FImageList.SetDataAndSize(AWidth, AHeight, ARaster, AFormat, PImage);
  if (Assigned(FEventSize)) then
    Result := FEventSize(ADevice, AWidth, AHeight, ARaster, AFormat, PImage)
  else
    Result := 0;
end;

function TGS_Display.EventSync(ADevice: Pointer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventSync: sync device=%p', [ADevice]);
  if (Assigned(FEventSync)) then
    Result := FEventSync(ADevice)
  else
    Result := 0;
end;

function TGS_Display.EventUpdate(ADevice: Pointer;
  X, Y, W, H: Integer): Integer;
begin
  // this event is called really often, don't use debug messages here
  if (Assigned(FEventUpdate)) then
    Result := FEventUpdate(ADevice, X, Y, W, H)
  else
    Result := 0;
end;

function TGS_Display.GetPage(AIdx: Integer): TGS_Image;
begin
  Result := nil;
  if (AIdx > -1) and (AIdx < FImageList.Count) then
    Result := FImageList[AIdx];
end;

function TGS_Display.GetPageCount: Integer;
begin
  Result := FImageList.Count;
end;

procedure TGS_Display.Init(AApi: TGS_Api);
begin
  FApi := AApi;
  // Init Callback
  FCallback.size := SizeOf(display_callback);
  FCallback.version_major := DISPLAY_VERSION_MAJOR;
  FCallback.version_minor := DISPLAY_VERSION_MINOR;
  FCallback.display_open := GSDisplayOpen;
  FCallback.display_preclose := GSDisplayPreclose;
  FCallback.display_close := GSDisplayClose;
  FCallback.display_presize := GSDisplayPresize;
  FCallback.display_size := GSDisplaySize;
  FCallback.display_sync := GSDisplaySync;
  FCallback.display_page := GSDisplayPage;
  FCallback.display_update := GSDisplayUpdate;
{$IFDEF USE_GSDisplayMemAlloc}
  FCallback.display_memalloc := GSDisplayMemAlloc;
  FCallback.display_memfree := GSDisplayMemFree;
{$ELSE}
  FCallback.display_memalloc := nil;
  FCallback.display_memfree := nil;
{$ENDIF}
  FCallback.display_separation := GSDisplaySeparation;
  FCallback.display_adjust_band_height := GSDisplayAdjustBandHeight;
  FCallback.display_rectangle_request := GSDisplayRectangleRequest;

  FImageList := TGS_ImageList.Create(Self);
end;

{$ENDREGION}

{$REGION 'TGS_ImageData' }

procedure TGS_ImageData.SetDataAndSize(Width, Height, Raster: Integer;
  Format: Cardinal; PImage: PByte);
begin
  // sets the image data, size and other infos
  Self.Raster := Raster;
  Self.ImageData := PImage;
  Self.Format := Format;
  Self.Width := Width;
  Self.Height := Height;
end;

{$ENDREGION}

{$REGION 'TGS_Image' }

constructor TGS_Image.Create(ADisplays: TGS_Display;
  AWidth, AHeight, ARaster: Integer; AFormat: Cardinal; PImage: PByte);
begin
  inherited Create;
  FGS_ImageDataLoaded := False;
  FDisplay := ADisplays;
  FGS_Format.Format := AFormat;
  FGS_Raster := ARaster;
  SetBmpInfoHeader(AWidth, AHeight);
  SetImageData(PImage);
end;

function TGS_Image.GetGrayLuminance(AColor: TColor;
  const ALum: array of Byte): TColor;
var
  Luminance: Byte;
begin
  Result := 0;
  if (High(ALum) >= 2) then
  begin
    Luminance := ( ((AColor and $00FF0000) shr 16 * ALum[0]) +
                   ((AColor and $0000FF00) shr 8 * ALum[1]) +
                   ((AColor and $000000FF) * ALum[2])) shr 8;
    Result := (AColor and $FF000000) or (Luminance shl 16) or
              (Luminance shl 8) or Luminance;
  end;
end;

function TGS_Image.GetGS_Format: Cardinal;
begin
  Result := FGS_Format.Format;
end;

function TGS_Image.ConvertImageDataLine(ASrcLine, ADestLine: PByte): Boolean;
var
  ASrcBytes, ADestBytes: PByte;
  i: Integer;
begin
  Result := False;
  case (FGS_Format.Color) of
    DISPLAY_COLORS_NATIVE:
    begin
      if (FGS_Format.Depth = DISPLAY_DEPTH_16) then
      begin
        if (FGS_Format.Endian = DISPLAY_LITTLEENDIAN) then
        begin
          if (FGS_Format.Native555 = DISPLAY_NATIVE_555) then
          begin // BGR555
            for i := 0 to Width - 1 do
            begin
              ASrcBytes := ASrcLine + i * 2;
              {$IFDEF LINUX}ADestBytes := ADestLine + i * 4;{$ENDIF}
              {$IFDEF MSWINDOWS}ADestBytes := ADestLine + i * 3;{$ENDIF}
              ConvertNative555(ASrcBytes, ADestBytes);
            end;
            Result := True;
          end else
          begin // BGR565
            for i := 0 to Width - 1 do
            begin
              ASrcBytes := ASrcLine + i * 2;
              {$IFDEF LINUX}ADestBytes := ADestLine + i * 4;{$ENDIF}
              {$IFDEF MSWINDOWS}ADestBytes := ADestLine + i * 3;{$ENDIF}
              ConvertNative565(ASrcBytes, ADestBytes);
            end;
            Result := True;
          end;
        end;
      end else
      if (FGS_Format.Depth = DISPLAY_DEPTH_8) then
      begin

      end;
    end;
    DISPLAY_COLORS_GRAY:
    begin
      if (FGS_Format.Depth = DISPLAY_DEPTH_8) then
      begin
        {$IFDEF MSWINDOWS}
        // convert to 24-bit gray scale
        for i := 0 to Width - 1 do
        begin
          ADestBytes := ADestLine + i * 3;
          ADestBytes[0] := (ASrcLine + i)[0];
          ADestBytes[1] := (ASrcLine + i)[0];
          ADestBytes[2] := (ASrcLine + i)[0];
        end;
        Result := True;
        {$ENDIF}
        // Linux direct copy
      end;
    end;
    DISPLAY_COLORS_RGB:
    begin
      if (FGS_Format.Depth = DISPLAY_DEPTH_8) then
      begin
        if ( ((FGS_Format.Alpha = DISPLAY_ALPHA_FIRST) or
              (FGS_Format.Alpha = DISPLAY_UNUSED_FIRST)) and
             (FGS_Format.Endian = DISPLAY_BIGENDIAN) ) then
        begin // Mac Format
          for i := 0 to Width - 1 do
          begin
            ASrcBytes := ASrcLine + i * 4;
            ADestBytes := ADestLine + i * 3;
            // first byte of the source is a filler
            ADestBytes[0] := ASrcBytes[1];
            ADestBytes[1] := ASrcBytes[2];
            ADestBytes[2] := ASrcBytes[3];
          end;
          Result := True;
        end else
        if ( (FGS_Format.Endian = DISPLAY_LITTLEENDIAN) ) then
        begin
          if ( (FGS_Format.Alpha = DISPLAY_UNUSED_LAST) or
               (FGS_Format.Alpha = DISPLAY_ALPHA_LAST)) then
          begin // Windows format + alpha = BGRx
            {$IFDEF MSWINDOWS}
            for i := 0 to Width - 1 do
            begin
              ASrcBytes := ASrcLine + i * 4;
              ADestBytes := ADestLine + i * 3;
              ADestBytes[0] := ASrcBytes[0];
              ADestBytes[1] := ASrcBytes[1];
              ADestBytes[2] := ASrcBytes[2];
              // last byte of the source is a filler
            end;
            Result := True;
            {$ENDIF}
            //Linux direct copy
          end else
          if ( (FGS_Format.Alpha = DISPLAY_UNUSED_FIRST) or
               (FGS_Format.Alpha = DISPLAY_ALPHA_FIRST)) then
          begin // xBGR
            {$IFDEF LINUX}
            for i := 0 to Width - 1 do
            begin
              ASrcBytes := ASrcLine + i * 4;
              ADestBytes := ADestLine + i * 4;
              ADestBytes[0] := ASrcBytes[1];
              ADestBytes[1] := ASrcBytes[2];
              ADestBytes[2] := ASrcBytes[3];
              ADestBytes[3] := ASrcBytes[0];
            end;
            Result := True;
            {$ENDIF}
            //For Windows we don't need to convert it
          end else
          if (FGS_Format.Alpha = DISPLAY_ALPHA_NONE) then
          begin
            {$IFDEF LINUX}
            for i := 0 to Width - 1 do
            begin
              ASrcBytes := ASrcLine + i * 3;
              ADestBytes := ADestLine + i * 4;
              ADestBytes[0] := ASrcBytes[0];
              ADestBytes[1] := ASrcBytes[1];
              ADestBytes[2] := ASrcBytes[2];
              ADestBytes[3] := 0; // we need to add a filler
            end;
            Result := True;
            {$ENDIF}
            //For Windows we don't need to convert it
          end;
        end;
      end;
    end;
    DISPLAY_COLORS_CMYK:
    begin
      //no support for CMYK
    end;
  end;
end;

procedure TGS_Image.ConvertNative555(ASrcBytes, ADestBytes: PByte);
var
  w: UShort;
  AValue: Integer;
begin
  w := ASrcBytes[0] + (ASrcBytes[1] shl 8);
  AValue := (w shr 10) and $1F; //red
  ADestBytes[2] := (AValue shl 3) + (AValue shr 2);
  AValue := (w shr 5) and $1F;   // green
  ADestBytes[1] := (AValue shl 3) + (AValue shr 2);
  AValue := w and $1F;          // blue
  ADestBytes[0] := (AValue shl 3) + (AValue shr 2);
end;

procedure TGS_Image.ConvertNative565(ASrcBytes, ADestBytes: PByte);
var
  w: UShort;
  AValue: Integer;
begin
  w := ASrcBytes[0] + (ASrcBytes[1] shl 8);
  AValue := (w shr 11) and $1F; //red
  ADestBytes[0] := (AValue shl 3) + (AValue shr 2);
  AValue := (w shr 5) and $3F;   // green
  ADestBytes[1] := (AValue shl 3) + (AValue shr 2);
  AValue := w and $1F;          // blue
  ADestBytes[2] := (AValue shl 3) + (AValue shr 2);
end;

function TGS_Image.GetBitsPerPixel: Integer;
begin
  Result := -1;
  if (FBmpInfoHeader.biBitCount <= 1) then
    Result := 1
  else
  if (FBmpInfoHeader.biBitCount <= 4) then
    Result := 4
  else
  if (FBmpInfoHeader.biBitCount <= 8) then
    Result := 8
  else
  if (FBmpInfoHeader.biBitCount > 24) then
    Result := 32
  else
  if (FBmpInfoHeader.biBitCount > 8) then
    Result := 24;
end;

function TGS_Image.GetBytesPerLine: Integer;
begin
  Result := ( ((FBmpInfoHeader.biWidth * FBmpInfoHeader.biBitCount + 31)
               and not NativeUInt(31)) + 7 ) shr 3;
end;

function TGS_Image.GetScanLine(AIndex: Integer): Pointer;
begin
  Result := {$IFDEF FPC}GetRawImagePtr^.GetLineStart(AIndex){$ENDIF}
            {$IFDEF DELPHI}Scanline[AIndex]{$ENDIF};
end;

constructor TGS_Image.Create(ADisplays: TGS_Display; AData: TGS_ImageData);
begin
  inherited Create;
  FGS_ImageDataLoaded := False;
  FDisplay := ADisplays;
  FGS_Format.Format := AData.Format;
  FGS_Raster := AData.Raster;
  SetBmpInfoHeader(AData.Width, AData.Height);
  SetImageData(AData.ImageData);
end;

function TGS_Image.GetPixelFormatFromBits: TPixelFormat;
begin
  case (FBitsPerPixel) of
    1: Result := pf1bit;
    4: Result := pf4bit;
    8: Result := pf8bit;
    15: Result := pf15bit;
    16: Result := pf16bit;
    24: Result := pf24bit;
    32: Result := pf32bit;
    else
      Result := pfDevice;
  end;
end;

procedure TGS_Image.MemCopy(Source, Dest: Pointer; ALength: Integer);
begin
  Move(Source^, Dest^, SizeInt(ALength));
end;

procedure TGS_Image.SetBmpInfoHeader(AWidth, AHeight: Integer);
begin
  FBmpInfoHeader.biSize := SizeOf(BmpInfoHeader);
  FBmpInfoHeader.biHeight := AHeight;
  FBmpInfoHeader.biWidth := AWidth;

  FBmpInfoHeader.biPlanes := 1;
  case (FGS_Format.Color) of
    DISPLAY_COLORS_NATIVE:
      case (FGS_Format.Depth) of
        DISPLAY_DEPTH_1:
        begin
          FBmpInfoHeader.biBitCount := 1;
          FBmpInfoHeader.biClrUsed := 2;
          FBmpInfoHeader.biClrImportant := 2;
        end;
        DISPLAY_DEPTH_4:
        begin
          FBmpInfoHeader.biBitCount := 4;
          FBmpInfoHeader.biClrUsed := 16;
          FBmpInfoHeader.biClrImportant := 16;
        end;
        DISPLAY_DEPTH_8:
        begin
          FBmpInfoHeader.biBitCount := 8;
          FBmpInfoHeader.biClrUsed := 96;
          FBmpInfoHeader.biClrImportant := 96;
        end;
        DISPLAY_DEPTH_16:
        begin
          if (FGS_Format.Endian = DISPLAY_BIGENDIAN) then
          begin
            FBmpInfoHeader.biBitCount := 24;
            FBmpInfoHeader.biClrUsed := 0;
            FBmpInfoHeader.biClrImportant := 0;
          end else
          begin
            FBmpInfoHeader.biBitCount := 16;
            FBmpInfoHeader.biClrUsed := 0;
            FBmpInfoHeader.biClrImportant := 0;
          end;
        end;
        else exit;
    end;
    DISPLAY_COLORS_GRAY:
      case (FGS_Format.Depth) of
        DISPLAY_DEPTH_1:
        begin
          FBmpInfoHeader.biBitCount := 1;
          FBmpInfoHeader.biClrUsed := 2;
          FBmpInfoHeader.biClrImportant := 2;
        end;
        DISPLAY_DEPTH_4:
        begin
          FBmpInfoHeader.biBitCount := 4;
          FBmpInfoHeader.biClrUsed := 16;
          FBmpInfoHeader.biClrImportant := 16;
        end;
        DISPLAY_DEPTH_8:
        begin
          if (FGS_Format.FirstRow = DISPLAY_BOTTOMFIRST) then //Windows
          begin
            FBmpInfoHeader.biBitCount := 24;
            FBmpInfoHeader.biClrUsed := 0;
            FBmpInfoHeader.biClrImportant := 0;
          end else
          begin
            FBmpInfoHeader.biBitCount := 8;
            FBmpInfoHeader.biClrUsed := 256;
            FBmpInfoHeader.biClrImportant := 256;
          end;
        end;
        else exit; //TODO: raise an error
    end;
    DISPLAY_COLORS_RGB:
    begin
      if (FGS_Format.Depth <> DISPLAY_DEPTH_8) then
        exit;
      if ((FGS_Format.Alpha = DISPLAY_UNUSED_FIRST) and
          (FGS_Format.FirstRow = DISPLAY_BOTTOMFIRST)) then
      begin
        FBmpInfoHeader.biBitCount := 32;
        FBmpInfoHeader.biClrUsed := 0;
        FBmpInfoHeader.biClrImportant := 0;
      end else
      begin
        FBmpInfoHeader.biBitCount := 24;
        FBmpInfoHeader.biClrUsed := 0;
        FBmpInfoHeader.biClrImportant := 0;
      end;
    end;
    DISPLAY_COLORS_CMYK:
    begin
      FBmpInfoHeader.biBitCount := 24;
      FBmpInfoHeader.biClrUsed := 0;
      FBmpInfoHeader.biClrImportant := 0;
      //TODO: covert it ->dwing.c
    end;
    DISPLAY_COLORS_SEPARATION:
    begin
      FBmpInfoHeader.biBitCount := 24;
      FBmpInfoHeader.biClrUsed := 0;
      FBmpInfoHeader.biClrImportant := 0;
    end;
  end;
  FBmpInfoHeader.biCompression := 0;
  FBmpInfoHeader.biSizeImage := 0;
  FBmpInfoHeader.biXPelsPerMeter := 0;
  FBmpInfoHeader.biYPelsPerMeter := 0;
  FBitsPerPixel := GetBitsPerPixel;
end;

procedure TGS_Image.SetImageData(PImage: PByte);
var
  DestBytes, CurrentBytes: PByte;
  i, ALineIdx: Integer;
begin
  if (Assigned(PImage)) then
  begin
    // initialize the size of the image
    SetSize(FBmpInfoHeader.biWidth, FBmpInfoHeader.biHeight);
    PixelFormat := GetPixelFormatFromBits;

    for i := 0 to FBmpInfoHeader.biHeight - 1 do
    begin
      // get the pointer to the image data of the bitmap line
      DestBytes := GetScanLine(i);
      ALineIdx := i;
      if (FGS_Format.FirstRow = DISPLAY_BOTTOMFIRST) then
        // In Windows we will paint the image bottom first, so we need to start
        // at the last row and end at the first row
        ALineIdx := FBmpInfoHeader.biHeight - 1 - i;
      CurrentBytes := PImage + FGS_Raster * ALineIdx;
      // convert and copy the data if needed
      // when not needed the function returns false and we do a direct copy
      if (not ConvertImageDataLine(CurrentBytes, DestBytes)) then
        // copy the image data buffer to the bitmap memory
        MemCopy(CurrentBytes, DestBytes, FGS_Raster);
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGS_ImageList' }

function TGS_ImageList.AddFromImageData: Integer;
begin
  Result := inherited Add(TGS_Image.Create(FDisplays, FImageData));
end;

constructor TGS_ImageList.Create(ADisplays: TGS_Display);
begin
  inherited Create(True);
  FDisplays := ADisplays;
end;

procedure TGS_ImageList.InitImageData(ADevice: Pointer);
begin
  FImageData.Device := ADevice;
  FImageData.Raster := 0;
  FImageData.Format := 0;
  FImageData.ImageData := nil;
  FImageData.Width := 0;
  FImageData.Height := 0;
end;

procedure TGS_ImageList.SetDataAndSize(AWidth, AHeight, ARaster: Integer;
  AFormat: Cardinal; PImage: PByte);
begin
  FImageData.SetDataAndSize(AWidth, AHeight, ARaster, AFormat, PImage);
end;

{$ENDREGION}

end.
