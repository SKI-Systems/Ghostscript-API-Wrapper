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

// Main unit to implement the Wrapper units in delphi classes
unit SkiSys.GS_Api;

interface

uses
  SkiSys.GS_Dll, SkiSys.GS_Types, SkiSys.GS_Errors, SkiSys.GS_ParameterTypes,
  SkiSys.GS_gdevdsp,

  System.Classes, System.SysUtils, WinApi.Windows, System.AnsiStrings,
  System.Generics.Collections, Vcl.Graphics;

{$MINENUMSIZE 4}

type
  TAnsiStringArray = array of AnsiString;
  TGSEvent_Std = procedure(const AText: String) of object;

  TGS_Api = class;

  /// <summary>
  ///  Thread to run InitWithArgs as a thread with the arguments
  /// </summary>
  TGS_ApiThread = class(TThread)
  private
    FArgs: TStrings;
    [WEAK]FApi: TGS_Api;
  protected
    procedure Execute; override;
  public
    constructor Create(AApi: TGS_Api; Args: TStrings);
  end;

  /// <summary>
  ///  Image record to store the image info
  /// </summary>
  TGS_ImageData = record
    Device: Pointer;
    Raster: Integer;
    Format: Cardinal;
    /// <summary>
    ///  Image data buffer
    /// </summary>
    ImageData: PByte;
    Height: Integer;
    Width: Integer;
    ByteWidth: Integer;
    procedure SetDataAndSize(Width, Height, Raster: Integer; Format: Cardinal;
                             PImage: PByte);
  end;

  TGS_Display = class;

  TGS_Image = class(Vcl.Graphics.TBitmap)
  private
    [WEAK]FDisplay: TGS_Display;
    FBmpInfoHeader: BITMAPINFOHEADER;
    FGS_Device: Pointer;
    FGS_Format: Cardinal;
    FGS_ImageData: PByte; //Pointer to the Data, will be filled after OnSync/OnPage
    FGS_ImageDataLoaded: Boolean;
    FGS_Raster: Integer;
    //FByteWidth: Integer;
  protected
    procedure SetBmpInfoHeader(AWidth, AHeight: Integer);
    procedure SetImageData(PImage: PByte);
    property BmpInfoHeader: BITMAPINFOHEADER read FBmpInfoHeader write FBmpInfoHeader;
    /// <summary>
    ///  Pointer to the image data buffer
    /// </summary>
    property ImageData: PByte read FGS_ImageData;
  public
    constructor Create(ADisplays: TGS_Display; AWidth, AHeight, ARaster: Integer;
                       AFormat: Cardinal; PImage: PByte); reintroduce; overload;
    constructor Create(ADisplays: TGS_Display; AData: TGS_ImageData); reintroduce; overload;
  public (*** PROPERTIES ***)
    property GS_Device: Pointer read FGS_Device;
    property GS_Format: Cardinal read FGS_Format;
    property GS_ImageDataLoaded: Boolean read FGS_ImageDataLoaded;
    property GS_Raster: Integer read FGS_Raster;
  end;

  TGS_ImageList = class(TObjectList<TGS_Image>)
  private
    FDisplays: TGS_Display;
    FImageData: TGS_ImageData;
  public
    /// <summary>
    ///  Create the image from the ImageData and add it to the list
    /// </summary>
    function AddFromImageData: Integer;
    // constructor
    constructor Create(ADisplays: TGS_Display);
    /// <summary>
    ///  Initialize the ImgaeData when the device is open
    /// </summary>
    procedure InitImageData(ADevice: Pointer);
    /// <summary>
    ///  Set the size and the buffer pointer for the image buffer. The image
    ///  buffer isn't filled at this moment.
    /// </summary>
    procedure SetDataAndSize(AWidth, AHeight, ARaster: Integer; AFormat: Cardinal;
                             PImage: PByte);
  end;

  //new definition of the events
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

  /// <summary>
  ///  The display class to create a preview
  /// </summary>
  TGS_Display = class
  private
    [WEAK]FApi: TGS_Api;
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
    procedure DebugLog(AMessage: string); overload;
    procedure DebugLogFmt(AFormat: string; const Args: array of const); overload;
    function EventAdjustBandHeight(ADevice: Pointer; ABandHeight: Integer): Integer; virtual;
    function EventClose(ADevice: Pointer): Integer; virtual;
    procedure EventMemAlloc(ADevice: Pointer; ASize: SIZE_T); virtual;
    function EventMemFree(ADevice, AMem: Pointer): Integer; virtual;
    function EventOpen(ADevice: Pointer): Integer; virtual;
    function EventPage(ADevice: Pointer; ACopies, AFlush: Integer): Integer; virtual;
    function EventPreclose(ADevice: Pointer): Integer; virtual;
    function EventPresize(ADevice: Pointer;
                          AWidth, AHeight, ARaster: Integer;
                          AFormat: Cardinal): Integer; virtual;
    function EventRectangleRequest(ADevice, AMemory: Pointer;
                                   out ARaster, APlaneRaster: Integer;
                                   out X, Y, W, H: Integer): Integer; virtual;
    function EventSeparation(ADevice: Pointer; AComponent: Integer;
                             AComponentName: PAnsiChar; C, M, Y, K: Word): Integer; virtual;
    function EventSize(ADevice: Pointer;
                       AWidth, AHeight, ARaster: Integer;
                       AFormat: Cardinal; PImage: PByte): Integer; virtual;
    function EventSync(ADevice: Pointer): Integer; virtual;
    function EventUpdate(ADevice: Pointer; X, Y, W, H: Integer): Integer; virtual;
    function GetPageCount: Integer; virtual;
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
    ///
    /// </summary>
    property OnAdjustBandHeight: TGS_DisplayAdjustBandHeightEvent read FEventAdjustBandHeight
                                                               write FEventAdjustBandHeight;
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
    ///
    /// </summary>
    /// <param name="ARaster">ARaster is byte count of a row.</param>
    property OnPresize: TGS_DisplayPresizeEvent read FEventPresize write FEventPresize;
    /// <summary>
    ///
    /// </summary>
    property OnRectangleRequest: TGS_DisplayRectangleRequestEvent read FEventRectangleRequest
                                                               write FEventRectangleRequest;
    /// <summary>
    ///
    /// </summary>
    property OnSeparation: TGS_DisplaySeparationEvent read FEventSeparation write FEventSeparation;
    /// <summary>
    ///  Device has been resized.
    ///  New pointer to raster returned in pimage
    /// </summary>
    property OnSize: TGS_DisplaySizeEvent read FEventSize write FEventSize;
    /// <summary>
    ///
    /// </summary>
    property OnSync: TGS_DisplayEvent read FEventSync write FEventSync;
    /// <summary>
    ///
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
    [WEAK]FApi: TGS_API;
  public
    Product: string;
    Copyright: string;
    Revision: LongInt;
    RevisionStr: string;
    RevisionDate: LongInt;
  public (*** PUBLIC METHODS ***)
    /// <summary>
    ///  create the object and read the revision data from the dll
    /// </summary>
    constructor Create(AApi: TGS_API); overload;
    /// <summary>
    ///  Read the revision data from the dll and fill the fields
    /// </summary>
    procedure GetRevision(AApi: TGS_API);
  end;

  /// <summary>
  ///  Base API to use the Ghostscript dll functions
  /// </summary>
  TGS_Api = class(TObject)
  private
    FRevision: TGS_Revision;
    FDebug: Boolean;
    FDebugParams: TGSDebugParams;
    FDllPath: string;
    FEventAfterExecute: TNotifyEvent;
    FEventStdError: TGSEvent_Std;
    FEventStdIn: TGSEvent_Std;
    FEventStdOut: TGSEvent_Std;
    FInitWithArgs: Boolean;
    FInstance: Pointer;
    FLastError: string;
    FLastErrors: TStringList; // list of errors occured during the process
    FLastErrorCode: Integer;
    FThreadRunning: Boolean;
    FThreadUsed: Boolean;
    function GetLastErrors: string;
    /// <summary>
    ///  Register all callouts to communicate with the Ghostscript library
    /// </summary>
    procedure SetCallouts;
    procedure SetLastErrorInternal(AText: string; AErrorCode: Integer = -1);
    function GetLogStdOut: TStrings;
  protected
    FLogStdIn: TStringList;  // log StdIn
    FLogStdOut: TStringList; // log StdOut
    /// <summary>
    ///  Clear the internal log vars
    /// </summary>
    procedure ClearInternalLog; virtual;
    /// <summary>
    ///  Write a log when the API is in Debug Mode
    /// </summary>
    procedure DebugLog(AText: string); virtual;
    /// <summary>
    ///  Free the gs_instance
    /// </summary>
    procedure FreeGSInstance; virtual;
    /// <summary>
    ///  Convert a TStrings object to a TAnsiStringArray
    /// </summary>
    function GetAnsiStrArray(AStrings: TStrings): TAnsiStringArray; virtual;
    /// <summary>
    ///  Convert a TAnsiStringArray to a PArgv Pointer
    /// </summary>
    function GetPAnsiCharArray(AAnsiStrings: TAnsiStringArray): PArgv; virtual;
    /// <summary>
    ///  Is called when the object will be created and initialize the API and the DLL
    /// </summary>
    procedure Init(ADllPath: string); virtual;
    /// <summary>
    ///  Get a new Instance Pointer for API calls, it has to be called everytime if
    ///  you want to use InitWithArgs again. (To avoid an fatal error from gsapi at 2nd call)
    /// </summary>
    procedure InitGSInstance; virtual;
    /// <summary>
    ///  Initialize the log vars to store the Ghostscript output
    /// </summary>
    procedure InitInternalLog; virtual;
    /// <summary>
    ///  Main methode of Ghostscript lirary to execute commands using pointers
    /// </summary>
    function InitWithArgs(AArgs: PArgv): Boolean; overload; virtual;
    /// <summary>
    ///  Set the default values of the class
    /// </summary>
    procedure SetDefaultValues; virtual;
    /// <summary>
    ///  Set the last error from the API and the Ghostscript lirary
    /// </summary>
    procedure SetLastError(AText: string; AErrorCode: Integer = -1); virtual;
    /// <summary>
    ///  Set the last error code from the API aund the Ghostscript lirary
    /// </summary>
    procedure SetLastErrorCode(ACode: gs_error_type);
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
    ///  Set the StdOut and filter some informations and call the OnStdOut Event
    /// </summary>
    procedure StdOut(AText: string); virtual;
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
    ///  Set Debug to True to get extended informations about which Params will be
    ///  set for the convert operation
    /// </summary>
    property Debug: Boolean read FDebug write FDebug;
    /// <summary>
    ///  Ghostscript Debug Parameters
    /// </summary>
    property DebugParams: TGSDebugParams read FDebugParams write FDebugParams;
    /// <summary>
    ///  The LastError
    /// </summary>
    property LastError: string read FLastError;
    /// <summary>
    ///  The LastErrors, we have to read the Error from StdError and we can get
    ///  more as one. So we put the all in a list.
    /// </summary>
    property LastErrors: string read GetLastErrors;
    /// <summary>
    ///  The Last Error Code
    /// </summary>
    property LastErrorCode: Integer read FLastErrorCode;
    /// <summary>
    ///  The event will be called after an InitWithArgs operation was executed
    /// </summary>
    property OnAfterExecute: TNotifyEvent read FEventAfterExecute write FEventAfterExecute;
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
    ///  Main methode to execute Ghostscript commands
    /// </summary>
    function InitWithArgs(AStrings: TStrings): Boolean; overload; virtual;
    /// <summary>
    ///  Main methode to execute Ghostscript commands in a thread. This API will
    ///  only execute one thread at a time. After the thread is finished the
    ///  OnAfterExecute Event is called.
    /// </summary>
    procedure InitWithArgsStart(AStrings: TStrings);
  end;

implementation

{$REGION 'Callback Functions'}

function GSCallout(instance: Pointer; callout_handle: Pointer;
                   device_name: PAnsiChar;
                   id, size: Integer; data: Pointer): Integer; stdcall;
var
  ADisplayCallback: p_gs_display_get_callback_t;
  AApi: TGS_Api;
begin
  Result := -1;
  AApi := TGS_Api(callout_handle);
  if (AApi = nil) then
    raise Exception.Create('GSCallout: TGS_Api object not found');
  // only check the display callback
  if ((device_name <> nil) and (System.AnsiStrings.StrComp(device_name, 'display') = 0)) then
  begin
    case id of
      DISPLAY_CALLOUT_GET_CALLBACK:
      begin
        // we have to use the given pointer, that the record will get back
        ADisplayCallback := p_gs_display_get_callback_t(data);
        ADisplayCallback.callback := @AApi.GSDisplay.FCallback;
        ADisplayCallback.caller_handle := AApi.GSDisplay;
        AApi.DebugLog('TGSDisplay: Display structure initialized');
        Result := 0;
      end;
    end;
  end;
end;

function GSStdIn(ACaller: Pointer; ABuffer: PAnsiChar; ALen: Integer) : Integer; stdcall;
var
  Text: AnsiString;
  AApi: TGS_Api;
begin
  Text := '';
  AApi := TGS_Api(ACaller);

  System.AnsiStrings.StrPCopy(ABuffer, Text);
  AApi.StdIn(string(Text));
  Result := Length(Text);
end;

function GSStdOut(ACaller: Pointer; ABuffer: PAnsiChar; ALen: Integer): Integer; stdcall;
var
  AStr: AnsiString;
  Buffer: PAnsiChar;
  AApi: TGS_Api;
begin
	GetMem(Buffer, ALen + 1);
	FillMemory(Buffer, ALen + 1, 0);
	CopyMemory(Buffer, ABuffer, ALen);

  AApi := TGS_Api(ACaller);
  AStr := AnsiString(Buffer);
	AApi.StdOut(string(AStr));

  FreeMem(Buffer);
	Result := ALen;
end;

function GSStdErr(ACaller: Pointer; ABuffer: PAnsiChar; ALen: Integer): Integer; stdcall;
var
  AStr: AnsiString;
  Buffer: PAnsiChar;
  AApi: TGS_Api;
begin
	GetMem(Buffer, ALen + 1);
	FillMemory(Buffer, ALen + 1, 0);
	CopyMemory(Buffer, ABuffer, ALen);

  AApi := TGS_Api(ACaller);
  AStr := AnsiString(Buffer);
  AApi.StdError(string(AStr));

  FreeMem(Buffer);
  Result := ALen;
end;

function GSPoll(ACaller: Pointer): Integer; stdcall;
begin
  Result := 0;
  //TGS_Api(ACaller).Poll;
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

{$REGION 'TGS_Api' }

constructor TGS_Api.Create;
begin
  Init('');
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
    StdOut(AText + #13#10);
end;

destructor TGS_Api.Destroy;
begin
  if (GSDisplay <> nil) then
    FreeAndNil(GSDisplay);
  if (FDebugParams <> nil) then
    FreeAndNil(FDebugParams);
  if (FLastErrors <> nil) then
    FreeAndNil(FLastErrors);
  if (FLogStdIn <> nil) then
    FreeAndNil(FLogStdIn);
  if (FLogStdOut <> nil) then
    FreeAndNil(FLogStdOut);
  if (FRevision <> nil) then
    FreeAndNil(FRevision);
  FreeGSInstance;
  inherited;
end;

procedure TGS_Api.FreeGSInstance;
begin
  if (Assigned(FInstance)) then
  begin
    gsapi_delete_instance(FInstance);
    FInstance := nil;
  end;
end;

function TGS_Api.GetAnsiStrArray(AStrings: TStrings): TAnsiStringArray;
var
  i: Integer;
begin
  SetLength(Result, AStrings.Count);
  for i := 0 to High(Result) do
  begin
    Result[i] := AnsiString(AStrings[i]);
  end;
end;

function TGS_Api.GetLastErrors: string;
begin
  Result := FLastErrors.Text;
end;

function TGS_Api.GetLogStdOut: TStrings;
begin
  Result := FLogStdOut;
end;

function TGS_Api.GetPAnsiCharArray(AAnsiStrings: TAnsiStringArray): PArgv;
var
  i: Integer;
begin
  SetLength(Result, High(AAnsiStrings) + 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := PAnsiChar(AAnsiStrings[i]);
  end;
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
    if (FDllPath[length(FDllPath)-1] <> '\') then
      FDllPath := FDllPath + '\';
    ADllFile := FDllPath + GS_DLL;
    SetDllDirectory(PChar(FDllPath));
  end;

  if (not FileExists(ADllFile)) then
    raise Exception.CreateFmt('couldn''t find the Ghostscript Dll at %s', [ADllFile]);

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
    if (FLastErrorCode <> 0) then
    begin
      raise Exception.CreateFmt('Error(%d): GS Instance couldn''t be created!', [FLastErrorCode]);
    end;
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
begin
  ClearInternalLog;
  try
    try
      // create a new instance, if gsapi_init_with_args was used before
      // to prevent a fatal error from ghostscript at the 2nd try
      InitGSInstance;
      gsapi_set_arg_encoding(FInstance, Integer(GS_ARG_ENCODING_UTF8));
      FLastErrorCode := gsapi_init_with_args(FInstance, High(AArgs) + 1, AArgs);
    except
      on E: Exception do
      begin
        FLastErrorCode := Integer(gs_error_unknownerror);
        SetLastError('Error InitWithArgs: ' + e.Message);
      end;
    end;
  finally
    FInitWithArgs := True;
    gsapi_exit(FInstance);
  end;
  Result := FLastErrorCode = 0;
end;

function TGS_Api.InitWithArgs(AStrings: TStrings): Boolean;
var
  AAnsiStrs: TAnsiStringArray;
  AArgs: PArgv;
  i: Integer;
begin
  Result := False;
  try
    if (AStrings <> nil) and (AStrings.Count > 0) then
    begin
      if (Debug) then
      begin
        StdOut('---  Debug Init Parameters  ---' + #13#10);
        for i := 0 to AStrings.Count - 1 do
          StdOut('SetParam: ' + AStrings[i] + #13#10);
        StdOut('---  end  ---' + #13#10);
      end;

      AAnsiStrs := GetAnsiStrArray(AStrings);
      AArgs := GetPAnsiCharArray(AAnsiStrs);
      Result := InitWithArgs(AArgs);
      if (not Self.FThreadUsed) then
        ThreadFinished(Self);
    end else
      raise Exception.Create('InitWithArgs: No parameters set, operation canceled');
  finally
    FThreadUsed := False;
  end;
end;

procedure TGS_Api.InitWithArgsStart(AStrings: TStrings);
begin
  // only 1 thread can run at a time
  Self.FThreadRunning := True;
  Self.FThreadUsed := True;
  TGS_ApiThread.Create(Self, AStrings);
end;

procedure TGS_Api.SetDefaultValues;
begin
  FDllPath := GetCurrentDir;
  FDebug := False;
  FInstance := nil;
  FLastError := '';
  FLastErrorCode := 0;
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
  if (gsapi_set_poll(FInstance, @GSPoll) <> 0) then
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
    SetLastErrorInternal(AText);
    // push the external event
    if (Assigned(FEventStdError)) then
      FEventStdError(AStr);
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
  FLogStdOut.Text := FLogStdOut.Text + AStr;
  if (Assigned(FEventStdOut)) then
    FEventStdOut(AStr);
end;

procedure TGS_Api.ThreadFinished(Sender: TObject);
begin
  FThreadRunning := False;
  if (Assigned(FEventAfterExecute)) then
    FEventAfterExecute(Self);
end;

{$ENDREGION}

{$REGION 'TGS_Revision' }

constructor TGS_Revision.Create(AApi: TGS_API);
begin
  FApi := AApi;
  GetRevision(AApi);
end;

procedure TGS_Revision.GetRevision(AApi: TGS_API);
var
  AError, AStr: string;
  ARevision: gsapi_revision_t;
begin
  try
    AApi.FLastErrorCode := gsapi_revision(@ARevision, sizeof(ARevision));
    if (AApi.FLastErrorCode > -1) then
    begin
      Self.Product := string(AnsiString(ARevision.product));
      Self.Copyright := string(AnsiString(ARevision.copyright));
      Self.Revision := ARevision.revision;
      AStr := IntToStr(ARevision.revision);
      Self.RevisionStr := Format('%s.%s.%s', [AStr[1], copy(AStr, 2, 2),
                                                       copy(AStr, 4, Length(AStr))]);
      Self.RevisionDate := ARevision.revisiondate;
    end;
  except
    on E: Exception do
    begin
      AError := 'Error on TGS_Revision.GetRevision: ' + E.Message;
      AApi.SetLastError(AError);
      raise Exception.Create(AError);
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGS_ApiThread' }

constructor TGS_ApiThread.Create(AApi: TGS_Api; Args: TStrings);
begin
  Self.FreeOnTerminate := True;
  Self.FApi := AApi;
  Self.FArgs := Args;
  Self.OnTerminate := AApi.ThreadFinished;
  inherited Create(False);
end;

procedure TGS_ApiThread.Execute;
begin
  if (not Terminated) then
  begin
    try
      FApi.InitWithArgs(FArgs);
    finally
      // free the ArgumentList
      FreeAndNil(FArgs);
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
  DebugLogFmt('TGS_Displays.EventMemAlloc: device=%d band_height=%d',
              [UInt(ADevice), UInt(ABandHeight)]);
  if (Assigned(FEventAdjustBandHeight)) then
    Result := FEventAdjustBandHeight(ADevice, ABandHeight)
  else
    Result := 0;
end;

function TGS_Display.EventClose(ADevice: Pointer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventClose: close device %d', [UInt(ADevice)]);
  if (Assigned(FEventClose)) then
    Result := FEventClose(ADevice)
  else
    Result := 0;
end;

procedure TGS_Display.EventMemAlloc(ADevice: Pointer; ASize: SIZE_T);
begin
  DebugLogFmt('TGS_Displays.EventMemAlloc: device=%d size=%d',
              [UInt(ADevice), UInt(ASize)]);
  //can be used to implement to allocate your own memory for the bitmap
  if (Assigned(FEventMemAlloc)) then
    FEventMemAlloc(ADevice, ASize);
end;

function TGS_Display.EventMemFree(ADevice, AMem: Pointer): Integer;
begin
  DebugLogFmt('TGS_Displays.EventMemFree: device=%d mem=%d',
              [UInt(ADevice), UInt(AMem)]);
  if (Assigned(FEventMemFree)) then
    Result := FEventMemFree(ADevice, AMem)
  else
    Result := 0;
end;

function TGS_Display.EventOpen(ADevice: Pointer): Integer;
const
  DebugMsg = 'TGSDisplays.EventOpen: open device %d';
begin
  DebugLogFmt(DebugMsg, [UInt(ADevice)]);
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
  DebugLog('TGSDisplays.EventPreclose');
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
             'memory=%d raster=%d plane_raster=%d x=%d y=%d w=%d h=%d';
begin
  DebugLogFmt(DebugMsg, [UInt(AMemory), ARaster, APlaneRaster, X, Y, W, H]);
  if (Assigned(FEventRectangleRequest)) then
    Result := FEventRectangleRequest(ADevice, AMemory, ARaster, APlaneRaster,
                                     X, Y, W, H)
  else
    Result := 0;
end;

function TGS_Display.EventSeparation(ADevice: Pointer;
  AComponent: Integer; AComponentName: PAnsiChar; C, M, Y, K: Word): Integer;
const
  DebugMsg = 'TGSDisplays.EventSeparation: device=%d' +
             'component=%d component_name=%d c=%d m=%d y=%d k=%d';
begin
  DebugLogFmt(DebugMsg, [UInt(ADevice), AComponent, AComponentName, C, M, Y, K]);
  if (Assigned(FEventSeparation)) then
    Result := FEventSeparation(ADevice, AComponent, String(AnsiString(AComponentName)),
                               C, M, Y, K)
  else
    Result := 0;
end;

function TGS_Display.EventSize(ADevice: Pointer;
  AWidth, AHeight, ARaster: Integer; AFormat: Cardinal; PImage: PByte): Integer;
const
  DebugMsg = 'TGSDisplays.EventSize: device=%d width=%d height=%d' +
                                    'raster=%d format=%d pimage=%d';
begin
  DebugLogFmt(DebugMsg, [UInt(ADevice), AWidth, AHeight, ARaster, AFormat,
                         UInt(PImage)]);
  FImageList.SetDataAndSize(AWidth, AHeight, ARaster, AFormat, PImage);
  if (Assigned(FEventSize)) then
    Result := FEventSize(ADevice, AWidth, AHeight, ARaster, AFormat, PImage)
  else
    Result := 0;
end;

function TGS_Display.EventSync(ADevice: Pointer): Integer;
begin
  DebugLogFmt('TGSDisplays.EventSync: sync device=%d', [UInt(ADevice)]);
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
{$ELSE}
  FCallback.display_memalloc := nil;
{$ENDIF}
  FCallback.display_memfree := GSDisplayMemFree;
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
  FGS_Format := AFormat;
  FGS_Raster := ARaster;
  SetBmpInfoHeader(AWidth, AHeight);
  SetImageData(PImage);
end;

constructor TGS_Image.Create(ADisplays: TGS_Display; AData: TGS_ImageData);
begin
  inherited Create;
  FGS_ImageDataLoaded := False;
  FDisplay := ADisplays;
  FGS_Format := AData.Format;
  FGS_Raster := AData.Raster;
  SetBmpInfoHeader(AData.Width, AData.Height);
  SetImageData(AData.ImageData);
end;

procedure TGS_Image.SetBmpInfoHeader(AWidth, AHeight: Integer);
begin
  FBmpInfoHeader.biSize := SizeOf(FBmpInfoHeader);
  FBmpInfoHeader.biHeight := AHeight;
  FBmpInfoHeader.biWidth := AWidth;

  FBmpInfoHeader.biPlanes := 1;
  case (FGS_Format and DISPLAY_COLORS_MASK) of
    DISPLAY_COLORS_NATIVE:
      case (FGS_Format and DISPLAY_DEPTH_MASK) of
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
          if (FGS_Format and DISPLAY_ENDIAN_MASK) = DISPLAY_BIGENDIAN then
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
      case (FGS_Format and DISPLAY_DEPTH_MASK) of
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
          FBmpInfoHeader.biClrUsed := 256;
          FBmpInfoHeader.biClrImportant := 256;
        end;
        else exit;
    end;
    Integer(DISPLAY_COLORS_RGB):
    begin
      if (FGS_Format and DISPLAY_DEPTH_MASK) <> DISPLAY_DEPTH_8 then
        exit;
      if (((FGS_Format and DISPLAY_ALPHA_MASK) = DISPLAY_UNUSED_LAST)and
          ((FGS_Format and DISPLAY_ENDIAN_MASK) = DISPLAY_LITTLEENDIAN)) then
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
    end;
  end;
  FBmpInfoHeader.biCompression := 0;
  FBmpInfoHeader.biSizeImage := 0;
  FBmpInfoHeader.biXPelsPerMeter := 0;
  FBmpInfoHeader.biYPelsPerMeter := 0;
  //FByteWidth := trunc(((FBmpInfoHeader.biWidth * FBmpInfoHeader.biBitCount + 31 ) and (65504)) / 8);
end;

procedure TGS_Image.SetImageData(PImage: PByte);
var
  ABmpInfo: BITMAPINFO;
begin
  if (Assigned(PImage)) then
  begin
    Self.Width := FBmpInfoHeader.biWidth;
    Self.Height := FBmpInfoHeader.biHeight;
    ABmpInfo.bmiHeader := FBmpInfoHeader;
    FGS_ImageDataLoaded := (SetDIBits(0, Self.Handle, 0, FBmpInfoHeader.biHeight,
                                      PImage, ABmpInfo, DIB_RGB_COLORS)) > 0;
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
