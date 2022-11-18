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

unit SkiSys.GS_Converter;

{$IFDEF FPC} //Free Pascal
  {$MODE DELPHI}
  {$H+}
{$ELSE} //Delphi
  {$DEFINE DELPHI}
{$ENDIF}

interface

uses
  SkiSys.GS_API, SkiSys.GS_ParameterTypes, SkiSys.GS_ParameterConst,
  SkiSys.GS_Errors
{$IFDEF DELPHI}
  , System.Classes, System.SysUtils
{$ENDIF}
{$IFDEF FPC}
  , Classes, SysUtils
{$ENDIF}
  ;

type
  TGS_Converter = class(TGS_Api)
  private
    FUserParams: TStringList;
  protected
    /// <summary>
    ///  Check that all InFiles exists
    /// </summary>
    function CheckFiles(const InFiles: array of string): Boolean; virtual;
    /// <summary>
    ///  Set all parameters and check the files, if everything is ok this function
    ///  will execute InitWithArgs
    /// </summary>
    function Convert(const InFiles: array of string; OutFile: string;
                     Threaded: Boolean = False): Boolean;
    /// <summary>
    ///  initialize the API
    /// </summary>
    procedure Init(ADllPath: string); override;
    /// <summary>
    ///  Set the OutputFile as File and expand the filename case sensitiv.
    /// </summary>
    /// <remarks>
    ///  override this function without inherited to specify other OutputFile
    ///  formats like printers
    /// </remarks>
    procedure SetOutputFile(AList: TStringList; AValue: string); virtual;
    /// <summary>
    ///  Override SetParams to include a list of UserParams
    /// </summary>
    procedure SetParams(AList: TStringList); override;
    /// <summary>
    ///  set the value of the private var FUserParams
    /// </summary>
    procedure SetUserParameters(const Value: TStringList); virtual;
    /// <summary>
    ///  Set or replace parameters with the UserParams
    /// </summary>
    procedure SetUserParams(AParams: TStringList); virtual;
    /// <summary>
    ///  Will be executed after InitWithArgs is finished
    /// </summary>
    procedure ThreadFinished(Sender: TObject); override;
  public
    // destructor
    destructor Destroy; override;
    /// <summary>
    ///  Main function to execute Ghostscript commands
    /// </summary>
    function InitWithArgs(AStrings: TStrings; Threaded: Boolean): Boolean; overload;
  public
    /// <summary>
    ///  A list of ghostscript parameters added before InitWithArgs is executed
    /// </summary>
    property UserParams: TStringList read FUserParams write SetUserParameters;
  end;

  TGS_PdfConverter = class(TGS_Converter)
  private
    FPDFAX_DefFile: string;
    FParams: TPDFAXParams;
  protected
    /// <summary>
    ///  initialize the API
    /// </summary>
    procedure Init(ADllPath: string); override;
    /// <summary>
    ///  Override SetParams to include the TPDFAXParams
    /// </summary>
    procedure SetParams(AList: TStringList); override;
  public (*** PROPERTIES AND VARS ***)
    /// <summary>
    ///  Some of the ghostscript parameters they are often used
    /// </summary>
    property Params: TPDFAXParams read FParams write FParams;
    /// <summary>
    ///  You can set your own PDFA-Definition ps file here.
    /// </summary>
    property PDFA_DefFile: string read FPDFAX_DefFile write FPDFAX_DefFile;
  public (*** METHODS ***)
    // destructor
    destructor Destroy; override;
    /// <summary>
    ///  Print the file on the printer. When the printer doesn't exists, a printer
    ///  will be shown under Windows.
    /// </summary>
    /// <param name="InFile">
    ///  The input file(s).
    ///  This function checks, if ";" is in the filename and split the files.
    /// </param>
    function Print(InFile: string; PrinterName: string;
                   Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Concat the given files and print them as 1 job on the printer. When the
    ///  printer doesn't exists a printer dialog will appear under Windows.
    /// </summary>
    function Print(const InFiles: array of string; PrinterName: string;
                   Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Convert the InFile to a PDF file
    /// </summary>
    /// <param name="InFile">
    ///  The input file(s).
    ///  This function checks, if ";" is in the filename and split the files.
    /// </param>
    function ToPdf(InFile, OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Combine and convert all InFiles to a PDF file
    /// </summary>
    function ToPdf(const InFiles: array of string; OutFile: string;
                   Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Convert the InFile to a PDF-A file
    /// </summary>
    /// <param name="InFile">
    ///  The input file(s).
    ///  This function checks, if ";" is in the filename and split the files.
    /// </param>
    function ToPdfa(InFile, OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Combine and convert all InFiles to a PDF-A file
    /// </summary>
    function ToPdfa(const InFiles: array of string; OutFile: string;
                    Threaded: Boolean = False): Boolean; overload;
  end;

implementation

{$REGION 'TGS_Converter'}

function TGS_Converter.CheckFiles(const InFiles: array of string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(InFiles) do
  begin
    // ignore empty items
    if (not InFiles[i].IsEmpty) and (not FileExists(InFiles[i])) then
    begin
      SetLastError(Format('The File: %s does not exist', [InFiles[i]]));
      Result := False;
    end;
  end;
  if (not Result) then
    SetLastErrorCode(gs_error_ioerror); // Error Code defined from Ghostscript
end;

function TGS_Converter.Convert(const InFiles: array of string;
  OutFile: string; Threaded: Boolean): Boolean;
var
  AList, FileList: TStringList;
  i: Integer;
begin
  Result := False;

  if (CheckFiles(InFiles)) then
  begin
    FileList := TStringList.Create;
    AList := TStringList.Create;
    try
      // Expand all Filenames to a Full Path
      for i := Low(InFiles) to High(InFiles) do
        if (not InFiles[i].IsEmpty) then
          FileList.Add(InFiles[i]);
      try
        // Set Debug and Other Params
        SetParams(AList);

        SetOutputFile(AList, OutFile);
        // All given files have to be in the Linux File Format,
        // otherwise gs may produce device input errors
        // The changes in version 10.00.0 need a case sensitive filename
        // so we add the files case sensitive and in Linux File Format
        for i := 0 to FileList.Count - 1 do
          AList.Add(TGSParams.GetFullLinuxFilePath(FileList[i]));
      except
        on E: Exception do
        begin
          SetLastError(E.Message);
          ThreadFinished(Self);
        end;
      end;
      Result := InitWithArgs(AList, Threaded);
    finally
      if (not Threaded) then
        FreeAndNil(AList);
      FreeAndNil(FileList);
    end;
  end else
    ThreadFinished(Self);
end;

destructor TGS_Converter.Destroy;
begin
  FreeAndNil(FUserParams);
  inherited;
end;

procedure TGS_Converter.Init(ADllPath: string);
begin
  inherited Init(ADllPath);
  FUserParams := TStringList.Create;
end;

function TGS_Converter.InitWithArgs(AStrings: TStrings; Threaded: Boolean): Boolean;
begin
  // add a threaded methode to execute InitWithArgs
  if (Threaded) then
  begin
    InitWithArgsStart(AStrings);
    Result := True;
  end else
    Result := inherited InitWithArgs(AStrings);
end;

procedure TGS_Converter.SetOutputFile(AList: TStringList; AValue: string);
begin
  if (AValue <> '') and (not AValue.StartsWith('%')) then
    AList.Add('-sOutputFile=' + TGSParams.GetFullLinuxFilePath(AValue, True))
end;

procedure TGS_Converter.SetParams(AList: TStringList);
begin
  inherited;
  SetUserParams(AList);
end;

procedure TGS_Converter.SetUserParameters(const Value: TStringList);
begin
  if (Value <> UserParams) then
  begin
    UserParams.Clear;
    if (Value <> nil) then
      UserParams.Assign(Value);
  end;
end;

procedure TGS_Converter.SetUserParams(AParams: TStringList);
var
  AParamName, AParam: string;
  i, j, idx: Integer;
begin
  for i := 0 to UserParams.Count - 1 do
  begin
    AParam := Trim(UserParams[i]);
    AParamName := AParam;
    if (AParamName.StartsWith('-')) then
    begin
      //try to find the param name with = at the end
      idx := AParamName.IndexOf('=');
      if (idx > -1) then
        AParamName := AParamName.Substring(0, idx);
      idx := -1;
      //TODO: SourceParam (could be not possible to detect)
      //the source param has no = at the end and we have to find in another way

      // try to find the param in the List
      for j := 0 to AParams.Count - 1 do
      begin
        if (AParams[j].StartsWith(AParamName)) then
        begin
          idx := j;
          break;
        end;
      end;

      if (idx > -1) then
      begin
        DebugLog(Format('Userparam %s overrides existing parameter %s', [AParam, AParams[idx]]));
        AParams[idx] := AParam;
      end else
      begin
        AParams.Add(Trim(UserParams[i]));
        DebugLog(Format('Userparam %s added', [AParam]));
      end;
    end;
  end;
end;

procedure TGS_Converter.ThreadFinished(Sender: TObject);
begin
  inherited;
  StdOutLine('---  Operation convert finished!  ---');
end;

{$ENDREGION}

{$REGION 'TGS_PdfConverter' }

destructor TGS_PdfConverter.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TGS_PdfConverter.Init(ADllPath: string);
begin
  inherited Init(ADllPath);
  FParams := TPDFAXParams.Create;
  // PDFA Params have some default settings, which we have to turn off
  Params.EmbededFonts := False;
  Params.SubsetFonts := True;
  Params.Pdfa := False;
  FPDFAX_DefFile := ADllPath + PathDelim + 'PDFA_def.ps';
end;

function TGS_PdfConverter.Print(InFile, PrinterName: string;
  Threaded: Boolean): Boolean;
begin
  Result := Print(InFile.Split([';']), PrinterName, Threaded);
end;

function TGS_PdfConverter.Print(const InFiles: array of string;
  PrinterName: string; Threaded: Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  // set the printer device for windows
  if (Params.Device <> DEVICES_PRINTERS[MSWindowsPrinter]) then
    Params.Device := DEVICES_PRINTERS[MSWindowsPrinter];
  if (PrinterName.StartsWith('%printer%')) then
    Params.OutputFile := '%printer%' + Printername
  else
    Params.OutputFile := Printername;
  {$ENDIF}
  //TODO: Add Linux Printers
  Result := Convert(InFiles, '', Threaded);
end;

procedure TGS_PdfConverter.SetParams(AList: TStringList);
begin
  inherited;
  Params.SetParams(AList);
end;

function TGS_PdfConverter.ToPdf(InFile, OutFile: string; Threaded: Boolean): Boolean;
begin
  Result := Convert(InFile.Split([';']), OutFile, Threaded);
end;

function TGS_PdfConverter.ToPdf(const InFiles: array of string; OutFile: string;
  Threaded: Boolean): Boolean;
begin
  Result := Convert(InFiles, OutFile, Threaded);
end;

function TGS_PdfConverter.ToPdfa(const InFiles: array of string; OutFile: string;
  Threaded: Boolean): Boolean;
var
  AFiles: array of string;
  i: Integer;
begin
  Params.Pdfa := True;
  SetLength(AFiles, High(InFiles) + 2);
  // PDFA Definition File has to be the first file
  AFiles[0] := FPDFAX_DefFile;
  for i := 1 to High(AFiles) do
    AFiles[i] := InFiles[i];
  Result := Convert(AFiles, OutFile, Threaded);
end;

function TGS_PdfConverter.ToPdfa(InFile, OutFile: string; Threaded: Boolean): Boolean;
begin
  Params.Pdfa := True;

  Result := ToPdfa(InFile.Split([';']), OutFile, Threaded);
end;

{$ENDREGION}

end.

