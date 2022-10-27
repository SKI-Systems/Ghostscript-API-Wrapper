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
  SkiSys.GS_API, SkiSys.GS_ParameterTypes, SkiSys.GS_Errors
{$IFDEF DELPHI}
  , System.Classes, System.SysUtils
{$ENDIF}
{$IFDEF FPC}
  , Classes, SysUtils
{$ENDIF}
  ;

type
  TGS_PdfConverter = class(TGS_Api)
  private
    FPDFAX_DefFile: string;
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
    ///  Override SetParams to include a list of UserParams
    /// </summary>
    procedure SetParams(AList: TStringList); override;
    /// <summary>
    ///  Set or replace parameters with the UserParams
    /// </summary>
    procedure SetUserParams(AParams: TStringList); virtual;
    /// <summary>
    ///  Will be executed after InitWithArgs is finished
    /// </summary>
    procedure ThreadFinished(Sender: TObject); override;
  public (*** PROPERTIES AND VARS ***)
    /// <summary>
    ///  Some of the ghostscript parameters they are often used
    /// </summary>
    Params: TPDFAXParams;
    /// <summary>
    ///  A list of ghostscript parameters added before InitWithArgs is executed
    /// </summary>
    UserParams: TStringList;
    /// <summary>
    ///  You can set your own PDFA-Definition ps file here.
    /// </summary>
    property PDFA_DefFile: string read FPDFAX_DefFile write FPDFAX_DefFile;
  public (*** METHODS ***)
    // destructor
    destructor Destroy; override;
    /// <summary>
    ///  Combine and convert all InFiles to a PDF file
    /// </summary>
    function ToPdf(const InFiles: array of string; OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Convert the InFile to a PDF file
    /// </summary>
    function ToPdf(InFile, OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Combine and convert all InFiles to a PDF-A file
    /// </summary>
    function ToPdfa(const InFiles: array of string; OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Convert the InFile to a PDF-A file
    /// </summary>
    function ToPdfa(InFile, OutFile: string; Threaded: Boolean = False): Boolean; overload;
    /// <summary>
    ///  Main function to execute Ghostscript commands
    /// </summary>
    function InitWithArgs(AStrings: TStrings; Threaded: Boolean): Boolean; overload;
  end;

implementation

{$REGION 'TGS_PdfConverter' }

function TGS_PdfConverter.CheckFiles(const InFiles: array of string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(InFiles) do
  begin
    if (not FileExists(InFiles[i])) then
    begin
      SetLastError(Format('The File: %s does not exist', [InFiles[i]]));
      Result := False;
    end;
  end;
  if (not Result) then
    SetLastErrorCode(gs_error_ioerror); // Error Code defined from Ghostscript
end;

function TGS_PdfConverter.Convert(const InFiles: array of string;
  OutFile: string; Threaded: Boolean): Boolean;
var
  Dir: string;
  AList, FileList: TStringList;
  i: Integer;
begin
  Result := False;
  OutFile := ExpandFileName(OutFile);

  if (CheckFiles(InFiles)) then
  begin
    FileList := TStringList.Create;
    AList := TStringList.Create;
    try
      // Expand all Filenames to a Full Path
      Dir := GetCurrentDir + '\';
      for i := Low(InFiles) to High(InFiles) do
        FileList.Add(InFiles[i]);
      try
        Params.SetParams(AList);
        // Set Debug and Other Params
        SetParams(AList);
        if (OutFile <> '') then
          AList.Add('-sOutputFile=' + Params.GetFullLinuxFilePath(OutFile, True));

        // All given files have to be in the Linux File Format,
        // otherwise gs may produce device input errors
        // The changes in version 10.00.0 need a case sensitive filename
        // so we add the files case sensitive and in Linux File Format
        for i := 0 to FileList.Count - 1 do
          AList.Add(Params.GetFullLinuxFilePath(FileList[i]));
      except
        on E: Exception do
        begin
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
  if (not Threaded) then
    ThreadFinished(Self);
end;

destructor TGS_PdfConverter.Destroy;
begin
  if (UserParams <> nil) then
    FreeAndNil(UserParams);
  if (Params <> nil) then
    FreeAndNil(Params);
  inherited;
end;

procedure TGS_PdfConverter.Init(ADllPath: string);
begin
  inherited Init(ADllPath);
  UserParams := TStringList.Create;
  Params := TPDFAXParams.Create;
  // PDFA Params have some default settings, which we have to turn off
  Params.EmbededFonts := False;
  Params.SubsetFonts := True;
  Params.Pdfa := False;
  FPDFAX_DefFile := ADllPath + '\PDFA_def.ps';
end;

function TGS_PdfConverter.InitWithArgs(AStrings: TStrings; Threaded: Boolean): Boolean;
begin
  // add a threaded methode to execute InitWithArgs
  if (Threaded) then
  begin
    InitWithArgsStart(AStrings);
    Result := True;
  end else
    Result := inherited InitWithArgs(AStrings);
end;

procedure TGS_PdfConverter.ThreadFinished(Sender: TObject);
begin
  StdOutLine('---  Operation convert finished!  ---');
  inherited;
end;

function TGS_PdfConverter.ToPdf(InFile, OutFile: string; Threaded: Boolean): Boolean;
begin
  Result := Convert([InFile], OutFile, Threaded);
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
  Result := Convert([FPDFAX_DefFile ,InFile], OutFile, Threaded);
end;

procedure TGS_PdfConverter.SetParams(AList: TStringList);
begin
  SetUserParams(AList);
  inherited;
end;

procedure TGS_PdfConverter.SetUserParams(AParams: TStringList);
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
      //TODO: SourceParam
      //the source param has no = at the end and we have to find otherwise

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

{$ENDREGION}

end.

