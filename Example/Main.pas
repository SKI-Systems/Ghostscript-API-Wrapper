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

unit Main;

interface

uses
  SkiSys.GS_Api, SkiSys.GS_Converter, SkiSys.GS_ParameterConst, SkiSys.GS_gdevdsp,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.UITypes,
  Vcl.ExtCtrls, System.IniFiles, Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components, Vcl.ComCtrls,
  Vcl.Graphics;

type
  TFMain = class(TForm)
    OpenDialog: TOpenDialog;
    Pages: TPageControl;
    Tab_Operation: TTabSheet;
    BBtn_Test: TBitBtn;
    LEd_PdfFile: TLabeledEdit;
    SBtn_OpenFile: TSpeedButton;
    M_UserParams: TMemo;
    M_Output: TMemo;
    M_Errors: TMemo;
    Tab_PDFView: TTabSheet;
    P_PDF_Top: TPanel;
    UpDown_Pages: TUpDown;
    LEd_PageCount: TLabeledEdit;
    ScrollBoxImage: TScrollBox;
    Img_Page: TImage;
    RGrp_Devices: TRadioGroup;
    procedure BBtn_TestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SBtn_OpenFileClick(Sender: TObject);
    procedure UpDown_PagesClick(Sender: TObject; Button: TUDBtnType);
  private
    FCurrentPage: Integer;
    PDFConvert: TGS_PdfConverter;
    GSDllDir: string;
    ICCProfileDir: string;
    Ini: TIniFile;
    function GetOutputDir(ADir, AName: string): string;
    procedure ReadIni;
    procedure SetCursorToEnd(AMemo: TMemo);
    procedure SetPage(APage: Integer);
    procedure SetPreview;
    procedure StdError(const AText: string);
    procedure StdIn(const AText: string);
    procedure StdOut(const AText: string);
    procedure ThreadFinished(Sender: TObject);
  protected
    procedure Convert;
    procedure Convert_DisplayPdf(SearchDir, InputFile: string; UseThread: Boolean);
    procedure Convert_DisplayPdfA(SearchDir, ICCProfileDir, InputFile: string; UseThread: Boolean);
    procedure Convert_WritePdf(SearchDir, InputFile: string; UseThread: Boolean);
    procedure Convert_WritePdfA(SearchDir, ICCProfileDir, InputFile: string; UseThread: Boolean);
  public
    { Public-Deklarationen }
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

procedure TFMain.BBtn_TestClick(Sender: TObject);
begin
  Convert;
end;

procedure TFMain.FormCreate(Sender: TObject);
var
  ADir: string;
begin
  ReadIni;
  ADir := GSDllDir;
  if (not DirectoryExists(ADir)) then
    ADir := '';

  // create an API Converter instance
  PDFConvert := TGS_PdfConverter.Create(ADir);
  // set the events for the Ghostscript output
  PDFConvert.OnStdError := StdError;
  PDFConvert.OnStdIn := StdIn;
  PDFConvert.OnStdOut := StdOut;

  Pages.ActivePage := Tab_Operation;
  FCurrentPage := 0;
  SetPreview;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  if (Ini <> nil) then
  begin
    Ini.WriteString('Path', 'LastFile', LEd_PdfFile.Text);
    Ini.WriteString('Path', 'GS_DLL_Path', GSDllDir);
    Ini.WriteString('Path', 'ICCProfileDir', ICCProfileDir);
    Ini.WriteString('User', 'Params', M_UserParams.Lines.Text.Replace(#13#10, '*#*'));
    Ini.WriteInteger('User', 'ConvertDevice', RGrp_Devices.ItemIndex);
    FreeAndNil(Ini);
  end;
  if (PDFConvert <> nil) then
    FreeAndNil(PDFConvert);
end;

function TFMain.GetOutputDir(ADir, AName: string): string;
begin
  Result := ADir + AName + '\';
  if (not DirectoryExists(Result)) then
    CreateDir(Result);
end;

procedure TFMain.ReadIni;
var
  AFile: string;
begin
  // read stored user informations from the ini file
  AFile := ChangeFileExt(Application.ExeName, '.ini');
  Ini := TIniFile.Create(AFile);
  GSDllDir := Ini.ReadString('Path', 'GS_DLL_Path', '..\..\..\bin');
  ICCProfileDir := Ini.ReadString('Path', 'ICCProfileDir', '..\..\..\ICC-Profiles');
  LEd_PdfFile.Text := Ini.ReadString('Path', 'LastFile', '');
  M_UserParams.Lines.Text := Ini.ReadString('User', 'Params', '').Replace('*#*', #13#10);
  RGrp_Devices.ItemIndex := Ini.ReadInteger('User', 'ConvertDevice', 0);
end;

procedure TFMain.SBtn_OpenFileClick(Sender: TObject);
begin
  if (OpenDialog.Execute) then
    LEd_PdfFile.Text := OpenDialog.FileName;
end;

procedure TFMain.SetCursorToEnd(AMemo: TMemo);
begin
  // set the cursor at the end of the memo (auto scroll)
  with AMemo do
  begin
    SelStart := Perform(EM_LINEINDEX, Lines.Count - 1, 0) + 0;
    SelLength := 0;
    Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TFMain.SetPage(APage: Integer);
var
  ABmp: TGS_Image;
begin
  // get a page image preview (only performed with Device='display')
  if (PDFConvert.GSDisplay.PageCount > 0) and (PDFConvert.GSDisplay.PageCount > APage) then
  begin
    ABmp := PDFConvert.GSDisplay.GetPage(APage);
    if (ABmp <> nil) then
    begin
      Img_Page.Picture.Assign(ABmp);
      Img_Page.Width := ABmp.Width;
      Img_Page.Height := ABmp.Height;
      FCurrentPage := APage;
      SetPreview;
    end;
  end;
end;

procedure TFMain.SetPreview;
var
  ShownPage: Integer;
begin
  ShownPage := 0;
  if (PDFConvert.GSDisplay.PageCount > 0) then
    ShownPage := FCurrentPage + 1;
  LEd_PageCount.Text := Format('%d/%d', [ShownPage, PDFConvert.GSDisplay.PageCount]);
end;

procedure TFMain.StdError(const AText: string);
begin
  //write the Ghostscript errors and API messages in a TMemo
  M_Errors.Text := M_Errors.Text + AText;
  SetCursorToEnd(M_Errors);
end;

procedure TFMain.StdIn(const AText: string);
begin
  M_Output.Lines.Add('IN: ' + AText);
end;

procedure TFMain.StdOut(const AText: string);
var
  AStr: string;
begin
  //write the Ghostscript output in a TMemo
  AStr := AText.Replace(#10, #13#10);
  M_Output.Text := M_Output.Text + AStr;
  SetCursorToEnd(M_Output);
  Application.ProcessMessages;
end;

procedure TFMain.Convert;
var
  ADir, AFile, AProfileDir: string;
  AThread: Boolean;
begin
  BBtn_Test.Enabled := False;
  Screen.Cursor := crHourGlass;
  AFile := LEd_PdfFile.Text;
  AThread := True; // run it in a thread to prevent the gui from freezing

  try
    if (not FileExists(AFile)) then
      raise EFileNotFoundException.CreateFmt('The file: %s does not exist', [AFile]);

    ADir := ExtractFilePath(ParamStr(0)) + '\';
    AProfileDir := ICCProfileDir + '\';

    PDFConvert.OnAfterExecute := ThreadFinished;

{$IFDEF DEBUG}
    (* You can set different debug options for th API *)

    // shows the parameters and other informations in the OnStdOut
    PDFConvert.Debug := True;
    // shows the used parameters as cmd args
    // the output is without "" you have to fix this by your self
    PDFConvert.DebugShowCmdArgs := True;
    // shows the communictaion of Ghosscript with API, if you use
    // PDFConvert.Params.Device = 'display';
    PDFConvert.GSDisplay.Debug := True;

    // debug options for Ghostscript
    PDFConvert.DebugParams.DebugParams :=
      [dparCompiledFonts, dparCffFonts,
      dparCIEColor, dparFontApi, dparTTFFonts, dparInitialization];

{$ENDIF}
    // set a title for the PDF-File
    // when a Title allready exists this param will be ignored
    PDFConvert.Params.PdfTitle := 'GS Example Title';

    case RGrp_Devices.ItemIndex of
      0: Convert_DisplayPdf(ADir, AFile, AThread);
      1: Convert_DisplayPdfA(ADir, AProfileDir, AFile, AThread);
      2: Convert_WritePdf(ADir, AFile, AThread);
      3: Convert_WritePdfA(ADir, AProfileDir, AFile, AThread);
      else raise Exception.Create('No Convert Device selected');
    end;

  except
    on E: Exception do begin
      ThreadFinished(nil);
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TFMain.Convert_DisplayPdf(SearchDir, InputFile: string;
  UseThread: Boolean);
var
  OutputDir: string;
begin
  OutputDir := GetOutputDir(SearchDir, 'pdf');

  // Device -> display is set as a device so we can not use ColorConversionStrategy
  PDFConvert.Params.Device := DISPLAY_DEVICE_NAME; // show pdf when finished
  PDFConvert.UserParams.Clear;
  // add the parameters from the Memo
  PDFConvert.UserParams.AddStrings(M_UserParams.Lines);
  PDFConvert.ToPdf(InputFile, OutputDir + ChangeFileExt(ExtractFileName(InputFile), '.pdf'), UseThread);
end;

procedure TFMain.Convert_DisplayPdfA(SearchDir, ICCProfileDir, InputFile: string;
  UseThread: Boolean);
var
  OutputDir: string;
begin
  OutputDir := GetOutputDir(SearchDir, 'pdfa');

  PDFConvert.Params.SubsetFonts := False;
  PDFConvert.Params.EmbededFonts := True;
  PDFConvert.Params.ICCProfile := ICCProfileDir + 'default_cmyk.icc';
  PDFConvert.Params.PDFAOutputConditionIdentifier := 'CMYK';
  // Device -> display is set as a device so we can not use ColorConversionStrategy or if set it will be ignored
  PDFConvert.Params.ColorConversionStrategy := ccsCMYK;
  PDFConvert.Params.Device := DISPLAY_DEVICE_NAME; // show pdf when finished -> there will be no output file
  PDFConvert.UserParams.Clear;
  // add the user parameters from the Memo
  PDFConvert.UserParams.AddStrings(M_UserParams.Lines);
  // start the convert operation
  PDFConvert.ToPdfa(InputFile, OutputDir + ChangeFileExt(ExtractFileName(InputFile), '.pdf'), UseThread);
end;

procedure TFMain.Convert_WritePdf(SearchDir, InputFile: string; UseThread: Boolean);
var
  OutputDir, AName: string;
begin
  AName := 'pdf';
  OutputDir := GetOutputDir(SearchDir, AName);

  // Device-> pdfwrite
  PDFConvert.Params.Device := DEVICES_HIGH_LEVEL[pdfwrite];
  PDFConvert.Params.ColorConversionStrategy := ccsRGB;
  PDFConvert.UserParams.Clear;
  // user param example you can add every gs param in default form with the user params
  PDFConvert.UserParams.Add('-sProcessColorModel=DeviceRGB');
  PDFConvert.UserParams.AddStrings(M_UserParams.Lines);
  PDFConvert.ToPdf(InputFile, OutputDir + ChangeFileExt(ExtractFileName(InputFile), '.' + AName), UseThread);
end;

procedure TFMain.Convert_WritePdfA(SearchDir, ICCProfileDir, InputFile: string;
  UseThread: Boolean);
var
  OutputDir: string;
begin
  OutputDir := GetOutputDir(SearchDir, 'pdfa');

  PDFConvert.Params.SubsetFonts := False;
  PDFConvert.Params.EmbededFonts := True;
  PDFConvert.Params.ICCProfile := ICCProfileDir + 'default_cmyk.icc';
  PDFConvert.Params.PDFAOutputConditionIdentifier := 'CMYK';
  // Device-> pdfwrite
  PDFConvert.Params.Device := DEVICES_HIGH_LEVEL[pdfwrite];
  PDFConvert.Params.ColorConversionStrategy := ccsCMYK;
  PDFConvert.UserParams.Clear;
  PDFConvert.UserParams.Add('-sProcessColorModel=DeviceCMYK');
  PDFConvert.UserParams.AddStrings(M_UserParams.Lines);
  PDFConvert.ToPdfa(InputFile, OutputDir + ChangeFileExt(ExtractFileName(InputFile), '.pdf'), UseThread);
end;

procedure TFMain.ThreadFinished(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  if (PDFConvert.LastErrorCode < 0) then
    MessageDlg(PDFConvert.LastErrors, mtError, [mbOK], 0);
  BBtn_Test.Enabled := True;
  SetPage(0);
  // Show the Preview, if created -> only availible with Device := 'display'
  if (RGrp_Devices.ItemIndex in [0, 1]) and (PDFConvert.LastErrorCode = 0) then
    Pages.ActivePage := Tab_PDFView;
end;

procedure TFMain.UpDown_PagesClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: SetPage(FCurrentPage + 1);
    btPrev: SetPage(FCurrentPage - 1);
  end;
end;

end.
