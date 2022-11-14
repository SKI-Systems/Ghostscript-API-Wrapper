program GS_API_ExampleLazarus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem, ctypes,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
			Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

