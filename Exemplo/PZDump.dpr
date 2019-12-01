program PZDump;

uses
  Vcl.Forms,
  UDump in 'UDump.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
