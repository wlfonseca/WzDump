unit UDump;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, ZAbstractConnection, ZConnection, WDump, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Samples.Gauges, Vcl.Buttons;

type
  TForm13 = class(TForm)
    Button1: TButton;
    ZConnection1: TZConnection;
    Gauge1: TGauge;
    EdtDestino: TLabeledEdit;
    EdtDataBase: TLabeledEdit;
    EdtHost: TLabeledEdit;
    EdtUser: TLabeledEdit;
    EdtPasswd: TLabeledEdit;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    EdtValuesPerInsert: TLabeledEdit;
    EdtTabelas: TLabeledEdit;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure WDump1Progress(progress, countofobjects, actualnumberobject: Integer; typeofobject, nameobject: string);
    procedure WDump1Progress2(progress, countofobjects, actualnumberobject: Integer; typeofobject, nameobject: string);
    procedure WDump1Error(sErro, NameObject: string);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure conectar;
    procedure efetuarbackup;
    procedure desconectar;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
begin

  conectar;
  efetuarbackup;
  ShowMessage('Finalizado');

end;

procedure TForm13.conectar;
begin
  try
    with ZConnection1 do
    begin
      HostName := EdtHost.text;
      User := EdtUser.text;
      Password := EdtPasswd.Text;
      Database := EdtDataBase.Text;

      Connected := true;
      showmessage('Conectado!');
    end;
  except
    on e: Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TForm13.desconectar;
begin
  try
    with ZConnection1 do
    begin
      HostName := '';
      User := '';
      Password := '';
      Database := '';
      Connected := False;
    end;
  except
    on e: Exception do
      raise Exception.Create(E.Message);
  end;
end;
procedure TForm13.efetuarbackup;
var
  Wdump: TWDump;
begin
  Wdump := TWDump.Create(self);
  try
    WDump.DumperTables := True;
    WDump.conn := ZConnection1;
    WDump.caminho := EdtDestino.Text;
    WDump.Database := EdtDataBase.Text;
    WDump.ValuesPerInsert := StrToInt(EdtValuesPerInsert.Text);
    WDump.TableNames := edtTabelas.text;
    wdump.Gauge := Gauge1;

    WDump.Dumper;
  finally
    FreeAndNil(Wdump);
  end;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm13.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    EdtDestino.Text := SaveDialog1.FileName;
end;

procedure TForm13.WDump1Error(sErro, NameObject: string);
begin
  showmessage('erro ' + NameObject);
end;

procedure TForm13.WDump1Progress(progress, countofobjects, actualnumberobject: Integer; typeofobject, nameobject: string);
begin
  Label1.caption := nameobject + ' ' + typeofobject + ' ' + ' ' + actualnumberobject.ToString + ' de ' + countofobjects.ToString + ' percent % ' + progress.ToString;
  progressbar1.Max := countofobjects;
  progressbar1.Position := actualnumberobject;
  application.ProcessMessages;
end;

procedure TForm13.WDump1Progress2(progress, countofobjects, actualnumberobject: Integer; typeofobject, nameobject: string);
begin
  showmessage(nameobject);
end;

end.

