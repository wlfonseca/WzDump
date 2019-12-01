unit UDump;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ZAbstractConnection, ZConnection, WDump, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Gauges;

type
  TForm13 = class(TForm)
    Button1: TButton;
    ZConnection1: TZConnection;
    WDump1: TWDump;
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
    procedure Button1Click(Sender: TObject);
    procedure WDump1Progress(progress, countofobjects,
      actualnumberobject: Integer; typeofobject, nameobject: string);
    procedure WDump1Error(sErro: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    procedure conectar;
    procedure efetuarbackup;
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
  //]2DHENVAY4QN
  conectar;
  //WDump1.TableNames := 'cep';
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
      showmessage('Csonectado!');
    end;
  except
    on e: Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TForm13.efetuarbackup;
begin
  WDump1.conn := ZConnection1;
  WDump1.caminho := EdtDestino.Text;
  WDump1.Database := EdtDataBase.Text;
  WDump1.ValuesPerInsert := StrToInt(EdtValuesPerInsert.Text);
  WDump1.TableNames := edtTabelas.text;

  WDump1.Dumper;
end;




procedure TForm13.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TForm13.WDump1Error(sErro: string);
begin
  ShowMessage('buuu '+serro);
end;

procedure TForm13.WDump1Progress(progress, countofobjects,
  actualnumberobject: Integer; typeofobject, nameobject: string);
begin
 Label1.caption :=  nameobject+ ' ' +typeofobject+ ' '+' '+actualnumberobject.ToString+' de '+countofobjects.ToString+' percent % '+progress.ToString;
 progressbar1.Max := countofobjects;
 progressbar1.Position := actualnumberobject;
 application.ProcessMessages;
end;

end.

