object Form13: TForm13
  Left = 0
  Top = 0
  Caption = 'Form13'
  ClientHeight = 318
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 0
    Top = 253
    Width = 333
    Height = 33
    Align = alBottom
    Progress = 0
    ExplicitLeft = 24
    ExplicitTop = 200
    ExplicitWidth = 281
  end
  object Label1: TLabel
    Left = 8
    Top = 234
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 120
    Top = 203
    Width = 89
    Height = 33
    Caption = 'Fazer Backup'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EdtDestino: TLabeledEdit
    Left = 24
    Top = 114
    Width = 281
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Destino'
    TabOrder = 1
    Text = 'c:\sga\backup.sql'
  end
  object EdtDataBase: TLabeledEdit
    Left = 24
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Database'
    TabOrder = 2
    Text = 'lojas'
  end
  object EdtHost: TLabeledEdit
    Left = 24
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Host'
    TabOrder = 3
    Text = 'localhost'
  end
  object EdtUser: TLabeledEdit
    Left = 184
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Usu'#225'rio'
    TabOrder = 4
    Text = 'root'
  end
  object EdtPasswd: TLabeledEdit
    Left = 184
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 30
    EditLabel.Height = 13
    EditLabel.Caption = 'Senha'
    PasswordChar = '*'
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 286
    Width = 333
    Height = 32
    Align = alBottom
    TabOrder = 6
  end
  object EdtValuesPerInsert: TLabeledEdit
    Left = 24
    Top = 160
    Width = 81
    Height = 21
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = 'Values por insert'
    TabOrder = 7
    Text = '99'
  end
  object EdtTabelas: TLabeledEdit
    Left = 184
    Top = 164
    Width = 121
    Height = 21
    EditLabel.Width = 37
    EditLabel.Height = 13
    EditLabel.Caption = 'Tabelas'
    TabOrder = 8
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    Catalog = ''
    HostName = 'tef.net.br'
    Port = 0
    Database = 'tefnet_erp'
    User = 'tefnet_erp'
    Password = 'tefnet753951'
    Protocol = 'postgresql'
    Left = 368
    Top = 32
  end
  object WDump1: TWDump
    Caminho = 'c:\novo.sql'
    Conn = ZConnection1
    Gauge = Gauge1
    Database = 'tefnet_erp'
    DumperViews = False
    DumperTriggers = False
    DumperTables = True
    DumperFunctions = False
    DumperEvents = False
    DumperProcedures = False
    OnProgress = WDump1Progress
    ValuesPerInsert = 18
    Left = 368
    Top = 88
  end
end
