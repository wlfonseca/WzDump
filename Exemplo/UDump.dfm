object Form13: TForm13
  Left = 0
  Top = 0
  Caption = 'LeBackup'
  ClientHeight = 318
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 0
    Top = 253
    Width = 341
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
  object SpeedButton1: TSpeedButton
    Left = 292
    Top = 113
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
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
    Width = 262
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
    Width = 132
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Database'
    TabOrder = 2
    Text = 'database'
  end
  object EdtHost: TLabeledEdit
    Left = 24
    Top = 32
    Width = 132
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
    Width = 131
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
    Width = 131
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
    Width = 341
    Height = 32
    Align = alBottom
    TabOrder = 6
  end
  object EdtValuesPerInsert: TLabeledEdit
    Left = 24
    Top = 160
    Width = 90
    Height = 21
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = 'Values por insert'
    TabOrder = 7
    Text = '999'
  end
  object EdtTabelas: TLabeledEdit
    Left = 184
    Top = 160
    Width = 131
    Height = 21
    EditLabel.Width = 37
    EditLabel.Height = 13
    EditLabel.Caption = 'Tabelas'
    TabOrder = 8
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    Catalog = ''
    HostName = 'localhost'
    Port = 0
    Database = 'lojas'
    User = 'root'
    Password = ''
    Protocol = 'mysql'
    Left = 368
    Top = 32
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.sql'
    FileName = '.sql'
    Filter = 'SQL|*.sql'
    Left = 272
    Top = 198
  end
end
