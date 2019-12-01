{*********************************************************}
{                                                         }
{                 WZDUmp Mysql Objects                    }
{               WZeos Backup component                    }
{                                                         }
{        Originally written by Wellington Fonseca         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 2018-2019 Cw2 Development Group        }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{                                                         }
{                                                         }
{                                 Cw2 Development Group.  }
{*********************************************************}
unit WDump;

interface

uses
  ZDataset, windows, ZConnection, System.Sysutils, Vcl.Dialogs, System.Classes,
  Vcl.Forms, Data.DB, Vcl.Samples.Gauges, Vcl.StdCtrls;

type
  TWProgress = procedure(progress, countofobjects, actualnumberobject: Integer; typeofobject, nameobject: string) of object;

  TWError = procedure(sErro, NameObject: string) of object;

  TWDump = class(TComponent)
  private
    Fcaminho: string;
    Fconn: TZConnection;
    FProgress: TGauge;
    FOnError: TWError;
    FDatabase: string;
    FTableNames: string;
    FDumperTables: Boolean;
    FDumperFunctions: boolean;
    FDumperViews: boolean;
    FDumperTriggers: boolean;
    FDumperEvents: boolean;
    FDumperProcedures: Boolean;
    FValuesPerInsert: Integer;
    FControlCancel: Boolean;
    FOnProgress: TWprogress;
    procedure DumpFunctions;
    procedure DumpProcedures;
    procedure DumpTables;
    procedure DumpEspecificsTables;
    procedure DumpViews;
    procedure DumpTriggers;
    procedure DumpEvents;
    procedure Setcaminho(const Value: string);
    procedure Setconn(const Value: TZConnection);
    procedure Setprogress(const Value: TGauge);
    function GetFieldValueForDump(Field: TField): string;
    function StringReplaceExt(const S: string; OldPattern, NewPattern: array of string; Flags: TReplaceFlags): string;
    function mysql_real_escape_string(const unescaped_string: string): string;
    function removedefine(str: string): string;
    procedure InsertIntoFile(caminho, mensagem: string);
    procedure SetDatabase(const Value: string);
    procedure SetDumperEvents(const Value: boolean);
    procedure SetDumperFunctions(const Value: boolean);
    procedure SetDumperTriggers(const Value: boolean);
    procedure SetDumperViews(const Value: boolean);
    procedure SetDumperProcedures(const Value: Boolean);
    procedure SetTableNames(const Value: string);
    procedure SetDumperTables(const Value: boolean);
    function GetFDatabase: string;
    function GetValuesPerInsert: Integer;
    procedure SetValuesPerInsert(const Value: Integer);
    function statementPGCreateTable(table: string): string;
  public
    procedure Dumper;
    procedure CancelDumper;
  published
    property Caminho: string read Fcaminho write Setcaminho;
    property Conn: TZConnection read Fconn write Setconn;
    property Gauge: TGauge read Fprogress write Setprogress;
    property Database: string read GetFDatabase write SetDatabase;
    property TableNames: string read FTableNames write SetTableNames;
    property DumperViews: boolean read FDumperViews write SetDumperViews;
    property DumperTriggers: boolean read FDumperTriggers write SetDumperTriggers;
    property DumperTables: boolean read FDumperTables write SetDumperTables;
    property DumperFunctions: boolean read FDumperFunctions write SetDumperFunctions;
    property DumperEvents: boolean read FDumperEvents write SetDumperEvents;
    property DumperProcedures: Boolean read FDumperProcedures write SetDumperProcedures;
    property OnProgress: TWprogress read FOnProgress write FOnProgress;
    property OnError: TWError read FOnError write FOnError;
    property ValuesPerInsert: Integer read GetValuesPerInsert write SetValuesPerInsert;
  end;

procedure Register;

const
  EOL = #13 + #10;

implementation

procedure TrimAppMemorySize;
var
  MainHandle: THandle;
begin
  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID);
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
  except
  end;
  Application.ProcessMessages;
end;

procedure TWDump.InsertIntoFile(caminho, mensagem: string);
var
  log: TextFile;
begin
  AssignFile(log, caminho);

  if fileexists(caminho) then
    Append(log)
  else
    Rewrite(log);
  Writeln(log, mensagem);
  Closefile(log);
end;

function Explode(Texto, Separador: string): TStrings;
var
  strItem: string;
  ListaAuxUTILS: TStrings;
  NumCaracteres, TamanhoSeparador, I: Integer;
begin
  ListaAuxUTILS := TStringList.Create;
  strItem := '';
  NumCaracteres := Length(Texto);
  TamanhoSeparador := Length(Separador);
  I := 1;
  while I <= NumCaracteres do
  begin
    if (Copy(Texto, I, TamanhoSeparador) = Separador) or (I = NumCaracteres) then
    begin
      if (I = NumCaracteres) then
        strItem := strItem + Texto[I];
      ListaAuxUTILS.Add(trim(strItem));
      strItem := '';
      I := I + (TamanhoSeparador - 1);
    end
    else
      strItem := strItem + Texto[I];

    I := I + 1;
  end;
  Explode := ListaAuxUTILS;
end;

function TWDump.removedefine(str: string): string;
var
  mtipo, antesdefine, posdefine: string;
begin
  try
    antesdefine := Explode(str, 'DEFINER')[0];
  except
    Result := str;
  end;

  try
    posdefine := Explode(str, 'SQL SECURITY DEFINER')[1];
    Result := antesdefine + ' SQL SECURITY DEFINER ' + posdefine;
  except
    try
      posdefine := Explode(str, 'FUNCTION')[1];
      Result := antesdefine + ' FUNCTION ' + posdefine;
    except
      try
        posdefine := Explode(str, 'TRIGGER')[1];
        Result := antesdefine + ' TRIGGER ' + posdefine;
      except
        try
          posdefine := Explode(str, 'EVENT')[1];
          Result := antesdefine + ' EVENT ' + posdefine;
        except
          try
            posdefine := Explode(str, 'PROCEDURE')[1];
            Result := antesdefine + ' PROCEDURE ' + posdefine;
          except
            Result := str;
          end;
        end;
      end;
    end;
  end;
end;

function TWDump.StringReplaceExt(const S: string; OldPattern, NewPattern: array of string; Flags: TReplaceFlags): string;
var
  i: integer;
begin
  Assert(Length(OldPattern) = (Length(NewPattern)));
  Result := S;
  for i := Low(OldPattern) to High(OldPattern) do
  begin
    Result := StringReplace(Result, OldPattern[i], NewPattern[i], Flags);
  end;
end;

function TWDump.mysql_real_escape_string(const unescaped_string: string): string;
begin
  Result := StringReplaceExt(unescaped_string, ['\', #34, #0, #10, #13, #26, ';'], ['\\', '\'#34, '\0', '\n', '\r', '\Z', '\;'], [rfReplaceAll]);
end;

function TWDump.GetFDatabase: string;
begin
  Result := FDatabase
end;

function TWDump.GetFieldValueForDump(Field: TField): string;
var
  FmtSet: TFormatSettings;
  s: string;
  i: integer;
  i64: int64;
  d: double;
  cur: currency;
  dt: TDateTime;
begin
  if Field.IsNull then
    if (Field.DataType = ftDate) and (Fconn.Protocol = 'mysql') then
      Result := '0000-00-00'
    else
      Result := 'NULL'
  else
  begin
    case Field.DataType of
      ftSmallint, ftWord, ftInteger:
        begin
          i := Field.AsInteger;
          Result := IntToStr(i);
        end;
      ftLargeint:
        begin
          i64 := TLargeintField(Field).AsLargeInt;
          Result := IntToStr(i64);
        end;
      ftFloat:
        begin
          d := Field.AsFloat;
          FmtSet.DecimalSeparator := '.';
          Result := FloatToStr(d, FmtSet);
        end;
      ftBCD:
        begin
          cur := Field.AsCurrency;
          FmtSet.DecimalSeparator := '.';
          Result := CurrToStr(cur, FmtSet);
        end;
      ftFMTBcd:
        begin
          Result := Field.AsString;
          if FormatSettings.DecimalSeparator <> '.' then
            Result := StringReplace(Result, FormatSettings.DecimalSeparator, '.', []);
        end;
      ftBoolean:
        begin
          Result := BoolToStr(Field.AsBoolean, False);
        end;
      ftDate:
        begin
          dt := Field.AsDateTime;
          FmtSet.DateSeparator := '-';
          Result := '''' + FormatDateTime('yyyy-mm-dd', dt, FmtSet) + '''';
        end;
      ftTime:
        begin
          dt := Field.AsDateTime;
          FmtSet.TimeSeparator := ':';
          Result := '''' + FormatDateTime('hh:nn:ss', dt, FmtSet) + '''';
        end;
      ftDateTime:
        begin
          dt := Field.AsDateTime;
          FmtSet.DateSeparator := '-';
          FmtSet.TimeSeparator := ':';
          Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dt, FmtSet) + '''';
        end;
    else
      Result := QuotedStr(mysql_real_escape_string(Field.Value));
    end;
  end;
end;

function TWDump.GetValuesPerInsert: Integer;
begin
  if FValuesPerInsert <> 0 then
    Result := FValuesPerInsert
  else
    Result := 18;
end;

procedure TWDump.DumpProcedures;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  percent, count, numfields: integer;
begin
  q2 := TZQuery.Create(nil); //function
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);
  q3.Connection := Fconn;
  Fconn.Connected := true;

  if assigned(Fprogress) then
    Fprogress.Progress := 0;

  AssignFile(arq, Fcaminho);
  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);

  linha := '';

  with q2 do
  begin
    close;
    sql.Clear;
    sql.Text := 'SHOW PROCEDURE STATUS WHERE Db =' + QuotedStr(GetFDatabase);
    try
      Open;
    except
      on e: Exception do
        if Assigned(FOnError) then
          Fonerror(E.Message, '');
    end;
  end;

  if q2.RecordCount > 0 then
  begin
    if Assigned(Fprogress) then
      Fprogress.MaxValue := q2.RecordCount;
    while not q2.eof do
    begin
      if Assigned(Fprogress) then
        Fprogress.Progress := q2.RecNo;
      application.ProcessMessages;

      if FControlCancel then
        Break;
      with q3 do
      begin
        close;
        sql.Clear;
        sql.Text := 'SHOW CREATE PROCEDURE ' + q2.FieldByName('Name').AsString;
        try
          Open;
        except
          on e: Exception do
            if Assigned(FOnError) then
              FonError(E.Message, q2.FieldByName('Name').AsString);
        end;
      end;

      if Assigned(FonProgress) then
        FOnProgress((q2.RecNo * 100 div q2.RecordCount), q2.RecordCount, q2.RecNo, 'PROCEDURE', q2.FieldByName('Name').AsString);

      if q3.FieldByName('Create Procedure').AsString <> '' then
      begin
        linha := 'DELIMITER //' + EOL;
        linha := linha + removedefine(q3.FieldByName('Create Procedure').AsString) + EOL;
        linha := linha + '//' + EOL;
        linha := linha + 'DELIMITER ;' + EOL;
        Writeln(arq, linha);
      end;
      q2.Next;
    end;
  end;
  Closefile(arq);
end;

procedure TWDump.DumpEvents;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  count, numfields: integer;
begin

  q2 := TZQuery.Create(nil);
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);
  q3.Connection := Fconn;
  Fconn.Connected := true;

  if Assigned(Fprogress) then
    Fprogress.Progress := 0;

  AssignFile(arq, Fcaminho);

  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);

  linha := '';

  with q2 do
  begin
    close;
    sql.Clear;
    sql.Text := 'SELECT *, EVENT_SCHEMA AS `Db`, EVENT_NAME AS `Name` FROM information_schema.`EVENTS` WHERE `EVENT_SCHEMA` = ' + QuotedStr(GetFDatabase);
    try
      Open;
    except
      on E: Exception do
        if Assigned(FOnError) then
          Fonerror('Erro na obtenção da lista de Eventos ' + E.Message, GetFDatabase);
    end;
  end;

  if q2.RecordCount > 0 then
  begin
    if Assigned(Fprogress) then
      Fprogress.MaxValue := q2.RecordCount;
    while not q2.eof do
    begin
      if Assigned(Fprogress) then
        Fprogress.Progress := Fprogress.Progress + 1;

      if Assigned(FonProgress) then
        FOnProgress((q2.RecNo * 100 div q2.RecordCount), q2.RecordCount, q2.RecNo, 'EVENT', q2.FieldByName('EVENT_NAME').AsString);

      application.ProcessMessages;
      with q3 do
      begin
        close;
        sql.Clear;
        sql.Text := 'SHOW CREATE EVENT ' + q2.FieldByName('EVENT_NAME').AsString;
        try
          Open;
        except
          on E: exception do
            if Assigned(FOnError) then
              Fonerror(E.Message, q2.FieldByName('EVENT_NAME').AsString);
        end;
      end;

      if q3.FieldByName('Create Event').AsString <> '' then
      begin
        linha := 'DELIMITER //' + EOL;
        linha := linha + removedefine(q3.FieldByName('Create Event').AsString) + EOL;
        linha := 'DROP EVENT IF EXISTS ' + q2.FieldByName('Name').AsString + ';' + EOL + linha + EOL;
        linha := linha + '//' + EOL;
        linha := linha + 'DELIMITER ;' + EOL;
        Writeln(arq, linha);
      end;
      q2.Next;
    end;
  end;
  Closefile(arq);
end;

procedure TWDump.DumpFunctions;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  count, numfields: integer;
begin
  q2 := TZQuery.Create(nil); //function
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);
  q3.Connection := Fconn;
  Fconn.Connected := true;
  if Assigned(Fprogress) then
    Fprogress.Progress := 0;

  AssignFile(arq, Fcaminho);
  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);

  linha := '';
  with q2 do
  begin
    close;
    sql.Clear;
    sql.Text := 'SHOW FUNCTION STATUS WHERE Db =' + QuotedStr(GetFDatabase);
    try
      Open;
    except
      on E: Exception do
        if Assigned(FOnError) then
          FonError(E.Message, GetFDatabase);
    end;
  end;

  if q2.RecordCount > 0 then
  begin
    if Assigned(Fprogress) then
      Fprogress.MaxValue := q2.RecordCount;
    while not q2.eof do
    begin
      if Assigned(Fprogress) then
        Fprogress.Progress := q2.RecNo;

      if Assigned(FonProgress) then
        FOnProgress((q2.RecNo * 100 div q2.RecordCount), q2.RecordCount, q2.RecNo, 'FUNCTION', q2.FieldByName('Name').AsString);

      application.ProcessMessages;
      with q3 do
      begin
        close;
        sql.Clear;
        sql.Text := 'SHOW CREATE FUNCTION ' + q2.FieldByName('Name').AsString;
        try
          Open;
        except
          on e: Exception do
            if Assigned(FOnError) then
              FOnError(e.Message, q2.FieldByName('Name').AsString);
        end;
      end;

      if q3.FieldByName('Create Function').AsString <> '' then
      begin
        linha := 'DELIMITER //' + EOL;
        linha := linha + q3.FieldByName('Create Function').AsString + EOL;
        linha := removedefine(linha);
        linha := 'DROP FUNCTION IF EXISTS ' + q2.FieldByName('Name').AsString + ';' + EOL + linha + EOL;
        linha := linha + '//' + EOL;
        linha := linha + 'DELIMITER ;' + EOL;
        Writeln(arq, linha);
      end;
      q2.Next;
    end;

  end;
  Closefile(arq);
end;

{ tWDump }

procedure TWDump.CancelDumper;
begin
  FControlCancel := True;
end;

procedure TWDump.Dumper;
var
  arq: TextFile;
  metadata: Boolean;
begin
  if fileexists(Fcaminho) then
    DeleteFile(Fcaminho);
  metadata := Conn.UseMetadata;
  Conn.UseMetadata := false;

  if FDumperTables then
  begin
    if trim(FTablenames) = '' then
      DumpTables
    else
      DumpEspecificsTables;
  end;
  if FDumperFunctions then
    DumpFunctions;
  if FDumperViews then
    DumpViews;
  if FDumperProcedures then
    DumpProcedures;
  if FDumperTriggers then
    DumpTriggers;
  if FDumperEvents then
    DumpEvents;

  Conn.UseMetadata := metadata;
end;

function TWDump.statementPGCreateTable(table: string): string;
begin
  Result := ' SELECT '+QuotedStr(table)+' as table, ''CREATE TABLE '' || relname || E'''+EOL+'('+EOL+''' || ' +
  '  array_to_string(    array_agg(  ''    '' || column_name || ' +
  QuotedStr(' ') + ' ||  type || ' + QuotedStr(' ') + ' || not_null'+
  ' ), E'','+EOL+''' ) || E'''+EOL+');'+EOL+''' from ( SELECT c.relname, a.attname AS column_name, pg_catalog.format_type(a.atttypid, a.atttypmod) as type, case when a.attnotnull  then ''NOT NULL'' ' + ' else ''NULL'' END as not_null FROM pg_class c, pg_attribute a, pg_type t WHERE c.relname = ' + QuotedStr(table) + '  AND a.attnum > 0  AND a.attrelid = c.oid    AND a.atttypid = t.oid  ORDER BY a.attnum ) as tabledefinition group by relname';
end;

procedure TWDump.DumpTables;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  count, percent, numfields: integer;
begin

  q2 := TZQuery.Create(nil);
  q2.Connection := Fconn;

  with q2 do
  begin
    close;
    sql.Clear;
    //sql.Text := 'SELECT * FROM  INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ' + QuotedStr(GetFDatabase) + ' and table_type = ' + QuotedStr('BASE TABLE');
    //sql.Text := 'SHOW TABLES';

    if Connection.Protocol = 'mysql' then
      sql.text := 'show table status  where engine  is not null'
    else if Connection.Protocol = 'postgresql' then
      sql.Text := 'SELECT  table_name as show_tables FROM information_schema.tables WHERE   table_type = ' + QuotedStr('BASE TABLE') + ' AND  table_schema NOT IN (''pg_catalog'', ''information_schema'') order by show_tables;';
{
      sql.Text := '       SELECT  ''CREATE TABLE '' || relname || E''\n(\n'' || '+
    '  array_to_string(    array_agg(  ''    '' || column_name || '+QuotedStr(' ')+' ||  type || '+QuotedStr(' ')+' || not_null'' ), E'',\n'' ) || E''\n);\n'' from ( SELECT c.relname, a.attname AS column_name, pg_catalog.format_type(a.atttypid, a.atttypmod) as type, case when a.attnotnull  then ''NOT NULL'' '+
    ' else ''NULL'' END as not_null FROM pg_class c, pg_attribute a, pg_type t WHERE c.relname = ''item''  AND a.attnum > 0  AND a.attrelid = c.oid    AND a.atttypid = t.oid  ORDER BY a.attnum ) as tabledefinition group by relname;';
 }
    try
      Open;
    except
      on e: Exception do
      begin
        Closefile(arq);
        if Assigned(FOnError) then
          Fonerror(E.Message, GetFDatabase);
      end;
    end;
  end;

  if q2.RecordCount > 0 then
  begin
    while not q2.eof do
    begin
      application.ProcessMessages;
      FTableNames := FTableNames + q2.Fields[0].AsString + ',';
      application.ProcessMessages;
      q2.Next;
    end;
    delete(FTableNames, length(FTableNames), 1);
    DumpEspecificsTables;
  end
  else
    Fonerror('Tabelas Não encontradas', GetFDatabase);
end;

procedure TWDump.DumpEspecificsTables;
var
  q2, q3, q4, q5, Qconsulta: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  numrows, row, rowsperselect, rowinit, rowend, i, counter, numfields: integer;
  Lista: TStrings;
begin
  Lista := Tstrings.Create;
  q2 := TZQuery.Create(nil);
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);

  q3.Connection := Fconn;
  q4 := TZQuery.Create(nil);
  q4.Connection := Fconn;
  q5 := TZQuery.Create(nil);
  q5.Connection := Fconn;

  rowsperselect := 100000;

  if assigned(Fprogress) then
    Fprogress.Progress := 0;

  try
    Lista := explode(Ftablenames, ',');
  except
    on E: exception do
      if Assigned(FOnError) then
        Fonerror(E.Message, Ftablenames);
  end;

  AssignFile(arq, Fcaminho);

  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);
 if Fconn.Protocol = 'mysql' then
  Writeln(arq, 'SET FOREIGN_KEY_CHECKS=0;');

  for i := 0 to Lista.count - 1 do
  begin
    application.ProcessMessages;
    with q3 do
    begin
      if active then
        close;
      sql.Clear;
      if connection.Protocol = 'mysql' then
        sql.Text := 'show create table ' + Lista[i]
      else if connection.Protocol = 'postgresql' then
        sql.Text := statementPGCreateTable(Lista[i]);

      try
        Open;
      except
        on E: exception do
        begin
          if Assigned(FOnError) then
          begin
            Fonerror(E.Message, Ftablenames);
          end;
        end;
      end;
    end;

    if q3.active and (q3.RecordCount > 0) then
    begin
      Writeln(arq, 'DROP TABLE IF EXISTS ' + Lista[i] + ';');
      Writeln(arq, q3.fields[1].Asstring + ';');
      campo := '';
      with q4 do
      begin
        close;
        sql.clear;
        if Connection.Protocol = 'mysql' then
          sql.text := 'show columns from ' + Lista[i]
        else
            sql.Text := 'SELECT COLUMN_NAME as field FROM information_schema.COLUMNS WHERE TABLE_NAME ='+ QuotedStr(Lista[i]) ;

        try
          open;
        except
          on e: Exception do
          begin
            if Assigned(FOnerror) then
              FOnError(E.Message, Lista[i]);
            CloseFile(arq);
          end;
        end;
      end;
      while not q4.eof do
      begin
        campo := campo + q4.fieldbyname('field').Asstring + ',';
        application.ProcessMessages;
        q4.Next;
      end;

      delete(campo, length(campo), 1);

      with q5 do
      begin
        Close;
        sql.clear;
        sql.text := 'select count(*) from "' + Lista[i]+'"';

        try
          Open;
        except
          on e: exception do
          begin
            showmessage('Erro obtendo total de '+ Lista[i]+' query '+sql.text+' erro '+e.message);
          end;
        end;


        numrows := fields[0].AsInteger;
      end;

      if assigned(Fprogress) then
      begin
        Fprogress.Progress := 0;
        Fprogress.MaxValue := numrows;
        Fprogress.Refresh;
      end;

      row := 0;
      if numrows > 0 then
        if Assigned(FonProgress) then
          FOnProgress((Fprogress.Progress * 100 div numrows), Lista.count, (i + 1),  'Table ', Lista[i]);

      while row < numrows do
      begin
        FreeAndnil(q5);
        q5 := TZQuery.Create(Self);
        q5.Connection := Fconn;

        TrimAppMemorySize;
        application.ProcessMessages;
        with q5 do
        begin
          Close;
          sql.clear;
          if connection.Protocol = 'mysql' then
             sql.text := 'select * from ' + Lista[i] + ' limit ' + IntToStr(row) + ',' + IntToStr(rowsperselect)
             else
               sql.text := 'select * from "' + Lista[i] + '" limit ' + IntToStr(rowsperselect) + ' OFFSET ' + IntToStr(row) ;

          Open;
        end;
        row := (row + rowsperselect);


        if q5.active then
          if q5.RecordCount > 0 then //se tiver dados
          begin
            q5.First;
            pvalues := EmptyStr;

            counter := 0;
            while not q5.eof do
            begin
              application.ProcessMessages;

              if assigned(Fprogress) then
                Fprogress.Progress := Fprogress.Progress + 1;

              if Assigned(FonProgress) then
                FOnProgress((Fprogress.Progress * 100 div numrows), Lista.count, (i + 1), 'Table', Lista[i]);

              values := emptystr;
              q4.first;
              while not q4.eof do
              begin
                if not q5.fieldbyname(q4.fieldbyname('field').Asstring).IsBlob then
                  values := values + GetFieldValueForDump(q5.fieldbyname(q4.fieldbyname('field').Asstring)) + ','
                else
                  values := values + QuotedStr(mysql_real_escape_string(q5.fieldbyname(q4.fieldbyname('field').Asstring).AsString)) + ',';
                q4.Next;
                application.ProcessMessages;
              end;

              if (campo <> '') and (values <> '') then
              begin
                linha := EmptyStr;
                delete(values, length(values), 1);
              end;

              if (counter < GetValuesPerInsert) then
              begin
                pvalues := pvalues + '(' + values + ')' + ',';
                counter := counter + 1;
                IntToStr(counter);
                q5.Next;
              end
              else
              begin
                TrimAppMemorySize;
                delete(pvalues, length(pvalues), 1);
                linha := 'insert into ' + Lista[i] + ' values ' + pvalues + ';';

                if trim(pvalues) <> '' then
                  Writeln(arq, linha);
                pvalues := EmptyStr;
                counter := 0;
              end;
              application.ProcessMessages;

            end;

            delete(pvalues, length(pvalues), 1);
            linha := 'insert into ' + Lista[i] + ' values ' + pvalues + ';';
            if trim(pvalues) <> '' then
              Writeln(arq, linha);
            pvalues := EmptyStr;
          end;
      end;
    end;
    application.ProcessMessages;
  end;
  q3.Free;
  q4.Free;
  q5.Free;
 if Fconn.Protocol = 'mysql' then
  Writeln(arq, 'SET FOREIGN_KEY_CHECKS=1;');
  Closefile(arq);
end;

procedure TWDump.DumpViews;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  count, numfields: integer;
begin

  q2 := TZQuery.Create(nil); //view
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);
  q3.Connection := Fconn;

  AssignFile(arq, Fcaminho);
  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);

  try
    if assigned(Fprogress) then
      Fprogress.Progress := 0;
    with q2 do
    begin
      Close;
      sql.Clear;
      sql.Text := 'SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_SCHEMA = ' + QuotedStr(GetFDatabase);
      try
        Open;
      except
        on e: Exception do
          if Assigned(FOnError) then
            Fonerror('Erro na obtenção da lista de Views ' + E.Message, GetFDatabase);
      end;
      if assigned(Fprogress) then
        Fprogress.MaxValue := q2.RecordCount;

      while not q2.eof do
      begin
        Fprogress.Progress := Fprogress.Progress + 1;
        application.processmessages;

        if Assigned(FonProgress) then
          FOnProgress((q2.Recno * 100 div q2.RecordCount), q2.RecordCount, q2.RecNo, 'VIEW', q2.FieldByName('table_name').Asstring);

        with q3 do
        begin
          close;
          sql.Clear;
          sql.Text := 'SHOW CREATE VIEW ' + q2.FieldByName('table_name').Asstring;
          Open;
        end;

        application.ProcessMessages;

        linha := 'DROP VIEW IF EXISTS ' + q2.FieldByName('table_name').Asstring + ';' + EOL;
        linha := linha + 'DELIMITER //' + EOL;
        linha := linha + removedefine(q3.FieldByName('Create View').AsString) + ';' + EOL;
        linha := linha + '//' + EOL;
        linha := linha + 'DELIMITER ;' + EOL;
        Writeln(arq, linha);

        q2.Next;
      end;
    end;
  finally
    q2.Free;
    q3.Free;
  end;
  Closefile(arq);
end;

procedure TWDump.DumpTriggers;
var
  q2, q3, q4, q5: TZQuery;
  linha, campo, values, pvalues: string;
  arq: TextFile;
  count, numfields: integer;
begin
  q2 := TZQuery.Create(nil); //view
  q2.Connection := Fconn;
  q3 := TZQuery.Create(nil);
  q3.Connection := Fconn;

  AssignFile(arq, Fcaminho);
  if fileexists(Fcaminho) then
    Append(arq)
  else
    Rewrite(arq);
  try
    if assigned(Fprogress) then
      Fprogress.Progress := 0;
    with q2 do
    begin
      Close;
      sql.Clear;
      sql.Text := 'SHOW TRIGGERS FROM ' + GetFDatabase;
      try
        Open;
      except
        on e: Exception do
          raise Exception.Create('Erro na obtenção da lista de TRIGGERS ' + E.message + ' ' + sql.Text);
      end;
      if assigned(Fprogress) then
        Fprogress.MaxValue := q2.RecordCount;

      while not q2.eof do
      begin
        Fprogress.Progress := Fprogress.Progress + 1;
        application.processmessages;
        if Assigned(FonProgress) then
          FOnProgress((q2.RecNo * 100 div q2.RecordCount), q2.RecordCount, q2.RecNo, 'TRIGGER', q2.FieldByName('Trigger').Asstring);

        with q3 do
        begin
          close;
          sql.Clear;
          sql.Text := 'SHOW CREATE TRIGGER ' + q2.FieldByName('Trigger').Asstring;
          Open;
        end;

        if q3.RecordCount > 0 then
        begin
          linha := 'DROP TRIGGER IF EXISTS ' + q2.FieldByName('Trigger').Asstring + ';' + EOL;
          linha := linha + 'DELIMITER //' + EOL;
          linha := linha + removedefine(q3.FieldByName('SQL Original Statement').AsString) + ';' + EOL;
          linha := linha + '//' + EOL;
          linha := linha + 'DELIMITER ;' + EOL;
          Writeln(arq, linha);
        end;
        q2.Next;
      end;
    end;
  finally
    q2.Free;
    q3.Free;
  end;
  Closefile(arq);
end;

procedure TWDump.Setcaminho(const Value: string);
begin
  if Value <> '' then
    Fcaminho := Value
  else
    raise Exception.Create('Caminho não pode ser vazio.');
end;

procedure TWDump.Setconn(const Value: TZConnection);
begin
  if not Assigned(Value) then
    raise Exception.Create('Error Conexão não identificada');
  Fconn := Value;
end;

procedure TWDump.SetDatabase(const Value: string);
begin
  if Value <> '' then
    FDatabase := Value
  else
    raise Exception.Create('Informar Database');
end;

procedure TWDump.SetDumperEvents(const Value: boolean);
begin
  FDumperEvents := Value;
end;

procedure TWDump.SetDumperFunctions(const Value: boolean);
begin
  FDumperFunctions := Value;
end;

procedure TWDump.SetDumperProcedures(const Value: Boolean);
begin
  FDumperProcedures := Value;
end;

procedure TWDump.SetDumperTables(const Value: boolean);
begin
  FDumperTables := Value;
end;

procedure TWDump.SetDumperTriggers(const Value: boolean);
begin
  FDumperTriggers := Value;
end;

procedure TWDump.SetDumperViews(const Value: boolean);
begin
  FDumperViews := Value;
end;

procedure TWDump.Setprogress(const Value: TGauge);
begin
  Fprogress := Value;
end;

procedure TWDump.SetTableNames(const Value: string);
begin
  FTablenames := Value;
end;

procedure TWDump.SetValuesPerInsert(const Value: Integer);
begin
  FValuesPerInsert := Value;
end;

procedure Register;
begin
  RegisterComponents('Zeos Access', [TWDump]);
end;

end.

